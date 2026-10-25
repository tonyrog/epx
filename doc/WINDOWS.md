# epx on Windows: porting notes

Status: **an untested draft**. `c_src/epx_backend_win32.c` was written
against the documented Win32 API without access to a Windows compiler. It
has only been syntax checked on Linux with stub headers. The NIF and
the rest of the C library have **not** been ported yet. This file lists
what is still missing and the order to do it in.

## 1. The win32 backend (`c_src/epx_backend_win32.c`)

Plain Win32 + GDI, no extra dependencies (links with `-lgdi32 -luser32`).

* **GUI thread.** `win32_init` starts its own thread with
  `_beginthreadex`. That thread owns every HWND and runs a
  `GetMessage`/`DispatchMessage` loop. A Win32 window only gets messages
  on the thread that created it, and only that thread may destroy it.
  The epx callbacks run on the NIF `backend_main` thread. They hand
  create, destroy, adjust and sync work to the GUI thread with
  `SendMessage` to a hidden message-only window (`HWND_MESSAGE`).
  `SendMessage` is synchronous, so the caller gets the result back.
* **Back buffer.** Each window has a 32-bit top-down DIB section
  (memory layout B,G,R,X, which is `EPX_FORMAT_BGRA`) selected into a
  memory DC. `pix_draw` converts and copies pixmap pixels into it with
  `epx_copy_area`. Any epx pixel format with at least 8 bits per pixel
  works, and BGRA pixmaps take the fast memcpy path. `pix_draw` makes no
  GDI calls and holds a per-window `CRITICAL_SECTION`. `win_swap` posts
  `WM_EPX_FLUSH` (coalesced), and the GUI thread `BitBlt`s the dirty
  rectangle to the screen. `pix_sync` does a synchronous flush followed
  by `UpdateWindow`. The buffer grows by 50% and never shrinks, as in
  x11. `WM_PAINT` repaints from the back buffer and reports
  `EPX_EVENT_EXPOSE`, which is what x11 does with backing store.
* **Events.** The window procedure turns messages into `epx_event_t`
  and pushes them into a ring buffer (1024 entries, consecutive motion
  events merged). A **manual-reset event object** is signalled while the
  ring is non-empty, and `evt_attach` returns that `HANDLE` in place of
  an fd. `evt_read` pops events, applies the same `FilterEvent` as x11,
  and returns `1+pending`.

Message mapping:

| Win32 | epx |
|---|---|
| `WM_CLOSE` | `EPX_EVENT_CLOSE` (the window is *not* destroyed) |
| `WM_SIZE`, `WM_MOVE` | `EPX_EVENT_CONFIGURE` with the client area origin in screen coordinates and its size (deduplicated, updates `rarea`) |
| `WM_PAINT` | blit from the back buffer + `EPX_EVENT_EXPOSE` |
| `WM_SETFOCUS` / `WM_KILLFOCUS` | `FOCUS_IN` / `FOCUS_OUT` |
| `WM_MOUSEMOVE` | `POINTER_MOTION` with button state; the first one sends `ENTER` and starts `TrackMouseEvent` |
| `WM_MOUSELEAVE` | `LEAVE` |
| `WM_[LMR]BUTTONDOWN/UP` | `BUTTON_PRESS/RELEASE`, with `SetCapture` while a button is held |
| `WM_MOUSEWHEEL`, `WM_MOUSEHWHEEL` | press+release pairs with `EPX_WHEEL_UP/DOWN/LEFT/RIGHT` per 120 units, as x11 does with buttons 4-7 |
| `WM_(SYS)KEYDOWN/UP` | `KEY_PRESS/RELEASE`, see below |
| `WM_GETMINMAXINFO` | enforces `min_width/min_height/max_width/max_height` |

Keyboard:

* The loop does **not** call `TranslateMessage`. On key down,
  `key_event` maps virtual keys to `EPX_KBD_KEY_*` (arrows, keypad with
  and without NumLock, F1-F12, modifiers left/right, Pause, Print, Menu)
  and gets characters from `ToUnicode`, which keeps dead keys working.
  The sym is saved by scan code and reused for the matching release.
* `key.mod` is built from `GetKeyState`: L/R shift, ctrl, alt and win
  (meta), the NumLock/CapsLock/ScrollLock toggles, and AltGr
  (LCtrl+RAlt). This is more complete than x11, which only keeps the
  lock bits plus the modifier key itself.
* Auto-repeat comes as repeated key-down messages (lParam bit 30). They
  are dropped when the window mask has `EPX_EVENT_NO_AUTO_REPEAT`.
  Without that flag, x11 sends release+press pairs, but win32 sends only
  presses.
* `key.code` is scan code | extended<<8.
* `key.sym` may be any BMP character below `0xF800`. x11 truncates to
  Latin-1.

Parameters:

* backend_open / backend_adjust: `dpi_aware` (bool, opt-in, calls
  `SetProcessDPIAware` for the whole erl process). `use_opengl` is
  accepted and ignored.
* window_adjust: `x`, `y`, `width`, `height` (client area; the frame is
  added with `AdjustWindowRectEx`), `name` (UTF-8, or Latin-1 if it is
  not valid UTF-8), `show`, `select` (restore, raise and
  `SetForegroundWindow`), `min_width`, `min_height`, `max_width`,
  `max_height`. `border_width` is ignored. A min or max value that is
  not set means no limit; x11 uses the current size instead.
* backend_info: `display` (HINSTANCE), `double_buffer`, `use_exposure`,
  `use_opengl`.
* window_info: `name`, `window` (the HWND as a binary).

Not implemented: OpenGL/WGL, grab/grab_key, cursors, `EPX_EVENT_RESIZE`
(x11 never sends it in practice), per-monitor DPI, X buttons, and NIF
hot upgrade (see open issues).

## 2. Changes needed in `c_src/epx_nif.c`

The NIF backend loop expects a pollable unix fd from `evt_attach` and
uses a `pipe()` to wake its threads. Neither exists on Windows.

1. **Includes:** `<unistd.h>` and `<poll.h>` (already marked
   `// WINDOWS fixme`) become `#ifdef _WIN32 #include <windows.h> #else ... #endif`.
2. **Thread wakeup:** in `epx_thread_t`, replace `int wake[2]` with a
   **semaphore** (`HANDLE wake`). An auto-reset event is not enough
   because the pipe counts one token per message:
   * `epx_thread_start`: `thr->wake = CreateSemaphoreW(NULL, 0, LONG_MAX, NULL)`
   * `epx_thread_wakeup`: `ReleaseSemaphore(thr->wake, 1, NULL)`
   * `epx_thread_wokeup`: `WaitForSingleObject(thr->wake, INFINITE)`
   * `epx_thread_stop` / error path: `CloseHandle(thr->wake)`.
3. **`backend_poll`:** replace `poll()` with:
   ```c
   HANDLE hs[2] = { (HANDLE) message.handle, self->wake };
   DWORD r = WaitForMultipleObjects(2, hs, FALSE, INFINITE);
   if (r == WAIT_OBJECT_0)        { /* send EPX_MESSAGE_EVENT_READY */ }
   else if (r == WAIT_OBJECT_0+1) {
       /* the wait already took the semaphore token: use
        * epx_queue_get(&self->q, &sys_message) directly,
        * NOT epx_message_recv (that would wait for a second token) */
   }
   ```
   The backend handle is a manual-reset event. It stays signalled until
   `evt_read` drains the queue. That is safe because `backend_poll` only
   waits again after `backend_main` has read the events and sent a new
   `EPX_MESSAGE_POLL`. Use `message.handle != EPX_INVALID_HANDLE` to
   decide whether to wait on one handle or two (the `none` backend).
4. **Casts:** `(int)((long)message.handle)` truncates on Win64, where
   `long` is 32 bits (LLP64). Keep the handle as a pointer, or use
   `intptr_t`. Check other `long` ⇄ pointer casts too, for example in
   `epx_lock.c`'s callback return value.
5. **Threads:** `enif_thread_*` / `enif_mutex_*` / `enif_cond_*` are
   already portable. The `pthread_t` code and
   `erl_drv_steal_main_thread` are only built under `__APPLE__`, so
   nothing changes there. Windows has no main-thread requirement for
   GUI, so `DO_NOT_STEAL` is fine.
6. **NIF linking:** on Windows, a NIF DLL does not link against erts.
   `erl_nif.h` turns every `enif_*` call into a call through the
   `WinDynNifCallbacks` table, which `ERL_NIF_INIT` fills in. That works
   with a MinGW-built DLL loaded into the MSVC-built `erl.exe`. Rules
   that follow from this: free memory with the allocator that allocated
   it, and do not pass `FILE*` or CRT fds across the DLL boundary.

## 3. Other C library issues

Ranked by how surely they break things:

1. **`BYTE_ORDER`:** `include/epx_pixel.h` (and `epx_gl.c` and others)
   test `#if BYTE_ORDER == BIG_ENDIAN` without including anything that
   defines it. On glibc it comes in through `<sys/types.h>`. If MinGW
   does not define it, the test becomes `0 == 0` and **every pixel is
   handled as big endian**. Fix with a portable block in `epx_pixel.h`:
   ```c
   #if !defined(BYTE_ORDER) && defined(__BYTE_ORDER__)
   #define BYTE_ORDER    __BYTE_ORDER__
   #define LITTLE_ENDIAN __ORDER_LITTLE_ENDIAN__
   #define BIG_ENDIAN    __ORDER_BIG_ENDIAN__
   #endif
   ```
   or add `-DBYTE_ORDER=1234 -DLITTLE_ENDIAN=1234 -DBIG_ENDIAN=4321` to
   the WIN32 CFLAGS.
2. **`c_src/epx_gl.c`** includes `<endian.h>` unconditionally, and
   MinGW-w64 does not provide it. Guard it with
   `#if !defined(_WIN32)`. The file is in `LIB_OBJS`, so the build fails
   without the guard.
3. **`mmap`:** `epx_font.c`, `epx_sft.c` and `epx_anim.c` use
   `mmap`/`<sys/mman.h>`. Replace it on Windows with
   `CreateFileMapping`/`MapViewOfFile`, or with malloc and read.
   `schrift.c` already has a `_WIN32` mapping path.
4. **Binary file mode:** `open(path, O_RDONLY)` and `fopen(file, "r")`
   open files in text mode on Windows, which corrupts binary font and
   animation data. Use `O_RDONLY|O_BINARY` and `"rb"`.
5. **Compiler:** `epx_debug.h` uses GNU named variadic macros
   (`args...`), `epx_simd.c` uses GCC inline asm for `cpuid`, and the
   code uses designated initializers. MinGW-w64 gcc or clang handle all
   of these. **MSVC does not**: it needs `__VA_ARGS__` and `__cpuid`.
   Use MinGW-w64 (MSYS2) first.
6. SIMD: the SSE2/AVX2/MMX files should build with gcc on x86_64. ARM64
   Windows (NEON) is untested.

## 4. Erlang side

* `src/epx_backend.erl`, `assumed_backend/0` has no Windows clause and
  always returns `"none"` there. Add:
  ```erlang
  {win32,_} ->
      case is_backend("win32") of
          true -> "win32";
          false -> "none"
      end;
  ```
  (Setting `EPX_BACKEND=win32` already works.)
* `epx:init/0` loads `priv/epx_nif` without an extension, and
  `erlang:load_nif` adds `.dll`. The DLL must therefore be
  `priv/epx_nif.dll`, and `c_src/Makefile` now names it that on MinGW.
* `priv/epx-config` is a sh script. It is only used by C tools, not at
  runtime.

## 5. Building (MSYS2 / MinGW-w64), untested

1. Install Erlang/OTP for Windows (the official installer) and
   [MSYS2](https://www.msys2.org/).
2. In an **MSYS2 UCRT64** shell:
   `pacman -S mingw-w64-ucrt-x86_64-gcc make`
3. `c_src/Makefile` finds `ERL_TOP` from `which erl`, which does not
   work with `erl.exe` or with paths that contain spaces. Give a path
   without spaces, for example a junction:
   ```sh
   cmd //c mklink //J C:\\erl "C:\\Program Files\\Erlang OTP"
   make -C c_src nif ERL_TOP=/c/erl
   ```
   `erl_nif.h` must be in `$(ERL_TOP)/usr/include`. If it is not, use
   `$(ERL_TOP)/erts-*/include`.
4. `c_src/Makefile` now detects MinGW from `uname -s`
   (`MINGW*`/`UCRT*`/`CLANG*`). It then sets `WIN32=Yes`, which adds
   `-DWIN32`, `epx_backend_win32.o`, `-lgdi32 -luser32`, `$(CC) -shared`
   and the `.dll` name. Linux and macOS builds are not affected.
5. Compile the Erlang code (`cd src; make`, or `rebar3 compile` with a
   pre-built DLL). Then run
   `erl -pa ebin` and `epx:start()`, or start with `EPX_BACKEND=win32`
   set in the environment.

**rebar.config** has no Windows `port_specs` yet. It would need roughly
`{"win32", "priv/epx_nif.dll", [... same list as linux, with
"c_src/epx_backend_win32.c" instead of x11/xcb/fb ...]}`, plus
`{"win32", "CFLAGS", "$CFLAGS -DWIN32"}` and
`{"win32", "LDFLAGS", "$LDFLAGS -lgdi32 -luser32"}`. The `post_hooks`
use `mv`, `ar`, `ranlib` and `sed`, so they need an MSYS2 shell. On
Windows, the rebar port compiler defaults to MSVC (`cl.exe`); set
`CC=gcc`. Using the Makefile is easier.

## 6. First test checklist

1. `epx:backend_list()` contains `"win32"`.
2. Open a window, fill a BGRA pixmap and `pixmap_draw` it. Check the
   colours (B and R swapped means a BYTE_ORDER or format problem).
   Repeat with an RGB/ARGB pixmap to exercise the conversion path.
3. Resize, minimize/restore and cover/uncover the window, and check that
   CONFIGURE and EXPOSE arrive and the content stays.
4. Close button → `{epx_event, Win, close}`. The window must stay until
   it is detached.
5. Keyboard: letters with shift/caps, a dead key (´ + e → é), AltGr
   characters, the keypad with NumLock on and off, F1-F12, arrows,
   Ctrl+letter (control codes), Alt+F4.
6. Mouse: buttons, drag outside the window (capture), wheel, horizontal
   wheel, enter/leave.
7. Close and re-open the backend several times (thread and class
   cleanup).

## 7. Open issues, most important first

1. **NIF event loop** (section 2). Without it nothing runs: `poll()` and
   `pipe()` do not exist.
2. **BYTE_ORDER / endian.h / mmap / binary mode** (section 3). Without
   these the library either fails to build or draws wrong colours and
   loads broken fonts.
3. **First compile and run on real Windows.** The backend has never
   been compiled with real headers. Likely trouble spots:
   `GetModuleHandleExW` with a function address, `SetProcessDPIAware`
   needing `_WIN32_WINNT >= 0x0600`, and `WM_MOUSEHWHEEL`.
4. **Build integration:** a rebar.config Windows port spec, the
   `epx_backend.erl` default backend clause, and Erlang paths with
   spaces.
5. **NIF upgrade/reload:** the GUI thread and the registered window
   procedures live in the loaded DLL. `win32_upgrade` only swaps the
   callback table, so unloading the old DLL while the backend is open
   would crash. A real upgrade must stop and restart the GUI thread and
   re-register the classes.
6. **Shutdown:** `win32_finish` does not detach windows that are still
   open (x11 does not either). Their `Win32Window` structs leak, and the
   HWNDs die with the GUI thread.
7. **Keyboard semantics:** x11 differences (full modifier state,
   repeat without release, Unicode syms). AltGr also produces a fake
   LCtrl press. Decide what epx apps expect.
8. **High DPI:** without `dpi_aware` Windows bitmap-scales the window
   (blurry). With it, sizes are physical pixels. Per-monitor awareness
   (`WM_DPICHANGED`) is not handled.
9. **OpenGL** (WGL), cursors, grab, clipboard, and IME
   (`WM_IME_*`/`WM_CHAR` for CJK input, which `ToUnicode` does not
   cover): not done.
10. **Performance:** every frame is converted into the DIB section, then
    BitBlt'ed. For BGRA pixmaps, `SetDIBitsToDevice` straight from the
    pixmap (`BI_RGB`, or `BI_BITFIELDS` for 565/RGBA) would skip one
    copy, but GDI calls would then have to run on the drawing thread.
