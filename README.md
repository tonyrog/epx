Erlang PiXel library
====================

epx is a library for manipulating pixels. Various operation exist
to operate on the pixels.

To create a small pixmap and set some pixels you do not even have to
start the epx application, just do:

    Pixmap = epx:pixmap_create(10, 10, rgb),
    epx:pixmap_fill(Pixmap, red),
    epx_pixmap_put_pixel(Pixmap, 4, 4, white).

Now you can read the pixels:
 
    {A,R,G,B} = epx:pixmap_get_pixel(Pixmap, 3, 3).
    <<R,G,B>> = epx:pixmap_get_pixels(Pixmap, 3, 3, 1, 1).

# Backends

    x11       X11 (Xlib), the default when DISPLAY is set
    xcb       X11 (xcb)
    wayland   Wayland (xdg-shell, shm buffers), used when only WAYLAND_DISPLAY
              is set, or with EPX_BACKEND=wayland / {epx,[{backend,"wayland"}]}
    fb        Linux frame buffer + evdev input
    macos     macOS (cocoa)
    win32     Windows GDI, UNTESTED draft, see doc/WINDOWS.md
    none      no display

The backend is selected with the EPX_BACKEND environment variable or the
`backend` application environment. If no backend can be opened epx:start()
still returns ok, but window_attach/1 and pixmap_attach/1 raise
`{no_backend,default}`.

Note that the window manager `close` event is always sent to the window
owner, independent of the event mask given to window_create/5.

# Fonts

epx draws text with efnt fonts (pre-rendered glyph bitmaps). The font
server (epx_font) loads the .efnt files in the font path, and also
registers the TrueType (.ttf) files there as scalable fonts. When a
size is asked for that has no efnt font, it is generated from the
ttf file with libschrift and kept in a disk cache, so every size is
available:

    {ok,Font} = epx_font:match([{name,"Arial"},{size,17},{weight,bold}]),
    epx_gc:set_font(Font),
    epx_gc:set_foreground_color({0,0,0,0}),   %% text color, alpha 0
    epx:draw_string(Pixmap, X, Y, "Hello").

    epx_font:i().                 %% list fonts, ttf fonts show size *
    epx_font:i([{name,"Arial"}]).
    epx_font:scalable().          %% [{Family,Style,File}]
    epx_font:cache_clear().       %% remove generated fonts not in use

An efnt font in the exact size is used when there is one. A generated
font is made once and then used by all processes. It is written to
the cache directory, so the next start only has to load it from disk.
Generation takes about 20-100 ms. The generated fonts are not hinted,
so their advance widths follow the ttf file and can be a pixel
narrower than the hand made efnt fonts.

Application environment (epx):

    font_path        ["dir",...] extra directories with .efnt/.ttf files
                     (priv/fonts and priv/truetype are always used)
    font_cache_dir   directory for generated fonts
                     (default: the user cache dir, epx/fonts)
    font_cache_size  max bytes in the cache (default 64M); the oldest
                     fonts that are not loaded are removed

`epx_efnt:write_ttf(TTF, Size, File)` makes an efnt file directly.

# Documentation

    doc/EPXW.md     epxw window behaviour: callbacks, client api, drawing
    doc/TEXT.md     glyphs, transforms, text along a path (epx_text)
    doc/EPXY.md     epxy widget api
    doc/WINDOWS.md  notes for the win32 backend

# Tests

    make
    erlc -o test test/*.erl
    # regression tests (pixmap/draw defects, see epx_bugs.md, png reader, epxy names)
    erl -pa ebin -pa test -noshell -eval 'eunit:test(epx_regression_tests,[verbose]),halt().'
    # epxw window behaviour (redraw merging, partial update, client api, widgets)
    erl -pa ebin -pa test -noshell -eval 'eunit:test(epxw_tests,[verbose]),halt().'
    # epxy widgets (windows, buttons, sliders, text, menus, panels)
    erl -pa ebin -pa test -noshell -eval 'eunit:test(epxy_tests,[verbose]),halt().'
    # ttf -> efnt generation and the font cache
    erl -pa ebin -pa test -noshell -eval 'eunit:test(epx_font_cache_tests,[verbose]),halt().'
    # transformed TrueType text, strings and paths
    erl -pa ebin -pa test -noshell -eval 'eunit:test(epx_text_tests,[verbose]),halt().'
    # TrueType (libschrift) rendering: headless check, png and window
    erl -pa ebin -pa test -noshell -eval 'test_sft_text:check(),halt().'
    erl -pa ebin -pa test -noshell -eval 'test_sft_text:png("/tmp/sft.png"),halt().'
    erl -pa ebin -pa test -noshell -eval 'test_sft_text:show(8000),halt().'
    # wayland backend
    erl -pa ebin -pa test -noshell -s test_wayland run -s init stop

# Available backend parameters for backend_open / backend_adjust / window_adjust 

## x11 backend

    x11_display             string
    use_opengl              bool|int       0|1    (0)
    use_off_screen          bool|int       0|1    (1)
    use_expsure             bool|int       0|1    (0)
    
## x11 window

    x                       int
    y                       int
    width                   int
    height                  int
    border_width            int
    show                    int            0|1
    select                  int            0|1
    min_width               int
    minheight               int
    max_width               int
    max_height              int
    name                    string
    
%% macos backend (cocoa)

    use_opengl              int            0|1  (0)
    
## macos window

    width                   int
    height                  int
    show                    bool|int       0|1
    select                  bool|int       0|1

## fb backend

    framebuffer_device      string    (/dev/fb0)
    direct_pixmap_draw	    bool|int   0|1  (0)
    double_buffer	        bool|int   0|1  (1)
    lcd_pi32		        bool|int   0|1  (0)
    input_mouse_device      string
    input_keyboard_device   string
    input_absolute_device   string
    input_relative_device   string
    
    width 	    	    int
    virt_width		    int
    height		        int
    virt_height		    int
    pixel_format	    string
    pixel_type		    int
    pixclock		    int
    xoffset		        int
    yoffset		        int
    left_margin		    int
    right_margin        int
    upper_margin        int
    lower_margin	    int
    hsync_len		    int
    vsync_len		    int
    sync		        uint
    vmode		        uint
    
# epx on frame buffer device (linux)

    epx:debug(debug).
    epx:start().
    {ok, BID } = epx_backend:create("fb", [{backend,"fb"}, {width, 1280}, {height, 1024}, {input_mouse_device, "/dev/input/event7"}, {input_keyboard_device, "/dev/input/event5"}]).
    B = epx_backend:backend(BID).
    W=epx:window_create(0,0,1280,1024, [button_press, key_press, key_release]).
    epx:window_attach(W, B).


## howto run framebuffer on vmware (ubuntu)

Append "vmwgfx.enable_fbdev=no" to GRUB_CMDLINE_LINUX_DEFAULT in file /etc/default/grub. "no" is not a correct value, driver will complain. But this is a bug workaround :-)

May look something like this:

    GRUB_CMDLINE_LINUX_DEFAULT="splash quiet vmwgfx.enable_fbdev=no"

Then the fb driver may be probed:

    sudo modprobe uvesafb

## Packages needed to build epx using top-level Makefile on Raspberry pi and Ubuntu

	sudo apt-get install libx11-dev autoconf libgif-dev
