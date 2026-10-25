# epxw – window behaviour

`epxw` is a `gen_server` that owns one epx window. It handles the
window chrome: scroll bars, tool bars, a status bar, zoom, a context
menu and the selection rectangle. The application supplies callbacks
that draw the content and react to events.

```
          +----------------------- window ------------------------+
          | top bar                                    draw(top)  |
          +-------+-------------------------------------+--+------+
          | left  |  content (view coordinates)         |V | right|
          | bar   |  draw/3 or draw(content,...)        |  | bar  |
          |       |                                     |  |      |
          |       +-------------------------------------+--+      |
          |       |  horizontal scroll bar              |  |      |
          +-------+-------------------------------------+--+------+
          | bottom bar / status bar                 draw(bottom)  |
          +-------------------------------------------------------+
```

All callbacks run in the epxw process, one at a time. The window, the
pixmaps and the epx state belong to that process.

## Starting

```erlang
epxw:start(User) -> {ok,Pid} | ignore | {error,Reason}
epxw:start(User, UserArgs, Options) -> {ok,Pid} | ignore | {error,Reason}
epxw:start_link(User, UserArgs, Options) -> {ok,Pid} | ignore | {error,Reason}
```

* `User` is a callback module, or a map of callback funs. A map may also
  name a module: `#{ module => Mod, draw => fun my_draw/3 }`. A fun in
  the map overrides the module function with the same name. Callbacks
  that are missing, or that have the wrong arity, are simply not
  called.
* `UserArgs` is passed to `init/1`.
* `Options` is a property list that sets the window options and the
  profile (see below). When `User` is a module, the application
  environment of that module is appended to the options.

### Module style

```erlang
-module(hello).
-behaviour(epxw).
-export([start/0, init/1, draw/3, key_press/2]).

start() -> epxw:start(?MODULE, [], [{title,"Hello"},{width,320},{height,200}]).

init([]) -> {ok, #{ text => "Hello" }}.

draw(Pixels, _Dirty, State = #{ text := Text }) ->
    epx_gc:set_foreground_color(black),
    epx:draw_string(Pixels, 10, 40, Text),
    State.

%% type to append text
key_press({key_press,Sym,_Mod,_Code}, State = #{ text := Text })
  when is_integer(Sym), Sym >= $\s, Sym < 127 ->
    epxw:invalidate(),
    State#{ text => Text ++ [Sym] };
key_press(_Event, State) ->
    State.
```

### Map style

The map style suits small tools, tests and code that builds its
callbacks at runtime:

```erlang
epxw:start(#{ init => fun(_) -> {ok, 0} end,
              draw => fun(Pixels, _Dirty, N) -> draw_counter(Pixels, N), N end,
              key_press => fun(_, N) -> epxw:invalidate(), N+1 end },
           [], [{width,200},{height,100}]).
```

### Options

| option | default | meaning |
|---|---|---|
| `width`, `height` | 640, 480 | size of the content area (bars and scroll bars are added) |
| `title` | `"Untitled"` | window title |
| `x`, `y` | 50, 50 | window position |
| `widgets` | `[]` | named widgets to create at start, `[{Name, Props}]` (see *Named widgets*) |
| `fps` | 30 | frame rate of widget animations |
| `pixel_format` | `argb` | format of the window pixmap |
| `view_width`, `view_height` | width, height | initial size of the view (scrollable document) |
| `scale`, `xscale`, `yscale` | `{1,1}` | initial view scale |

Every field in the profile can also be given as an option:

| profile option | default | meaning |
|---|---|---|
| `scheme` | `logo` | colour scheme used to look up the colour names below |
| `screen_color` | `grey2` | content background |
| `top_bar`, `left_bar`, `right_bar`, `bottom_bar` | 0, 0, 0, 18 | tool bar sizes in pixels, 0 = no bar |
| `status_bar` | `true` | the bottom bar shows `set_status_text/1`; when `false` the bottom bar is drawn by `draw(bottom,...)` |
| `scroll_bar_size`, `scroll_hndl_size` | 16, 10 | 0 hides the scroll bars |
| `scroll_horizontal`, `scroll_vertical` | `bottom`, `right` | scroll bar placement (`top`/`bottom`, `left`/`right`) |
| `scroll_xstep`, `scroll_ystep` | 4, 4 | step for arrow keys and the wheel |
| `max_dirty` | 10 | dirty rectangles kept before they are merged into one |
| `dirty_list` | `false` | `draw` gets a list of rectangles instead of a single rectangle |
| `partial_update` | `true` | redraw only the dirty area, and send only that area to the window (see *Drawing*) |
| `menu_*`, `window_font_*`, `*_color` | | fonts and colours for the menu, status and bars |

A window without chrome, as an embedded display or a widget window
would want, uses `[{bottom_bar,0},{scroll_bar_size,0}]`.

## Two ways to talk to the window

### Inside a callback

The `epxw:*` API below works on the state of the calling epxw
process. That state is kept in the process dictionary while a callback
runs. A change made through the API, such as `invalidate/1` or
`set_view_rect/1`, is picked up when the callback returns.

These functions can only be called from inside a callback, from the
epxw process itself. Anywhere else they raise
`error(not_in_epxw_callback)`.

### From another process (client mode)

Other processes talk to the window process with ordinary messages.

| mechanism | handled by |
|---|---|
| `gen_server:call(Win, Request)` | `handle_call/3` |
| `gen_server:cast(Win, Request)` | `handle_cast/2` |
| `Win ! Msg` (timers, ports, subscriptions) | `handle_info/2` |
| `epxw:call(Win, Fun)` | `Fun(UserState) -> {Reply, NewUserState}`, run inside the window |
| `epxw:call(Win, Fun, Timeout)` | as `call/2`, with a timeout |
| `epxw:cast(Win, Fun)` | `Fun(UserState) -> NewUserState`, run inside the window |

The fun in `epxw:call/2` and `epxw:cast/2` runs as a callback, so the
whole callback API can be used in it:

```erlang
%% from any process
epxw:cast(Win, fun(S) -> epxw:set_status_text("saved"), S end),
{W,H} = epxw:call(Win, fun(S) -> {{epxw:width(), epxw:height()}, S} end).
```

A window without a `handle_call/3` stops with `missing_handle_call` when
it receives a plain `gen_server:call`. The `epxw:call/2` form needs no
`handle_call/3`.

## Callbacks

Every callback is optional. "State" below means the user state that
`init/1` returned. Positions in events are given in **view
coordinates**, unless the table says otherwise (see *Coordinates*).

| callback | called when | arguments | returns | typical use |
|---|---|---|---|---|
| `init(UserArgs)` | at start, after the window is created | `UserArgs` from `start` | `{ok,S}` \| `{ok,S,Timeout\|hibernate\|{continue,C}}` \| `{stop,R}` \| `ignore` | create the state; may use the API (`set_view_size`, `profile_set` ...) |
| `configure(Rect, S)` | right after `init`, and when the window is resized | `{X,Y,W,H}` in **window** coordinates | `S` | lay out content for the new size |
| `draw(Pixels, Dirty, S)` | when the content needs redrawing | the pixmap, with the view transform and a clip set; `Dirty` = view rect (or a list when `dirty_list`) | `S` | draw the content. The dirty area has already been cleared to `screen_color` |
| `draw(Where, Pixels, Dirty, S)` | content (only if there is no `draw/3`), and every redraw of a tool bar | `Where` = `content`\|`top`\|`left`\|`right`\|`bottom`; tool bars use **window** coordinates, clipped to the bar | `S` | draw tool bars, or the bottom bar when `status_bar` is false |
| `key_press(Event, S)` | key pressed | `{key_press, Sym, Mods, Code}` | `S` | keyboard input. `command/3` runs first |
| `key_release(Event, S)` | key released | `{key_release, Sym, Mods, Code}` | `S` | |
| `command(Sym, Mod, S)` | before key_press, and when a menu item is chosen | `Sym` = key symbol or menu command; `Mod` = `#keymod{shift,ctrl,alt}` | `{noreply,S}` (consumed) \| `{reply,{Sym1,Mod1},S}` (epxw runs its own handling for `Sym1`) | shortcuts such as Ctrl+S, and menu actions. Return `{reply,{Sym,Mod},S}` to keep scrolling and zoom on arrows, PgUp/PgDn, Home/End and `+`/`-` |
| `button_press(Event, S)` | mouse button pressed outside the scroll bars and the menu | `{button_press, [Button], {X,Y}}` | `S` | clicks. A left press also starts a selection, enables motion and redraws everything |
| `button_release(Event, S)` | button released | `{button_release, [Button], {X,Y}}` | `S` | end of click or drag. For the wheel, epxw scrolls after the callback |
| `motion(Event, S)` | pointer moves, while a left drag is active or after `enable_motion()` | `{motion, Buttons, {X,Y}}` | `S` | drag and hover. Queued motion events are dropped, so only the latest position arrives |
| `select({Phase, Rect}, S)` | left press (`start`), drag (`continue`), release (`stop`) | `Phase`, `Rect` = selection rectangle | `S` | rubber band selection. Call `invalidate(OldRect)` and `invalidate(Rect)` to have it drawn |
| `menu({menu, Pos}, S)` | right press, and on every redraw while the menu is shown | `Pos` in view coordinates | `{reply, Menu, S}` \| `{reply, undefined, S}` \| `{noreply, S}` | context menu. `Menu` comes from `epx_menu:create/2`; the chosen item arrives in `command/3`. With no menu, `button_press` is called instead |
| `enter(Event, S)`, `leave(Event, S)` | pointer enters or leaves the window | `{enter,{X,Y}}`, `{leave,{X,Y}}` | `S` | hover effects |
| `focus_in(focus_in, S)`, `focus_out(focus_out, S)` | keyboard focus changes | | `S` | cursor blink, and so on |
| `close(S)` | the user closes the window | | `{noreply, S}` keeps the window open, anything else stops it | clean up, ask to save, or veto the close |
| `widget(Name, Event, S)` | a named widget signals | `Name` = widget name, `Event` = map with `event` (`button_press`, `button_release`, `changed`, `motion`, `focus_in`, `focus_out`) and `value`, `xy` ... | `S` | react to buttons, sliders, text input (see *Named widgets*) |
| `handle_call(Req, From, S)` | `gen_server:call` | as in gen_server | as in gen_server, with the user state | client requests |
| `handle_cast(Req, S)` | `gen_server:cast` | | `{noreply,S}` ... | |
| `handle_info(Msg, S)` | other messages | | `{noreply,S}` ... | timers, animation, data feeds |
| `handle_continue(C, S)` | `{continue,C}` | | `{noreply,S}` ... | |
| `terminate(Reason, S)` | the server stops | | ignored | cleanup |
| `code_change(Old, S, Extra)` | code upgrade | | `{ok,S}` | |

Events from the window to the callbacks (except `handle_*`) use the
same event tuples as `epx`, see `epx:window_create/5`. The event mask
of the window is set from the callbacks that exist. The key and button
events are always on, because epxw uses them for scrolling.

A crash in an event or draw callback is printed, and the state from
before the callback is kept. A crash in a `handle_*` callback stops the
server.

## Callback API (inside callbacks)

| function | description |
|---|---|
| `invalidate()` | mark all content dirty |
| `invalidate(Rect)` | mark a view rectangle dirty |
| `draw(Fun)` | draw now: `Fun(Pixels, VisibleRect)` with the view transform and the clip set. Only the area invalidated in `Fun` (or everything, if nothing was invalidated) is sent to the window. Returns the result of `Fun` |
| `draw(Where, Fun)` | draw now into `content`, `top`, `left`, `right` or `bottom`. The bars use window coordinates |
| `draw_menu_pixels(Pixels)` | redraw an open menu on top of the content, from inside `draw/1` |
| `window()`, `screen()`, `pixels()` | the epx window, the window pixmap, and the pixmap to draw in |
| `width()`, `height()` | content size in pixels |
| `keymod()` | current `#keymod{}` |
| `view_rect()`, `set_view_rect(R)`, `union_view_rect(R)`, `set_view_size(W,H)` | the scrollable document area, in view coordinates |
| `view_pos()`, `view_xpos()`, `view_ypos()`, `set_view_pos(X,Y)`, `set_view_xpos(X)`, `set_view_ypos(Y)` | the scroll position, which is the view point shown at the top left |
| `view_width()`, `view_height()` | size of the view rectangle |
| `visible_rect()` | the visible part of the view |
| `scale()`, `set_scale(Sx,Sy)`, `set_xscale(Sx)`, `set_yscale(Sy)`, `zoom()` | view scale; `+`/`-` change the zoom by steps of 1.07² |
| `view_to_window_x/y/pos/width/height/rect`, `window_to_view_x/y/pos/width/height/rect` | coordinate conversion |
| `enable_motion()`, `disable_motion()` | turn motion events on or off outside drags |
| `set_status_text(Text)` | text in the status bar |
| `set_menu(Menu)`, `set_menu(Menu, Pos)` | open a menu (centred, or at `Pos`) |
| `file_menu()`, `edit_menu()` | ready-made menus |
| `profile_get(Key)`, `profile_set(Key,Value)` | profile options |
| `widget_new(Name,Props)`, `widget_set(Name,Props)`, `widget_get(Name,Keys)`, `widget_delete(Name)`, `widgets()` | named widgets |
| `window_profile_get/set`, `window_info_get/set` | colours, fonts, bar sizes and scroll bar layout |

## Coordinates

* **Window coordinates** are pixels in the window, with the origin at
  the top left. They are used for tool bars, `configure` and
  `draw(top|left|right|bottom, ...)`.
* **View coordinates** are the coordinates of your document. They are
  used for content drawing and for every pointer event. The mapping
  from view to window is

  ```
  Wx = (Vx - view_xpos) * Sx + Cx
  Wy = (Vy - view_ypos) * Sy + Cy
  ```

  `{Cx,Cy}` is the top left corner of the drawing area, inside the tool
  bars and any top or left scroll bar.

The scroll bars show up when the scaled view rectangle is larger than
the drawing area. Drawing in `draw/3` happens with the pixmap transform
(ltm) set, so you draw in view coordinates and epx scales and
translates for you.

## Drawing and performance

* **Dirty area.** `invalidate/0,1` adds to the dirty area. Up to
  `max_dirty` rectangles are kept, after which they are merged into
  their bounding box.
* **Redraws are merged.** After an event, cast or info message, the
  redraw is scheduled as a message to the window itself. A burst of
  events, such as fast key repeat or a stream of `handle_info` updates,
  gives a single `draw` call. After `handle_call` and `epxw:call/2` the
  redraw runs before the reply is sent, so the caller knows the screen
  is up to date.
* **Partial update.** When only a part of the content is dirty, the
  content is clipped to the dirty bounding box while `draw` runs. Only
  the dirty area and the thin chrome strips (bars, scroll bars, status)
  are sent to the window. Drawing everything in `draw` is still
  correct, it just costs more. Something drawn outside the invalidated
  area does not show until that area is invalidated, so invalidate both
  the old and the new position of anything that moves. If an
  application relies on the old full-window update,
  `{partial_update,false}` restores it.
* **Full redraws.** `configure`, scrolling, zoom, a left press or
  release and the menu all redraw everything.
* **Animation** can use `epxw:draw(Fun)` from `handle_info`. It draws
  only what `Fun` invalidates, and skips the clear and the chrome
  redraw. `epxw_draw_demo` in epx_demo shows this.
* Motion and wheel events that have queued up are dropped, and so are
  all configure events except the last.

## Example: drag a box

```erlang
-module(dragbox).
-behaviour(epxw).
-export([start/0, init/1, draw/3, button_press/2, button_release/2, motion/2]).

start() -> epxw:start(?MODULE, [], [{width,400},{height,300}]).

init([]) -> {ok, #{ box => {50,50,40,40}, drag => false }}.

draw(Pixels, _Dirty, S = #{ box := Box }) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(red),
    epx:draw_rectangle(Pixels, Box),
    S.

button_press({button_press,[left],{X,Y}}, S = #{ box := Box }) ->
    S#{ drag => epx_rect:contains(Box, {X,Y}) };
button_press(_, S) -> S.

button_release(_, S) -> S#{ drag => false }.

motion({motion,[left],{X,Y}}, S = #{ drag := true, box := Box={_,_,W,H} }) ->
    New = {round(X)-W div 2, round(Y)-H div 2, W, H},
    epxw:invalidate(Box),
    epxw:invalidate(New),
    S#{ box => New };
motion(_, S) -> S.
```

## Named widgets

An epxw window can hold named widgets: buttons, switches, sliders,
values, text fields, menus, panels with tabs, rectangles, ellipses,
lines, and user-drawn widgets. They are the widgets of epxy, and use
the same properties (see doc/EPXY.md and `epxy_widget:defaults/0`).

```erlang
-module(counter).
-behaviour(epxw).
-export([start/0, init/1, widget/3]).

start() ->
    epxw:start(?MODULE, [],
               [{width,200},{height,80},{bottom_bar,0},{scroll_bar_size,0},
                {widgets,
                 [{"inc",   [{type,button},{x,10},{y,20},{width,60},{height,30},
                             {text,"+1"},{fill,solid},{color,lightblue}]},
                  {"count", [{type,value},{x,90},{y,20},{width,80},{height,30},
                             {value,0}]}]}]).

init([]) -> {ok, 0}.

widget("inc", #{ event := button_press }, N) ->
    epxw:widget_set("count", [{value, N+1}]),
    N+1;
widget(_Name, _Event, N) ->
    N.
```

* **Names** are dot separated paths. `"panel.ok"` is a child of
  `"panel"`, and its `x` and `y` are relative to the panel unless
  `{relative,false}` is set. Siblings are drawn in name order,
  numeric names before other names. The child named `"panel.Tab"` is
  the content of a panel tab, and only the selected tab is shown.
* **Drawing.** The widgets are drawn after the content `draw` callback,
  in view coordinates, so they scroll and zoom with the content. A
  changed widget invalidates only its own area (everything when it has
  children). Animated widgets
  (`animate`) redraw at `fps`.
* **Events.** Mouse and key events go to the widgets first. A left
  press on a widget, a drag that started on one, and key presses while
  a text field has focus are used by the widgets, and the other
  callbacks (`button_press`, `select`, `key_press`, `command`, ...) do
  not see them. All other events reach the callbacks as before.
* **Signals** arrive in `widget/3`. Signals caused by `widget_set/2`,
  such as focus changes, arrive after the callback that made the change.
* **From another process**, use `epxw:call/2`:
  `epxw:call(Win, fun(S) -> {epxw:widget_set("count",[{value,7}]), S} end)`.
* **Crashes.** A widget or user callback that crashes during an event
  is printed, and the event is passed on as if no widget wanted it.

## epxy on epxw

epxy is built from the same parts. `epxy_widget` is a pure library
with one widget as a map (properties, drawing, events). `epxy_tree` is
the widget tree of one window: names, draw order, hit testing, focus
and active widgets. Every epxy window is an epxw process
(`epxy_window`) with no chrome. The `epxy` server routes `"w.btn"` to
widget `"btn"` in window `w`, and keeps the `add_callback/3`
subscriptions.
