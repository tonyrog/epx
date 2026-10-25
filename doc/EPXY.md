
# epxy API 

epxy is a widget set that handles a range of GUI elements.

## general functions

``` erlang
epxy:start(Options::list()) -> {ok, Pid} | ignore | {error, Error}.
```

``` erlang
epxy:start_link(Options::list()) -> {ok, Pid} | ignore | {error, Error}.
```

``` erlang
epxy:stop() -> ok.
```

## widget functions

``` erlang
epxy:new(ID::epxy_id(), Options::epxy_options()) -> ok | {error,Reason::term()}.
```

``` erlang
epxy:delete(ID::epxy_id()) -> ok | {error,Reason::term()}.
```

``` erlang
epxy:set(ID::epxy_id(),Options::epxy_options()) -> ok | {error,Reason::term()}.
```

``` erlang
epxy:get(ID::epxy_id(),Options::epxy_keys()) -> [{Key::atom(),Value::term()}].
```

``` erlang
epxy:add_callback(ID::epxy_id(), Signal::term(), Cb::function()) -> {Ref::reference()}.
```

``` erlang
epxy:remove_callback(Ref::reference()) -> ok | {error,not_found}.
```


``` erlang
-type epxy_id() :: string().
-type epxy_keys() :: [Key::atom()].
-type epxy_options() :: [{Key::atom(),Value::term()}].
-type widget_type() :: window | panel | button | switch | slider |
	value | menu | rectangle | ellipse | line | text | user.
```

## Widget names

A widget id is a dot separated name. The first component decides the
window:

* `"w"` with `{type,window}` creates the window `w`.
* `"w.btn"`, where `w` is a window, creates `btn` in window `w`. The
  widget is stored, found and reported (in callbacks and in `get(...,
  [id])`) under its full name `"w.btn"`, so two windows can both have a
  `"btn"`.
* A name whose first component is not a window, for example `"btn"` or
  `"panel.btn"`, belongs to the default window `"screen"`. The
  prefix `"screen."` is optional, so `"screen.btn"` and `"btn"` name
  the same widget.
* Deleting a window also deletes all widgets in it. Deleting a widget
  also deletes the widgets below it.
* A panel with `{tabs,["one","two"]}` shows the child `"panel.one"`
  when `value` is 1, and `"panel.two"` when it is 2.

## Implementation

Every epxy window is an epxw window process (`epxy_window`) with no
bars or scroll bars. Its widgets are kept in the epxw widget tree
(`epxy_tree`), and one widget is a map handled by `epxy_widget`. The
`epxy` server maps names to windows and calls the subscribers. See
doc/EPXW.md, *Named widgets*, for using the same widgets directly in an
epxw application.

* Of the window properties, `x`, `y`, `width`, `height` and `color`
  are used when the window is created. Later, only `color` is applied
  to the open window. `events` is ignored, because epxw selects the
  events.
* A user widget, `{user,{M,F,As}}`, is called as
  `M:F(Type, Event, Widget, Window, XY, As...)` with `Type` one of
  `init`, `draw`, `event`, `select`. `Widget` is a map; use
  `epxy:widget_get/2` and `epxy:widget_set/2` on it. `Window` is a map
  with `id` (the window name) and `image` (the pixmap to draw in, for
  `draw`). For `init`, `Window` is the window name. Positions are
  `{X,Y,Z}`.

Widget keys and default values


``` erlang
	 id :: string(),     %% (structured) name of widget
	 type :: widget_type(),
	 window :: string(), %% id of base window (if type != window)
	 state  = normal,    %% normal,active
	 focus  = false,     %% has key focus
	 static = false,     %% object may not be deleted
	 hidden = false,     %% show/hiden false,true,all,none
	 disabled = false :: disabled(),
	 edit = false :: boolean(),  %% allow edit of text fields
	 relative = true :: boolean(), %% childrens are relative to parent
	 manual = false :: boolean(),  %% manual control over state (release)
	 user,               %% mfa when type=user
	 filter,             %% mfa for event filter
	 children_first = true :: boolean(), %% draw/select children first
	 last = false,       %% draw last (ontop)
	 x = 0   :: integer(),
	 y = 0   :: integer(),
	 z = 0   :: integer(),   %% define the order for overlap
	 width  = 0 :: non_neg_integer(),
	 height = 0 :: non_neg_integer(),
	 max_length = 0 :: non_neg_integer(),  %% 0=no limit
	 text = "",
	 tabs = [],
	 items = [],  %% menu items
	 border  :: number(),
	 border_color =  16#00000000,
	 shadow_x :: number(),
	 shadow_y :: number(),
	 round_w :: number(),
	 round_h :: number(),
	 orientation = horizontal :: horizontal|vertical,
	 image   :: epx:epx_pixmap(),
	 image2   :: epx:epx_pixmap(),
	 topimage :: epx:epx_pixmap(),
	 animation :: epx:epx_animation(),
	 animation2 :: epx:epx_animation(),
	 frame :: number(),
	 frame2 :: number(),
	 color,    %% = 16#ff000000,
	 color2,
	 active_color,
	 font_color = 16#00000000,
	 active_font_color,
	 fill   = none :: epx:epx_fill_style(),
	 events  :: epx:epx_window_event_flags(),
	 halign  = center :: left|right|center,  %% text alignment
	 valign  = center :: top|bottom|center,  %% text alignment
	 min     :: number(),          %% type=value|slider
	 max     :: number(),          %% type=value|slider
	 format = "~w" :: string(),    %% io_lib:format format
	 value = 0 :: number(),        %% type=value|slider
	 vscale :: number(),           %% scale value (multiplier)
	 animate = undefined,          %% animation state.
	 animate2 = undefined,         %% animation state of second animation.
	 font    :: epx:epx_font(),    %% type=text|button|value
	 win     :: epx:epx_window(),  %% type = window
	 backing :: epx:epx_pixmap(),
	 user_data = undefined :: term() %% what ever
```

