%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Generic epx server behaviour
%%% @end
%%% Created : 31 Jan 2020 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(epxw).

-behaviour(gen_server).

%% API
-export([start_link/3, start/3]).
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_continue/2, terminate/2, code_change/3]).
%% -export([handle_status/2]).

%% api callable from callbacks
-export([window/0, screen/0, pixels/0, width/0, height/0, keymod/0]).
-export([zoom/0, scale/0]).
-export([view_pos/0, view_xpos/0, view_ypos/0, 
	 view_width/0, view_height/0, view_rect/0]).
-export([set_view_size/2, set_view_rect/1, union_view_rect/1,
	 set_view_pos/2, set_view_xpos/1, set_view_ypos/1]).
-export([set_scale/2, set_xscale/1, set_yscale/1]).
-export([visible_rect/0]).
-export([invalidate/0, invalidate/1]).
-export([set_status_text/1]).
-export([file_menu/0, edit_menu/0]).
-export([enable_motion/0, disable_motion/0]).
-export([draw/1, draw/2]).
-export([view_to_window_x/1,
	 view_to_window_y/1,
	 view_to_window_pos/1,
	 view_to_window_height/1, 
	 view_to_window_width/1,
	 view_to_window_rect/1
	]).
-export([window_to_view_x/1, 
	 window_to_view_y/1,
	 window_to_view_pos/1, 
	 window_to_view_width/1,
	 window_to_view_height/1,
	 window_to_view_rect/1
	]).
-export([window_profile_get/1]).
-export([window_profile_set/2]).
-export([profile_get/1]).
-export([profile_set/2]).
-export([window_info_get/1]).
-export([window_info_set/2]).
	 
%% -define(DEBUG, true).

-define(USE_OFF_SCREEN, true).
-define(USE_EXPOSURE, false).

-include_lib("epx/include/epx.hrl").
-include_lib("epx/include/epx_menu.hrl").
-include_lib("epx/include/epx_window_content.hrl").

-type button() :: left | right | middle |
		  wheel_up | wheel_down | wheel_left | wheel_right.
-type keymod() :: ctrl_left | ctrl_right | ctrl |
		  shift_left | shift_right | shift |
		  alt_left | alt_right | alt |
		  meta_left | meta_right | meta |
		  num | caps | altgr | scr.
-type keysym() :: left | right | up | down | insert | delete |
		  home | 'end' | pageup | pagedown | 
		  f1 | f2 | f3 | f4 | f5 | f6 | f7 | 
		  f8 | f9 | f10 | f11 | f12 |
		  print | sysreq | pause | break | quit | menu | redraw |
		  integer().

-type button_press_event() :: 
	{button_press, Button::button(), Pos::epx:point()}.
-type button_release_event() :: 
	{button_release, Button::button(), Pos::epx:point()}.
-type key_press_event() ::
	{key_press, Sym::keysym(), KeyMods::[keymod()], KeyCode::integer()}.
-type key_release_event() ::
	{key_release, Sym::keysym(), KeyMods::[keymod()], KeyCode::integer()}.
-type enter_event() :: 
	{enter, Pos::epx:point()}.
-type leave_event() :: 
	{leave, Pos::epx:point()}.
-type focus_in_event() :: 
	focus_in.
-type focus_out_event() :: 
	focus_out.

-type toolbar() :: left|right|top|bottom.
-type where() :: content|toolbar().

-type region() :: epx:epx_rect() | [epx:epx_rect()].

%% epxw callbacks
-callback init(Args :: term()) -> {ok, State :: term()} |
				  {ok, State :: term(), Timeout :: timeout()} |
				  {ok, State :: term(), hibernate} |
				  {stop, Reason :: term()} |
				  ignore.

-callback configure(Event :: term(), State :: term()) -> 
    NewState :: term().
-callback key_press(Event :: key_press_event(), State :: term()) -> 
    NewState :: term().
-callback key_release(Event :: key_release_event(), State :: term()) -> 
    NewState :: term().
-callback button_press(Event :: button_press_event(), State :: term()) -> 
    NewState :: term().
-callback button_release(Event :: button_release_event(), State :: term()) -> 
    NewState :: term().
-callback enter(Event :: enter_event(), State :: term()) -> 
    NewState :: term().
-callback leave(Event :: leave_event(), State :: term()) -> 
    NewState :: term().
-callback focus_in(Event :: focus_in_event(), State :: term()) -> 
    NewState :: term().
-callback focus_out(Event :: focus_out_event(), State :: term()) -> 
    NewState :: term().
-callback close(State :: term()) -> 
    NewState :: term().
-callback draw(Pixels::epx:epx_pixmap(), DirtyArea::region(),
	       State :: term()) -> 
    NewState :: term().
-callback draw(Where::where(),
	       Pixels::epx:epx_pixmap(), DirtyArea::region(),
	       State :: term()) -> 
    NewState :: term().
-callback select(Rect::epx:epx_rect(), State :: term()) -> 
    NewState :: term().
-callback motion(Pos::epx:epx_point(), State :: term()) ->
    NewState :: term().
-callback command(Symbol::term(), Mod::epx_keymod(), State :: term()) -> 
    {noreply, NewState :: term()} |
    {reply, {Symbol1::term(), Mod1::epx_keymod()}, NewState :: term()}.


-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_continue(Info :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.


-optional_callbacks(
   [handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    handle_continue/2, 
    code_change/3,
    terminate/2
   ]).

-optional_callbacks(
   [init/1,
    configure/2,
    key_press/2,
    key_release/2,
    button_press/2,
    button_release/2,
    enter/2,
    leave/2,
    focus_in/2,
    focus_out/2,
    close/1,
    draw/3, draw/4,
    select/2,
    motion/2,
    command/3
   ]).

-ifdef(DEBUG).
-define(dbg(F,A), io:format(F "\n", A)).
-else.
-define(dbg(F,A), ok).
-endif.

%% -define(verbose(F,A), io:format(F "\n", A)).
-define(verbose(F,A), ok).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

-define(ld(Key, Env, Default),
	proplists:get_value(Key, Env, Default#profile.Key)).

-define(ldc(Scheme, Key, Env, Default),
	epx_profile:color_number(Scheme, ?ld(Key,Env,Default))).

%% profile with default values
-record(profile,
	{
	 scheme                 = logo, %% xterm,
	 screen_color           = grey2,
	 selection_alpha        = 100,
	 selection_color        = grey,
	 selection_border_width = 1,
	 selection_border_color = grey10,
	 max_dirty              = 10,    %% max number of rectangles in region
	 dirty_list             = false, %% call draw with [rect]/rect

	 top_bar                = 0,
	 left_bar               = 0,
	 right_bar              = 0,
	 bottom_bar             = 18,
	 status_bar             = true :: boolean(),  %% (bottom_bar > 0)

	 top_offset             = 0,
	 left_offset            = 0,
	 right_offset           = 0,
	 bottom_offset          = 0,

	 scroll_bar_size        = 16,
	 scroll_hndl_size       = 10,
	 scroll_xstep           = 4,
	 scroll_ystep           = 4,

	 %% menu_info
	 menu_font_name         = "Arial",
	 menu_font_size         = 14,
	 menu_font_color        = grey5,   %% light
	 menu_background_color  = grey10,  %% dark
	 menu_border_color      = green,

	 %% window_profile
	 window_font_name       = "Courier New",
	 window_font_size       = 14,
	 window_font_color      = grey10,
	 scroll_bar_color       = grey5,
	 scroll_hndl_color      = grey6,
	 scroll_vertical        = right,
	 scroll_horizontal      = bottom,
	 status_font_color      = grey5,

	 top_bar_color          = red,
	 left_bar_color         = green,
	 right_bar_color        = blue,
	 bottom_bar_color       = red6
	}).

-type callback() :: fun().
-type init_cb() :: fun().
-type configure_cb() :: fun((Rect::epx:epx_rect(),State::term()) ->
				   NewState::term()).
-type key_press_cb() :: fun((Event::key_press_event(),State::term()) ->
				   NewState::term()).
-type key_release_cb() :: fun((Event::key_release_event(),State::term()) ->
				     NewState::term()).
-type button_press_cb() :: fun((Event::button_press_event(),State::term()) ->
				      NewState::term()).
-type button_release_cb() :: fun((Event::button_release_event(),State::term())->
					NewState::term()).
-type enter_cb() :: fun((Event::enter_event(),State::term()) -> 
			       NewState::term()).
-type leave_cb() :: fun((Event::leave_event(),State::term()) -> 
			       NewState::term()).
-type focus_in_cb() :: fun((Event::focus_in_event(),State::term()) -> 
				  NewState::term()).
-type focus_out_cb() :: fun((Event::focus_out_event(),State::term()) -> 
				   NewState::term()).
-type close_cb() :: fun((State::term()) -> NewState::term()).

-type draw_cb() :: fun((Pixmap::epx:epx_pixmap(),DirtyArea::region(),
			State::term()) -> NewState::term()).
-type draw4_cb() :: fun((Where::where(),
			 Pixmap::epx:epx_pixmap(),DirtyArea::region(),
			 State::term()) -> NewState::term()).

-type select_phase() :: start|stop|continue.

-type select_cb() :: fun(({Phase::select_phase(),Rect::epx:epx_rect()},
			  State::term()) -> NewState::term()).
-type motion_cb() :: fun((Pos::epx:epx_point(),State::term()) ->
				NewState::term()).
-type command_cb() :: fun((Command::term(), Mod::epx_keymod(), State::term()) ->
				 NewState::term()).
-type menu_cb() :: fun((Event::term(), State::term()) ->
			      undefined | epx_menu:epx_menu()).

-record(callbacks, 
	{
	 init :: undefined | init_cb(),
	 handle_call :: undefined | callback(),
	 handle_cast :: undefined | callback(),
         handle_info :: undefined | callback(),
         handle_continue :: undefined | callback(),
	 code_change :: undefined | callback(),
         terminate :: undefined | callback(),
	 configure :: undefined | configure_cb(),
		      
	 key_press :: undefined | key_press_cb(),
	 key_release :: undefined | key_release_cb(),
	 button_press :: undefined | button_press_cb(),
         button_release :: undefined | button_release_cb(),
	 enter :: undefined | enter_cb(),
         leave :: undefined | leave_cb(),
	 focus_in :: undefined | focus_in_cb(),
         focus_out :: undefined | focus_out_cb(),
	 close  :: undefined | close_cb(),
	 draw  :: undefined | draw_cb(),
	 draw4 :: undefined | draw4_cb(),
	 select :: undefined | select_cb(),
	 motion :: undefined | motion_cb(),
         command :: undefined | command_cb(),
	 menu :: undefined | menu_cb()
 }).

-define(CALLBACK(S,Name), (((S)#state.user_cb)#callbacks.Name)).

-define(STATE_KEY, 'EPXW_STATE').

-define(SYS_MOTION,  16#01).
-define(USER_MOTION, 16#02).

-record(state, 
	{
	 window :: epx:epx_window(),
	 screen :: epx:epx_pixmap(),   %% on-screen pixels
	 pixels :: epx:epx_pixmap(),   %% off-screen pixels
	 pixel_format :: epx:epx_pixel_format(),
	 font   :: epx:epx_font(),
	 width  :: integer(),      %% window width
	 height :: integer(),      %% window height
	 dirty  :: all | [epx:epx_rect()],  %% [] clean!
	 status = "" :: string(),  %% bottom status text
	 profile :: #profile{},
	 menu_profile :: #menu_profile{},
	 window_profile :: #window_profile{},
	 content :: #window_content{},
	 keymod :: epx_keymod(), %% modifiers
	 motion = 0 :: integer(), %% motion mask
	 context_menu :: epx_menu:epx_menu(),
	 edit_menu    :: epx_menu:epx_menu(),
	 file_menu    :: epx_menu:epx_menu(),
	 menu         :: epx_menu:epx_menu(),  %% current menu
	 winfo :: #window_info{},
	 zoom  = 0 :: integer(),
	 scale = {1,1} :: {number(),number()},
	 pt1   :: undefine | {X::integer(),Y::integer()}, %% start pos
	 pt2   :: undefine | {X::integer(),Y::integer()}, %% cur pos
	 operation = none :: none | select | menu,
	 user_mod :: atom(),       %% user module
	 user_cb  :: #callbacks{}, %% user callbacks
	 user_state :: term(),     %% user state
	 ok
	}).

-define(SHIFT(State), (State#state.keymod)#keymod.shift).
-define(CTRL(State), (State#state.keymod)#keymod.ctrl).
-define(ALT(State), (State#state.keymod)#keymod.alt).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(UserMod::atom()|map(),
		 UserOpts::[{Key::atom(),Value::term()}],
		 Opts :: list()) ->
			{ok, Pid :: pid()} |
			{error, Error :: term()} |
			ignore.

start_link(User, UserOpts, Opts) when
      (is_atom(User) orelse is_map(User)), is_list(UserOpts), is_list(Opts) ->
    gen_server:start_link(?MODULE, [User,UserOpts,Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec start(User::atom()|map(), UserOpts::[{Key::atom(),Value::term()}],
	    Options :: list()) ->
		   {ok, Pid :: pid()} |
		   {error, Error :: term()} |
		   ignore.

start(User) ->
    start(User, [], []).

start(User, UserOpts, Opts) when
      (is_atom(User) orelse is_map(User)),
      is_list(UserOpts), is_list(Opts) ->
    gen_server:start(?MODULE, [User,UserOpts,Opts], []).

export_state(State) ->
    put(?STATE_KEY, State).

state() ->
    get(?STATE_KEY).

%% CALLBACK API
window() -> (state())#state.window.
screen() -> (state())#state.screen.
pixels() -> pixels(state()). %% screen or off-screen pixmap
width()  -> (state())#state.width.
height() -> (state())#state.height.
keymod() -> (state())#state.keymod.
view_pos() -> get_view_pos(state()).
view_xpos() -> get_view_xpos(state()).
view_ypos() -> get_view_ypos(state()).
view_width() -> get_view_width(state()).
view_height() -> get_view_height(state()).
view_rect() -> get_view_rect(state()).
visible_rect() -> get_visible_rect(state()).
zoom() -> (state())#state.zoom.
scale() -> (state())#state.scale.
file_menu() -> (state())#state.file_menu.
edit_menu() -> (state())#state.edit_menu.
enable_motion() -> 
    export_state(enable_motion_(true, ?USER_MOTION, state())).
disable_motion() ->
    export_state(enable_motion_(false, ?USER_MOTION, state())).


view_to_window_x(X) -> view_to_window_x(X,state()).
view_to_window_y(Y) -> view_to_window_y(Y,state()).
view_to_window_pos(Pos) -> view_to_window_pos(Pos,state()).
view_to_window_height(H) -> view_to_window_height(H,state()).
view_to_window_width(W) -> view_to_window_width(W,state()).
view_to_window_rect(Rect) -> view_to_window_rect(Rect,state()).

window_to_view_pos(Pos) -> window_to_view_pos(Pos,state()).
window_to_view_x(X) -> window_to_view_x(X,state()).
window_to_view_y(Y) -> window_to_view_y(Y,state()).
window_to_view_width(W) -> window_to_view_width(W,state()).
window_to_view_height(H) -> window_to_view_height(H,state()).
window_to_view_rect(Rect) -> window_to_view_rect(Rect,state()).

window_profile_get(Prop) ->
    S = state(),
    window_profile_get(Prop, S#state.window_profile).

window_profile_set(Prop, Value) ->
    S0 = state(),
    WProfile = window_profile_set(Prop, Value, S0#state.window_profile),
    S1 = S0#state { window_profile = WProfile },
    export_state(S1).

profile_get(Prop) ->
    S = state(),
    profile_get(Prop, S#state.profile).

profile_set(Prop, Value) ->
    S0 = state(),
    Profile = profile_set(Prop, Value, S0#state.profile),
    S1 = S0#state { profile = Profile },
    export_state(S1).

window_info_get(Prop) ->
    S = state(),
    window_info_get(Prop, S#state.winfo).

window_info_set(Prop, Value) ->
    S0 = state(),
    WInfo = window_info_set(Prop, Value, S0#state.winfo),
    S1 = S0#state { winfo = WInfo },
    export_state(S1).

set_view_size(W, H) when W >= 0, H >= 0 ->
    S0 = state(),
    X = get_view_left(S0),
    Y = get_view_top(S0),
    S1 = set_view_rect(S0, {X,Y,W,H}),
    export_state(S1).

%% set the view rectangle
set_view_rect(R={X, Y, W, H}) when X >= 0, Y >= 0, W >= 0, H >= 0 ->
    S0 = state(),
    S1 = set_view_rect(S0, R),
    export_state(S1).

%% union R with the view rectangle
union_view_rect(R={X, Y, W, H}) when X >= 0, Y >= 0, W >= 0, H >= 0 ->
    S0 = state(),
    S1 = union_view_rect(S0, R),
    export_state(S1).

set_view_xpos(X) when X >= 0 ->
    S0 = state(),
    S1 = set_view_xpos(S0, X),
    export_state(S1).

set_view_ypos(Y) when Y >= 0 ->
    S0 = state(),
    S1 = set_view_ypos(S0, Y),
    export_state(S1).

set_view_pos(X, Y) when X >= 0, Y >= 0 ->
    S0 = state(),
    S1 = set_view_pos(S0, X, Y),
    export_state(S1).

set_status_text(Text) ->
    S0 = state(),
    S1 = S0#state { status = lists:flatten(Text) },
    export_state(S1),
    ok.


set_scale(Sx, Sy) when is_number(Sx), is_number(Sy), 
		       Sx > 0, Sy > 0 ->
    S0 = state(),
    S1 = S0#state { scale = {Sx,Sy} },
    export_state(S1).

set_xscale(Sx) when is_number(Sx), Sx > 0 ->
    S0 = state(),
    {_,Sy} = S0#state.scale,
    S1 = S0#state { scale = {Sx,Sy} },
    export_state(S1).

set_yscale(Sy) when is_number(Sy), Sy > 0 ->
    S0 = state(),
    {Sx,_} = S0#state.scale,
    S1 = S0#state { scale = {Sx,Sy} },
    export_state(S1).

    
invalidate() ->
    S0 = state(),
    S1 = all_dirty_area(S0),
    export_state(S1),
    S1#state.dirty.

invalidate(undefined) ->
    undefined;
invalidate(Area={_X,_Y,_W,_H}) ->
    S0 = state(),
    S1 = add_dirty_area(Area,S0),
    export_state(S1),
    S1#state.dirty.

%% "standard" menus
menu(edit) ->
    [
     {"Undo", undo, "Ctrl+Z"},
     {"Redo", redo, "Ctrl+Shift+Z"},
     {"---"},
     {"Cut", cut, "Ctrl+X"},
     {"Copy", copy, "Ctrl+C"},
     {"Paste", paste, "Ctrl+V"},
     {"Delete", delete, "Del"},
     {"---"},
     {"Select All", select_all, "Ctrl+A"}
    ];
menu(file) ->
    [
     {"Open File...", open_file, "Ctrl+O"},
     {"Save", save, "Ctrl+S"},
     {"---"},
     {"Quit", quit, "Ctrl+Q"}
    ];
menu(context) ->
    [
     {"Cut", cut, "Ctrl+X"},
     {"Copy", copy, "Ctrl+C"},
     {"Paste", paste, "Ctrl+V"},
     {"Delete", delete, "Del"},
     {"---"},
     {"Select All", select_all, "Ctrl+A"}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.

init([User,UserOpts,Opts]) when (is_atom(User) orelse is_map(User)),
				is_list(UserOpts) ->
    %% application:load(epx),
    %% application:set_env(epx, use_off_screen, ?USE_OFF_SCREEN),
    %% application:set_env(epx, use_exposure, ?USE_EXPOSURE),
    application:ensure_all_started(epx),
    {UserMod,UserCb} = load_callbacks(User),
    process_flag(trap_exit, true),
    Env = if UserMod =:= undefined -> Opts;
	     true -> Opts ++ application:get_all_env(UserMod)
	  end,
    Profile = load_profile(Opts),
    MProfile = create_menu_profile(Profile),
    WProfile = create_window_profile(Profile),
    WContent = #window_content { profile = WProfile },

    Width = proplists:get_value(width, Env, 640),
    Height = proplists:get_value(height, Env, 480),
    Title  = proplists:get_value(title, Env, "Untitled"),
    PixelFormat = proplists:get_value(pixel_format, Env, argb),

    {ok,Font} = epx_font:match([{name,WProfile#window_profile.font_name},
				{size,WProfile#window_profile.font_size}]),
    %% FontColor =  WProfile#window_profile.font_color,

    {Sx0,Sy0} = proplists:get_value(scale, Env, {1,1}),
    Sx = proplists:get_value(xscale, Env, Sx0),
    Sy = proplists:get_value(yscale, Env, Sy0),

    epx_gc:set_font(Font),
    {W,H}  = epx_font:dimension(Font,"0"),

    WInfo = #window_info {
	       glyph_width  = W,
	       glyph_height = H,
	       glyph_ascent = epx:font_info(Font, ascent),
	       glyph_descent = epx:font_info(Font, descent),
	       %% tool bars & status bar
	       top_bar = Profile#profile.top_bar,
	       left_bar = Profile#profile.left_bar,
	       right_bar = Profile#profile.right_bar,
	       bottom_bar = Profile#profile.bottom_bar,
	       %% offsets
	       top_offset = Profile#profile.top_offset,
	       left_offset = Profile#profile.left_offset,
	       right_offset = Profile#profile.right_offset,
	       bottom_offset = Profile#profile.bottom_offset,
	       %% scrollbar
	       scroll_bar_size = Profile#profile.scroll_bar_size,
	       scroll_hndl_size = Profile#profile.scroll_hndl_size,
	       scroll_xstep = Profile#profile.scroll_xstep,
	       scroll_ystep = Profile#profile.scroll_ystep,
	       scroll_horizontal = WProfile#window_profile.scroll_horizontal,
	       scroll_vertical   = WProfile#window_profile.scroll_vertical
	       %% use default for rest options
	      },
    EventMask = init_event_mask([configure, close],
				[
				 {button_press,
				  UserCb#callbacks.button_press},
				 {button_release,
				  UserCb#callbacks.button_release},
				 {key_press,
				  UserCb#callbacks.key_press},
				 {key_release,
				  UserCb#callbacks.key_release},
				 {enter,
				  UserCb#callbacks.enter},
				 {leave,
				  UserCb#callbacks.leave},
				 {focus_in,
				  UserCb#callbacks.focus_in},
				 {focus_out,
				  UserCb#callbacks.focus_out}
				]),
    ScrollEvents = [key_press, key_release, button_press, button_release],
    Window = epx:window_create(50,50,Width,Height,EventMask++ScrollEvents),

    Width1 = Width +
	WInfo#window_info.left_bar +
	WInfo#window_info.right_bar +
	WInfo#window_info.scroll_bar_size,
    Height1 = Height +
	WInfo#window_info.top_bar +
	WInfo#window_info.bottom_bar +
	WInfo#window_info.scroll_bar_size,

    Screen = epx:pixmap_create(Width1, Height1, PixelFormat),
    Pixels = undefined, %% epx:pixmap_create(Width1, Height1, argb),
    ScreenColor = epx_profile:color(WProfile#window_profile.scheme,
				    WProfile#window_profile.background_color),
    epx:pixmap_fill(Screen, ScreenColor),
    %% epx:pixmap_fill(Pixels, ScreenColor),
    epx:window_attach(Window),
    epx:pixmap_attach(Screen),
    epx:window_adjust(Window, [{name, Title}]),

    EditMenu = epx_menu:create(MProfile, menu(edit)),
    FileMenu = epx_menu:create(MProfile, menu(file)),
    ContextMenu = epx_menu:create(MProfile, menu(context)),
    
    State0 = #state {
		window = Window,
		screen = Screen,
		pixels = Pixels,  %% (off-screen = true)
		pixel_format = PixelFormat,
		font   = Font,    %% window font
		width  = Width,
		height = Height,
		scale = {Sx,Sy},
		edit_menu   = EditMenu,
		file_menu   = FileMenu,
		context_menu = ContextMenu,
		winfo  = WInfo,
		profile = Profile,
		keymod = #keymod{},
		menu_profile = MProfile,
		window_profile = WProfile,
		content = WContent,
		user_mod = UserMod,
		user_cb  = UserCb,
		user_state = undefined
	       },

    ViewWidth = proplists:get_value(view_width, Env, Width),
    ViewHeight = proplists:get_value(view_height, Env, Height),
    State1 = set_view_rect(State0, {0,0,ViewWidth,ViewHeight}),
    export_state(State1),
    Rect = {0,0,Width,Height},
    case UserCb#callbacks.init of
	undefined ->
	    init_res(undefined, ok, Rect, State1);
	Init ->
	    case Init(UserOpts) of
		{ok,UserState} ->
		    init_res(UserState, ok, Rect, state());
		{ok,UserState,InitType} ->
		    init_res(UserState, InitType, Rect, state());
		Other ->  %% stop/ignore
		    Other
	    end
    end.

init_res(UserState, InitType, Rect, State) ->
    State1 = State#state { user_state = UserState},
    State2 = user_event(Rect,?CALLBACK(State1,configure),State1),
    State3 = draw_(State2, Rect),
    if InitType =:= ok ->
	    {ok, State3};
       true->
	    {ok, State3, InitType}
    end.

load_callbacks(UserMod) when is_atom(UserMod) ->
    {module,_} = code:ensure_loaded(UserMod),
    load_callbacks_(UserMod, #{});
load_callbacks(UserMap) when is_map(UserMap) ->
    UserMod =
	case maps:get(module, UserMap, undefined) of
	    undefined -> 
		undefined;
	    Mod ->
		{module,_} = code:ensure_loaded(Mod),
		Mod
	end,
    load_callbacks_(UserMod, UserMap).

load_callbacks_(UserMod, UserMap) ->
    {UserMod, 
     #callbacks {
	init = load_callback_(UserMod, UserMap, init, 1),
	handle_call = load_callback_(UserMod, UserMap, handle_call, 3),
	handle_cast = load_callback_(UserMod, UserMap, handle_cast, 2),
	handle_info = load_callback_(UserMod, UserMap,handle_info, 2),
	handle_continue = load_callback_(UserMod, UserMap, handle_continue, 2),
	code_change = load_callback_(UserMod, UserMap, code_change, 3),
	terminate = load_callback_(UserMod, UserMap, close, 2),
	close = load_callback_(UserMod, UserMap, close, 1),
	configure = load_callback_(UserMod, UserMap, configure, 2),
	key_press = load_callback_(UserMod, UserMap, key_press, 2),
	key_release = load_callback_(UserMod, UserMap, key_release, 2),
	button_press = load_callback_(UserMod, UserMap, button_press, 2),
	button_release = load_callback_(UserMod, UserMap, button_release, 2),
	enter = load_callback_(UserMod, UserMap, enter, 2),
	leave = load_callback_(UserMod, UserMap, leave, 2),
	focus_in = load_callback_(UserMod, UserMap, focus_in, 2),
	focus_out = load_callback_(UserMod, UserMap, focus_out, 2),
	draw  = load_callback_(UserMod, UserMap, draw, 3),
	draw4 = load_callback_(UserMod, UserMap, draw, 4),
	select = load_callback_(UserMod, UserMap, select, 2),
	motion = load_callback_(UserMod, UserMap, motion, 2),
	command = load_callback_(UserMod, UserMap, command, 3),
	menu = load_callback_(UserMod, UserMap, menu, 2)
       }}.

load_callback_(UserMod, UserMap, Func, Arity) ->
    case maps:get(Func, UserMap, undefined) of
	undefined ->
	    case erlang:function_exported(UserMod, Func, Arity) of
		true ->
		    erlang:make_fun(UserMod, Func, Arity);
		false when Func =:= draw, Arity =:= 3 ->
		    ?dbg("warning: no draw/3 function for module ~s\n", 
			 [UserMod]),
		    undefined;
		false ->
		    undefined
	    end;
	UserFun when is_function(UserFun, Arity) ->
	    UserFun;
	_UserFun  -> %% no matching arity
	    undefined
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_call(Request, From, State) ->
    case ?CALLBACK(State, handle_call) of
	undefined ->
	    {stop, missing_handle_call, State};
	HandleCall ->
	    export_state(State),
	    try HandleCall(Request, From, State#state.user_state) of
		Reply ->
		    reply(Reply)
	    catch
		error:Reason:Stack ->
		    io:format("handle_call: ~p\ncrash ~p\n~p\n", 
			      [Request, Reason, Stack]),
		    {stop, Reason, State}
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_cast(Request, State) ->
    case ?CALLBACK(State, handle_cast) of
	undefined ->
	    {noreply, State};
	HandleCast ->
	    export_state(State),
	    try HandleCast(Request, State#state.user_state) of
		Reply ->
		    reply(Reply)
	    catch
		error:Reason:Stack ->
		    io:format("handle_cast: ~p\ncrash ~p\n~p\n", 
			      [Request, Reason, Stack]),
		    {stop, Reason, State}
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(#epx_event{win=Win,data=Event}, State) when 
      Win =:= State#state.window ->
    case epx_event(Event, State) of
	{noreply, State1} ->
	    if State1#state.dirty =:= [] ->
		    {noreply,State1};
	       true ->
		    {noreply,draw_(State1)}
	    end;
	Reply ->
	    Reply
    end;
handle_info(Info, State) ->
    case ?CALLBACK(State, handle_info) of
	undefined ->
	    ?dbg("handle_info: unhandled event ~p", [Info]),
	    {noreply, State};
	HandleInfo ->
	    export_state(State),
	    try HandleInfo(Info, State#state.user_state) of
		Reply ->
		    reply(Reply)
	    catch
		error:Reason:Stack ->
		    io:format("handle_info: ~p\ncrash ~p\n~p\n", 
			      [Info, Reason, Stack]),
		    {stop, Reason, State}
	    end
    end.

reply({reply,Reply,UState}) ->
    State = dstate(UState),
    {reply, Reply, State};
reply({reply,Reply,UState,Arg}) ->
    State = dstate(UState),
    {reply, Reply, State, Arg};
reply({noreply,UState}) ->
    State = dstate(UState),
    {noreply, State};
reply({noreply,UState,Arg}) ->
    State = dstate(UState),
    {noreply, State,Arg};
reply({stop, Reason, UState}) ->
    State = dstate(UState),
    {stop, Reason, State}.

%% redraw state if dirty
dstate(UState) ->
    State = state(),
    if State#state.dirty =:= [] ->
	    State#state { user_state = UState };
       true ->
	    draw_(State#state { user_state = UState })
    end.


-spec handle_continue(Info :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
	  {stop, Reason :: term(), NewState :: term()}.

handle_continue(Info, State) ->
    case ?CALLBACK(State, handle_continue) of
	undefined ->
	    ?dbg("handle_continue: unhandled event ~p", [Info]),
	    {noreply, State};
	HandleContinue ->
	    export_state(State),
	    try HandleContinue(Info, State#state.user_state) of
		Reply ->
		    reply(Reply)
	    catch
		error:Reason:Stack ->
		    io:format("handle_continue: ~p\ncrash ~p\n~p\n", 
			      [Info, Reason, Stack]),
		    {stop, Reason, State}
	    end
    end.

epx_event(_Event={configure,Rect0}, State) ->
    Rect1 = {_X,_Y,W,H} = flush_configure(State#state.window, Rect0),
    if W =/= State#state.width;
       H =/= State#state.height ->
	    WInfo = State#state.winfo,
	    W1 = W +
		WInfo#window_info.left_bar +
		WInfo#window_info.right_bar +
		WInfo#window_info.scroll_bar_size,
	    H1 = H +
		WInfo#window_info.top_bar +
		WInfo#window_info.bottom_bar +
		WInfo#window_info.scroll_bar_size,
	    Screen = resize_pixmap(State#state.screen, W1, H1, 
				   State#state.pixel_format, true),
	    Pixels = if State#state.pixels =:= undefined -> undefined;
			true -> resize_pixmap(State#state.pixels, W1, H1,
					      State#state.pixel_format, false)
		     end,
	    State1 = State#state { screen = Screen, pixels = Pixels,
				   width=W, height=H },
	    State2 = user_event(Rect1,?CALLBACK(State1,configure),State1),
	    State3 = adjust_view_xpos(get_view_xpos(State2), State2),
	    State4 = adjust_view_ypos(get_view_ypos(State3), State3),
	    {noreply, all_dirty_area(State4)};
       true ->
	    {noreply, State}
    end;

epx_event(Event={key_press, Sym, Mod, _code}, State) ->
    if Sym =:= $\e, State#state.operation =:= menu ->
	    State1 = disable_sys_motion(State),
	    State2 = State1#state { operation = none },
	    State3 = all_dirty_area(State2),
	    State4 = user_event(Event,?CALLBACK(State3,key_press),State3),
	    {noreply, State4};
       true ->
	    M = set_mod(State#state.keymod, Mod),
	    State1 = State#state { keymod=M },
	    State2 = command(Sym, M, State1),
	    State3 = user_event(Event,?CALLBACK(State2,key_press),State2),
	    {noreply, State3 }
    end;
epx_event(Event={key_release, _Sym, Mod, _code}, State) ->
    M = clr_mod(State#state.keymod, Mod),
    State1 = State#state { keymod = M },
    State2 = user_event(Event,?CALLBACK(State1,key_release), State1),
    {noreply, State2};

epx_event(Event={button_press, [left], Pos}, State) ->
    if State#state.operation =:= menu ->
	    case epx_menu:find_row(State#state.menu,
				   State#state.pt1,
				   Pos) of
		{-1, _Menu} ->
		    {noreply, State };
		{_Row, Menu} ->
		    case epx_menu:command(Menu) of
			none ->
			    {noreply, State#state { menu=Menu }};
			{Cmd,Mod} ->
			    State1 = State#state { menu=Menu },
			    State2 = command(Cmd,Mod,State1),
			    State3 = all_dirty_area(State2),
			    {noreply, State3}
		    end
	    end;
       true ->
	    case scroll_hit(Pos, State) of
		false ->
		    %%Window = State#state.window,
		    State1 = enable_sys_motion(State),
		    State2 = user_event(Event, ?CALLBACK(State1,button_press),
					State1),
		    State3 = select(start,Pos,Pos,State2#state{pt1=Pos,pt2=Pos}),
		    State4 = all_dirty_area(State3),
		    {noreply, State4#state { operation=select }};
		State1 ->
		    {noreply, State1}
	    end
    end;
epx_event(Event={button_press, [right], Pos}, State) ->
    %% check for context menu
    Pos1 = window_to_view_pos(Pos,State),
    case ?CALLBACK(State, menu) of
	undefined ->
	    State1 = user_event(Event, ?CALLBACK(State,button_press), 
				State),
	    {noreply, State1};
	Menu ->
	    case Menu({menu,Pos1},State#state.user_state) of
		{noreply,UserState} ->
		    State1 = user_event(Event, ?CALLBACK(State,button_press), 
					State#state { user_state = UserState}),
		    {noreply, State1};
		{reply,undefined,UserState} ->
		    State1 = user_event(Event, ?CALLBACK(State,button_press), 
					State#state { user_state = UserState}),
		    {noreply, State1};
		{reply,Menu0,UserState} ->
		    State1 = enable_sys_motion(State),
		    case epx_menu:set_row(Menu0, -1) of
			undefined ->
			    State2 = State1#state { user_state = UserState},
			    State3 = user_event(Event,
						?CALLBACK(State2,button_press),
						State2),
			    {noreply, State3};
			Menu1 ->
			    State2 = State1#state { pt1 = Pos, pt2 = Pos,
						    operation = menu,
						    menu = Menu1,
						    user_state = UserState },
			    {noreply, all_dirty_area(State2)}
		    end
	    end
    end;

epx_event(Event={button_press,[wheel_left],_Pos}, State) ->
    State1 = user_event(Event, ?CALLBACK(State,button_press), State),
    {noreply, State1};
epx_event(Event={button_press,[wheel_right],_Pos}, State) ->
    State1 = user_event(Event, ?CALLBACK(State,button_press), State),
    {noreply, State1};    
epx_event(Event={button_press,[wheel_down],_Pos},State) ->
    State1 = user_event(Event, ?CALLBACK(State,button_press), State),
    {noreply, State1};
epx_event(Event={button_press,[wheel_up],_Pos},State) ->
    State1 = user_event(Event, ?CALLBACK(State,button_press), State),
    {noreply, State1};


epx_event(Event={button_release, [left], Pos}, State) ->
    flush_motion(State#state.window),
    Pt1 = State#state.pt1,
    State1 = State#state{pt1 = undefined, pt2 = undefined, operation = none},
    case get_motion(State1) of
	undefined ->
	    case State#state.operation of
		select ->
		    State2 = select(stop,Pt1,Pos,State1),
		    State3 = disable_sys_motion(State2),
		    State4 = all_dirty_area(State3),
		    State5 = user_event(Event,?CALLBACK(State4,button_release),State4),
		    {noreply, State5};
		menu ->
		    State2 = disable_sys_motion(State1),
		    State3 = all_dirty_area(State2),
		    {noreply, State3};
		_ ->
		    State2 = all_dirty_area(State1),
		    State3 = user_event(Event, ?CALLBACK(State2,button_release), State2),
		    {noreply, State3}
	    end;
	{vhndl,_Delta} ->
	    State2 = disable_sys_motion(State1),
	    {noreply, set_motion(State2, undefined)};
	{hhndl,_Delta} ->
	    State2 = disable_sys_motion(State1),
	    {noreply, set_motion(State2, undefined)}
    end;

epx_event(Event={button_release,[wheel_left],_Pos},State) ->
    flush_wheel(State#state.window),
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, scroll_left(State1)};
%% Fixme: transform X,Y into view coordinates before callback
epx_event(Event={button_release,[wheel_right],_Pos},State) ->
    flush_wheel(State#state.window),
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, scroll_right(State1)};
epx_event(Event={button_release, [wheel_up], _Pos}, State) ->
    flush_wheel(State#state.window),
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, scroll_up(State1)};
epx_event(Event={button_release, [wheel_down], _Pos}, State) ->
    flush_wheel(State#state.window),  %% optional?
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, scroll_down(State1)};

epx_event(Event={button_release, _Buttons, _Pos3D}, State) ->
    flush_motion(State#state.window),
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, State1};

epx_event(Event={motion,[],Pos},State) ->
    flush_motion(State#state.window),
    if State#state.operation =:= menu ->
	    {Row,Menu} = epx_menu:find_row(State#state.menu,
					   State#state.pt1,
					   Pos),
	    if Row =:= -1 ->
		    {noreply, State#state { menu=Menu }};
	       true ->
		    State1 = State#state { menu=Menu },
		    {noreply,all_dirty_area(State1)}
	    end;
       true ->
	    State1 = user_event(Event, ?CALLBACK(State, motion), State),
	    {noreply, State1}
    end;
epx_event(Event={motion,[left],Pos},State) ->
    flush_motion(State#state.window),
    case get_motion(State) of
	{vhndl,{_DX,Dy}} ->
	    {_,Y,_} = Pos,
	    {_,Y0,_,Height} = get_vscroll(State),
	    %% FIXME: Height == 0
	    Yr = (Y-Y0)/Height,  %% relative hit position
	    Vy = Yr*get_view_height(State), %% View position
	    State1 = adjust_view_ypos(Vy+Dy, State),
	    {noreply,all_dirty_area(State1)};
	{hhndl,{Dx,_Dy}} ->
	    {X,_,_} = Pos,
	    {X0,_,Width,_} = get_hscroll(State),
	    %% FIXME: Width == 0
	    Xr = (X-X0)/Width,  %% relative hit position
	    Vx = Xr*get_view_width(State), %% View position
	    State1 = adjust_view_xpos(Vx+Dx, State),
	    {noreply,all_dirty_area(State1)};
	_ ->
	    State1 = user_event(Event, ?CALLBACK(State, motion), State),
	    State2 = select(continue,State1#state.pt1,Pos,
			    State1#state{ pt2 = Pos }),
	    {noreply,State2}
    end;
epx_event(Event={enter, _Pos}, State) ->
    State1 = user_event(Event, ?CALLBACK(State,enter), State),
    {noreply, State1};
epx_event(Event={leave, _Pos}, State) ->
    State1 = user_event(Event, ?CALLBACK(State,leave), State),
    {noreply, State1};
epx_event(Event=focus_in, State) ->
    State1 = user_event(Event, ?CALLBACK(State,focus_in), State),
    {noreply, State1};
epx_event(Event=focus_out, State) ->
    State1 = user_event(Event, ?CALLBACK(State,focus_out), State),
    {noreply, State1};
epx_event(close, State) ->
    case ?CALLBACK(State, close) of
	undefined ->
	    {stop, normal, State};
	Cb ->
	    export_state(State),
	    Cb(State#state.user_state),
	    {stop, normal, state()}
    end;
epx_event(_Event, State) ->
    ?dbg("unhandled epx event: ~p\n", [_Event]),
    %% FIXME! Handle info!
    {noreply,State}.


%% Get drawing area top left corner in window coordinates
drawing_origin(State) ->
    drawing_origin_(State,scrollbars(State)).

drawing_origin_(State,ScrollBars) ->
    drawing_origin_(State,ScrollBars,toolbars(State)).

drawing_origin_(State,{HBar,VBar},{LeftBar,_RightBar,TopBar,_BottomBar}) ->
    ScrollBarSize = scroll_bar_size(State),
    X0 = case VBar of
	     none -> LeftBar;
	     left -> LeftBar+ScrollBarSize;
	     right -> LeftBar
	 end,
    Y0 = case HBar of
	     none   -> TopBar;
	     bottom -> TopBar;
	     top    -> TopBar+ScrollBarSize
	 end,
    {X0,Y0}.
    
%% Get drawing area width taking toolbars into account but no scrollbar
drawing_width0(State) ->
    {LeftBar,RightBar,_,_} = toolbars(State),
    State#state.width - (LeftBar+RightBar).

%% Get drawing area height taking toolbars into account but no scrollbar
drawing_height0(State) ->
    {_,_,TopBar,BottomBar} = toolbars(State),
    State#state.height - (TopBar+BottomBar).

all_dirty_area(State) ->
    State#state { dirty = all }.

add_dirty_area(_Area,State) when State#state.dirty =:= all ->
    %% io:format("add_dirty_area: ~w + all => all\n", [_Area]),
    State;
add_dirty_area(Area, State=#state{profile=Profile}) when
      tuple_size(Area) =:= 4 ->
    Len = length(State#state.dirty),
    Dirty = if Len >= Profile#profile.max_dirty ->
		    [epx_rect:union([Area|State#state.dirty])];
	       true ->
		    [Area|State#state.dirty]
	    end,
    %% io:format("add_dirty_area: ~w + ~w => ~w\n",[State#state.dirty,Area,Dirty]),
    State#state { dirty = Dirty }.

scroll_hit(Pos, State) ->
    HScroll = get_hscroll(State),
    case epx_rect:contains(HScroll, Pos) of
	true ->
	    {X0,_,Width,_} = HScroll,
	    {X,_Y,_} = Pos,
	    %% FIXME: Width == 0
	    Xr = (X-X0)/Width,  %% relative hit position
	    Vx = Xr*get_view_width(State), %% View position
	    %% hit horizontal scrollbar
	    State1 = enable_sys_motion(State),
	    HHndl = get_hhndl(State1),
	    case epx_rect:contains(HHndl, Pos) of
		true ->
		    %% hit horizontal scroll handle
		    {Hx,_Hy,_,_} = HHndl,
		    Dx = -(X-Hx),
		    State2 = adjust_view_xpos(Vx+Dx, State1),
		    State3 = set_motion(State2,{hhndl,{Dx,0}}),
		    all_dirty_area(State3);
		false ->
		    {_,_,Hw,_} = HHndl,
		    Dx = -(Hw/2),
		    State2 = adjust_view_xpos(Vx+Dx, State1),
		    State3 = set_motion(State2,{hhndl,{Dx,0}}),
		    all_dirty_area(State3)
	    end;
	false ->
	    VScroll = get_vscroll(State),
	    case epx_rect:contains(VScroll, Pos) of
		true ->
		    {_,Y0,_,Height} = VScroll,
		    {_X,Y,_} = Pos,
		    %% FIXME: Height == 0
		    Yr = (Y-Y0)/Height,  %% relative hit position
		    Vy = Yr*get_view_height(State), %% View position
		    %% hit vertical scrollbar
		    State1 = enable_sys_motion(State),
		    VHndl = get_vhndl(State),
		    case epx_rect:contains(VHndl, Pos) of
			true ->
			    %% hit vertical scroll handle
			    {_Hx,Hy,_,_} = VHndl,
			    Dy = -(Y-Hy),
			    State2 = adjust_view_ypos(Vy+Dy, State1),
			    State3 = set_motion(State2,{vhndl,{0,Dy}}),
			    all_dirty_area(State3);
			false ->
			    {_,_,_,Hh} = VHndl,
			    Dy = -(Hh/2),
			    State2 = adjust_view_ypos(Vy+Dy, State1),
			    State3 = set_motion(State2,{vhndl,{0,Dy}}),
			    all_dirty_area(State3)
		    end;
		false ->
		    false
	    end
    end.

command(Symbol,Mod,State) ->
    case ?CALLBACK(State,command) of
	undefined ->
	    command_(Symbol,Mod,State);
	Command ->
	    export_state(State),
	    case Command(Symbol,Mod,State#state.user_state) of
		{noreply, UserState} ->
		    State1 = state(),
		    State1#state { user_state = UserState };
		{reply,{Symbol1,Mod1},UserState} ->
		    State1 = state(),
		    State2 = State1#state { user_state = UserState },
		    command_(Symbol1,Mod1,State2)
	    end
    end.

command_(Sym, Mod, State) when 
      not Mod#keymod.shift,not Mod#keymod.ctrl,not Mod#keymod.alt ->
    command_(Sym, State);
command_(_Sym, _Mod, State) ->
    ?dbg("unhandled command ~p mod=~p", [_Sym, _Mod]),
    State.

%% no modifer (user may override with with noreply or modifier
command_(up, State) ->
    scroll_up(State);
command_(down, State) ->
    scroll_down(State);
command_(left, State) ->
    scroll_left(State);
command_(right, State) ->
    scroll_right(State);
command_(pageup, State) ->
    page_up(State);
command_(pagedown, State) ->
    page_down(State);
command_(home, State) ->
    scroll_home(State);
command_('end', State) ->
    scroll_end(State);
 %% zoom in - FIXME configure this
command_($-, State) ->
    Zoom = max(-20, State#state.zoom - 1),
    Scale = zscale(Zoom),
    State1 = State#state { zoom = Zoom, scale = Scale },
    State2 = adjust_view_xpos(get_view_xpos(State1), State1),
    State3 = adjust_view_ypos(get_view_ypos(State2), State2),
    all_dirty_area(State3);
 %% zoom out - FIXME configure this
command_($+, State) ->
    Zoom = min(20, State#state.zoom + 1),
    Scale = zscale(Zoom),
    State1 = State#state { zoom = Zoom, scale = Scale },
    State2 = adjust_view_xpos(get_view_xpos(State1), State1),
    State3 = adjust_view_ypos(get_view_ypos(State2), State2),
    all_dirty_area(State3);
command_(_Sym, State) ->
    ?dbg("unhandled command ~p\n", [_Sym]),
    State.


%% calculate scale from zoom (Sx=Sy) now
zscale(Z) ->
    Sx = Sy = math:pow(1.07, 2*(Z)),
    {Sx,Sy}.

scroll_up(State) ->
    State1 = step_up(State, scroll_ystep(State)),
    all_dirty_area(State1).

scroll_down(State) ->
    State1 = step_down(State, scroll_ystep(State)),
    all_dirty_area(State1).

scroll_left(State) ->
    State1 = step_left(State, scroll_xstep(State)),
    all_dirty_area(State1).

scroll_right(State) ->
    State1 = step_right(State, scroll_xstep(State)),
    all_dirty_area(State1).

scroll_home(State) ->
    State1 = set_view_ypos(State, 0),
    all_dirty_area(State1).

scroll_end(State) ->
    Y0 = get_view_bottom(State),
    Y1 = adjust_bottom_pos(Y0, State),
    State1 = set_view_ypos(State, Y1),
    all_dirty_area(State1).

%% Drawing area width when scrollbar is taken into account
drawing_width(State) ->
    drawing_width_(get_vscroll(State), State).
    
drawing_width_(none, State) -> drawing_width0(State);
drawing_width_(undefined, State) -> drawing_width0(State);
drawing_width_(_VBar,State) -> drawing_width0(State)-scroll_bar_size(State).

%% Drawing area height when scrollbar is taken into account
drawing_height(State) ->
    drawing_height_(get_hscroll(State), State).

drawing_height_(none, State) -> drawing_height0(State);
drawing_height_(undefined, State) -> drawing_height0(State);
drawing_height_(_VBar,State) -> drawing_height0(State)-scroll_bar_size(State).

%% adjust xpos given view x coord    
adjust_view_xpos(Vx, State) ->
    Vx1  = adjust_right_pos(max(0, Vx), State),
    set_view_xpos(State, Vx1).

%% adjust ypos given view y coord
adjust_view_ypos(Vy, State) ->
    Vy1 = adjust_bottom_pos(max(0,Vy), State),
    set_view_ypos(State, Vy1).

%% adjust view coordinate 
adjust_bottom_pos(Y, State) ->
    DrawingHeight = window_to_view_height(drawing_height(State), State),
    Ymax = max(0, get_view_bottom(State)-DrawingHeight),
    min(Y, Ymax).

adjust_top_pos(Y, _State) ->
    max(0, Y).

adjust_right_pos(X, State) ->
    DrawingWidth = window_to_view_width(drawing_width(State), State),
    Xmax = max(0, get_view_right(State)-DrawingWidth),
    min(X, Xmax).

adjust_left_pos(X, _State) ->
    max(0, X).

%% move a page up
page_up(State) ->
    case get_vscroll(State) of
	undefined ->
	    State;
	{_,_,_,H} ->
	    {_,_,_,VH} = get_vhndl(State), %% page size in window coords
	    Pr = VH/H,  %% page ratio
	    Page = Pr*get_view_height(State),
	    State1 = step_up(State, Page),
	    all_dirty_area(State1)
    end.

page_down(State) ->
    case get_vscroll(State) of
	undefined ->
	    State;
	{_,_,_,H} ->
	    {_,_,_,VH} = get_vhndl(State),  %% page size in window coords
	    %% FIXME: check H
	    R = VH/H,  %% page ratio
	    Page = R*get_view_height(State),
	    State1 = step_down(State, Page),
	    all_dirty_area(State1)
    end.

step_up(State, Step) ->
    Y = adjust_top_pos(get_view_ypos(State) - Step, State),
    set_view_ypos(State, Y).

step_down(State, Step) ->
    Y = adjust_bottom_pos(get_view_ypos(State) + Step, State),
    set_view_ypos(State, Y).

step_left(State, Step) ->
    X = adjust_left_pos(get_view_xpos(State) - Step, State),
    set_view_xpos(State, X).

step_right(State, Step) ->
    X = adjust_right_pos(get_view_xpos(State) + Step, State),
    set_view_xpos(State, X).

%% update mod keys
set_mod(M, [shift|Mod]) ->  set_mod(M#keymod {shift = true}, Mod);
set_mod(M, [ctrl|Mod]) ->   set_mod(M#keymod {ctrl = true}, Mod);
set_mod(M, [alt|Mod]) ->    set_mod(M#keymod {alt = true}, Mod);
set_mod(M, [_|Mod]) ->      set_mod(M, Mod);
set_mod(M, []) -> M.

clr_mod(M, [shift|Mod]) ->  clr_mod(M#keymod {shift = false}, Mod);
clr_mod(M, [ctrl|Mod]) ->   clr_mod(M#keymod {ctrl = false}, Mod);
clr_mod(M, [alt|Mod]) ->    clr_mod(M#keymod {alt = false}, Mod);
clr_mod(M, [_|Mod]) ->      clr_mod(M, Mod);
clr_mod(M, []) -> M.

flush_wheel(Window) ->
    receive
	{epx_event,Window,{_,[wheel_down],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_left],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_right],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_up],_}} ->
	    flush_wheel(Window)
    after 0 ->
	    ok
    end.

flush_configure(Win, Rect) ->
    receive
	{epx_event, Win, {configure, Rect1}} ->
	    flush_configure(Win, Rect1)
    after 0 ->
	    Rect
    end.

%%flush_expose(Win, Rect) ->
%%    receive
%%	{epx_event, Win, {expose, Rect1}} ->
%%	    flush_expose(Win, Rect1)
%%    after 0 ->
%%	    Rect
%%    end.

flush_motion(Win) ->
    receive
	{epx_event, Win, {motion, _Mod, _Pos}} ->
	    flush_motion(Win)
    after 0 ->
	    ok
    end.

enable_sys_motion(State) ->
    enable_motion_(true, ?SYS_MOTION, State).
disable_sys_motion(State) ->
    enable_motion_(false, ?SYS_MOTION, State).

enable_motion_(true, Motion, State) ->
    Motion1 = State#state.motion bor Motion,
    if State#state.motion =:= 0 -> %% need to enable?
	    epx:window_enable_events(State#state.window,[motion]);
       true ->
	    ok
    end,
    State#state { motion = Motion1 };
enable_motion_(false, Motion, State) ->
    Motion1 = State#state.motion band (bnot Motion),
    if Motion1 =:= 0 -> %% need to disable
	    epx:window_disable_events(State#state.window, [motion]);
       true ->
	    ok
    end,
    State#state { motion = Motion1 }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(Reason, State) ->
    case ?CALLBACK(State, terminate) of
	undefined ->
	    ok;
	Terminate ->
	    export_state(State),
	    Terminate(Reason, State#state.user_state)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(OldVsn, State, Extra) ->
    case ?CALLBACK(State, code_change) of
	undefined ->
	    {ok, State};
	CodeChange ->
	    export_state(State),
	    case CodeChange(OldVsn, State#state.user_state, Extra) of
		{ok,UState} ->
		    State1 = state(),
		    {ok, State1#state { user_state = UState }};
		Error ->
		    Error
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

select(Phase,{X1,Y1,_},{X2,Y2,_}, State) ->
    {X11,X22}=minmax(X1,X2),
    {Y11,Y22}=minmax(Y1,Y2),
    Area = {X11,Y11,(X22-X11)+1,(Y22-Y11)+1},
    user_event({Phase,Area},?CALLBACK(State,select),State).

minmax(A, B) when A < B -> {A,B};
minmax(A, B) -> {B,A}.

%% FIXME: allow user_event to return stop, noreply

user_event(_E, undefined, State) ->
    State;
user_event(E, Callback, State) ->
    UE = transform_event(E, State),
    export_state(State),
    try Callback(UE, State#state.user_state) of
	UserState ->
	    State1 = state(),
	    State1#state { user_state = UserState }
    catch
	error:Reason:Stack ->
	    io:format("user_event: crash ~p\n~p\n", [Reason, Stack]),
	    State
    end.


transform_event(Event={button_press,_,Pos}, State) ->
    Pos1 = window_to_view_pos(Pos,State),
    setelement(3, Event, Pos1);
transform_event(Event={button_release,_,Pos}, State) ->
    Pos1 = window_to_view_pos(Pos,State),
    setelement(3, Event, Pos1);
transform_event(Event={motion,_,Pos}, State) ->
    Pos1 = window_to_view_pos(Pos,State),
    setelement(3, Event, Pos1);
transform_event(Event={enter,Pos},State) ->
    Pos1 = window_to_view_pos(Pos,State),
    setelement(2, Event, Pos1);
transform_event(Event={leave,Pos},State) ->
    Pos1 = window_to_view_pos(Pos,State),
    setelement(2, Event, Pos1);
transform_event({Tag,{X,Y,W,H}},State) when is_number(X), is_number(Y),
					    is_number(W), is_number(H) ->
    {X1,Y1} = window_to_view_pos({X,Y,0},State),
    {W1,H1} = window_to_view_dim({W,H},State),
    {Tag,{X1,Y1,W1,H1}};
transform_event(Event, _State) ->
    Event.

%% Wx = (Vx - Tx)*Sx + Cx
-define(VIEW_TO_WINDOW_X(X, Tx, Sx, Cx), (((X)-(Tx))*(Sx) + (Cx))).
-define(VIEW_TO_WINDOW_Y(Y, Ty, Sy, Cy), (((Y)-(Ty))*(Sy) + (Cy))).
-define(VIEW_TO_WINDOW_W(W, Sx), ((W)*(Sx))).
-define(VIEW_TO_WINDOW_H(H, Sy), ((H)*(Sy))).

%% Vx = (Wx - Cx)/Sx + Tx
-define(WINDOW_TO_VIEW_X(X,Tx,Sx,Cx),  ((((X)-(Cx))/(Sx)) + (Tx))).
-define(WINDOW_TO_VIEW_Y(Y,Ty,Sy,Cy),  ((((Y)-(Cy))/(Sy)) + (Ty))).
-define(WINDOW_TO_VIEW_W(W, Sx), ((W)/(Sx))).
-define(WINDOW_TO_VIEW_H(H, Sy), ((H)/(Sy))).

%% Transform window coordinate 3d into view coodinate 2d
window_to_view_rect({X,Y,W,H}, State) ->
    #state { scale = {Sx,Sy},
	     content=#window_content{view_xpos=Tx,view_ypos=Ty}} = State,
    {Cx,Cy} = drawing_origin(State),
    { ?WINDOW_TO_VIEW_X(X,Tx,Sx,Cx), ?WINDOW_TO_VIEW_Y(Y,Ty,Sy,Cy),
      ?WINDOW_TO_VIEW_W(W,Sx), ?WINDOW_TO_VIEW_H(H, Sy) }.

window_to_view_pos({X,Y,_Z}, State) ->
    #state { scale = {Sx,Sy},
	     content=#window_content{view_xpos=Tx,view_ypos=Ty}} = State,
    {Cx,Cy} = drawing_origin(State),
    { ?WINDOW_TO_VIEW_X(X,Tx,Sx,Cx), ?WINDOW_TO_VIEW_Y(Y,Ty,Sy,Cy) }.

window_to_view_x(X, State=#state { scale = {Sx,_Sy},
				   content=#window_content{view_xpos=Tx}}) ->
    {Cx,_Cy} = drawing_origin(State),
    ?WINDOW_TO_VIEW_X(X,Tx,Sx,Cx).

window_to_view_y(Y, State=#state { scale = {_Sx,Sy},
				   content=#window_content{view_ypos=Ty}}) ->
    {_Cx,Cy} = drawing_origin(State),
    ?WINDOW_TO_VIEW_Y(Y,Ty,Sy,Cy).

window_to_view_dim({W,H}, #state { scale={Sx,Sy}}) ->
    { ?WINDOW_TO_VIEW_W(W,Sx), ?WINDOW_TO_VIEW_H(H, Sy) }.

window_to_view_height(H, #state { scale={_,Sy}}) ->
    ?WINDOW_TO_VIEW_H(H, Sy).

window_to_view_width(W, #state { scale={Sx,_}}) ->
    ?WINDOW_TO_VIEW_W(W, Sx).

view_to_window_height(H, #state { scale={_,Sy}}) ->
    ?VIEW_TO_WINDOW_H(H,Sy).

view_to_window_width(W, #state { scale={Sx,_}}) ->
    ?VIEW_TO_WINDOW_W(W,Sx).

view_to_window_x(X, 
		 State = 
		     #state { scale = {Sx,_Sy},
			      content= 
				  #window_content{view_xpos=Tx}}) ->
    {Cx,_Cy} = drawing_origin(State),
    ?VIEW_TO_WINDOW_X(X,Tx,Sx,Cx).

view_to_window_y(Y,
		 State = 
		     #state { scale = {_Sx,Sy},
			      content= 
				  #window_content{view_xpos=Ty}}) ->
    {_Cx,Cy} = drawing_origin(State),
    ?VIEW_TO_WINDOW_Y(Y,Ty,Sy,Cy).

view_to_window_pos({X,Y}, 
		   State = 
		       #state { scale = {Sx,Sy},
				content= 
				    #window_content{view_xpos=Tx,
						    view_ypos=Ty}}) ->
    
    {Cx,Cy} = drawing_origin(State),
    { ?VIEW_TO_WINDOW_X(X,Tx,Sx,Cx),?VIEW_TO_WINDOW_Y(Y,Ty,Sy,Cy) }.

view_to_window_rect({X,Y,W,H}, 
		    State = 
			#state { scale = {Sx,Sy},
				 content= 
				     #window_content{view_xpos=Tx,
						     view_ypos=Ty}}) ->
    {Cx,Cy} = drawing_origin(State),
    { ?VIEW_TO_WINDOW_X(X,Tx,Sx,Cx),?VIEW_TO_WINDOW_Y(Y,Ty,Sy,Cy),
      ?VIEW_TO_WINDOW_W(W,Sx), ?VIEW_TO_WINDOW_H(H,Sy)}.

get_scaled_view_width(S) ->
    view_to_window_width(get_view_width(S), S).

get_scaled_view_height(S) ->
    view_to_window_height(get_view_height(S), S).


draw_(State) ->
    draw_(State, State#state.dirty).
draw_(State, Dirty) ->
    try draw__(State, Dirty) of
	State1 ->
	    State1
    catch
	error:Reason:Stack ->
	    io:format("draw_ error: ~p\n~p\n", [Reason,Stack]),
	    State
    end.

draw__(State = #state { profile = Profile }, Dirty) ->
    Scheme = Profile#profile.scheme,
    ScreenColor = epx_profile:color(Scheme, Profile#profile.screen_color),
    Pixels = pixels(State),
    {Tx,Ty} = get_view_pos(State),
    ScrollBars = {HBar,VBar} = scrollbars(State),
    {Cx,Cy} = drawing_origin_(State,ScrollBars),
    {Sx,Sy} = State#state.scale,
    ?verbose("ZOOM = ~w, SCALE = ~w\n", [State#state.zoom, {Sx,Sy}]),

    epx:pixmap_ltm_reset(Pixels),
    epx:pixmap_ltm_translate(Pixels, Cx, Cy),
    epx:pixmap_ltm_scale(Pixels, Sx, Sy),
    epx:pixmap_ltm_translate(Pixels, -Tx, -Ty),

    fill_pixels(Pixels,Dirty,ScreenColor),

    {Dirty1, State1} = draw_content_(Pixels,Dirty,State),
    epx:pixmap_ltm_reset(Pixels),
    epx_gc:set_border_width(0), %% FIXME: use special gc for user content
    State2 = draw_vscroll(Pixels,VBar,HBar,State1),
    State3 = draw_hscroll(Pixels,HBar,VBar,State2),
    State4 = draw_top_bar(Pixels,State3),
    State5 = draw_left_bar(Pixels,State4),
    State6 = draw_right_bar(Pixels,State5),
    State7 = draw_bottom_bar(Pixels,State6),
    State8 = draw_menu(Pixels,State7),
    update_window(State8, Dirty1).

%% FIXME: we may want to reload the menu here?
draw_menu(Pixels, State) ->
    if State#state.operation =:= menu, State#state.pt1 =/= undefined ->
	    State1 = reload_menu(State),
	    epx_menu:draw(State1#state.menu, Pixels, State1#state.pt1),
	    State1;
       true ->
	    State
    end.

reload_menu(State) ->
    case ?CALLBACK(State, menu) of
	undefined -> 
	    State;
	Menu ->
	    case Menu({menu,State#state.pt1},State#state.user_state) of
		{noreply,UserState} ->
		    State#state { user_state = UserState};
		{reply,undefined,UserState} ->
		    State#state { user_state = UserState};
		{reply,Menu0,UserState} ->
		    {_Row,Menu1} = epx_menu:find_row(Menu0,
						     State#state.pt1,
						     State#state.pt2),
		    State#state { menu = Menu1,
				  user_state = UserState}
	    end
    end.

%% Update and intersect dirty region and let user code
%% update that area, return this area intersected with visible rect
draw_content_(Pixmap, Dirty, State) ->
    DirtyRegion = dirty_content_region(Dirty, State),
    export_state(State),
    UserState = user_draw_content(Pixmap,DirtyRegion,State),
    State1 = state(),
    {DirtyRegion, State1#state { user_state = UserState }}.

user_draw_content(Pixmap, Region, State=#state{user_cb=Cb}) ->
    case Cb#callbacks.draw of  %% prefere draw for content if present
	undefined ->
	    case Cb#callbacks.draw4 of
		undefined ->
		    State#state.user_state;
		Draw4 ->
		    ?verbose("DRAW REGION = ~w\n", [Region]),
		    Draw4(content,Pixmap,Region,State#state.user_state)
	    end;
	Draw ->
	    Draw(Pixmap, Region,State#state.user_state)
    end.

%% draw into toolbar area
-spec user_draw_area(Where::toolbar(), Pixels::epx:epx_pixmap(),
		     Region::epx:epx_rect(), State::#state{}) ->
	  #state{}.

user_draw_area(Where, Pixels, Region, State=#state{user_cb=Cb}) ->
    case Cb#callbacks.draw4 of
	undefined -> State;
	Draw4 -> 
	    export_state(State),
	    SaveClip = epx:pixmap_info(Pixels, clip),
	    epx:pixmap_set_clip(Pixels, Region),
	    ?verbose("DRAW ~w REGION = ~w\n", [Where, Region]),
	    UserState = Draw4(Where,Pixels, Region,State#state.user_state),
	    epx:pixmap_set_clip(Pixels, SaveClip),
	    State1 = state(),
	    State1#state { user_state = UserState }
    end.

%%
%% build content region as one rectangle or list of rectangles, 
%% the rectangles are intersected with the current window rectangle
%% in view coordinates.
%% 
dirty_content_region(Dirty, State) ->
    dirty_content_region_(Dirty, State, get_visible_rect(State)).

dirty_content_region_(all, State, ViewRect) ->
    if (State#state.profile)#profile.dirty_list ->
	    [ViewRect];
       true ->
	    ViewRect
    end;
dirty_content_region_(Dirty=[Rect], State, ViewRect) ->
    if (State#state.profile)#profile.dirty_list ->
	    intersect_region(Dirty, ViewRect);
       true ->
	    epx_rect:intersect(Rect, ViewRect)
    end;
dirty_content_region_(Rect, State, ViewRect) when tuple_size(Rect) =:= 4 ->
    if (State#state.profile)#profile.dirty_list ->
	    intersect_region([Rect], ViewRect);
       true ->
	    epx_rect:intersect(Rect, ViewRect)
    end;
dirty_content_region_(Dirty, State, ViewRect) when is_list(Dirty) ->
    if (State#state.profile)#profile.dirty_list ->
	    intersect_region(Dirty, ViewRect);
       true ->
	    epx_rect:intersect(epx_rect:union(Dirty), ViewRect)
    end.

dirty_window_area(Dirty, State) when tuple_size(Dirty) =:= 4 ->
    view_to_window_rect(Dirty, State);
dirty_window_area(Dirty, State) when is_list(Dirty) ->
    [view_to_window_rect(R, State) || R <- Dirty].
    

%% intersect (and remove empty) rectangles with Rect
intersect_region([R|Rs], Rect) ->
    case epx_rect:intersect(R, Rect) of
	{_,_,_,0} -> intersect_region(Rs, Rect);
	{_,_,0,_} -> intersect_region(Rs, Rect);
	R1 -> [R1|intersect_region(Rs, Rect)]
    end;
intersect_region([], _Rect) ->
    [].

%% 
%% Api version function to direct draw content
%%
-spec draw(Where::where(), 
	   Draw::fun((Pixmap::epx:epx_pixmap(),Area::epx:epx_rect()) ->
			    Result::term())) -> Result::term().

draw(content, Draw) ->
    draw(Draw);
draw(top, Draw) ->
    State = state(),
    case toolbars(State) of
	{_Left,_Right,Top,_Bottom}
	  when Top > 0 ->
	    DrawRect = {0,0,State#state.width,Top},
	    draw_area(Draw, DrawRect, State);
	_ ->
	    false
    end;
draw(left, Draw) ->
    State = state(),
    case toolbars(State) of
	{Left,_Right,Top,Bottom} when
	      Left > 0, State#state.height >= (Top+Bottom) ->
	    DrawRect = {0,Top,Left,State#state.height-(Top+Bottom)},
	    draw_area(Draw, DrawRect, State);
	_ ->
	    false
    end;
draw(right, Draw) ->
    State = state(),
    case toolbars(State) of
	{_Left,Right,Top,Bottom} when Right > 0, State#state.width >= Right,
				      State#state.height >= (Top+Bottom)->
	    DrawRect = {State#state.width-Right,Top,Right,
			State#state.height-(Top+Bottom)},
	    draw_area(Draw, DrawRect, State);
	_ ->
	    false
    end;
draw(bottom, Draw) ->
    State = state(),
    case toolbars(State) of
	{_Left,_Right,_Top,Bottom}  
	  when Bottom > 0, State#state.height >= Bottom ->
	    DrawRect = {0,State#state.height-Bottom,State#state.width,Bottom},
	    draw_area(Draw, DrawRect, State);
	_ ->
	    false
    end.

%% draw into toolbar areas, window coordinates
draw_area(Draw, DrawRect, State) when is_function(Draw, 2)->
    Pixels = pixels(State),
    SaveClip = epx:pixmap_info(Pixels, clip),
    epx:pixmap_ltm_reset(Pixels),
    epx:pixmap_set_clip(Pixels, DrawRect),
    Result = Draw(Pixels, DrawRect),

    copy_pixels(Pixels, DrawRect, State#state.screen),
    draw_pixels(State#state.screen, DrawRect, State#state.window,
		State#state.width,State#state.height),
    epx:sync(State#state.screen,State#state.window),
    epx:pixmap_set_clip(Pixels, SaveClip),
    Result.

%% Direct draw into content area, update and sync
draw(Draw) when is_function(Draw) ->
    State = state(),
    Pixels = pixels(State),
    SaveClip = epx:pixmap_info(Pixels, clip),
    {Tx,Ty} = get_view_pos(State),
    ScrollBars={HBar,VBar} = scrollbars(State),
    {Cx,Cy} = drawing_origin_(State,ScrollBars),
    W = drawing_width_(VBar,State),
    H = drawing_height_(HBar,State),
    {Sx,Sy} = State#state.scale,
    epx:pixmap_ltm_reset(Pixels),
    epx:pixmap_ltm_translate(Pixels, Cx, Cy),
    epx:pixmap_ltm_scale(Pixels, Sx, Sy),
    epx:pixmap_ltm_translate(Pixels, -Tx, -Ty),
    Clip = {Cx,Cy,W,H},
    ?verbose("CLIP = ~w\n", [Clip]),
    epx:pixmap_set_clip(Pixels, Clip),
    VisibleRect = {Tx, Ty, ?WINDOW_TO_VIEW_W(W,Sx), ?WINDOW_TO_VIEW_H(H,Sy) },
    ?verbose("draw:VISBLE_RECT = ~w\n", [VisibleRect]),
    Result = Draw(Pixels, VisibleRect),
    State1 = state(), %% pickup potential new state
    draw_window(Pixels, State1, State1#state.dirty),
    epx:sync(State#state.screen,State#state.window),
    epx:pixmap_ltm_reset(Pixels),
    epx:pixmap_set_clip(Pixels, SaveClip),
    export_state(State1#state { dirty = [] }),
    Result.

%% Area maybe a window rect (ltm = unit) or a dirty region in 
%% window coordinates.
draw_window(Pixels, State, Area) ->
    copy_pixels(Pixels, Area, State#state.screen),
    Area1 = dirty_window_area(Area, State),
    draw_pixels(State#state.screen,Area1,State#state.window,
		State#state.width,State#state.height).

update_window(State, Dirty) ->
    ?verbose("UPDATE Dirty = ~w\n", [Dirty]),
    copy_pixels(pixels(State),Dirty,State#state.screen),
    %% At this point Dirty MUST be in view coordinates and intersected
    %% with visible area. Not convert into window coordinates
    _Dirty1 = dirty_window_area(Dirty, State),
    ?verbose("UPDATE WINDOW Dirty = ~w\n", [_Dirty1]),
    Dirty2 = all,  %% FIXME!
    draw_pixels(State#state.screen,Dirty2,State#state.window,
		State#state.width,State#state.height),
    epx:sync(State#state.screen,State#state.window),
    State#state { dirty = [] }.

copy_pixels(undefined, _Rect, _Screen) -> %% no off-screen
    ok;
copy_pixels(Screen, _Rect, Screen) -> %% no off-screen 
    ok;
copy_pixels(_Pixels, [], _Screen) -> %% nothing
    ok;
copy_pixels(Pixels, all, Screen) -> %% everything
    epx:epx_copy_to(Pixels, Screen);
copy_pixels(Pixels, {X,Y,W,H}, Screen) ->
    epx:pixmap_copy_area(Pixels,Screen,X,Y,X,Y,W,H);
copy_pixels(Pixels, Rs, Screen) when is_list(Rs) ->
    lists:foreach(
      fun({X,Y,W,H}) ->
	      epx:pixmap_copy_area(Pixels,Screen,X,Y,X,Y,W,H)
      end, Rs).

fill_pixels(_Pixels, [], _Color) -> %% nothing
    ok;
fill_pixels(Pixels, all, Color) -> %% everything
    epx:pixmap_fill(Pixels, Color);
fill_pixels(Pixels, {X,Y,W,H}, Color) ->
    epx:pixmap_fill_area(Pixels,X,Y,W,H,Color);
fill_pixels(Pixels, Rs, Color) when is_list(Rs) ->
    lists:foreach(
      fun({X,Y,W,H}) ->
	      epx:pixmap_fill_area(Pixels,X,Y,W,H,Color)
      end, Rs).

%% NOTE! pixmap_draw work with window coodinates only
draw_pixels(Screen, all, Window, W, H) ->
    epx:pixmap_draw(Screen, Window, 0, 0, 0, 0, W, H);
draw_pixels(Screen, {X,Y,W,H}, Window, _W, _H) ->
    epx:pixmap_draw(Screen, Window, X, Y, X, Y, W, H);
draw_pixels(Screen, Rs, Window, _W, _H) when is_list(Rs) ->
    lists:foreach(
      fun({X,Y,W,H}) ->
	      epx:pixmap_draw(Screen, Window, X, Y, X, Y, W, H)
      end, Rs).

pixels(State) ->
    if State#state.pixels =/= undefined ->
	    State#state.pixels;
       true ->
	    State#state.screen
    end.

%% fill_area(Pixmap, all, Color) ->
%%    epx:pixmap_fill(Pixmap,Color);
%% fill_area(Pixmap, {X,Y,W,H}, Color) ->
%%    epx:pixmap_fill_area(Pixmap,X,Y,W,H,Color).

%% scrollbar
scrollbars(State) ->
    ScrollBarSize = scroll_bar_size(State),
    DrawingWidth = drawing_width0(State),
    DrawingHeight = drawing_height0(State),
    Width = get_scaled_view_width(State),
    Height = get_scaled_view_height(State),
    if Width > DrawingWidth ->
	    if Height > (DrawingHeight-ScrollBarSize) ->
		    {scroll_horizontal(State), scroll_vertical(State)};
	       true ->
		    {scroll_horizontal(State), none}
	    end;
       Height > DrawingHeight ->
	    if Width > (DrawingWidth-ScrollBarSize) ->
		    {scroll_horizontal(State), scroll_vertical(State)};
	       true ->
		    {none,                     scroll_vertical(State)}
	    end;
       true ->
	    {none, none}
    end.
       
draw_vscroll(Pixels,left,HBar,State) ->
    X0 = left_bar(State),
    draw_vscroll_(Pixels,X0,HBar,State);
draw_vscroll(Pixels,right,HBar,State) ->
    Size = scroll_bar_size(State),
    RightBar = right_bar(State),
    X0 = State#state.width-Size-RightBar,
    draw_vscroll_(Pixels,X0,HBar,State);
draw_vscroll(_Pixels,none,_OtherBar,State) ->
    set_vscroll(State, undefined, undefined).

draw_vscroll_(Pixels, X0, HBar, State) ->
    DrawingHeight = drawing_height_(HBar,State),
    if DrawingHeight > 0 ->
	    draw_vscroll__(Pixels, X0, DrawingHeight, HBar, State);
       true ->
	    set_vscroll(State, undefined, undefined)
    end.

draw_vscroll__(Pixels, X0, DrawingHeight, HBar, State) ->
    ScrollBarSize = scroll_bar_size(State),
    HndlSize = scroll_hndl_size(State),
    TopBar = top_bar(State),
    epx_gc:set_fill_style(solid),
    ScrollBarColor = scroll_bar_color(State),
    epx_gc:set_fill_color(ScrollBarColor),
    Y0 = case HBar of
	     none   -> TopBar;
	     bottom -> TopBar;
	     top    -> TopBar+ScrollBarSize
	 end,
    Rect = {X0,Y0,ScrollBarSize,DrawingHeight},
    epx_gc:set_border_width(0),
    epx:draw_rectangle(Pixels, Rect),
    epx_gc:set_fill_color(scroll_hndl_color(State)),
    SVh = get_scaled_view_height(State),
    %% scroll size as ratio of scaled height
    Ht = if SVh == 0 -> 0;
	    true -> DrawingHeight / SVh
	 end,
    HandleLength = Ht*DrawingHeight,
    %% Top position as ratio (0,1)
    Vh = get_view_height(State),
    Yt = if Vh == 0 -> 0;
	    true -> get_view_ypos(State) / Vh
	 end,
    HandlePos = Y0+Yt*DrawingHeight,
    Pad = (ScrollBarSize-HndlSize) / 2,
    HRect = {X0+Pad,HandlePos,HndlSize,HandleLength},
    %% FIXME style flat/round (rx,ry)
    %% epx:draw_roundrect(Pixels,HRect,5,5),
    epx:draw_rectangle(Pixels,HRect),
    set_vscroll(State, Rect, HRect).

draw_hscroll(Pixels,top,VBar,State) ->
    Y0 = top_bar(State),
    draw_hscroll_(Pixels,Y0,VBar,State);
draw_hscroll(Pixels,bottom,VBar,State) ->
    Size = scroll_bar_size(State),
    BottomBar = bottom_bar(State),
    Y0 = State#state.height-Size-BottomBar,
    draw_hscroll_(Pixels,Y0,VBar,State);
draw_hscroll(_Pixels,none,_OtherBar,State) ->
    set_hscroll(State, undefined, undefined).

draw_hscroll_(Pixels, Y0, VBar, State) ->
    DrawingWidth = drawing_width_(VBar,State),
    if DrawingWidth > 0 ->
	    draw_hscroll__(Pixels, Y0, DrawingWidth, VBar, State);
       true ->
	    set_hscroll(State, undefined, undefined)
    end.

draw_hscroll__(Pixels, Y0, DrawingWidth, VBar, State) ->
    ScrollBarSize = scroll_bar_size(State),
    HndlSize = scroll_hndl_size(State),
    epx_gc:set_fill_style(solid),
    ScrollBarColor = scroll_bar_color(State),
    epx_gc:set_fill_color(ScrollBarColor),
    LeftBar = left_bar(State),
    X0 = case VBar of
	     none -> LeftBar;
	     left -> LeftBar+ScrollBarSize;
	     right -> LeftBar
	 end,
    Rect = {X0,Y0,DrawingWidth,ScrollBarSize},
    epx_gc:set_border_width(0),
    epx:draw_rectangle(Pixels, Rect),
    %% Fill the square between vertical & horizonal
    case VBar of
	none -> 
	    ok;
	left ->
	    Square = {LeftBar,Y0,ScrollBarSize-2,ScrollBarSize-2},
	    epx_gc:set_border_width(1),
	    ScrollBarColor1 = darken(ScrollBarColor,20),
	    epx_gc:set_border_color(ScrollBarColor1),
	    epx:draw_rectangle(Pixels, Square);
	right ->
	    Square = {X0+DrawingWidth,Y0,
		      ScrollBarSize-2,ScrollBarSize-2},
	    epx_gc:set_border_width(1),
	    ScrollBarColor1 = darken(ScrollBarColor,20),
	    epx_gc:set_border_color(ScrollBarColor1),
	    epx:draw_rectangle(Pixels, Square)
    end,
    epx_gc:set_border_width(0),
    epx_gc:set_fill_color(scroll_hndl_color(State)),
    
    SVw = get_scaled_view_width(State),
    Wt = if SVw == 0 -> 0;
	    true -> DrawingWidth / SVw
	 end,
    HandleLength = Wt*DrawingWidth,
    Vw = get_view_width(State),
    Xt = if Vw == 0 -> 0;
	    true -> get_view_xpos(State) / Vw
	 end,
    HandlePos = X0+Xt*DrawingWidth,
    Pad = (ScrollBarSize-HndlSize) / 2,
    HRect = {HandlePos,Y0+Pad,HandleLength,HndlSize},
    %% FIXME style flat/round (rx,ry)
    %% epx:draw_roundrect(Pixels,HRect,5,5),
    epx:draw_rectangle(Pixels,HRect),
    set_hscroll(State, Rect, HRect).

get_hscroll(#state { content = WD }) -> WD#window_content.hscroll.
get_hhndl(#state { content = WD }) -> WD#window_content.hhndl.
get_vscroll(#state { content = WD }) -> WD#window_content.vscroll.
get_vhndl(#state { content = WD }) -> WD#window_content.vhndl.

set_hscroll(S=#state { content = WD }, Rect, Hndl) ->
    S#state { content = WD#window_content { hscroll=Rect,hhndl=Hndl }}.

set_vscroll(S=#state { content = WD }, Rect, Hndl) ->
    S#state { content = WD#window_content { vscroll=Rect,vhndl=Hndl }}.

toolbars(#state { winfo = WI }) ->
    #window_info { left_bar = L, right_bar = R,
		   top_bar = T, bottom_bar = B } = WI,
    {L, R, T, B}.

top_bar(#state { winfo = WI }) -> WI#window_info.top_bar.
left_bar(#state { winfo = WI }) -> WI#window_info.left_bar.
right_bar(#state { winfo = WI }) -> WI#window_info.right_bar.
bottom_bar(#state { winfo = WI }) -> WI#window_info.bottom_bar.

%%top_offset(#state { winfo = WI }) -> 
%%    WI#window_info.top_offset + WI#window_info.top_bar.
%%left_offset(#state { winfo = WI }) -> 
%%    WI#window_info.left_offset + WI#window_info.left_bar.
%%right_offset(#state { winfo = WI }) -> 
%%    WI#window_info.right_offset + WI#window_info.right_bar.
%%bottom_offset(#state { winfo = WI }) -> 
%%    WI#window_info.bottom_offset + WI#window_info.bottom_bar.

scroll_hndl_size(#state { winfo = WI }) -> WI#window_info.scroll_hndl_size.
scroll_bar_size(#state { winfo = WI }) -> WI#window_info.scroll_bar_size.

scroll_horizontal(#state { winfo = WI }) -> WI#window_info.scroll_horizontal.
scroll_vertical(#state { winfo = WI }) -> WI#window_info.scroll_vertical.

scroll_xstep(#state { winfo = WI }) -> WI#window_info.scroll_xstep.
scroll_ystep(#state { winfo = WI }) -> WI#window_info.scroll_ystep.

glyph_ascent(#state { winfo = WI }) -> WI#window_info.glyph_ascent.

%% profile acces
%% background_color(#state { content = WD }) ->
%%    P = WD#window_content.profile,
%%    epx_profile:color(P#window_profile.scheme,
%%		      P#window_profile.background_color).

top_bar_color(#state { content = WD }) ->
    P = WD#window_content.profile,
    epx_profile:color(P#window_profile.scheme,
		      P#window_profile.top_bar_color).

left_bar_color(#state { content = WD }) ->
    P = WD#window_content.profile,
    epx_profile:color(P#window_profile.scheme,
		      P#window_profile.left_bar_color).

right_bar_color(#state { content = WD }) ->
    P = WD#window_content.profile,
    epx_profile:color(P#window_profile.scheme,
		      P#window_profile.right_bar_color).

bottom_bar_color(#state { content = WD }) ->
    P = WD#window_content.profile,
    epx_profile:color(P#window_profile.scheme,
		      P#window_profile.bottom_bar_color).

scroll_bar_color(#state { content = WD }) ->
    P = WD#window_content.profile,
    epx_profile:color(P#window_profile.scheme,
		      P#window_profile.scroll_bar_color).

scroll_hndl_color(#state { content = WD }) ->
    P = WD#window_content.profile,
    epx_profile:color(P#window_profile.scheme,
		      P#window_profile.scroll_hndl_color).

darken({R,G,B},N) ->
    {max(0,R-N), max(0,G-N), max(0, B-N)}.

get_view_xpos(#state { content = WD }) -> WD#window_content.view_xpos.
get_view_ypos(#state { content = WD }) -> WD#window_content.view_ypos.

%% view position that is displayed in drawing area
get_view_pos(#state { content = WD }) -> 
    {WD#window_content.view_xpos,WD#window_content.view_ypos}.

set_view_xpos(S = #state{ content = WD}, X) ->
    S#state { content = WD#window_content { view_xpos = X }}.
set_view_ypos(S = #state{ content = WD}, Y) ->
    S#state { content = WD#window_content { view_ypos = Y }}.
set_view_pos(S = #state{ content = WD}, X, Y) ->
    S#state { content = WD#window_content { view_xpos = X, view_ypos = Y }}.

get_view_left(#state { content = WD }) -> WD#window_content.view_left.
get_view_right(#state { content = WD }) -> WD#window_content.view_right.
get_view_top(#state { content = WD }) -> WD#window_content.view_top.
get_view_bottom(#state { content = WD }) -> WD#window_content.view_bottom.
get_view_width(#state { content = WD }) ->
    (WD#window_content.view_right - WD#window_content.view_left).    
get_view_height(#state { content = WD }) ->
    (WD#window_content.view_bottom - WD#window_content.view_top).
get_view_rect(#state { content = WD }) ->
    #window_content { view_left = L, view_right = R, 
		      view_top  = T, view_bottom = B } = WD,
    { L, T, (R - L), (B - T) }.

set_view_rect(S=#state { content = WD }, {X,Y,W,H}) ->    
    S#state { content = WD#window_content { view_left = X,
					    view_right = X+W,
					    view_top   = Y,
					    view_bottom = Y+H }}.

union_view_rect(S=#state { content = WD }, {X0,Y0,W,H}) ->
    #window_content { view_left = L, view_right = R, 
		      view_top  = T, view_bottom = B } = WD,
    X1 = X0 + W,
    Y1 = Y0 + H,
    S#state { content = WD#window_content { view_left = min(X0,L),
					    view_right = max(X1,R),
					    view_top   = min(Y0,T),
					    view_bottom = max(Y1,B)}}.

%% Get the visible content rectangle in view coodinates
get_visible_rect(State) ->
    {Tx,Ty} = get_view_pos(State),
    W = drawing_width0(State),
    H = drawing_height0(State),
    {Sx,Sy} = State#state.scale,
    _Rect = {Tx, Ty, ?WINDOW_TO_VIEW_W(W,Sx), ?WINDOW_TO_VIEW_H(H,Sy) },
    ?verbose("VISIBLE_RECT = ~w\n", [_Rect]),
    _Rect.

get_motion(#state { content = WD }) -> 
    WD#window_content.motion.

set_motion(S=#state { content = WD }, Motion) ->
    S#state { content = WD#window_content { motion = Motion }}.

%% top & bottom bar has priority over left and right...
draw_top_bar(Pixels, State) ->
    case toolbars(State) of
	{_Left,_Right,Top,_Bottom}
	  when Top > 0 ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(top_bar_color(State)),
	    DrawRect = {0,0,State#state.width,Top},
	    epx:draw_rectangle(Pixels, DrawRect),
	    user_draw_area(top, Pixels, DrawRect, State);
	_ ->
	    State
    end.

%% bottom - status bar
draw_bottom_bar(Pixels, State) ->
    case toolbars(State) of
	{_Left,_Right,_Top,Bottom}  
	  when Bottom > 0, State#state.height >= Bottom ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(bottom_bar_color(State)), 
	    X0 = 0,
	    Y0 = State#state.height-Bottom,
	    DrawRect = {X0,Y0,State#state.width,Bottom},
	    epx:draw_rectangle(Pixels, DrawRect),
	    if (State#state.profile)#profile.status_bar ->
		    epx_gc:set_foreground_color({0,0,0}),
		    epx_gc:set_fill_style(none),
		    epx:draw_rectangle(Pixels, DrawRect),
		    epx_gc:set_font(State#state.font),
		    Profile = State#state.profile,
		    Scheme = Profile#profile.scheme,
		    WProfile = State#state.window_profile,
		    FontColor = WProfile#window_profile.status_font_color,
		    StatusColor = epx_profile:color(Scheme,FontColor),
		    set_text_color(StatusColor),
		    draw_text(Pixels, X0+10, Y0, 100, Bottom-2, 
			      State#state.status, State),
		    State;
	       true ->
		    user_draw_area(bottom, Pixels, DrawRect, State)
	    end;
	_ ->
	    State
    end.

draw_left_bar(Pixels, State) ->
    case toolbars(State) of
	{Left,_Right,Top,Bottom} when
	      Left > 0, State#state.height >= (Top+Bottom) ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(left_bar_color(State)),
	    DrawRect = {0,Top,Left,State#state.height-(Top+Bottom)},
	    epx:draw_rectangle(Pixels, DrawRect),
	    user_draw_area(left, Pixels, DrawRect, State);
	_ ->
	    State
    end.

draw_right_bar(Pixels, State) ->
    case toolbars(State) of
	{_Left,Right,Top,Bottom} when Right > 0, State#state.width >= Right,
				      State#state.height >= (Top+Bottom)->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(right_bar_color(State)),
	    DrawRect = {State#state.width-Right,Top,Right,
			State#state.height-(Top+Bottom)},
	    epx:draw_rectangle(Pixels, DrawRect),
	    user_draw_area(right, Pixels, DrawRect, State);
	_ ->
	    State
    end.

set_text_color(_RGB={R,G,B}) ->
    epx_gc:set_foreground_color({0,R,G,B});
set_text_color(_ARGB={_A,R,G,B}) ->
    epx_gc:set_foreground_color({0,R,G,B}).
    

draw_text(Pixels, X0, Y0, _W, _H, Text, S) ->
    X = X0,
    GA = glyph_ascent(S),
    Y = Y0+1+GA,
    epx:draw_string(Pixels, X, Y, Text).


%% check for various callback functions and return corresponding event-mask

init_event_mask(EventMask, [{EventFlag,Fun}|Fs]) when is_function(Fun) ->
    init_event_mask([EventFlag|EventMask], Fs);
init_event_mask(EventMask, [_|Fs]) ->
    init_event_mask(EventMask, Fs);
init_event_mask(Mask, []) ->
    Mask.

%% load #profile from environment
load_profile(E) ->
    D = #profile{},
    %% Special case
    S = ?ld(scheme, E, D),
    #profile {
       scheme = S,
       screen_color    = ?ldc(S,screen_color, E, D),
       selection_alpha = ?ld(selection_alpha, E, D),
       selection_color = ?ldc(S,selection_color, E, D),
       selection_border_width = ?ld(selection_border_width, E, D),
       selection_border_color = ?ldc(S,selection_border_color, E, D),

       max_dirty = ?ld(max_dirty, E, D),
       dirty_list = ?ld(dirty_list, E, D),

       menu_font_name = ?ld(menu_font_name, E, D),
       menu_font_size = ?ld(menu_font_size, E, D),
       menu_font_color = ?ldc(S,menu_font_color,E,D),
       menu_background_color = ?ldc(S,menu_background_color,E,D),
       menu_border_color = ?ldc(S,menu_border_color,E,D),
       
       window_font_name = ?ld(window_font_name, E, D),
       window_font_size = ?ld(window_font_size, E, D),
       window_font_color = ?ldc(S, window_font_color, E, D),
       status_font_color = ?ldc(S, status_font_color, E, D),
       scroll_bar_color  = ?ldc(S, scroll_bar_color, E, D),
       scroll_hndl_color = ?ldc(S, scroll_hndl_color, E, D),
       scroll_horizontal = ?ld(scroll_horizontal, E, D),
       scroll_vertical   = ?ld(scroll_vertical, E, D),
       top_bar_color     = ?ldc(S, top_bar_color, E, D),
       left_bar_color    = ?ldc(S, left_bar_color, E, D),
       right_bar_color   = ?ldc(S, right_bar_color, E, D),
       bottom_bar_color  = ?ldc(S, bottom_bar_color, E, D),

       top_bar     = ?ld(top_bar, E, D),
       left_bar    = ?ld(left_bar, E, D),
       right_bar   = ?ld(right_bar, E, D),
       bottom_bar  = ?ld(bottom_bar, E, D),
       status_bar  = ?ld(status_bar, E, D),
       
       top_offset     = ?ld(top_offset, E, D),
       left_offset    = ?ld(left_offset, E, D),
       right_offset   = ?ld(right_offset, E, D),
       bottom_offset  = ?ld(bottom_offset, E, D),

       scroll_bar_size = ?ld(scroll_bar_size, E, D),
       scroll_hndl_size = ?ld(scroll_hndl_size, E, D),
       scroll_xstep = ?ld(scroll_xstep, E, D),
       scroll_ystep = ?ld(scroll_ystep, E, D)
      }.

create_menu_profile(Profile) ->
    #menu_profile {
       scheme           = Profile#profile.scheme,
       font_name        = Profile#profile.menu_font_name,
       font_size        = Profile#profile.menu_font_size,
       font_color       = Profile#profile.menu_font_color,
       background_color = Profile#profile.menu_background_color,
       border_color     = Profile#profile.menu_border_color
      }.

create_window_profile(Profile) ->
    #window_profile {
       scheme           = Profile#profile.scheme,
       font_name        = Profile#profile.window_font_name,
       font_size        = Profile#profile.window_font_size,
       font_color       = Profile#profile.window_font_color,
       background_color = Profile#profile.screen_color,
       scroll_bar_color = Profile#profile.scroll_bar_color,
       scroll_hndl_color = Profile#profile.scroll_hndl_color,
       top_bar_color     = Profile#profile.top_bar_color,
       left_bar_color    = Profile#profile.left_bar_color,
       right_bar_color   = Profile#profile.right_bar_color,
       bottom_bar_color  = Profile#profile.bottom_bar_color,
       scroll_horizontal = Profile#profile.scroll_horizontal,
       scroll_vertical   = Profile#profile.scroll_vertical,
       status_font_color = Profile#profile.status_font_color
      }.

%% translate record field to record position
%% this should be possible to do as a compile time map (I WISH)
window_profile_keypos(Key) ->
    case Key of
	scheme  -> #window_profile.scheme;
	font_name -> #window_profile.font_name;
	font_size -> #window_profile.font_size;
	font_color -> #window_profile.font_color;
	background_color -> #window_profile.background_color;
	scroll_bar_color -> #window_profile.scroll_bar_color;
	scroll_hndl_color -> #window_profile.scroll_hndl_color;
	top_bar_color -> #window_profile.top_bar_color;
	left_bar_color -> #window_profile.left_bar_color;
	right_bar_color -> #window_profile.right_bar_color;
	bottom_bar_color -> #window_profile.bottom_bar_color;
	scroll_horizontal-> #window_profile.scroll_horizontal;
	scroll_vertical -> #window_profile.scroll_vertical;
	status_font_color -> #window_profile.status_font_color
    end.
    

window_profile_get(Key, WProfile) ->
    Pos = window_profile_keypos(Key),
    element(Pos, WProfile).

window_profile_set(Key, Value, WProfile) ->
    Pos = window_profile_keypos(Key),
    setelement(Pos, WProfile, Value).

profile_keypos(Key) ->
    case Key of
	scheme -> #profile.scheme;
	screen_color -> #profile.screen_color;
	selection_alpha -> #profile.selection_alpha;
	selection_color -> #profile.selection_color;
	selection_border_width -> #profile.selection_border_width;
	selection_border_color -> #profile.selection_border_color;
	max_dirty -> #profile.max_dirty;
	dirty_list -> #profile.dirty_list;
	top_bar -> #profile.top_bar;
	left_bar -> #profile.left_bar;
	right_bar -> #profile.right_bar;
	bottom_bar -> #profile.bottom_bar;
	status_bar -> #profile.status_bar;
	top_offset -> #profile.top_offset;
	left_offset -> #profile.left_offset;
	right_offset -> #profile.right_offset;
	bottom_offset -> #profile.bottom_offset;
	scroll_bar_size -> #profile.scroll_bar_size;
	scroll_hndl_size -> #profile.scroll_hndl_size;
	scroll_xstep -> #profile.scroll_xstep;
	scroll_ystep -> #profile.scroll_ystep;
	menu_font_name -> #profile.menu_font_name;
	menu_font_size -> #profile.menu_font_size;
	menu_font_color -> #profile.menu_font_color;
	menu_background_color -> #profile.menu_background_color;
	menu_border_color -> #profile.menu_border_color;
	window_font_name -> #profile.window_font_name;
	window_font_size -> #profile.window_font_size;
	window_font_color -> #profile.window_font_color;
	scroll_bar_color -> #profile.scroll_bar_color;
	scroll_hndl_color -> #profile.scroll_hndl_color;
	scroll_vertical -> #profile.scroll_vertical;
	scroll_horizontal -> #profile.scroll_horizontal;
	status_font_color -> #profile.status_font_color;
	top_bar_color -> #profile.top_bar_color;
	left_bar_color -> #profile.left_bar_color;
	right_bar_color -> #profile.right_bar_color;
	bottom_bar_color -> #profile.bottom_bar_color
    end.    

profile_get(Key, Profile) ->
    Pos = profile_keypos(Key),
    element(Pos, Profile).

profile_set(Key, Value, Profile) ->
    Pos = profile_keypos(Key),
    setelement(Pos, Profile, Value).


window_info_keypos(Key) ->
    case Key of
	font_name -> #window_info.font_name;
	font_size -> #window_info.font_size;
	font -> #window_info.font;
	gc -> #window_info.gc;
	glyph_width -> #window_info.glyph_width;
	glyph_height -> #window_info.glyph_height;
	glyph_ascent -> #window_info.glyph_ascent;
	glyph_descent -> #window_info.glyph_descent;
	top_bar -> #window_info.top_bar;
	left_bar -> #window_info.left_bar;
	right_bar -> #window_info.right_bar;
	bottom_bar -> #window_info.bottom_bar;
	top_offset -> #window_info.top_offset;
	left_offset -> #window_info.left_offset;
	right_offset -> #window_info.right_offset;
	bottom_offset -> #window_info.bottom_offset;
	scroll_bar_size -> #window_info.scroll_bar_size;
	scroll_hndl_size -> #window_info.scroll_hndl_size;
	scroll_xstep -> #window_info.scroll_xstep;
	scroll_ystep -> #window_info.scroll_ystep;
	scroll_horizontal -> #window_info.scroll_horizontal;
	scroll_vertical -> #window_info.scroll_vertical
    end.

window_info_get(Key, WInfo) ->
    Pos = window_info_keypos(Key),
    element(Pos, WInfo).

window_info_set(Key, Value, WInfo) ->
    Pos = window_info_keypos(Key),
    setelement(Pos, WInfo, Value).

resize_pixmap(undefined, W, H, PixelFormat, Attached) ->
    Pixmap = next_pixmap(W,H,PixelFormat),
    if Attached ->
	    epx:pixmap_attach(Pixmap);
       true ->
	    ok
    end,
    Pixmap;
resize_pixmap(Pixmap, W, H, PixelFormat, Attached) ->
    case epx:pixmap_info(Pixmap,[width,height]) of
	[{width,PW},{height,PH}] when PW < W; PH < H ->
	    if Attached ->
		    epx:pixmap_detach(Pixmap),
		    Pixmap1 = next_pixmap(W,H,PixelFormat),
		    epx:pixmap_attach(Pixmap1),
		    Pixmap1;
	       true ->
		    next_pixmap(W,H,PixelFormat)
	    end;
	_ ->
	    Pixmap
    end.

next_pixmap(W,H,PixelFormat) ->
    NPW = 1 bsl ceil(math:log2(W)),
    NPH = 1 bsl ceil(math:log2(H)),
    epx:pixmap_create(NPW, NPH, PixelFormat).
