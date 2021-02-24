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
-export([view_origin/0, view_width/0, view_height/0, view_rect/0]).
-export([set_view_size/2, set_view_rect/1]).
-export([invalidate/0, invalidate/1]).
-export([set_status_text/1]).
-export([file_menu/0, edit_menu/0]).
 
%% -define(DEBUG, true).

-define(USE_OFF_SCREEN, true).
-define(USE_EXPOSURE, false).

-include_lib("epx/include/epx.hrl").
-include_lib("epx/include/epx_menu.hrl").
-include_lib("epx/include/epx_window_content.hrl").

%% epxw callbacks
-callback init(Window :: epx:epx_window(), Screen::epx:epx_pixmap(),
	      [Opt::term()]) ->
    State :: term().
-callback configure(Event :: term(), State :: term()) -> 
    NewState :: term().
-callback key_press(Event :: term(), State :: term()) -> 
    NewState :: term().
-callback key_release(Event :: term(), State :: term()) -> 
    NewState :: term().
-callback button_press(Event :: term(), State :: term()) -> 
    NewState :: term().
-callback button_release(Event :: term(), State :: term()) -> 
    NewState :: term().
-callback enter(Event :: term(), State :: term()) -> 
    NewState :: term().
-callback leave(Event :: term(), State :: term()) -> 
    NewState :: term().
-callback focus_in(Event :: term(), State :: term()) -> 
    NewState :: term().
-callback focus_out(Event :: term(), State :: term()) -> 
    NewState :: term().
-callback close(State :: term()) -> 
    NewState :: term().
-callback draw(Pixels::epx:epx_pixmap(), Rect::epx:epx_rect(),
	       State :: term()) -> 
    NewState :: term().
-callback select(Rect::epx:epx_rect(), State :: term()) -> 
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
   [init/3, 
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
    draw/3,
    select/2,
    command/3
   ]).

-ifdef(DEBUG).
-define(dbg(F,A), io:format(F "\n", [A])).
-else.
-define(dbg(F,A), ok).
-endif.

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

-define(TEXT_COLOR,              {0,0,0,0}).       %% black text

%% profile with default values
-record(profile,
	{
	 scheme                        = logo, %% xterm,
	 screen_color                  = grey2,
	 selection_alpha               = 100,
	 selection_color               = grey,
	 selection_border_width        = 1,
	 selection_border_color        = grey10,

	 top_bar                       = 0,
	 left_bar                      = 0,
	 right_bar                     = 0,
	 bottom_bar                    = 18,

	 top_offset                    = 0,
	 left_offset                   = 0,
	 right_offset                  = 0,
	 bottom_offset                 = 0,

	 scroll_bar_size               = 16,
	 scroll_hndl_size              = 10,
	 scroll_xstep                  = 4,
	 scroll_ystep                  = 4,	 

	 %% menu_info
	 menu_font_name                = "Arial",
	 menu_font_size                = 14,
	 menu_font_color               = grey5,   %% light
	 menu_background_color         = grey10,  %% dark
	 menu_border_color             = green,

	 %% window_profile
	 window_font_name              = "Courier New",
	 window_font_size              = 14,
	 window_font_color             = grey10,
	 scroll_bar_color              = grey5,
	 scroll_hndl_color             = grey6,
	 scroll_vertical               = right,
	 scroll_horizontal             = bottom,

	 top_bar_color                 = red,
	 left_bar_color                = green,
	 right_bar_color               = blue,
	 bottom_bar_color              = red6
	}).

-record(callbacks, 
	{
	 init :: undefined | 
		 fun((Window::epx:epx_window(),
		      Screen::epx:epx_pixmap(),
		      UserOpts::[term()]) -> NewState::term()),
	 handle_call :: undefined | fun(),
	 handle_cast :: undefined | fun(),
         handle_info :: undefined | fun(),
         handle_continue :: undefined | fun(),
	 code_change :: undefined | fun(),
         terminate :: undefined | fun(),
	 configure :: undefined |
		      fun((Event::term(), State::term()) -> NewState::term()),
	 key_press :: undefined |
		      fun((Event::term(), State::term()) -> NewState::term()),
	 key_release :: undefined |
		      fun((Event::term(), State::term()) -> NewState::term()),
	 button_press :: undefined |
		      fun((Event::term(), State::term()) -> NewState::term()),
         button_release :: undefined |
		      fun((Event::term(), State::term()) -> NewState::term()),
	 enter :: undefined |
		      fun((Event::term(), State::term()) -> NewState::term()),
         leave :: undefined |
		      fun((Event::term(), State::term()) -> NewState::term()),
	 focus_in :: undefined |
		      fun((Event::term(), State::term()) -> NewState::term()),
         focus_out :: undefined |
		      fun((Event::term(), State::term()) -> NewState::term()),
	 close  :: undefined |
		   fun((State::term()) -> NewState::term()),
	 draw  :: undefined |
		  fun((Pixmap::epx:epx_pixmap(),Rect::epx:epx_rect(),
			   State::term()) -> NewState::term()),
	 select :: undefined |
		   fun((Rect::epx:epx_rect(),State::term())
		       -> NewState::term()),
         command :: undefined |
		    fun((Command::term(), Mod::epx_keymod(), State::term()) ->
			       NewState::term()),
	 menu :: undefined |
		 fun((Event::term(), State::term()) ->
			    undefined | epx_menu:epx_menu())
 }).

-define(CALLBACK(S,Name), (((S)#state.user_cb)#callbacks.Name)).

-define(STATE_KEY, 'EPXW_STATE').

-record(state, 
	{
	 window :: epx:epx_window(),
	 screen :: epx:epx_pixmap(),   %% on-screen pixels
	 pixels :: epx:epx_pixmap(),   %% off-screen pixels
	 font   :: epx:epx_font(),
	 width  :: integer(),      %% window width
	 height :: integer(),      %% window height	 
	 dirty  :: undefined | epx:epx_rect(),
	 status = "" :: string(),  %% bottom status text
	 profile :: #profile{},
	 menu_profile :: #menu_profile{},
	 window_profile :: #window_profile{},
	 content :: #window_content{},
	 keymod :: epx_keymod(), %% modifiers
	 esc    :: boolean(), %% escape modifier
	 context_menu :: epx_menu:epx_menu(),
	 edit_menu    :: epx_menu:epx_menu(),
	 file_menu    :: epx_menu:epx_menu(),
	 menu         :: epx_menu:epx_menu(),  %% current menu
	 winfo :: #window_info{},
	 zoom  = 0 :: integer(),  %% [-20...20 step2]?
	 scale = {1,1} :: {number(),number()},
	 pt1   :: undefine | {X::integer(),Y::integer()}, %% start pos
	 pt2   :: undefine | {X::integer(),Y::integer()}, %% cur pos
	 operation = none :: none | menu,
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
pixels() -> (state())#state.pixels.
width()  -> (state())#state.width.
height() -> (state())#state.height.
keymod() -> (state())#state.keymod.
view_origin() -> get_view_origin(state()).
view_width() -> get_view_width(state()).
view_height() -> get_view_height(state()).
view_rect() -> get_view_rect(state()).
zoom() -> (state())#state.zoom.
scale() -> (state())#state.scale.
file_menu() -> (state())#state.file_menu.
edit_menu() -> (state())#state.edit_menu.
    
     
set_view_size(W, H) when W >= 0, H >= 0 ->
    S0 = state(),
    X = get_view_left(S0),
    Y = get_view_top(S0),
    S1 = set_view_rect(S0, {X,Y,W,H}),
    export_state(S1).

set_view_rect(R={X, Y, W, H}) when X >= 0, Y >= 0, W >= 0, H >= 0 ->
    S0 = state(),
    S1 = set_view_rect(S0, R),
    export_state(S1).

set_status_text(Text) ->
    S0 = state(),
    S1 = S0#state { status = lists:flatten(Text) },
    export_state(S1),
    ok.
    
invalidate() ->
    S0 = state(),
    S1 = set_dirty_area(S0),
    export_state(S1),
    S1#state.dirty.

invalidate(undefined) ->
    undefined;
invalidate(Area={_X,_Y,_W,_H}) ->
    S0 = state(),
    S1 = set_dirty_area(Area,S0),
    export_state(S1),
    S1#state.dirty.

%% "standard" menus
menu(edit) ->
    [
     {"Undo", "Ctrl+Z"},
     {"Redo", "Ctrl+Shift+Z"},
     {"---", ""},
     {"Cut", "Ctrl+X"},
     {"Copy", "Ctrl+C"},
     {"Paste", "Ctrl+V"},
     {"Delete", "Del"},
     {"---", ""},
     {"Select All", "Ctrl+A"}
    ];
menu(file) ->
    [
     {"Open File...",  "Ctrl+O"},
     {"Save",  "Ctrl+S"},
     {"---", ""},
     {"Quit",  "Ctrl+Q"}
    ];
menu(context) ->
    [
     {"Cut", "Ctrl+X"},
     {"Copy", "Ctrl+C"},
     {"Paste", "Ctrl+V"},
     {"Delete", "Del"},
     {"---", ""},
     {"Select All", "Ctrl+A"}
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

    {ok,Font} = epx_font:match([{name,WProfile#window_profile.font_name},
				{size,WProfile#window_profile.font_size}]),
    %% FontColor =  WProfile#window_profile.font_color,

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

    Screen = epx:pixmap_create(Width1, Height1, argb),
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
		font   = Font,    %% window font
		width  = Width,
		height = Height,
		edit_menu   = EditMenu,
		file_menu   = FileMenu,
		context_menu = ContextMenu,
		winfo  = WInfo,
		profile = Profile,
		keymod = #keymod{},
		esc = false,
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

    State2 = case UserCb#callbacks.init of
		 undefined ->
		     State1;
		 Init ->
		     export_state(State1),
		     UserState = Init(Window, Screen, UserOpts),
		     State11 = state(),
		     State11#state { user_state = UserState}
	     end,
    Rect = {0, 0, Width, Height},
    State3 = user_event({configure,Rect},?CALLBACK(State2,configure),State2),
    {ok, draw(State3, Rect)}.

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
	init = load_callback_(UserMod, UserMap, init, 3),
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
	select = load_callback_(UserMod, UserMap, select, 2),
	command = load_callback_(UserMod, UserMap, command, 3),
	menu = load_callback_(UserMod, UserMap, menu, 2)
       }}.

load_callback_(UserMod, UserMap, Func, Arity) ->
    case maps:get(Func, UserMap, undefined) of
	undefined ->
	    case erlang:function_exported(UserMod, Func, Arity) of
		true ->
		    erlang:make_fun(UserMod, Func, Arity);
		false ->
		    undefined
	    end;
	UserFun when is_function(UserFun, Arity) ->
	    UserFun
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
	    Reply = HandleCall(Request, From, State#state.user_state),
	    reply(Reply)
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
	    Reply = HandleCast(Request, State#state.user_state),
	    reply(Reply)
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
	    if State1#state.dirty =:= undefined ->
		    {noreply,State1};
	       true ->
		    {noreply,draw(State1)}
	    end;
	Reply ->
	    Reply
    end;
handle_info('REDRAW',State) ->
    {noreply, draw(State)};
handle_info(Info, State) ->
    case ?CALLBACK(State, handle_info) of
	undefined ->
	    ?dbg("handle_info: unhandled event ~p", [Info]),
	    {noreply, State};
	HandleInfo ->
	    export_state(State),
	    Reply = HandleInfo(Info, State#state.user_state),
	    reply(Reply)
    end.

reply({reply,Reply,UState}) ->
    State = state(),
    {reply, Reply, State#state { user_state = UState }};
reply({reply,Reply,UState,Arg}) ->
    State = state(),
    {reply, Reply, State#state { user_state = UState }, Arg};
reply({noreply,UState}) ->
    State = state(),
    {noreply, State#state { user_state = UState }};
reply({noreply,UState,Arg}) ->
    State = state(),
    {noreply, State#state { user_state = UState },Arg};
reply({stop, Reason, UState}) ->
    State = state(),
    {stop, Reason, State#state { user_state = UState }}.

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
	    Reply = HandleContinue(Info, State#state.user_state),
	    reply(Reply)
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
	    Screen = resize_pixmap(State#state.screen, W1, H1, true),
	    Pixels = if State#state.pixels =:= undefined -> undefined;
			true -> resize_pixmap(State#state.pixels, W1, H1, false)
		     end,
	    State1 = State#state { screen = Screen, pixels = Pixels,
				   width=W, height=H },
	    State2 = user_event({configure,Rect1},
				?CALLBACK(State1,configure),
				State1),
	    State3 = adjust_view_xpos(get_view_xpos(State2), State2),
	    State4 = adjust_view_ypos(get_view_ypos(State3), State3),
	    {noreply, set_dirty_area(State4)};
       true ->
	    {noreply, State}
    end;

epx_event(Event={key_press, Sym, Mod, _code}, State) ->
    if Sym =:= $\e -> %% escape key processing
	    epx:window_disable_events(State#state.window,[motion]),
	    if not State#state.esc ->
		    {noreply, State#state { esc = true }};
	       true ->
		    {noreply, State}
	    end;
       true ->
	    M = set_mod(State#state.keymod, Mod),
	    State1 = State#state { keymod=M },
	    State2 = command(Sym, M, State1),
	    %% FIXME: if command is run then do not call key_press/key_release?
	    State3 = user_event(Event,?CALLBACK(State1,key_press),State2),
	    {noreply, State3#state { esc = false }}
    end;
epx_event(Event={key_release, _Sym, Mod, _code}, State) ->
    M = clr_mod(State#state.keymod, Mod),
    State1 = State#state { keymod = M },
    State2 = user_event(Event,?CALLBACK(State1,key_release), State1),
    {noreply, State2};
epx_event(Event={button_press,[wheel_down],_Pos},State) ->
    State1 = user_event(Event, ?CALLBACK(State,button_press), State),
    {noreply, State1};
epx_event(Event={button_release, [wheel_down], _Pos}, State) ->
    flush_wheel(State#state.window),  %% optional?
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, scroll_down(State1)};
epx_event(Event={button_press,[wheel_up],_Pos},State) ->
    State1 = user_event(Event, ?CALLBACK(State,button_press), State),
    {noreply, State1};
epx_event(Event={button_release, [wheel_up], _Pos}, State) ->
    flush_wheel(State#state.window),
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, scroll_up(State1)};
epx_event(Event={button_press,[wheel_left],_Pos}, State) ->
    State1 = user_event(Event, ?CALLBACK(State,button_press), State),
    {noreply, State1};
epx_event(Event={button_release,[wheel_left],_Pos},State) ->
    flush_wheel(State#state.window),
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, scroll_left(State1)};
epx_event(Event={button_press,[wheel_right],_Pos}, State) ->
    State1 = user_event(Event, ?CALLBACK(State,button_press), State),
    {noreply, State1};    
%% Fixme: transform X,Y into view coordinates before callback
epx_event(Event={button_release,[wheel_right],_Pos},State) ->
    flush_wheel(State#state.window),
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, scroll_right(State1)};
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
			    %% FIXME: avoid button_release?
			    State2 = command(Cmd,Mod,State1),
			    State3 = set_dirty_area(State2),
			    {noreply, State3}
		    end
	    end;
       true ->
	    case scroll_hit(Pos, State) of
		false ->
		    Window = State#state.window,
		    epx:window_enable_events(Window,[motion]),
		    State1 = user_event(Event, ?CALLBACK(State,button_press),
					State),
		    %% FIXME: select callback!
		    State2 = set_dirty_area(State1),
		    {noreply, State2#state { operation=select,
					     pt1=Pos,pt2=Pos }};
		State1 ->
		    {noreply, State1}
	    end
    end;
epx_event(Event={button_release, [left], _Pos}, State) ->
    flush_motion(State#state.window),
    State1 =
	case get_motion(State) of
	    undefined ->
		case State#state.pt1 of
		    undefined ->
			State;
		    Pt1 ->
			case State#state.operation of
			    select ->
				State0 = select(Pt1, State#state.pt2, State),
				set_dirty_area(State0);
			    _ ->
				State0 = set_dirty_area(State),
				user_event(Event, ?CALLBACK(State0,button_release), State0)
			end
		end;
	    {vhndl,_Delta} ->
		epx:window_disable_events(State#state.window, [motion]),
		set_motion(State, undefined);
	    {hhndl,_Delta} ->
		epx:window_disable_events(State#state.window, [motion]),
		set_motion(State, undefined)
	end,
    {noreply, State1#state{pt1 = undefined, pt2 = undefined, operation = none}};

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
		    epx:window_enable_events(State#state.window,[motion]),
		    case epx_menu:set_row(Menu0, -1) of
			undefined ->
			    State1 = user_event(Event,
						?CALLBACK(State,button_press),
						State#state { user_state = UserState}),
			    {noreply, State1};
			Menu1 ->
			    State1 = State#state { pt1 = Pos, pt2 = Pos,
						   operation = menu,
						   menu = Menu1 },
			    {noreply, set_dirty_area(State1)}
		    end
	    end
    end;
epx_event(Event={button_release, _Buttons, _Pos3D}, State) ->
    flush_motion(State#state.window),
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, State1};
epx_event({motion,[],Pos},State) ->
    flush_motion(State#state.window),
    if State#state.operation =:= menu ->
	    {Row,Menu} = epx_menu:find_row(State#state.menu,
					   State#state.pt1,
					   Pos),
	    if Row =:= -1 ->
		    {noreply, State#state { menu=Menu }};
	       true ->
		    State1 = State#state { menu=Menu },
		    {noreply,set_dirty_area(State1)}
	    end;
       true ->
	    %% User callback?
	    {noreply, State}
    end;
epx_event({motion,[left],Pos},State) ->
    flush_motion(State#state.window),
    case get_motion(State) of
	{vhndl,{_DX,Dy}} ->
	    {_,Y,_} = Pos,
	    {_,Y0,_,Height} = get_vscroll(State),
	    Yr = (Y-Y0)/Height,  %% relative hit position
	    Vy = Yr*get_view_height(State), %% View position
	    State1 = adjust_view_ypos(Vy+Dy, State),
	    {noreply,set_dirty_area(State1)};
	{hhndl,{Dx,_Dy}} ->
	    {X,_,_} = Pos,
	    {X0,_,Width,_} = get_hscroll(State),
	    Xr = (X-X0)/Width,  %% relative hit position
	    Vx = Xr*get_view_width(State), %% View position
	    State1 = adjust_view_xpos(Vx+Dx, State),
	    {noreply,set_dirty_area(State1)};
	_ ->
	    State1 = select(State#state.pt1, Pos, State#state{ pt2 = Pos }),
	    {noreply,State1}
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
    
%% Get drawing area width taking toolbars into account but wo scrollbar
drawing_width0(State) ->
    {LeftBar,RightBar,_,_} = toolbars(State),
    State#state.width - (LeftBar+RightBar).

%% Get drawing area height taking toolbars into account but wo scrollbar
drawing_height0(State) ->
    {_,_,TopBar,BottomBar} = toolbars(State),
    State#state.height - (TopBar+BottomBar).

set_dirty_area(State) -> 
    set_dirty_area(get_view_rect(State),State).
set_dirty_area(Area={_X,_Y,_W,_H},State) ->
    Dirty = epx_rect:union(Area,State#state.dirty),
    State#state { dirty = Dirty }.

scroll_hit(Pos, State) ->
    HScroll = get_hscroll(State),
    case epx_rect:contains(HScroll, Pos) of
	true ->
	    {X0,_,Width,_} = HScroll,
	    {X,_Y,_} = Pos,
	    Xr = (X-X0)/Width,  %% relative hit position
	    Vx = Xr*get_view_width(State), %% View position
	    %% hit horizontal scrollbar
	    epx:window_enable_events(State#state.window, [motion]),
	    HHndl = get_hhndl(State),
	    case epx_rect:contains(HHndl, Pos) of
		true ->
		    %% hit horizontal scroll handle
		    {Hx,_Hy,_,_} = HHndl,
		    Dx = -(X-Hx),
		    State1 = adjust_view_xpos(Vx+Dx, State),
		    State2 = set_motion(State1,{hhndl,{Dx,0}}),
		    set_dirty_area(State2);
		false ->
		    {_,_,Hw,_} = HHndl,
		    Dx = -(Hw/2),
		    State1 = adjust_view_xpos(Vx+Dx, State),
		    State2 = set_motion(State1,{hhndl,{Dx,0}}),
		    set_dirty_area(State2)
	    end;
	false ->
	    VScroll = get_vscroll(State),
	    case epx_rect:contains(VScroll, Pos) of
		true ->
		    {_,Y0,_,Height} = VScroll,
		    {_X,Y,_} = Pos,
		    Yr = (Y-Y0)/Height,  %% relative hit position
		    Vy = Yr*get_view_height(State), %% View position
		    %% hit vertical scrollbar
		    epx:window_enable_events(State#state.window, [motion]),
		    VHndl = get_vhndl(State),
		    case epx_rect:contains(VHndl, Pos) of
			true ->
			    %% hit vertical scroll handle
			    {_Hx,Hy,_,_} = VHndl,
			    Dy = -(Y-Hy),
			    State1 = adjust_view_ypos(Vy+Dy, State),
			    State2 = set_motion(State1,{vhndl,{0,Dy}}),
			    set_dirty_area(State2);
			false ->
			    {_,_,_,Hh} = VHndl,
			    Dy = -(Hh/2),
			    State1 = adjust_view_ypos(Vy+Dy, State),
			    State2 = set_motion(State1,{vhndl,{0,Dy}}),
			    set_dirty_area(State2)
		    end;
		false ->
		    false
	    end
    end.

command(Symbol,Mod,State) ->
    Command = ?CALLBACK(State,command),
    if Command =:= undefined ->
	    command_(Symbol,Mod,State);
       true ->
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

%% default handling on key up/down left/right pageup/pagedown home/end
 %% FIXME configure this
command_(up, _Mod, State) ->
    scroll_up(State);
command_(down, _Mod, State) ->
    scroll_down(State);
command_(left, _Mod, State) ->
    scroll_left(State);
command_(right, _Mod, State) ->
    scroll_right(State);
command_(pageup, _Mod, State) ->
    page_up(State);
command_(pagedown, _Mod, State) ->
    page_down(State);
command_(home, _Mod, State) ->
    scroll_home(State);
command_('end', _Mod, State) ->
    scroll_end(State);
 %% zoom in - FIXME configure this
command_($-, _Mod, State) ->
    Zoom = min(10, State#state.zoom + 1),
    Scale = zscale(Zoom),
    State1 = State#state { zoom = Zoom, scale = Scale },
    State2 = adjust_view_xpos(get_view_xpos(State1), State1),
    State3 = adjust_view_ypos(get_view_ypos(State2), State2),
    set_dirty_area(State3);
 %% zoom out - FIXME configure this
command_($+, _Mod, State) ->
    Zoom = max(-10, State#state.zoom - 1),
    Scale = zscale(Zoom),
    State1 = State#state { zoom = Zoom, scale = Scale },
    State2 = adjust_view_xpos(get_view_xpos(State1), State1),
    State3 = adjust_view_ypos(get_view_ypos(State2), State2),
    set_dirty_area(State3);
command_(_Symbol, _Mod, State) ->
    ?dbg("unhandled command ~p\n", [_Symbol]),
    State.

%% calculate scale from zoom (Sx=Sy) now
zscale(Z) ->
    Sx = Sy = math:pow(1.07, 2*(Z)),
    {Sx,Sy}.

scroll_up(State) ->
    State1 = step_up(State, scroll_ystep(State)),
    set_dirty_area(State1).

scroll_down(State) ->
    State1 = step_down(State, scroll_ystep(State)),
    set_dirty_area(State1).

scroll_left(State) ->
    State1 = step_left(State, scroll_xstep(State)),
    set_dirty_area(State1).

scroll_right(State) ->
    State1 = step_right(State, scroll_xstep(State)),
    set_dirty_area(State1).

scroll_home(State) ->
    State1 = set_view_ypos(State, 0),
    set_dirty_area(State1).

scroll_end(State) ->
    Y0 = get_view_bottom(State),
    Y1 = adjust_bottom_pos(Y0, State),
    State1 = set_view_ypos(State, Y1),
    set_dirty_area(State1).

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
	    set_dirty_area(State1)
    end.

page_down(State) ->
    case get_vscroll(State) of
	undefined ->
	    State;
	{_,_,_,H} ->
	    {_,_,_,VH} = get_vhndl(State),  %% page size in window coords
	    R = VH/H,  %% page ratio
	    Page = R*get_view_height(State),
	    State1 = step_down(State, Page),
	    set_dirty_area(State1)
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

flush_expose(Win, Rect) ->
    receive
	{epx_event, Win, {expose, Rect1}} ->
	    flush_expose(Win, Rect1)
    after 0 ->
	    Rect
    end.

flush_motion(Win) ->
    receive
	{epx_event, Win, {motion, _Mod, _Pos}} ->
	    flush_motion(Win)
    after 0 ->
	    ok
    end.

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

select({X1,Y1,_},{X2,Y2,_}, State) ->
    {X11,X22}=minmax(X1,X2),
    {Y11,Y22}=minmax(Y1,Y2),
    Area = {X11,Y11,(X22-X11)+1,(Y22-Y11)+1},
    user_event({select,Area},?CALLBACK(State,select),State).

minmax(A, B) when A < B -> {A,B};
minmax(A, B) -> {B,A}.

user_event(_E, undefined, State) ->
    State;
user_event(E, Callback, State) ->
    UE = transform_event(E, State),
    export_state(State),
    UserState = Callback(UE, State#state.user_state),
    State1 = state(),
    State1#state { user_state = UserState }.


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
transform_event({select,{X,Y,W,H}},State) when is_number(X), is_number(Y),
					       is_number(W), is_number(H) ->
    {X1,Y1,_} = window_to_view_pos({X,Y,0},State),
    {W1,H1} = window_to_view_dim({W,H},State),
    {select,{X1,Y1,W1,H1}};
transform_event(Event, _State) ->
    Event.

%% Wx = (Vx - Tx)/Sx + Cx
%% Vx = (Wx - Cx)*Sx + Tx

%% Transform window coordinate into view coodinate
window_to_view_pos({Wx,Wy,Wz}, State) ->
    #state { scale = {Sx,Sy},
	     content=#window_content{view_xpos=Tx,view_ypos=Ty}} = State,
    {Cx,Cy} = drawing_origin(State),
    { (Wx-Cx)*Sx + Tx, (Wy-Cy)*Sy + Ty, Wz}.

window_to_view_xpos(Wx, State) ->
    #state { scale = {Sx,_Sy},content=#window_content{view_xpos=Tx}} = State,
    {Cx,_Cy} = drawing_origin(State),
    (Wx-Cx)*Sx + Tx.

window_to_view_ypos(Wy, State) ->
    #state { scale = {_Sx,Sy},content=#window_content{view_ypos=Ty}} = State,
    {_Cx,Cy} = drawing_origin(State),
    (Wy-Cy)*Sy + Ty.

window_to_view_dim({Ww,Wh}, #state { scale={Sx,Sy}}) ->
    { Ww*Sx, Wh*Sy }.

window_to_view_height(Wh, #state { scale={_,Sy}}) ->
    Wh * Sy.

window_to_view_width(Ww, #state { scale={Sx,_}}) ->
    Ww * Sx.

view_to_window_height(Vh, #state { scale={_,Sy}}) ->
    Vh / Sy.

view_to_window_width(Vw, #state { scale={Sx,_}}) ->
    Vw / Sx.

get_scaled_view_width(S) ->
    view_to_window_width(get_view_width(S), S).

get_scaled_view_height(S) ->
    view_to_window_height(get_view_height(S), S).

draw(State) ->
    draw(State, State#state.dirty).
    
draw(State = #state { profile = Profile }, Dirty) ->
    Scheme = Profile#profile.scheme,
    ScreenColor = epx_profile:color(Scheme, Profile#profile.screen_color),
    {HBar,VBar} = scrollbars(State),
    Pixels = pixels(State),
    {Tx,Ty} = get_view_origin(State),
    ScrollBars={HBar,VBar} = scrollbars(State),
    W = drawing_width0(State),
    H = drawing_height0(State),
    {Cx,Cy} = drawing_origin_(State,ScrollBars),
    {Sx,Sy} = State#state.scale,
    
    fill_area(Pixels,undefined,ScreenColor),
    epx:pixmap_ltm_reset(Pixels),
    epx:pixmap_ltm_translate(Pixels, Cx, Cy),
    epx:pixmap_ltm_scale(Pixels, 1/Sx, 1/Sy),
    epx:pixmap_ltm_translate(Pixels, -Tx, -Ty),

    %% set clip rect!
    VisibleRect = {Tx, Ty, W/Sx, H/Sy},  %% in view coordinates
    State1 = draw_content(Pixels,VisibleRect,State),
    epx:pixmap_ltm_reset(Pixels),
    epx_gc:set_border_width(0), %% FIXME: use special gc for user content
    State2 = draw_vscroll(Pixels,VBar,HBar,State1),
    State3 = draw_hscroll(Pixels,HBar,VBar,State2),
    State4 = draw_top_bar(Pixels,State3),
    State5 = draw_left_bar(Pixels,State4),
    State6 = draw_right_bar(Pixels,State5),
    State7 = draw_bottom_bar(Pixels,State6),
    if State#state.pt1 =/= undefined, State#state.operation =:= menu ->
	    epx_menu:draw(State#state.menu, Pixels, State#state.pt1);
       true ->
	    ok
    end,
    update_window(State7, Dirty).

update_window(State, Dirty) ->
    if State#state.pixels =/= undefined ->
	    copy_area(State#state.pixels,Dirty,State#state.screen);
       true ->
	    ok
    end,
    epx:pixmap_draw(State#state.screen, State#state.window,
		    0, 0, 0, 0,
		    State#state.width, State#state.height),
    epx:sync(State#state.screen,State#state.window),
    State#state { dirty = undefined }.

pixels(State) ->
    if State#state.pixels =/= undefined ->
	    State#state.pixels;
       true ->
	    State#state.screen
    end.

copy_area(Src, undefined, Dst) ->
    epx:epx_copy_to(Src, Dst);
copy_area(Src, {X,Y,W,H}, Dst) ->
    epx:pixmap_copy_area(Src,Dst,X,Y,X,Y,W,H).

fill_area(Pixmap, undefined, Color) ->
    epx:pixmap_fill(Pixmap,Color);
fill_area(Pixmap, {X,Y,W,H}, Color) ->
    epx:pixmap_fill_area(Pixmap,X,Y,W,H,Color).

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
    ScaledHeight = get_scaled_view_height(State),
    %% scroll size as ratio of scaled height
    Ht = DrawingHeight / ScaledHeight,
    HandleLength = Ht*DrawingHeight,
    %% Top position as ratio (0,1)
    Yt = get_view_ypos(State) / get_view_height(State),
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
    ScaledWidth = get_scaled_view_width(State),
    Wt = DrawingWidth / ScaledWidth,
    HandleLength = Wt*DrawingWidth,
    Xt = get_view_xpos(State) / get_view_width(State),
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

top_offset(#state { winfo = WI }) -> 
    WI#window_info.top_offset + WI#window_info.top_bar.
left_offset(#state { winfo = WI }) -> 
    WI#window_info.left_offset + WI#window_info.left_bar.
right_offset(#state { winfo = WI }) -> 
    WI#window_info.right_offset + WI#window_info.right_bar.
bottom_offset(#state { winfo = WI }) -> 
    WI#window_info.bottom_offset + WI#window_info.bottom_bar.

scroll_hndl_size(#state { winfo = WI }) -> WI#window_info.scroll_hndl_size.
scroll_bar_size(#state { winfo = WI }) -> WI#window_info.scroll_bar_size.

scroll_horizontal(#state { winfo = WI }) -> WI#window_info.scroll_horizontal.
scroll_vertical(#state { winfo = WI }) -> WI#window_info.scroll_vertical.

scroll_xstep(#state { winfo = WI }) -> WI#window_info.scroll_xstep.
scroll_ystep(#state { winfo = WI }) -> WI#window_info.scroll_ystep.

glyph_width(#state { winfo = WI }) -> WI#window_info.glyph_width.
glyph_height(#state { winfo = WI }) -> WI#window_info.glyph_height.
glyph_ascent(#state { winfo = WI }) -> WI#window_info.glyph_ascent.
glyph_descent(#state { winfo = WI }) -> WI#window_info.glyph_descent.

%% profile acces
background_color(#state { content = WD }) ->
    P = WD#window_content.profile,
    epx_profile:color(P#window_profile.scheme,
		      P#window_profile.background_color).

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
get_view_origin(#state { content = WD }) -> 
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

get_motion(#state { content = WD }) -> WD#window_content.motion.

set_motion(S=#state { content = WD }, Motion) ->
    S#state { content = WD#window_content { motion = Motion }}.

draw_content(Pixmap, Dirty, State) ->
    case ?CALLBACK(State, draw) of
	undefined ->
	    ?dbg("warning: no draw/3 function for module ~s\n", 
		 [State#state.user_mod]),
	    State;
	Draw ->
	    Rect = if Dirty =:= undefined ->
			   {0,0,State#state.width,State#state.height};
		      tuple_size(Dirty) =:= 4 ->
			   Dirty
		   end,
	    %% fixme: pixmap_set_clip to shield pixmap
	    export_state(State),
	    UserState = Draw(Pixmap,Rect,State#state.user_state),
	    State1 = state(),
	    State1#state { user_state = UserState }
    end.

%% top & bottom bar has priority over left and right...
draw_top_bar(Pixels, State) ->
    case toolbars(State) of
	{_Left,_Right,Top,_Bottom}
	  when Top > 0 ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(top_bar_color(State)), 
	    DrawRect = {0,0,State#state.width,Top},
	    epx:draw_rectangle(Pixels, DrawRect),
	    State;
	_ ->
	    State
    end.

draw_bottom_bar(Pixels, State) ->
    case toolbars(State) of
	{_Left,_Right,_Top,Bottom}  
	  when Bottom > 0,
	       State#state.height >= Bottom ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(bottom_bar_color(State)), 
	    X0 = 0,
	    Y0 = State#state.height-Bottom,
	    DrawRect = {X0,Y0,State#state.width,Bottom},
	    epx:draw_rectangle(Pixels, DrawRect),
	    epx_gc:set_foreground_color({0,0,0}),
	    epx_gc:set_fill_style(none),
	    epx:draw_rectangle(Pixels, DrawRect),
	    epx_gc:set_font(State#state.font),
	    epx_gc:set_foreground_color((State#state.window_profile)#window_profile.font_color),
	    draw_text(Pixels, X0+10, Y0, 100, Bottom-2, 
		      State#state.status, State),
	    State;
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
	    State;
	_ ->
	    State
    end.

draw_right_bar(Pixels, State) ->
    case toolbars(State) of
	{_Left,Right,Top,Bottom} when 
	      Right > 0,
	      State#state.width >= Right,
	      State#state.height >= (Top+Bottom)->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(right_bar_color(State)),
	    DrawRect = {State#state.width-Right,Top,Right,
			State#state.height-(Top+Bottom)},
	    epx:draw_rectangle(Pixels, DrawRect),
	    State;
	_ ->
	    State
    end.

draw_text(Pixels, X0, Y0, _W, _H, Text, S) ->
    X = X0,
    GA = glyph_ascent(S),
    Y = Y0+1+GA,
    epx_gc:set_foreground_color(?TEXT_COLOR),
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
       menu_font_name = ?ld(menu_font_name, E, D),
       menu_font_size = ?ld(menu_font_size, E, D),
       menu_font_color = ?ldc(S,menu_font_color,E,D),
       menu_background_color = ?ldc(S,menu_background_color,E,D),
       menu_border_color = ?ldc(S,menu_border_color,E,D),
       
       window_font_name = ?ld(window_font_name, E, D),
       window_font_size = ?ld(window_font_size, E, D),
       window_font_color = ?ldc(S, window_font_color, E, D),
       scroll_bar_color  = ?ldc(S, scroll_bar_color, E, D),
       scroll_hndl_color = ?ldc(S, scroll_hndl_color, E, D),
       scroll_horizontal = ?ld(scroll_horizontal, E, D),
       scroll_vertical   = ?ld(scroll_vertical, E, D),
       top_bar_color     = ?ldc(S, top_bar_color, E, D),
       left_bar_color    = ?ldc(S, left_bar_color, E, D),
       right_bar_color   = ?ldc(S, right_bar_color, E, D),
       bottom_bar_color  = ?ldc(S, bottom_bar_color, E, D),

       top_bar     = ?ldc(S, top_bar, E, D),
       left_bar    = ?ldc(S, left_bar, E, D),
       right_bar   = ?ldc(S, right_bar, E, D),
       bottom_bar  = ?ldc(S, bottom_bar, E, D),
       
       top_offset     = ?ldc(S, top_offset, E, D),
       left_offset    = ?ldc(S, left_offset, E, D),
       right_offset   = ?ldc(S, right_offset, E, D),
       bottom_offset  = ?ldc(S, bottom_offset, E, D),

       scroll_bar_size = ?ldc(S, scroll_bar_size, E, D),
       scroll_hndl_size = ?ldc(S, scroll_hndl_size, E, D),
       scroll_xstep = ?ldc(S, scroll_xstep, E, D),
       scroll_ystep = ?ldc(S, scroll_ystep, E, D)
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
       scroll_vertical   = Profile#profile.scroll_vertical
      }.

resize_pixmap(undefined, W, H, Attached) ->
    Pixmap = next_pixmap(W,H),
    if Attached ->
	    epx:pixmap_attach(Pixmap);
       true ->
	    ok
    end,
    Pixmap;
resize_pixmap(Pixmap, W, H, Attached) ->
    case epx:pixmap_info(Pixmap,[width,height]) of
	[{width,PW},{height,PH}] when PW < W; PH < H ->
	    if Attached ->
		    epx:pixmap_detach(Pixmap),
		    Pixmap1 = next_pixmap(W,H),
		    epx:pixmap_attach(Pixmap1),
		    Pixmap1;
	       true ->
		    next_pixmap(W,H)
	    end;
	_ ->
	    Pixmap
    end.

next_pixmap(W,H) ->
    NPW = 1 bsl ceil(math:log2(W)),
    NPH = 1 bsl ceil(math:log2(H)),
    epx:pixmap_create(NPW, NPH, argb).
