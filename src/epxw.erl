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
-export([window/0, screen/0, pixels/0, width/0, height/0]).
-export([content_pos/0, content_pos/2, pixmap_pos/2]).
-export([content_width/0, content_height/0, content_rect/0]).
-export([invalidate/0, invalidate/1]).
-export([set_status_text/1]).
 
-define(DEBUG, true).

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
-callback close(State :: term()) -> 
    NewState :: term().
-callback draw(Pixels::epx:epx_pixmap(), Rect::epx:epx_rect(),
	       State :: term()) -> 
    NewState :: term().

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
%% -callback format_status(Opt, StatusData) -> Status when
%%      Opt :: 'normal' | 'terminate',
%%      StatusData :: [PDict | State],
%%      PDict :: [{Key :: term(), Value :: term()}],
%%      State :: term(),
%%      Status :: term().

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
    close/1,
    draw/3]).

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

%% color profile with default values
-record(profile,
	{
	 scheme                        = logo, %% xterm,
	 screen_color                  = grey2,
	 selection_alpha               = 100,
	 selection_color               = grey,
	 selection_border_width        = 1,
	 selection_border_color        = grey10,

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
	 scroll_horizontal             = right,
	 scroll_vertical               = bottom,
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
         code_changed :: undefined | fun(),
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
         close  :: undefined |
		      fun((State::term()) -> NewState::term()),
	 draw  :: undefined |
		      fun((Pixmap::epx:epx_pixmap(),Rect::epx:epx_rect(),
			   State::term()) -> NewState::term())
 }).

-define(CALLBACK(S,Name), (((S)#state.user_cb)#callbacks.Name)).

-record(state, 
	{
	 window :: #epx_window{},
	 screen :: #epx_pixmap{},   %% on-screen pixels
	 pixels :: #epx_pixmap{},   %% off-screen pixels
	 width  :: integer(),      %% window width
	 height :: integer(),      %% window height
	 invalid :: undefined | epx:epx_rect(),
	 status = "" :: string(),  %% bottom status text
	 profile :: #profile{},
	 menu_profile :: #menu_profile{},
	 window_profile :: #window_profile{},
	 content :: #window_content{},
	 keymod :: #keymod{}, %% modifiers
	 esc    :: boolean(), %% escape modifier
	 menu,                %% global menu state
	 winfo :: #window_info{},
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

%% dictionary ugly api
window() -> (get(epx_server_state))#state.window.
screen() -> (get(epx_server_state))#state.screen.  %% temporary config change
pixels() -> (get(epx_server_state))#state.pixels.  %% temporary config change
width()  -> (get(epx_server_state))#state.width.   %% temporary config change
height() -> (get(epx_server_state))#state.height.  %% temporary config change
content_pos() -> get_view_pos(get(epx_server_state), 0, 0).
content_pos(X,Y) -> get_view_pos(get(epx_server_state), X, Y).
content_width() -> get_view_width(get(epx_server_state)).
content_height() -> get_view_height(get(epx_server_state)).
content_rect() ->
    S = get(epx_server_state),
    {get_view_xpos(S),get_view_ypos(S),get_view_width(S),get_view_height(S)}.
    
pixmap_pos(X,Y) -> get_rview_pos(get(epx_server_state), X, Y).
    

invalidate() -> self() ! {'INVALIDATE',all}.
invalidate(Area={_X,_Y,_W,_H}) -> self() ! {'INVALIDATE',Area}.
set_status_text(Text) -> self() ! {'SET_STATUS_TEXT', lists:flatten(Text)}.
    
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
	       bottom_bar = 18 %% candy use a bottom status bar
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
				  UserCb#callbacks.leave}
				]),
    Window = epx:window_create(50, 50, Width, Height, EventMask),
    Screen = epx:pixmap_create(Width, Height, argb),
    Pixels = undefined, %% epx:pixmap_create(Width, Height, argb),
    ScreenColor = epx_profile:color(WProfile#window_profile.scheme,
				    WProfile#window_profile.background_color),
    epx:pixmap_fill(Screen, ScreenColor),
    %% epx:pixmap_fill(Pixels, ScreenColor),
    epx:window_attach(Window),
    epx:pixmap_attach(Screen),
    epx:window_adjust(Window, [{name, Title}]),
    UserState = case UserCb#callbacks.init of
		    undefined -> undefined;
		    Init -> Init(Window, Screen, UserOpts)
		end,
    State0 = #state {
		window = Window,
		screen = Screen,
		pixels = Pixels,  %% (off-screen = true)
		width  = Width,
		height = Height,
		winfo  = WInfo,
		profile = Profile,
		keymod = #keymod{},
		esc = false,
		menu_profile = MProfile,
		window_profile = WProfile,
		content = WContent,
		user_mod = UserMod,
		user_cb  = UserCb,
		user_state = UserState
	       },
    State1 = set_view_rect(State0, 0, Width-scroll_bar_size(State0)-1,
			   0, (1.5)*Height),
    Rect = {0, 0, Width, Height},
    State2 = user_event({configure,Rect}, ?CALLBACK(State1,configure), State1),
    {ok, draw(State2, Rect)}.


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
	code_changed = load_callback_(UserMod, UserMap, code_change, 3),
	terminate = load_callback_(UserMod, UserMap, close, 2),
	close = load_callback_(UserMod, UserMap, close, 1),
	configure = load_callback_(UserMod, UserMap, configure, 2),
	key_press = load_callback_(UserMod, UserMap, key_press, 2),
	key_release = load_callback_(UserMod, UserMap, key_release, 2),
	button_press = load_callback_(UserMod, UserMap, button_press, 2),
	button_release = load_callback_(UserMod, UserMap, button_release, 2),
	enter = load_callback_(UserMod, UserMap, enter, 2),
	leave = load_callback_(UserMod, UserMap, leave, 2),
	draw  = load_callback_(UserMod, UserMap, draw, 3)
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
    case erlang:function_exported(State#state.user_mod, handle_call, 3) of
	true ->
	    put(epx_server_state, State),
	    Reply = apply(State#state.user_mod, handle_call,
			  [Request, From, State#state.user_state]),
	    reply(Reply, State);
	false ->
	    {stop, missing_handle_call, State}
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
    case erlang:function_exported(State#state.user_mod, handle_cast, 2) of
	true ->
	    put(epx_server_state, State),
	    Reply = apply(State#state.user_mod, handle_cast,
			  [Request, State#state.user_state]),
	    reply(Reply, State);
	false ->
	    {noreply, State}
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
    epx_event(Event, State);
handle_info({'INVALIDATE',Area},State) ->
    All = {0,0,State#state.width,State#state.height},
    Invalid = collect_invalidate(Area, All, State#state.invalid),
    self() ! 'REDRAW',
    {noreply, State#state { invalid = Invalid} };
handle_info('REDRAW',State) ->
    State1 = draw(State),
    {noreply, State1#state { invalid = undefined }};
handle_info({'SET_STATUS_TEXT', Text}, State) ->
    {noreply, State#state { status = Text }};
handle_info(Info, State) ->
    case erlang:function_exported(State#state.user_mod, handle_info, 2) of
	true ->
	    put(epx_server_state, State),
	    Reply = apply(State#state.user_mod, handle_info,
			  [Info, State#state.user_state]),
	    reply(Reply, State);
	false ->
	    ?dbg("handle_info: unhandled event ~p", [Info]),
	    {noreply, State}
    end.

reply({reply,Reply,UState},State) ->
    {reply, Reply, State#state { user_state = UState }};
reply({reply,Reply,UState,Arg},State) ->
    {reply, Reply, State#state { user_state = UState }, Arg};
reply({noreply,UState}, State) ->
    {noreply, State#state { user_state = UState }};
reply({noreply,UState,Arg}, State) ->
    {noreply, State#state { user_state = UState },Arg};
reply({stop, Reason, UState}, State) ->
    {stop, Reason, State#state { user_state = UState }}.

-spec handle_continue(Info :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
	  {stop, Reason :: term(), NewState :: term()}.

handle_continue(Info, State) ->
    case erlang:function_exported(State#state.user_mod, handle_info, 2) of
	true ->
	    put(epx_server_state, State),
	    Reply = apply(State#state.user_mod, handle_continue,
			  [Info, State#state.user_state]),
	    reply(Reply, State);
	false ->
	    ?dbg("handle_continue: unhandled event ~p", [Info]),
	    {noreply, State}
    end.



collect_invalidate(all, All, _Invalid) ->
    collect_invalidate(All,All);
collect_invalidate(Area, All, Invalid) ->
    collect_invalidate(epx_rect:union(Area,Invalid),All).

collect_invalidate(Invalid,All) ->
    receive
	{'INVALIDATE',all} ->
	    collect_invalidate(All,All);
	{'INVALIDATE',Area} ->
	    collect_invalidate(epx_rect:union(Area, Invalid),All)
    after 10 ->
	    Invalid
    end.

epx_event(_Event={configure,Rect0}, State) ->
    Rect1 = {_X,_Y,W,H} = flush_configure(State#state.window, Rect0),
    if W =/= State#state.width; H =/= State#state.height ->
	    Screen = resize_pixmap(State#state.screen, W, H, true),
	    Pixels = if State#state.pixels =:= undefined -> undefined;
			true -> resize_pixmap(State#state.pixels, W, H, false)
		     end,
	    State1 = State#state { screen = Screen, 
				   pixels = Pixels,
				   width=W, height=H },
	    State2 = user_event({configure,Rect1},
				?CALLBACK(State1,configure),
				State1),
	    {noreply, draw(State2)};
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
	    State2 = user_event(Event,
				?CALLBACK(State1,key_press),
				State1),
	    {noreply, State2#state { esc = false }}
    end;
epx_event(Event={key_release, _Sym, Mod, _code}, State) ->
    M = clr_mod(State#state.keymod, Mod),
    State1 = State#state { keymod = M },
    State2 = user_event(Event,?CALLBACK(State1,key_release), State1),
    {noreply, State2};

epx_event(Event={button_release, [wheel_down], _Pos3D}, State) ->
    flush_wheel(State#state.window),
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, scroll_down(State1)};
epx_event(Event={button_release, [wheel_up], _Pos3D}, State) ->
    flush_wheel(State#state.window),
    State1 = user_event(Event, ?CALLBACK(State,button_release), State),
    {noreply, scroll_up(State1)};

epx_event(Event={button_press, _Buttons, _Pos3D={X0,Y0,_}}, State) ->
    case lists:member(left, _Buttons) of
	true ->
	    case scroll_hit({X0,Y0}, State) of
		false ->
		    State1 = user_event(Event, ?CALLBACK(State,button_press), State),
		    {noreply, State1};
		State1 ->
		    {noreply, State1}
	    end;
	false ->
	    State1 = user_event(Event, ?CALLBACK(State,button_press), State),
	    {noreply, State1}
    end;
epx_event(Event={button_release, _Buttons, _Pos3D}, State) ->
    flush_motion(State#state.window),
    State1 =
	case get_motion(State) of
	    undefined ->
		user_event(Event, ?CALLBACK(State,button_release), State);
	    {vhndl,_Delta} ->
		epx:window_disable_events(State#state.window, [motion]),
		set_motion(State, undefined);
	    {hhndl,_Delta} ->
		epx:window_disable_events(State#state.window, [motion]),
		set_motion(State, undefined)
	end,
    {noreply, State1};


epx_event(Event={enter, _Pos3D}, State) ->
    State1 = user_event(Event, ?CALLBACK(State,enter), State),
    {noreply, State1};
epx_event(Event={leave, _Pos3D}, State) ->
    State1 = user_event(Event, ?CALLBACK(State,leave), State),
    {noreply, State1};
epx_event(close, State) ->
    case ?CALLBACK(State, close) of
	undefined ->
	    {stop, normal, State};
	Cb ->
	    put(epx_server_state, State),
	    Cb(State#state.user_state),
	    {stop, normal, State}
    end;
epx_event(_Event, State) ->
    ?dbg("unhandled epx event: ~p\n", [_Event]),
    {noreply,State}.

scroll_hit(Pos, State) ->
    case epx_rect:contains(get_hscroll(State), Pos) of
	true ->
	    epx:window_enable_events(State#state.window, [motion]),
	    case epx_rect:contains(get_hhndl(State), Pos) of
		true ->
		    {Xv,Yv,_,_} = get_hhndl(State),
		    {Xp,Yp} = Pos,
		    Delta = {Xp-Xv, Yp-Yv},
		    set_motion(State,{hhndl,Delta});
		false ->
		    RightOffset = right_offset(State),
		    {_,_,W,_} = get_hscroll(State),
		    {_,_,Vw,_} = get_hhndl(State),
		    {Xp,_Yp} = Pos,
		    VW = get_view_width(State),
		    X2  = trunc((Xp-(Vw div 2))*(VW/W)),
		    R = max(0, (get_view_right(State)-RightOffset)-W),
		    X  = clamp(X2, 0, R),
		    State1 = set_view_xpos(State, X),
		    State2 = set_motion(State1,{hhndl,{(Vw div 2),0}}),
		    draw(State2)
	    end;
	false ->
	    case epx_rect:contains(get_vscroll(State), Pos) of
		true ->
		    epx:window_enable_events(State#state.window, [motion]),
		    case epx_rect:contains(get_vhndl(State), Pos) of
			true ->
			    {Xv,Yv,_,_} = get_vhndl(State),
			    {Xp,Yp} = Pos,
			    Delta = {Xp-Xv, Yp-Yv},
			    set_motion(State,{vhndl,Delta});
			false ->
			    BottomOffset = bottom_offset(State),
			    {_,_,_,H} = get_vscroll(State),
			    {_,_,_,Vh} = get_vhndl(State),
			    {_Xp,Yp} = Pos,
			    VH = get_view_height(State),
			    Y2  = trunc((Yp-(Vh div 2))*(VH/H)),
			    B = max(0,(get_view_bottom(State)+BottomOffset)-H),
			    Y  = clamp(Y2, 0, B),
			    State1 = set_view_ypos(State, Y),
			    State2 = set_motion(State1,{vhndl,{0,(Vh div 2)}}),
			    draw(State2)
		    end;
		false ->
		    false
	    end
    end.

scroll_up(State) ->
    State1 = step_up(State, scroll_ystep(State)),
    draw(State1).

scroll_down(State) ->
    State1 = step_down(State, scroll_ystep(State)),
    draw(State1).

step_up(State, Step) ->
    Y = max(0, get_view_ypos(State) - Step),
    set_view_ypos(State, Y).

step_down(State, Step) ->
    BottomOffset = bottom_offset(State),
    H = case get_hscroll(State) of
	    undefined -> State#state.height;
	    _ -> State#state.height - scroll_bar_size(State)
	end,
    B = max(0, (get_view_bottom(State)+BottomOffset) - H),
    Y = min(get_view_ypos(State) + Step, B),
    set_view_ypos(State, Y).


clamp(Value, Min, Max) ->
    if Value < Min -> Min;
       Value > Max -> Max;
       true -> Value
    end.


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
	    put(epx_server_state, State),
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
    case erlang:function_exported(State#state.user_mod, code_change, 3) of
	true ->
	    put(epx_server_state, State),
	    case apply(State#state.user_mod, code_change,
		       [OldVsn, State#state.user_state, Extra]) of
		{ok,UState} ->
		    {ok, State#state { user_state = UState }};
		Error ->
		    Error
	    end;
	false ->
	    {ok, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

user_event(_E, undefined, State) ->
    State;
user_event(E, Callback, State) ->
    put(epx_server_state, State),
    UserState = Callback(E, State#state.user_state),
    State#state { user_state = UserState }.

draw(State) ->
    draw(State, undefined).
    
draw(State = #state { profile = Profile }, Dirty) ->
    Scheme = Profile#profile.scheme,
    ScreenColor = epx_profile:color(Scheme, Profile#profile.screen_color),
    HBar = horizontal_scrollbar(State),
    VBar = vertical_scrollbar(State),
    Pixels = pixels(State),
    fill_area(Pixels,Dirty,ScreenColor),
    State1 = draw_content(Pixels,Dirty,State),
    State2 = draw_scrollbar(Pixels,State1,scroll_vertical(State1),HBar),
    State3 = draw_scrollbar(Pixels,State2,scroll_horizontal(State2),VBar),
    State4 = draw_top_bar(Pixels,State3),
    State5 = draw_left_bar(Pixels,State4),
    State6 = draw_right_bar(Pixels,State5),
    State7 = draw_bottom_bar(Pixels,State6),
    if State7#state.pixels =/= undefined ->
	    copy_area(State1#state.pixels,Dirty,State1#state.screen);
       true ->
	    ok
    end,
    epx:pixmap_draw(State2#state.screen, State2#state.window,
		    0, 0, 0, 0,
		    State2#state.width, State2#state.height),    
    epx:sync(State2#state.screen,State2#state.window),
    State2.

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
    %% io:format("fill Pixmap: ~w\n", [epx:pixmap_info(Pixmap)]),
    epx:pixmap_fill_area(Pixmap,X,Y,W,H,Color).


%% scrollbar
draw_scrollbar(Pixels, S, left, HBar) ->
    LeftBar = left_bar(S),
    draw_vertical_scrollbar(Pixels, S, LeftBar, HBar);
draw_scrollbar(Pixels, S, right, HBar) ->
    Size = scroll_bar_size(S),
    RightBar = right_bar(S),
    draw_vertical_scrollbar(Pixels, S,S#state.width-Size-RightBar,HBar);
draw_scrollbar(Pixels, S, top, VBar) ->
    TopBar = top_bar(S),
    draw_horizontal_scrollbar(Pixels, S, TopBar, VBar);
draw_scrollbar(Pixels, S, bottom, VBar) ->
    Size = scroll_bar_size(S),
    BottomBar = bottom_bar(S),
    draw_horizontal_scrollbar(Pixels, S,S#state.height-Size-BottomBar,VBar).

vertical_scrollbar(S) ->
    WH = S#state.height,
    %% VH = D#d.view_bottom - D#d.view_top,
    VH = get_view_height(S),
    if VH > WH ->  scroll_vertical(S);
       true  -> none
    end.

draw_vertical_scrollbar(Pixels, S, X0, HBar) ->
    Size = scroll_bar_size(S),
    HndlSize = scroll_hndl_size(S),
    {_,_,TopBar,BottomBar} = bar(S),
    WH = if HBar =:= none -> S#state.height - (TopBar+BottomBar);
	    true -> S#state.height - (Size+TopBar+BottomBar)
	 end,
    VH = get_view_height(S),
    if VH > WH ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(scroll_bar_color(S)),
	    Y0 = case HBar of
		     none   -> TopBar;
		     bottom -> TopBar;
		     top    -> TopBar+Size
		 end,
	    Rect = {X0,Y0,Size,WH},
	    DrawRect = {X0,TopBar,Size,
			S#state.height-(TopBar+BottomBar)},
	    epx:draw_rectangle(Pixels, DrawRect),
	    epx_gc:set_fill_color(scroll_hndl_color(S)),
	    Part = WH / VH,
	    HandleLength = trunc(Part*WH),
	    Top = get_view_ypos(S),
	    HandlePos = trunc(Part*Top),
	    Pad = (Size - HndlSize) div 2,
	    HRect = {X0+Pad,Y0+HandlePos,HndlSize,HandleLength},
	    epx:draw_roundrect(Pixels,HRect,5,5),
	    set_vscroll(S, Rect, HRect);
       true ->
	    set_vscroll(S, undefined, undefined)
    end.

horizontal_scrollbar(S) ->
    WW = S#state.width,
    %% VW = D#d.view_right - D#d.view_left,
    VW = get_view_width(S),
    case VW > WW of
	true -> scroll_horizontal(S);
	false -> none
    end.
	    
%% fixme remove vertical scrollbar if present!
draw_horizontal_scrollbar(Pixels, S, Y0, VBar) ->
    Size = scroll_bar_size(S),
    HndlSize = scroll_hndl_size(S),
    {LeftBar,RightBar,_,_} = bar(S),
    WW = if VBar =:= none -> S#state.width - (LeftBar+RightBar);
	    true -> S#state.width - (Size+LeftBar+RightBar)
	 end,
    VW = get_view_width(S),
    if VW > WW ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(scroll_bar_color(S)),
	    X0 = case VBar of
		     none -> LeftBar;
		     left -> LeftBar;
		     right -> LeftBar+Size
		 end,
	    Rect = {X0,Y0,WW,Size},
	    DrawRect = {LeftBar,Y0,S#state.width-(LeftBar+RightBar),
			Size},
	    epx:draw_rectangle(Pixels, DrawRect),
	    epx_gc:set_fill_color(scroll_hndl_color(S)),
	    Part = WW / VW,
	    Left = get_view_xpos(S), 
	    HandleLength = trunc(Part*WW),
	    HandlePos = trunc(Part*Left),
	    Pad = (Size - HndlSize) div 2,
	    HRect = {HandlePos,Y0+Pad,HandleLength,HndlSize},
	    epx:draw_roundrect(Pixels,HRect,5,5),
	    set_hscroll(S, Rect, HRect);
       true ->
	    set_hscroll(S, undefined, undefined)
    end.

get_hscroll(#state { content = WD }) -> WD#window_content.hscroll.
get_hhndl(#state { content = WD }) -> WD#window_content.hhndl.
get_vscroll(#state { content = WD }) -> WD#window_content.vscroll.
get_vhndl(#state { content = WD }) -> WD#window_content.vhndl.

set_hscroll(S=#state { content = WD }, Rect, Hndl) ->
    io:format("set_hscroll rect=~w, hndl=~w\n", [Rect,Hndl]),
    S#state { content = WD#window_content { hscroll = Rect,
					    hhndl = Hndl }}.
set_vscroll(S=#state { content = WD }, Rect, Hndl) ->
    S#state { content = WD#window_content { vscroll = Rect,
					    vhndl = Hndl }}.

bar(#state { winfo = WI }) ->
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

get_view_xpos(#state { content = WD }) -> WD#window_content.view_xpos.
get_view_ypos(#state { content = WD }) -> WD#window_content.view_ypos.

get_view_pos(#state { content = WD }, X, Y) -> 
    {WD#window_content.view_xpos+X,WD#window_content.view_ypos+Y}.

get_rview_pos(#state { content = WD }, X, Y) -> 
    { X-WD#window_content.view_xpos, Y-WD#window_content.view_ypos}.

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
    WD#window_content.view_right - WD#window_content.view_left.
get_view_height(#state { content = WD }) ->
    WD#window_content.view_bottom - WD#window_content.view_top.

set_view_rect(S=#state { content = WD }, L, R, T, B) ->    
    S#state { content = WD#window_content { view_left = L,
					    view_right = R,
					    view_top   = T,
					    view_bottom = B }}.

get_motion(#state { content = WD }) -> WD#window_content.motion.

set_motion(S=#state { content = WD }, Motion) ->
    io:format("set_motion: ~w\n", [Motion]),
    S#state { content = WD#window_content { motion = Motion }}.

draw_content(Pixmap, Dirty, State) ->
    case ?CALLBACK(State, draw) of
	undefined ->
	    io:format("warning: no draw/3 function for module ~s\n", 
		      [State#state.user_mod]),
	    State;
	Draw ->
	    Rect = if Dirty =:= undefined -> {0,0,State#state.width,
					      State#state.height};
		      tuple_size(Dirty) =:= 4 -> Dirty
		   end,
	    %% fixme: pixmap_set_clip to shield pixmap
	    put(epx_server_state, State),
	    UserState = Draw(Pixmap,Rect,State#state.user_state),
	    State#state { user_state = UserState }
    end.

%% top & bottom bar has priority over left and right...
draw_top_bar(Pixels, S) ->
    case bar(S) of
	{_Left,_Right,0,_Bottom} ->
	    S;
	{_Left,_Right,Top,_Bottom} ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(top_bar_color(S)), 
	    DrawRect = {0,0,S#state.width,Top},
	    epx:draw_rectangle(Pixels, DrawRect),
	    S
    end.

draw_bottom_bar(Pixels, S) ->
    case bar(S) of
	{_Left,_Right,_Top,0} ->
	    S;
	{_Left,_Right,_Top,Bottom} ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(bottom_bar_color(S)), 
	    X0 = 0,
	    Y0 = S#state.height-Bottom,
	    DrawRect = {X0,Y0,S#state.width,Bottom},
	    epx:draw_rectangle(Pixels, DrawRect),
	    epx_gc:set_foreground_color({0,0,0}),
	    epx_gc:set_fill_style(none),
	    epx:draw_rectangle(Pixels, DrawRect),
	    draw_text(Pixels, X0+10, Y0, 100, Bottom-2, 
		      S#state.status, S),
	    S
    end.

draw_left_bar(Pixels, S) ->
    case bar(S) of
	{0,_,_,_} ->
	    S;
	{Left,_Right,Top,Bottom} ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(left_bar_color(S)),
	    DrawRect = {0,Top,Left, S#state.height-(Top+Bottom)},
	    epx:draw_rectangle(Pixels, DrawRect),
	    S
    end.

draw_right_bar(Pixels, S) ->
    case bar(S) of
	{_Left,0,_Top,_Bottom} -> 
	    S;
	{_Left,Right,Top,Bottom} ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(right_bar_color(S)),
	    DrawRect = {S#state.width-Right,Top,Right,
			S#state.height-(Top+Bottom)},
	    epx:draw_rectangle(Pixels, DrawRect),
	    S
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
       bottom_bar_color  = ?ldc(S, bottom_bar_color, E, D)
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
       bottom_bar_color  = Profile#profile.bottom_bar_color
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
