%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2018, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    epx GUI server
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------

-module(epxy).

-behaviour(gen_server).

%% API
-export([start/1, start_link/1, stop/0]).
-export([new/2, delete/1, set/2, get/2, inject/2]).
-export([add_callback/3, remove_callback/1]).

%% User API - maybe called from user type callbacks and internally
-export([widget_set/2, widget_get/2]).

%% Utils
-export([color_add/2, color_interpolate/3, color_add_argb/2, 
	 color_interpolate_argb/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("epx/include/epx.hrl").
-include_lib("epx/include/epx_image.hrl").

-define(SERVER, ?MODULE).
-define(DICT_T, term()).  %% dict:dict()
-define(SETS_T, term()).  %% sets:set()
-define(TREE_DB_T, term()).  %% table()?

-define(DEFAULT_WIDTH,  320).
-define(DEFAULT_HEIGHT, 240).
-define(DEFAULT_WINDOW_ID, "screen").

-define(MAX_TICKS, 16#ffffffff).  %% about 49.7 days

-define(is_string(Cs), is_list((Cs))).

%% HARD DEBUG
-define(dbg(F), ok).
-define(dbg(F,A), ok).
%% -define(dbg(F,A), io:format((F),(A))).
%% -define(dbg(F), io:format((F))).

-type widget_type() :: window | panel | button | switch | slider | value |
		       menu | rectangle | ellipse | line | text | user.

%% convert to map?
-record(widget,
	{
	  id :: string(),     %% (structured) name of widget
	  type :: widget_type(),
	  window :: string(), %% id of base window (if type != window)
	  state  = normal,    %% or active,selected,closed ..
	  static = false,     %% object may not be deleted
	  hidden = false,     %% show/hiden false,true,all,none
	  disabled = false,   %% enable disable event input false,true,all,none
	  edit = false,       %% allow edit of text fields
	  relative = true :: boolean(), %% childrens are relative to parent
	  user,               %% mfa when type=user
	  filter,             %% mfa for event filter
	  children_first = true :: boolean(), %% draw/select children first
	  x = 0   :: integer(),
	  y = 0   :: integer(),
	  z = 0   :: integer(),   %% define the order for overlap
	  width  = 0 :: non_neg_integer(),
	  height = 0 :: non_neg_integer(),
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
	  font_color = 16#00000000,
	  fill   = none :: epx:epx_fill_style(),
	  events  :: epx:epx_window_event_flags(),
	  halign  = center :: top|bottom|center,
	  valign  = center :: left|right|center,
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
	}).

-record(sub,
	{
	  ref :: reference(),
	  mon :: reference(),
	  id  :: string(),
	  callback :: atom() | function(),
	  signal :: term()
	}).

-type xy() :: {integer(), integer()}.

-record(state, {
	  redraw_timer = undefined,
	  active = [] :: [{string(),xy()}],    %% active widgets (ids) pressed
	  focus  = [] :: [{string(),xy()}],    %% focused widgets (ids)
	  subs = [] :: [#sub{}],
	  fps = 30.0 :: number(),       %% animation frames per second
	  mpf = 1000/30.0 :: number(),  %% millis per frame
	  clock :: reference(),         %% clock reference
	  redraw_tick :: number(),      %% aprox redraw clock
	  default_font :: epx:epx_font(),
	  wset    :: ?SETS_T,           %% set of window id
	  wtree   :: ?TREE_DB_T         %% tree_db of all widgets
	  %% widgets are now stored in process dictionary
	  %% widgets :: ?DICT_T   %% term => #widget{}
	 }).

-define(MEMBER(X,E1,E2), 
	case X of E1->true;E2->true;_->false end).
-define(MEMBER(X,E1,E2,E3), 
	case X of E1->true;E2->true;E3->true;_->false end).
-define(MEMBER(X,E1,E2,E3,E4),
	case X of E1->true;E2->true;E3->true;E4->true;_->false end).
-define(MEMBER(X,E1,E2,E3,E4,E5,E6),
	case X of E1->true;E2->true;E3->true;E4->true;
	    E5->true;E6->true;_->false end).
-define(MEMBER(X,E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11),
	case X of 
	    E1->true;E2->true;E3->true;E4->true;
	    E5->true;E6->true;E7->true;E8->true;
	    E9->true;E10->true;E11->true;
	    _->false end).
-define(MEMBER(X,E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12),
	case X of 
	    E1->true;E2->true;E3->true;E4->true;
	    E5->true;E6->true;E7->true;E8->true;
	    E9->true;E10->true;E11->true;E12->true;
	    _->false end).
-define(MEMBER(X,E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,
	       E11,E12,E13,E14,E15,E16,E17,E18,E19,E20,
	       E21,E22,E23,E24,E25,E26),
	case X of 
	    E1->true;E2->true;E3->true;E4->true;
	    E5->true;E6->true;E7->true;E8->true;
	    E9->true;E10->true;
	    E11->true;E12->true;E13->true;E14->true;
	    E15->true;E16->true;E17->true;E18->true;
	    E19->true;E20->true;
	    E21->true;E22->true;E23->true;E24->true;
	    E25->true;E26->true;
	    _->false end).
-define(MEMBER(X,E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,
	       E11,E12,E13,E14,E15,E16,E17,E18,E19,E20,
	       E21,E22,E23,E24,E25,E26,E27),
	case X of 
	    E1->true;E2->true;E3->true;E4->true;
	    E5->true;E6->true;E7->true;E8->true;
	    E9->true;E10->true;
	    E11->true;E12->true;E13->true;E14->true;
	    E15->true;E16->true;E17->true;E18->true;
	    E19->true;E20->true;
	    E21->true;E22->true;E23->true;E24->true;
	    E25->true;E26->true;E27->true;
	    _->false end).

-define(TABS_X_OFFSET, 10).  %% should scale with size!
-define(TABS_Y_OFFSET, 10).  %% should scale with size!
-define(TABS_X_PAD, 16).      %% should scale with font size!
-define(TABS_Y_PAD, 8).      %% should scale with font size!
-define(TABS_COLOR, 16#ffcccccc).  %% configure this

-define(EOT, '$end_of_table').

new(ID,Flags) ->
    gen_server:call(?MODULE, {new,ID,Flags}).

delete(ID) ->
    gen_server:call(?MODULE, {delete,ID}).

set(ID, Flags) ->
    gen_server:call(?MODULE, {set,ID,Flags}).

get(ID, Flags) ->
    gen_server:call(?MODULE, {get,ID,Flags}).

add_callback(ID, Signal, Cb) ->
    gen_server:call(?MODULE, {add_callback, self(), ID, Signal, Cb}).

remove_callback(Ref) ->
    gen_server:call(?MODULE, {remove_callback, Ref}).

inject(WinID,Event) ->
    gen_server:cast(?MODULE, {inject, WinID, Event}).

stop() ->
    gen_server:call(?MODULE, stop).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start(Options) ->
    application:start(epx),
    gen_server:start({local, ?SERVER}, ?MODULE, Options, []).

start_link(Options) ->
    application:start(epx),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    Backend = epx_backend:default(),
    Fps = proplists:get_value(fps, Args, 30.0),
    Name = epx:backend_info(Backend, name),
    Width =
	if Name =:= "fb" ->
		case proplists:get_value(width, Args) of
		    undefined -> epx:backend_info(Backend, width);
		    W0 -> W0
		end;
	   true ->
		proplists:get_value(width, Args, ?DEFAULT_WIDTH)
	end,
    Height =
	if Name =:= "fb" ->
		case proplists:get_value(height, Args) of
		    undefined -> epx:backend_info(Backend, height);
		    H0 -> H0
		end;
	   true ->
		proplists:get_value(height, Args, ?DEFAULT_HEIGHT)
	end,
    Font = %% load a default font
	case epx_font:match([{name,"Arial"},{size,12}]) of
	    false -> undefined;
	    {ok,F} -> F
	end,
    Default = window_new([{id,?DEFAULT_WINDOW_ID},
			  {static,true},
			  {x,50},{y,50},
			  {width,Width},{height,Height},
			  {events, [key_press,key_release,
				    button_press, button_release,
				    %% configure,resize,focus,
				    %% crossing, motion
				    button,wheel]},
			  {color, 16#ffffffff}]),
    Tree = tree_db:new(wtree),
    tree_db:insert(Tree, {?DEFAULT_WINDOW_ID,?DEFAULT_WINDOW_ID}),
    self() ! refresh,
    %% This clock will run for 49,71 days before timeout, but we
    %% use it as a cheap? clock source.
    Clock = clock_create(),
    widget_store(Default),
    {ok, #state{ default_font = Font,
		 fps = Fps,
		 clock = Clock,
		 wset = sets:from_list([Default#widget.id]),
		 wtree = Tree
	       }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({set,ID,Flags}, _From, State) ->
    case widget_find(ID) of
	error ->
	    %% ?dbg("set ~s not found\n", [ID]),
	    {reply,{error,enoent},State};
	{ok,W} ->
	    try widget_set(W,Flags) of
		W1 ->
		    W2 = update_widget(W1),
		    widget_store(W2),
		    self() ! refresh,
		    {reply, ok, State}
	    catch
		error:Reason ->
		    lager:warning("set ~s ~p crashed ~p\n", [ID,Flags,Reason]),
		    {reply, {error,Reason}, State}
	    end
    end;
handle_call({get,ID,Flags},_From,State) ->
    case widget_find(ID) of
	error ->
	    {reply,{error,enoent}, State};
	{ok,W} ->
	    {reply,{ok,widget_get(W,Flags)}, State}
    end;
handle_call({new,ID,Flags}, _From, State) ->
    case id_split(ID) of
	false -> %% unstructured ID maybe a window or something below screen
	    case widget_find(ID) of
		error ->
		    case widget_create(ID,?DEFAULT_WINDOW_ID,Flags,State) of
			{ok,_W} ->
			    {reply, ok, State};
			Error={error,Reason} ->
			    lager:warning("widget ~p not created ~p\n",
				       [ID, Reason]),
			    {reply, Error, State}
		    end;
		{ok,_W} ->
		    {reply,{error,ealready},State}
	    end;
	{Root,ID1} ->
	    case widget_find(Root) of
		error ->
		    case widget_create(ID,?DEFAULT_WINDOW_ID,Flags,State) of
			{ok,_W} ->
			    {reply, ok, State};
			Error={error,Reason} ->
			    lager:warning("widget ~p not created ~p\n",
				       [ID, Reason]),
			    {reply, Error, State}
		    end;
		{ok,Win} when Win#widget.type =:= window ->
		    case widget_create(ID1,Win#widget.id,Flags,State) of
			{ok,_W} ->
			    {reply, ok, State};
			Error={error,Reason} ->
			    lager:warning("widget ~p not created ~p\n",
				       [ID, Reason]),
			    {reply, Error, State}
		    end;
		{ok,_W} -> %% not under a window, must be under "screen"
		    case widget_create(ID,?DEFAULT_WINDOW_ID,Flags,State) of
			{ok,_W1} ->
			    {reply, ok, State};
			Error={error,Reason} ->
			    lager:warning("widget ~p not created ~p\n",
				       [ID, Reason]),
			    {reply, Error, State}
		    end
	    end
    end;
handle_call({delete,ID}, _From, State) ->
    case widget_find(ID) of
	error ->
	    {reply,{error,enoent}, State};
	{ok,W} ->
	    State1 = 
		if W#widget.type =:= window, W#widget.static ->
			widget_delete_tree(W#widget.id, State);
		   W#widget.type =:= window ->
			epx:window_detach(W#widget.win),
			epx:pixmap_detach(W#widget.backing),
			widget_delete(W,widget_delete_tree(W#widget.id,State));
		   true ->
			widget_delete(W, State)
		end,
	    self() ! refresh,
	    {reply, ok, State1}
    end;
handle_call({add_callback,Pid,ID,Signal,Cb}, _From, State) ->
    Ref = erlang:monitor(process, Pid),
    Sub = #sub{id=ID,ref=Ref,signal=Signal,callback=Cb},
    Subs = [Sub|State#state.subs],
    {reply, {ok,Ref}, State#state { subs=Subs}};
handle_call({remove_callback,Ref}, _From, State) ->
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {reply, {error, not_found}, State};
	{value, Sub, Subs} ->
	    erlang:demonitor(Sub#sub.ref, [flush]),
	    {reply, ok, State#state { subs=Subs} }
    end;
handle_call(stop,_From,State) ->
    {stop, normal, State};
    
handle_call(_Request, _From, State) ->
    lager:debug("unknown call ~p", [_Request]),
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({inject, WinID, Event}, State) ->
    Window = widget_fetch(WinID),
    handle_event(Event, Window, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({epx_event,Win,Event}, State) ->
    ?dbg("event: ~p\n", [Event]),
    %% find window widget (fixme: add reverse map at some point) ?
    case fold_windows(
	   fun(W,Acc) ->
		   if W#widget.win =:= Win -> [W|Acc];
		      true -> Acc
		   end
	   end, [], State) of
	[] ->
	    lager:error("window ~p not found", [Win]),
	    {noreply, State};
	[W] ->  %% should only be one!
	    handle_event(Event, W, State)
    end;
handle_info(refresh, State) ->
    {noreply, redraw_schedule(State)};
handle_info({timeout,TRef,redraw}, State)
  when TRef =:= State#state.redraw_timer ->
    %% lager:debug("redraw"),
    put(animations, false),
    State1 = redraw_state(State#state { redraw_timer=undefined}),
    case get(animations) of
	false ->
	    {noreply, State1};
	true ->
	    {noreply, redraw_schedule(State1)}
    end;
handle_info(clock_restart, State) ->
    %% fixme how to handle handle active timers ?
    {noreply, State#state { clock = clock_create() }};

handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {noreply, State};
	{value, _Sub, Subs} ->
	    {noreply, State#state { subs=Subs} }
    end;
handle_info(_Info, State) ->
    lager:debug("info = ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    fold_windows(fun unmap_window/2, State, State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% get first component of ID
id_split(ID) ->
    case string:chr(ID,$.) of
	0 -> false;
	I -> {A,[$.|B]} = lists:split(I-1,ID),
	     {A,B}
    end.

widget_create(ID,WID,Flags,State) ->
    W0 = #widget{ id=ID, font=State#state.default_font },
    try widget_set(W0,Flags) of
	W1 ->
	    W2 = case W1#widget.type of
		     window ->
			 window_open(W1);
		     user ->
			 W11 = W1#widget { window=WID },
			 user(W11#widget.user,init,undefined,W11,WID,undefined);
		     _ ->
			 W1#widget { window=WID }
		 end,
	    Y = if W2#widget.type =:= window ->
			W2#widget.id;
		   true ->
			W2#widget.window++"."++W2#widget.id
		end,
	    tree_db:insert(State#state.wtree,{Y,W2#widget.id}),
	    W3 = update_widget(W2),
	    widget_store(W3),
	    self() ! refresh,
	    {ok,W3}
    catch
	error:Reason ->
	    lager:warning("widget ~p not created ~p\n", [ID, Reason]),
	    {error,Reason}
    end.

widget_store(W) ->
    %% ?dbg("widget_store: ~p\n", [W]),
    put(W#widget.id, W),
    W.

widget_erase(ID) ->
    erase(ID).

widget_fetch(ID) ->
    case get(ID) of
	W when is_record(W, widget) -> W
    end.

widget_find(ID) ->
    case get(ID) of
	undefined ->
	    error;
	W when is_record(W, widget) -> 
	    {ok,W}
    end.

handle_event(Event={key_press,_Sym,_Mod,_Code},Window,State) ->
    ?dbg("event: ~p\n", [Event]),
    case State#state.focus of
	[] ->
	    {noreply, State};
	Ws0 ->
	    Ws = [{widget_fetch(ID),XY} || {ID,XY} <- Ws0],
	    State1 = widgets_event(Ws, Event, Window, State),
	    {noreply, State1}
    end;
handle_event(Event={key_release,_Sym,_Mod,_Code},Window,State) ->
    ?dbg("event: ~p\n", [Event]),
    case State#state.focus of
	[] ->
	    {noreply, State};
	Ws0 ->
	    Ws = [{widget_fetch(ID),XY} || {ID,XY} <- Ws0],
	    State1 = widgets_event(Ws, Event, Window, State),
	    {noreply, State1}
    end;
handle_event(Event={button_press,Button,Where},Window,State) ->
    case lists:member(left,Button) of
	true ->
	    %% locate an active widget at position (X,Y)
	    WinID = Window#widget.id,
	    case widgets_at_location(Where,{0,0},WinID,State) of
		[] ->
		    {noreply, State};
		Ws ->
		    ?dbg("press widget = ~p\n", [[V#widget.id||{V,_XY}<-Ws]]),
		    State1 = widgets_event(Ws,Event,Window,State),
		    {noreply, State1}
	    end;
	false ->
	    {noreply, State}
    end;
handle_event(Event={button_release,Button,Where},Window,State) ->
    case lists:member(left,Button) of
	true ->
	    WinID = Window#widget.id,
	    Ws0 = widgets_at_location(Where,{0,0},WinID,State),
	    %% also add active widgets not already on the list
	    ?dbg("release widget = ~p\n", [[V#widget.id||{V,_XY}<-Ws0]]),
	    Ws1 = lists:foldl(
		    fun({Wid,Offs},Ws) ->
			    case widget_member(Wid, Ws) of
				false -> [{widget_fetch(Wid),Offs}|Ws];
				true -> Ws
			    end
		    end, Ws0, State#state.active),
	    State1 = 
		lists:foldl(
		  fun({W,Offs},Si) ->
			  case widget_event(Event,W,Offs,Window,Si) of
			      W ->
				  Si; %% no changed
			      W1 ->
				  widget_store(W1),
				  self() ! refresh,
				  Si
			  end
		  end, State, Ws1),
	    {noreply, State1#state { active = [] }};
	false ->
	    {noreply, State}
    end;
handle_event(Event={motion,Button,Where},Window,State) ->
    case lists:member(left,Button) of
	true ->
	    case State#state.active of
		[] ->
		    WinID = Window#widget.id,
		    case widgets_at_location(Where,{0,0},WinID,State) of
			[] ->
			    {noreply, State};
			Ws ->
			    State1 = widgets_motion(Ws, Event, Window, State),
			    {noreply, State1}
		    end;
		Ws0 ->
		    Ws = [{widget_fetch(ID),XY} || {ID,XY} <- Ws0],
		    State1 = widgets_motion(Ws, Event, Window, State),
		    {noreply, State1}
	    end;
	false ->
	    {noreply, State}
    end;

handle_event({configure,{_X,_Y,_Width,_Height}},_Window,State) ->
    {noreply, State};
handle_event({resize,{_Width,_Height,_Depth}},_Window,State) ->
    {noreply, State};
handle_event({enter, _Where},_Window,State) ->
    {noreply, State};
handle_event({leave, _Where},_Window,State) ->
    {noreply, State};
handle_event(focus_in,_Window,State) ->
    {noreply, State};
handle_event(focus_out,_Window,State) ->
    {noreply, State};
handle_event(Event=close,Window,State) ->
    if Window#widget.static =:= true -> %% may not be deleted
	    {noreply, State};
       true ->
	    unmap_window(Window,State),
	    Window1 = widget_event(Event,Window,{0,0},Window,State),
	    State1 = widget_delete(Window1, State),
	    self() ! refresh,
	    {noreply, State1}
    end;
handle_event(_Event,_W,State) ->
    lager:error("unknown event: ~p", [_Event]),
    {noreply, State}.


%% member in widget,offset list
widget_member(Wid, [{W1,_}|Ws]) ->
    if Wid =:= W1#widget.id -> true;
       true -> widget_member(Wid, Ws)
    end;
widget_member(_W, []) ->
    false.

widgets_motion([{W,XY}|Ws],Event,Window,State) ->
    case widget_event(Event,W,XY,Window,State) of
	W -> 
	    widgets_motion(Ws,Event,Window,State);
	W1 ->
	    widget_store(W1),
	    self() ! refresh,
	    widgets_motion(Ws,Event,Window,State)
    end;
widgets_motion([],_Event,_Window,State) ->
    State.

widgets_event([{W,XY}|Ws],Event,Window,State) ->
    case widget_event(Event,W,XY,Window,State) of
	W -> 
	    widgets_event(Ws,Event,Window,State);
	W1 ->
	    Focus = 
		if W#widget.state =/= focus, W1#widget.state =:= focus ->
			%% Focus changed
			case State#state.focus of
			    [] ->
				?dbg("focus_in ~s\n", [W1#widget.id]),
				[{W1#widget.id,XY}];
			    [{F,_Fxy}] -> %% old focuse
				W0 = widget_fetch(F),
				?dbg("focus_out ~s\n", [W0#widget.id]),
				widget_store(W0#widget { state = normal }),
				?dbg("focus_in ~s\n", [W1#widget.id]),
				[{W1#widget.id,XY}]
			end;
		   W#widget.state =:= focus, W1#widget.state =/= focus ->
			%% Focus changed
			?dbg("focus_out ~s\n", [W1#widget.id]),
			State#state.focus -- [{W1#widget.id,XY}];
		   true ->
			State#state.focus
		end,
	    Active = [{W1#widget.id,XY}|State#state.active],
	    widget_store(W1),
	    self() ! refresh,
	    widgets_event(Ws,Event,Window,State#state { active=Active,
							focus=Focus })
    end;
widgets_event([],_Event,_Window,State) ->
    State.

%%
%% Find all widgets in window WinID that is hit by the
%% point (X,Y).
%% FIXME? Return a Z sorted list
%%
widgets_at_location(Pos,XY,WinID,State) ->
    Ws = select_tree(WinID,Pos,XY,[],State),
    %% sort according to Z order
    %% lists:sort(fun(A,B) -> A#widget.z > B#widget.z end, Ws).
    lists:reverse(Ws).

select_tree(?EOT,_Pos,_XY,Acc,_State) ->
    Acc;
select_tree(ID,Pos,XY,Acc,State) ->
    ChildID = tree_db:first_child(State#state.wtree, ID),
    select_siblings(ChildID,Pos,XY,Acc,State).

select_siblings(?EOT,_Pos,_XY,Acc,_State) ->
    Acc;
select_siblings(ID,Pos,XY,Acc,State) ->
    Acc1 = select_one(ID,Pos,XY,Acc,true,State),
    NextSibling = tree_db:next_sibling(State#state.wtree, ID),
    select_siblings(NextSibling,Pos,XY,Acc1,State).

select_one(ID,Pos,XY,Acc,ChildrenFirst,State) ->
    [{_,Wid}] = tree_db:lookup(State#state.wtree,ID),
    W = widget_fetch(Wid),
    XY1 = widget_pos(W,XY),
    if ChildrenFirst ->
	    Acc1 = select_children(W,ID,Pos,XY1,Acc,State),
	    select_widget(W,Pos,XY1,Acc1,State);
       true ->
	    Acc1 = select_widget(W,Pos,XY1,Acc,State),
	    select_children(W,ID,Pos,XY1,Acc1,State)
    end.

select_children(W,_ID,_Pos,_XY,Acc,_State) when W#widget.disabled =:= all ->
    Acc;
select_children(W,ID,Pos,XY,Acc,State) when 
      W#widget.type =:= panel, W#widget.disabled =:= false ->
    case tab_at_location(W,Pos,XY) of
	0 -> %% check in current tab
	    V = W#widget.value,
	    N = length(W#widget.tabs),
	    if V =:= 0 ->
		    %% no child selected
		    Acc;
	       V >= 1, V =< N ->
		    Tab = lists:nth(V, W#widget.tabs),
		    TabID = ID++[list_to_binary(Tab)],
		    select_one(TabID,Pos,XY,Acc,false,State);
	       true ->
		    lager:error("panel tab ~w not defined in ~s\n",
				[V,W#widget.id]),
		    Acc
	    end;
	_Tab ->
	    ?dbg("tab at ~w = ~w\n", [XY,_Tab]),
	    [{W,XY}|Acc]
    end;
select_children(_W,ID,Pos,XY,Acc,State) ->
    select_tree(ID,Pos,XY,Acc,State).


select_widget(W,_Pos,_XY,Acc,_State)
  when W#widget.disabled =:= true; W#widget.disabled =:= all ->
    Acc;
select_widget(W,Pos={Xi,Yi,_Z},XY={X,Y},Acc,_State) ->
    case in_rect(Xi,Yi,X,Y,W#widget.width,W#widget.height) of
	true ->
	    %% Option to allow select even outside bounding box?
	    if W#widget.type =:= user ->
		    case user(W#widget.user,select,Pos,W,undefined,XY) of
			W1 when is_record(W1, widget) ->
			    [{W1,XY}|Acc];
			_ ->
			    Acc
		    end;
	       true ->
		    [{W,XY}|Acc]
	    end;
	false ->
	    case topimage_at_location(W,Pos,XY,W#widget.topimage) of
		true ->
		    [{W,XY}|Acc];
		false ->
		    Acc
	    end
    end.

%% Check if (X,Y) is within any of the panel tabs
tab_at_location(W,{Xi,Yi,_Z},_XY={Xw,Yw}) ->
    {_Ascent,TextDims,MaxW,MaxH} = tabs_item_box(W),
    N = length(TextDims),
    Width =  (MaxW+?TABS_X_PAD),
    Height = (MaxH+?TABS_Y_PAD),
    case W#widget.orientation of
	horizontal ->
	    Xoffs = (W#widget.width - (Width*N)) div 2,
	    X0 = Xw + Xoffs,
	    Y0 = Yw + ?TABS_Y_OFFSET,
	    case in_rect(Xi,Yi,X0,Y0,Width*N,Height) of
		false -> 0;
		true  -> ((Xi-Xoffs) div Width)+1
	    end;
	vertical ->
	    Yoffs = (W#widget.height - (Height*N)) div 2,
	    Y0 = Yw + Yoffs,
	    X0 = Xw + ?TABS_X_OFFSET,
	    case in_rect(Xi,Yi,X0,Y0,Width,Height*N) of
		false -> 0;
		true -> ((Yi-Yoffs) div Height)+1
	    end
    end.

item_at_location(W,{Xi,Yi,_Z},_XY={Xw,Yw}) ->
    case length(W#widget.items) of
	0 -> 0;
	N ->
	    case in_rect(Xi,Yi,Xw,Yw,
			 W#widget.width,W#widget.height) of
		false -> 0;
		true ->
		    H = W#widget.height div N,
		    ((Yi-Yw) div H)+1
	    end
    end.

in_rect(X,Y,Xr,Yr,Wr,Hr) ->
    if X >= Xr, X < Xr + Wr, Y >= Yr, Y < Yr + Hr -> true;
       true -> false
    end.
    
%%
%% Check if (X,Y) hit inside a topimage (used in slider)
%%
topimage_at_location(_W,_Pos,_XY,undefined) ->
    false;
topimage_at_location(W=#widget {orientation=horizontal},{X,Y,_},
		     _XY={Xi,Yi},Image) ->
    Height = epx:pixmap_info(Image,height),
    Y1 = Yi + (W#widget.height - Height) div 2,
    Y2 = Yi + (W#widget.height + Height) div 2,
    (X >= Xi) andalso (X < Xi + W#widget.width) 
	andalso (Y >= Y1) andalso (Y =< Y2);
topimage_at_location(W=#widget {orientation=vertical},{X,Y,_},
		     _XY={Xi,Yi},Image) ->
    Width = epx:pixmap_info(Image,width),
    X1 = Xi + (W#widget.width - Width) div 2,
    X2 = Xi + (W#widget.width + Width) div 2,
    (Y >= Yi) andalso (Y < Yi + W#widget.height) 
	andalso (X >= X1) andalso (X =< X2).

widget_pos(W, XY) ->
    if W#widget.relative ->
	    {Xo,Yo} = XY,
	    {W#widget.x+Xo, W#widget.y+Yo};
       true ->
	    {W#widget.x, W#widget.y}
    end.

-ifdef(not_used).
image_at_location(_W,_X,_Y,undefined) ->
    false;
image_at_location(W,X,Y,Image) ->
    Height = epx:pixmap_info(Image,height),
    Width = epx:pixmap_info(Image,width),
    if X >= W#widget.x, Y >= W#widget.y,
       X =< W#widget.x + Width - 1, 
       Y =< W#widget.y + Height - 1 ->
	    true;
       true ->
	    false
    end.
-endif.

-ifdef(not_used).
animation_at_location(_W,_X,_Y,undefined) ->
    false;
animation_at_location(W,X,Y,Anim) ->
    Height = epx:animation_info(Anim,height),
    Width = epx:animation_info(Anim,width),
    if X >= W#widget.x, Y >= W#widget.y,
       X =< W#widget.x + Width - 1, 
       Y =< W#widget.y + Height - 1 ->
	    true;
       true ->
	    false
    end.
-endif.
      

%% generate a callback event and start animate the button

%% fixme add widget event_filter the filter fun works like
%% lists:filter return true to include and false/crash to discard
widget_event(Event, W, XY, Window, State) ->
    case W#widget.filter of
	undefined -> %% include 
	    widget_event_(Event, W, XY, Window, State);
	Filter ->
	    try user(Filter,filter,Event,W,Window,XY) of
		false -> %% discard
		    W;
		true ->  %% include
		    widget_event_(Event, W, XY, Window, State)
	    catch
		error:_ ->  %% discard
		    W
	    end
    end.

widget_event_(Event={button_press,_Button,Where},W,XY,Window,State) ->
    case W#widget.type of
	button ->
	    callback_all(W#widget.id, State#state.subs, [{value,1}]),
	    W#widget { state=active, value=1 };

	switch ->
	    {WState,Value} =
		case W#widget.state of
		    active -> {normal,0};
		    _ -> {active,1}
		end,
	    callback_all(W#widget.id, State#state.subs, [{value,Value}]),
	    W#widget { frame=Value, state=WState, value=Value };

	slider ->
	    case widget_slider_value(W,Where,XY) of
		false ->
		    lager:debug("slider min/max/width error"),
		    W;
		Value ->
		    epx:window_enable_events(Window#widget.win, [motion]),
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    W#widget { state=active, value = Value }
	    end;

	panel ->
	    case tab_at_location(W,Where,XY) of
		0 ->
		    lager:debug("panel box select error"),
		    W;
		Value ->
		    ?dbg("tab at ~w = ~w\n", [XY,Value]),
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    W#widget { value = Value }
	    end;

	menu ->
	    case item_at_location(W,Where,XY) of
		0 ->
		    lager:debug("menu box select error"),
		    W;
		Value ->
		    ?dbg("item at ~w = ~w\n", [XY,Value]),
		    epx:window_enable_events(Window#widget.win, [motion]),
		    W#widget { value = Value }
	    end;
	user ->
	    case user(W#widget.user,event,Event,W,Window,XY) of
		W1 when is_record(W1, widget) ->
		    W1;
		_ ->
		    W
	    end;
	text ->
	    W1 = 
		case W#widget.state of
		    focus -> W;
		    _ ->
			if W#widget.edit -> W#widget { state = focus };
			   true -> W
			end
		end,
	    {Xi,Yi} = XY,
	    {X,Y,_} = Where,
	    callback_all(W1#widget.id,State#state.subs,
			 [{press,1},{x,X-Xi},{y,Y-Yi}]),
	    W1;
	_ ->
	    {Xi,Yi} = XY,
	    {X,Y,_} = Where,
	    callback_all(W#widget.id,State#state.subs,
			 [{press,1},{x,X-Xi},{y,Y-Yi}]),
	    W#widget { state=selected }
    end;
widget_event_(Event={button_release,_Button,Where},W,XY,Window,State) ->
    case W#widget.type of
	button ->
	    callback_all(W#widget.id, State#state.subs, [{value,0}]),
	    W#widget{state=normal, value=0};
	switch ->
	    W;
	slider ->
	    epx:window_disable_events(Window#widget.win, [motion]),
	    W#widget{state=normal};
	panel ->
	    W;
	menu ->
	    epx:window_disable_events(Window#widget.win, [motion]),
	    Value = W#widget.value,
	    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
	    W#widget{state=normal};
	user ->
	    case user(W#widget.user,event,Event,W,Window,XY) of
		W1 when is_record(W1, widget) ->
		    W1;
		_ ->
		    W
	    end;
	text ->
	    {Xi,Yi} = XY,
	    {X,Y,_} = Where,
	    callback_all(W#widget.id,State#state.subs,
			 [{press,0},{x,X-Xi},{y,Y-Yi}]),
	    W;
	_ ->
	    {Xi,Yi} = XY,
	    {X,Y,_} = Where,
	    callback_all(W#widget.id,State#state.subs,
			 [{press,0},{x,X-Xi},{y,Y-Yi}]),
	    W#widget { state=normal }
    end;
widget_event_(Event={motion,_Button,Where},W,XY,Window,State) ->
    case W#widget.type of
	slider ->
	    case widget_slider_value(W,Where,XY) of
		false -> W;
		Value ->
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    W#widget { value = Value }
	    end;
	menu ->
	    case item_at_location(W,Where,XY) of
		Value when Value =/=  W#widget.value ->
		    W#widget { value = Value };
		_ -> W
	    end;
	user ->
	    case user(W#widget.user,event,Event,W,Window,XY) of
		W1 when is_record(W1, widget) -> W1;
		_ -> W
	    end;
	_ ->
	    W
    end;
widget_event_(_Event={key_press,Sym,_Mod,_Code},W,_XY,_Window,State) ->
    case W#widget.type of
	text ->
	    case Sym of
		$\r ->
		    Value = W#widget.text,
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    %% will release focus!
		    ?dbg("focus_out ~s\n", [W#widget.id]),
		    W#widget { state = normal };
		$\e ->
		    %% will release focus!
		    ?dbg("focus_out ~s\n", [W#widget.id]),
		    W#widget { state = normal };
		$\t ->
		    %% will release focus!
		    ?dbg("focus_out ~s\n", [W#widget.id]),
		    W#widget { state = normal };
		$\b -> %% delete backwards
		    Text1 = case W#widget.text of
				"" -> "";
				Text -> lists:reverse(tl(lists:reverse(Text)))
			    end,
		    W#widget { text = Text1 };
		_ when is_integer(Sym), Sym >= $\s, Sym =< $~ ->
		    Text1 = W#widget.text ++ [Sym],
		    W#widget { text = Text1 };
		_ ->
		    ?dbg("ignore symbol ~p\n", [_Event]),
		    W
	    end;
	_ ->
	    W
    end;
widget_event_(close,W,_XY,_Window,State) ->
    callback_all(W#widget.id,State#state.subs,[{closed,true}]),
    W#widget { state=closed };
widget_event_(Event,W,XY,Window,_State) ->
    case W#widget.type of
	user ->
	    case user(W#widget.user,event,Event,W,Window,XY) of
		W1 when is_record(W1, widget) -> W1;
		_ -> W
	    end;
	_ ->
	    W
    end.

%% Calcuate the slider value given coordinate X,Y either horizontal or
%% vertical. return a floating point value between 0 and 1
widget_slider_value(W=#widget {min=Min,max=Max,orientation=horizontal},
		    {X,_Y,_Z},{Xi,_Yi}) ->
    %% Xi = if W#widget.relative -> W#widget.x+Xo; true -> W#widget.x end,
    Width = W#widget.width-2,
    if is_number(Min), is_number(Max), Width > 0 ->
	    X0 = Xi+1,
	    X1 = X0 + Width - 1,
	    Xv = clamp(X, X0, X1),
	    R = (Xv - X0) / (X1 - X0),
	    trunc(Min + R*(Max - Min));
       true ->
	    false
    end;
widget_slider_value(W=#widget {min=Min, max=Max,orientation=vertical},
		    {_X,Y,_Z},{_Xi,Yi}) ->
    %% Yi = if W#widget.relative -> W#widget.y+Yo; true -> W#widget.y end,
    Height = W#widget.height-2,
    if is_number(Min), is_number(Max), Height > 0 ->
	    Y1 = Yi+1,
	    Y0 = Y1 + Height - 1,
	    Yv = clamp(Y, Y1, Y0),
	    R = (Y0 - Yv) / (Y0 - Y1),
	    trunc(Min + R*(Max - Min));
       true ->
	    false
    end.

callback_all(Wid, Subs, Env) ->
    lists:foreach(
      fun(#sub{id=ID,signal=Signal,callback=Callback}) ->
	      if ID =:= Wid ->
		      callback(Callback,Signal,ID,Env);
		 true ->
		      ok
	      end
      end, Subs).
%%
%% note that event signals may loopback and be time consuming,
%% better to spawn them like this.
%%
callback(undefined,_Signal,_ID,_Env)  ->
    ok;
callback(Cb,Signal,ID,Env) when is_atom(Cb) ->
    spawn(fun() -> Cb:event(Signal,ID,Env) end);
callback(Cb,_Signal,_ID,Env) when is_function(Cb, 1) ->
    spawn(fun() -> Cb(Env) end);
callback(Cb,Signal,_ID,Env) when is_function(Cb, 2) ->
    spawn(fun() -> Cb(Signal,Env) end);
callback(Cb,Signal,ID,Env) when is_function(Cb, 3) ->
    spawn(fun() -> Cb(Signal,ID,Env) end).

window_new(Flags) ->
    W = widget_set(#widget{},[{type,window}|Flags]),
    window_open(W).

window_open(W) ->
    Events = 
	if W#widget.events =:= undefined ->
		[key_press,key_release,
		 button_press,button_release,
		 button,wheel];
	   true ->
		W#widget.events
	end,
    Win = epx:window_create(W#widget.x, W#widget.y,
			    W#widget.width, W#widget.height, 
			    Events),
    epx:window_attach(Win),
    Backing = epx:pixmap_create(W#widget.width, W#widget.height),
    epx:pixmap_attach(Backing),
    Px = epx:pixmap_create(W#widget.width, W#widget.height),
    W#widget { win=Win, image=Px, backing=Backing, events=Events }.

widget_set(W,VKs) ->
    widget_set_(W,VKs,0).

widget_set_(W,[{Key,Value}|Ks],Mask) ->
    case keypos(Key) of
	0 ->
	    lager:debug("key ~p not found, ignored ~p", [Key]),
	    widget_set_(W,Ks,Mask);
	I ->
	    %% ?dbg("widget_set_: ~s ~p[~w] = ~p\n", [W#widget.id,Key,I,Value]),
	    Mask1 = Mask bor (1 bsl I),
	    case validate(I,Value) of
		true ->
		    widget_set_(setelement(I,W,Value),Ks,Mask1);
		{true,Value1} ->
		    widget_set_(setelement(I,W,Value1),Ks,Mask1);
		false ->
		    lager:debug("value ~p for key ~p not valid, ignored", 
				[Value,Key]),
		    widget_set_(W,Ks,Mask1)
	    end
    end;
widget_set_(W,[],Mask) ->
    %% some special cases 
    if Mask band (1 bsl #widget.state) =/= 0 ->
	    case W#widget.state of
		active -> W#widget { value=1 };
		normal -> W#widget { value=0 };
		_ -> W
	    end;
       Mask band ((1 bsl #widget.value) bor
		  (1 bsl #widget.max) bor
		  (1 bsl #widget.min)) =/= 0 ->
	    V = clamp(W#widget.value, W#widget.min, W#widget.max),
	    W#widget { value=V };
       true ->
	    W
    end.
	    
widget_get(W,Keys) ->
    widget_get_(W,Keys,[]).

widget_get_(W,[K|Ks],Acc) ->
    case keypos(K) of
	0 ->
	    lager:debug("key not found ~p", [K]),
	    widget_get_(W,Ks,Acc);
	I ->
	    widget_get_(W,Ks,[{K,element(I,W)}|Acc])
    end;
widget_get_(_W,[],Acc) ->
    lists:reverse(Acc).

keypos(Key) ->
    case Key of
	id     -> #widget.id;
	type   -> #widget.type;
	window -> #widget.window;
	state  -> #widget.state;
	static -> #widget.static;
	edit   -> #widget.edit;
	hidden -> #widget.hidden;
	disabled -> #widget.disabled;
	relative -> #widget.relative;
	user -> #widget.user;
	filter -> #widget.filter;
	children_first -> #widget.children_first;
	x -> #widget.x;
	y -> #widget.y;
	z -> #widget.z;
	width -> #widget.width;
	height -> #widget.height;
	text -> #widget.text;
	tabs -> #widget.tabs;
	items -> #widget.items;
	border -> #widget.border;
	border_color -> #widget.border_color;
	shadow_x -> #widget.shadow_x;
	shadow_y -> #widget.shadow_y;
	round_w -> #widget.round_w;
	round_h -> #widget.round_h;
	orientation -> #widget.orientation;
	image -> #widget.image;
	image2 -> #widget.image2;
	topimage -> #widget.topimage;
	animation -> #widget.animation;
	animation2 -> #widget.animation2;
	frame -> #widget.frame;
	frame2 -> #widget.frame2;
	color -> #widget.color;
	color2 -> #widget.color2;
	font_color -> #widget.font_color;
	fill     -> #widget.fill;
	events   -> #widget.events;
	halign   -> #widget.halign;
	valign   -> #widget.valign;
	min      -> #widget.min;
	max      -> #widget.max;
	format   -> #widget.format;
	value    -> #widget.value;
	vscale   -> #widget.vscale;
	animate  -> #widget.animate;
	animate2 -> #widget.animate2;
	font     -> #widget.font;
	win      -> #widget.win;
	backing  -> #widget.backing;
	user_data -> #widget.user_data;
	_ -> 0
    end.

%% return true|false|{true,Value'}
validate(#widget.type,Type) ->
    ?MEMBER(Type,window,panel,button,switch,slider,value,menu,
	    rectangle,ellipse,line,text,user);
validate(#widget.id,ID) when is_atom(ID) -> {true, atom_to_list(ID)};
validate(#widget.id,ID) when is_list(ID) -> true;
validate(#widget.user,{M,F,As}) -> 
    is_atom(M) andalso is_atom(F) andalso is_list(As);
validate(#widget.filter,{M,F,As}) -> 
    is_atom(M) andalso is_atom(F) andalso is_list(As);
validate(#widget.static,Arg) ->  ?MEMBER(Arg,true,false);
validate(#widget.hidden,Arg) ->  ?MEMBER(Arg,true,false,none,all);
validate(#widget.disabled,Arg) -> ?MEMBER(Arg,true,false,none,all);
validate(#widget.edit,Arg)     ->  ?MEMBER(Arg,true,false);
validate(#widget.x,Arg) -> is_integer(Arg);
validate(#widget.y,Arg) -> is_integer(Arg);
validate(#widget.z,Arg) -> is_integer(Arg);
validate(#widget.shadow_x,Arg) -> is_integer(Arg);
validate(#widget.shadow_y,Arg) -> is_integer(Arg);
validate(#widget.round_w,Arg) -> is_integer(Arg);
validate(#widget.round_h,Arg) -> is_integer(Arg);
validate(#widget.children_first,Arg) -> ?MEMBER(Arg,true,false);
validate(#widget.relative,Arg) ->  ?MEMBER(Arg,true,false);
validate(#widget.width,Arg) -> is_integer(Arg) andalso (Arg >= 0);
validate(#widget.height,Arg) -> is_integer(Arg) andalso (Arg >= 0);
validate(#widget.text,Arg) when is_list(Arg) -> true;
validate(#widget.text,Arg) when is_atom(Arg) -> {true,atom_to_list(Arg)};
validate(#widget.tabs,Arg) -> is_list(Arg);
validate(#widget.items,Arg) -> is_list(Arg);
validate(#widget.border,Arg) -> is_integer(Arg);
validate(#widget.border_color,Arg) -> validate_color(Arg);
validate(#widget.orientation,Arg) -> ?MEMBER(Arg,horizontal,vertical);
validate(#widget.image,Arg) -> validate_image(Arg);
validate(#widget.topimage,Arg) -> validate_image(Arg);
validate(#widget.animation,Arg) -> validate_animation(Arg);
validate(#widget.animation2,Arg) -> validate_animation(Arg);
validate(#widget.frame,Arg) -> is_integer(Arg);
validate(#widget.frame2,Arg) -> is_integer(Arg);
validate(#widget.animate,Arg) -> ?MEMBER(Arg,continuous,sequence,undefined);
validate(#widget.animate2,Arg) -> ?MEMBER(Arg,continuous,sequence,undefined);
validate(#widget.font,Arg) -> validate_font(Arg);
validate(#widget.color,Arg) -> validate_color(Arg);
validate(#widget.color2,Arg) -> validate_color(Arg);
validate(#widget.font_color,Arg) -> validate_color(Arg);
validate(#widget.fill,Arg) -> ?MEMBER(Arg,solid,blend,sum,aalias,textured,none);
validate(#widget.events,Arg) ->
    ?MEMBER(Arg,key_press,key_release,motion,button_press,button_release,
	    focus_in,focus_out,focus,enter,leave,configure,resize,crossing,
	    button,left,middle,right,wheel,wheel_up,wheel_down,
	    wheel_left,wheel_right,close,destroyed,all,none);
validate(#widget.halign,Arg) ->
    ?MEMBER(Arg,left,right,center);
validate(#widget.valign,Arg) ->
    ?MEMBER(Arg,top,bottom,center);
validate(#widget.min,Arg) -> is_number(Arg);
validate(#widget.max,Arg) -> is_number(Arg);
validate(#widget.value,Arg) -> is_number(Arg);
validate(#widget.vscale,Arg) -> is_number(Arg);
validate(#widget.format,Arg) -> is_list(Arg);
validate(#widget.state,Arg) -> ?MEMBER(Arg,active,normal);
validate(#widget.user_data,_Arg) -> true;
validate(_,_Arg) -> false.

validate_color(Arg) ->
    case parse_color(Arg) of
	false -> false;
	C -> {true,C}
    end.

validate_font(Arg) when is_list(Arg) ->
    case epx_font:match(Arg) of
	false ->
	    lager:error("unable to load font ~p", [Arg]),
	    false;
	{ok,Font} ->
	    {true,Font}
    end;
validate_font(Arg) when is_record(Arg,epx_font) -> true;
validate_font(_) -> false.

validate_animation(Arg) when is_list(Arg) ->
    File = text_expand(Arg, []),
    try epx:animation_open(File) of
	Anim ->
	    lager:debug("open animation file ~s.",[File]),
	    {true,Anim}
    catch
	error:_Reason ->
	    lager:error("unable to open animation file ~s:~p",
			[File,_Reason]),
	    false
    end;
validate_animation(_) ->
    false.

validate_image(Arg) when is_list(Arg) ->
    File = text_expand(Arg, []),
    case epx_image:load(File) of
	{ok,Image} ->
	    lager:debug("load image file ~s.",[File]),
	    case Image#epx_image.pixmaps of
		[Pixmap] ->
		    lager:debug("pixmap created ~s.",[File]),
		    {true,Pixmap};
		_ ->
		    lager:error("no pixmap found in ~s",[File]),
		    false
	    end;
	Error ->
	    lager:error("unable to load image file ~s:~p",
			[File,Error]),
	    false
    end;
validate_image(Arg) when is_record(Arg,epx_pixmap) -> true;
validate_image(_) -> false.

redraw_schedule(State) ->
    if is_reference(State#state.redraw_timer) ->
	    State;
       State#state.redraw_timer =:= undefined ->
	    RedrawTick = clock_timeout(State, State#state.mpf),
	    Timer = erlang:start_timer(trunc(State#state.mpf), self(), redraw),
	    State#state { redraw_timer = Timer,
			  redraw_tick = RedrawTick }
    end.


clock_create() ->
    erlang:start_timer(?MAX_TICKS, self(), clock_restart).

%% clock ticks since (millis) since last clock restart
clock_read(State) ->
    ?MAX_TICKS - erlang:read_timer(State#state.clock).

%% calculate an absolute timeout value (relative clock source)
clock_timeout(State, Time) when is_number(Time), Time >= 0 ->
    Tick = clock_read(State),
    trunc(Tick + Time) band ?MAX_TICKS.
    
redraw_state(State) ->
    %% ?dbg("redraw state\n"),
    fold_windows(fun clear_window/2, State, State),
    fold_windows(fun draw_window/2, State, State),
    fold_windows(fun update_window/2, State, State),
    State.

widget_delete(W = #widget{id=Wid,type=Type}, State) ->
    widget_erase(Wid),
    if Type =:= window ->
	    tree_db:delete(State#state.wtree, Wid),
	    Wset1 = sets:del_element(Wid,State#state.wset),
	    State#state { wset=Wset1 };
       true ->
	    tree_db:delete(State#state.wtree,
			   W#widget.window++"."++Wid),
	    State
    end.

%% delete children but not the root
widget_delete_tree(Root, State) ->
    L = tree_db:to_list(State#state.wtree,Root ++ ".*"),
    lists:foreach(
      fun({K,ID}) ->
	      tree_db:delete(State#state.wtree,K),
	      widget_erase(ID)
      end, L),
    State.

%% fold over windows
fold_windows(Fun, Acc, State) ->
    sets:fold(fun(Wid,Acc1) ->
		      case widget_find(Wid) of
			  error -> Acc1;
			  {ok,W} -> Fun(W, Acc1)
		      end
	      end, Acc, State#state.wset).

clear_window(Win,_State) ->
    epx:pixmap_fill(Win#widget.image, Win#widget.color).

update_window(Win,_State) ->
    %% some framebuf flicker unless we draw into a copy!
    epx:pixmap_copy_to(Win#widget.image, Win#widget.backing),
    epx:pixmap_draw(Win#widget.backing,
		    Win#widget.win,0,0,0,0,
		    Win#widget.width,Win#widget.height).

%% Check automatix adjustments needed after create/set
update_widget(W) ->
    case W#widget.type of
	menu when W#widget.width =:= 0; W#widget.height =:= 0 ->
	    {_Ascent,TextDims,MaxW,MaxH} = menu_item_box(W),
	    N = length(TextDims),
	    Width = if W#widget.width =:= 0 -> MaxW+?TABS_X_PAD;
		       true -> W#widget.width
		    end,
	    Height = if W#widget.height =:= 0 -> (MaxH+?TABS_Y_PAD)*N;
			true -> W#widget.height
		     end,
	    W#widget { width=Width, height=Height };
	_ ->
	    W
    end.

unmap_window(Win,_State) ->
    epx:window_detach(Win#widget.win),
    epx:pixmap_detach(Win#widget.backing).


draw_window(Win, State) ->
    draw_tree(Win#widget.id, Win, {0,0}, State).

draw_tree(?EOT, _Win, _XY, State) ->
    State;
draw_tree(ID, Win, XY, State) ->
    draw_siblings(tree_db:first_child(State#state.wtree, ID), 
		  Win, XY, State).

draw_siblings(?EOT, _Win, _XY, State) ->
    State;
draw_siblings(ID, Win, XY, State) ->
    State1 = draw_one(ID, Win, XY, State),
    draw_siblings(tree_db:next_sibling(State#state.wtree, ID), 
		  Win, XY, State1).

draw_one(ID, Win, XY, State) ->
    case tree_db:lookup(State#state.wtree,ID) of
	[] ->
	    lager:error("widget ~p not in the tree", [ID]),
	    State;
	[{_,Wid}] ->
	    %% ?dbg("draw_one: ~p\n", [Wid]),
	    W = widget_fetch(Wid),
	    draw_one_(ID, Win, XY, W, W#widget.children_first, State)
    end.

draw_one_(ID, Win, XY, W, ChildrenFirst, State) ->
    XY1 = widget_pos(W,XY),
    if ChildrenFirst ->
	    State1 = draw_children(ID, Win, XY1, W, State),
	    draw_widget(W, Win, XY1, State1),
	    State1;
       true ->
	    draw_widget(W, Win, XY1, State),
	    draw_children(ID, Win, XY1, W, State)
    end.

%% Fixme: implement hidden =:= none to override all hidden children!
draw_children(_ID, _Win, _XY, W, State) when W#widget.hidden =:= all ->
    State;
draw_children(ID, Win, XY, W, State) when 
      W#widget.type =:= panel, W#widget.disabled =:= false ->
    V = W#widget.value,
    N = length(W#widget.tabs),
    if V =:= 0 ->
	    %% no child selected
	    State;
       V >= 1, V =< N ->
	    Tab = lists:nth(V, W#widget.tabs),
	    %% tree children first
	    TabID = ID++[list_to_binary(Tab)],
	    draw_child(TabID, Win, XY, State);
       true ->
	    lager:error("panel tab ~w not defined in ~s\n",
			[V,W#widget.id]),
	    State
    end;
draw_children(ID, Win, XY, _W, State) ->
    draw_tree(ID, Win, XY, State).

draw_child(ID, Win, XY, State) ->
    case tree_db:lookup(State#state.wtree,ID) of
	[] ->
	    lager:error("widget ~p not in the tree", [ID]),
	    State;
	[{_,Wid}] ->
	    W = widget_fetch(Wid),
	    draw_one_(ID, Win, XY, W, false, State)
    end.


draw_widget(W, _Win, _XY, _State) when 
      W#widget.hidden =:= true; W#widget.hidden =:= all ->
    ok;
draw_widget(W, Win, XY={X,Y}, _State) ->
    case W#widget.type of
	window ->
	    %% do not draw (yet), we may use this
	    %% to draw multiple/embedded windows in the future
	    ok;
	user ->
	    epx_gc:draw(
	      fun() ->
		      user(W#widget.user,draw,undefined,W,Win,XY)
	      end);
	panel ->
	    epx_gc:draw(
	      fun() ->
		      draw_tabs(Win, X, Y, W)
	      end);
	button ->
	    epx_gc:draw(
	      fun() ->
		      draw_text_box(Win, X, Y, W, W#widget.text)
	      end);

	switch ->
	    epx_gc:draw(
	      fun() ->
		      draw_text_box(Win, X, Y, W, W#widget.text)
	      end);

	slider ->
	    epx_gc:draw(
	      fun() ->
		      draw_background(Win, X, Y, W),
		      draw_border(Win, X, Y, W, W#widget.border),
		      draw_value_bar(Win, X, Y, W, W#widget.topimage)
	      end);

	value ->
	    epx_gc:draw(
	      fun() ->
		      Value = case W#widget.vscale of
				  undefined -> W#widget.value;
				  Scale -> Scale * W#widget.value
			      end,
		      Format = W#widget.format,
		      Text = 
			  if Value =:= undefined ->
				  "-";
			     Format =:= undefined ->
				  if is_integer(Value) ->
					  integer_to_list(Value);
				     is_float(Value) ->
					  io_lib_format:fwrite_g(Value);
				     true ->
					  "?"
				  end;
			     true ->
				  lists:flatten(io_lib:format(Format,[Value]))
			  end,
		      draw_text_box(Win, X, Y, W, Text)
	      end);

	menu ->
	    epx_gc:draw(
	      fun() ->
		      draw_background(Win, X, Y, W),
		      draw_menu(Win, X, Y, W)
	      end);

	rectangle ->
	    epx_gc:draw(
	      fun() ->
		      draw_background(Win, X, Y, W)
	      end);

	ellipse ->
	    epx_gc:draw(
	      fun() ->
		      epx_gc:set_fill_style(W#widget.fill),
		      set_color(W, W#widget.color),
		      epx:draw_ellipse(Win#widget.image, 
				       X, Y,
				       W#widget.width, W#widget.height)
	      end);

	line ->
	    epx_gc:draw(
	      fun() ->
		      set_color(W, W#widget.color),
		      epx:draw_line(Win#widget.image, 
				    X, Y,
				    X+W#widget.width-1,
				    Y+W#widget.height-1)
	      end);

	text ->
	    epx_gc:draw(
	      fun() ->
		      draw_focus(Win, X, Y, W),
		      draw_border(Win, X, Y, W, W#widget.border),
		      draw_text_box(Win, X, Y, W, W#widget.text)
	      end);
	Type ->
	    lager:debug("bad widget type ~p", [Type])
    end.

%% draw widget button/value with centered text
draw_text_box(Win, X, Y, W, Text) ->
    if is_list(Text), Text =/= "" ->
	    Font = W#widget.font,
	    epx_gc:set_font(Font),
	    Ascent = epx:font_info(Font, ascent),
	    {TxW,TxH} = epx_font:dimension(epx_gc:current(), Text),
	    {X1,Y1} = get_coord_xy(W,X,Y),
	    Width = if W#widget.width =:= 0 -> TxW; 
		       true -> W#widget.width 
		    end,
	    Height = if W#widget.height =:= 0 -> TxH; 
			true -> W#widget.height
		     end,
	    draw_background(Win,X,Y,Width,Height,W),
	    set_font_color(W#widget.font_color),
	    draw_text(Win,Ascent,Text,TxW,TxH, 
		      X1,Y1,Width,Height,W#widget.halign,W#widget.valign);
       true ->
	    draw_background(Win, X, Y, W)
    end.

get_coord_xy(W,X0,Y0) ->
    case W#widget.state of
	normal ->
	    {X0,Y0};
	active ->
	    if is_integer(W#widget.shadow_x),
	       is_integer(W#widget.shadow_y) ->
		    {X0+(W#widget.shadow_x bsr 1),
		     Y0+(W#widget.shadow_y bsr 1)};
	       true ->
		    {X0, Y0}
	    end;
	_ ->
	    {X0, Y0}
    end.

%% 
%%  Put tabs as a row (horizontal) 
%%  or column (vertical)
%%
draw_tabs(Win,X,Y,W) ->
    {Ascent,TextDims,MaxW,MaxH} = tabs_item_box(W),
    N = length(TextDims),
    Width =  (MaxW+?TABS_X_PAD),
    Height = (MaxH+?TABS_Y_PAD),
    case W#widget.orientation of
	horizontal ->
	    X0 = X + (W#widget.width - (Width*N)) div 2,
	    Y0 = (Y + ?TABS_Y_OFFSET),
	    set_color(W, ?TABS_COLOR),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Win#widget.image, X0, Y0, Width*N, Height),
	    draw_h_tabs(Win, W, 1, Ascent, TextDims, X0, Y0, Width, Height,
			W#widget.halign, W#widget.valign);
	vertical ->
	    Y0 =  Y + (W#widget.height - (Height*N)) div 2,
	    X0 = (X + ?TABS_X_OFFSET),
	    set_color(W, ?TABS_COLOR),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Win#widget.image, X0, Y0, Width, Height*N),
	    draw_v_tabs(Win, W, 1, Ascent, TextDims, X0, Y0, Width, Height,
			W#widget.halign, W#widget.valign)
    end.

draw_menu(Win,X,Y,W) ->
    {Ascent,TextDims,MaxW,MaxH} = menu_item_box(W),
    N = length(TextDims),
    Width =  (MaxW+?TABS_X_PAD),
    Height = (MaxH+?TABS_Y_PAD),
    Y0 =  Y + (W#widget.height - (Height*N)) div 2,
    X0 = X, %% (X + ?TABS_X_OFFSET),
    set_color(W, ?TABS_COLOR),
    epx_gc:set_fill_style(solid),
    epx:draw_rectangle(Win#widget.image, X0, Y0, Width, Height*N),
    draw_items(Win, W, 1, Ascent, TextDims, X0, Y0, Width, Height,
	       W#widget.halign, W#widget.valign).
    
tabs_item_box(W) ->
    item_box(W, W#widget.tabs).

menu_item_box(W) ->
    item_box(W, W#widget.items).

item_box(W, Items) ->
    Font = W#widget.font,
    epx_gc:set_font(Font),
    Ascent = epx:font_info(Font, ascent),
    TextDims = [ {epx_font:dimension(epx_gc:current(), Text),Text} || 
		   Text <- Items ],
    MaxW = lists:max([Wi || {{Wi,_},_} <- TextDims]),
    MaxH = lists:max([Hi || {{_,Hi},_} <- TextDims]),
    {Ascent,TextDims,MaxW,MaxH}.    

draw_h_tabs(Win, W, I, Ascent, [{{TxW,TxH},Text}|TextDims],
	    Xi, Yi, Width, Height, Halign, Valign) ->
    %% darken selected field
    if I =:= W#widget.value ->
	    Color = color_sub(?TABS_COLOR, 16#00333333),
	    epx_gc:set_fill_color(Color),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Win#widget.image, Xi, Yi, Width, Height);
       true ->
	    ok
    end,
    epx_gc:set_foreground_color(16#00000000),
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Win#widget.image, Xi, Yi, Width, Height),
    set_font_color(W#widget.font_color),
    draw_text(Win, Ascent, Text, TxW, TxH, Xi, Yi,
	      Width, Height, Halign, Valign),
    draw_h_tabs(Win, W, I+1, Ascent, TextDims,
	      Xi+Width, Yi, Width, Height, Halign, Valign);
draw_h_tabs(_Win, _W, _I, _Ascent, [], 
	  Xi, _Yi, _Width, _Height, _Halign, _Valign) ->
    Xi.

draw_v_tabs(Win, W, I, Ascent, [{{TxW,TxH},Text}|TextDims],
	    Xi, Yi, Width, Height, Halign, Valign) ->
    %% darken selected field
    if I =:= W#widget.value ->
	    Color = color_sub(?TABS_COLOR, 16#00333333),
	    epx_gc:set_fill_color(Color),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Win#widget.image, Xi, Yi, Width, Height);
       true ->
	    ok
    end,
    epx_gc:set_foreground_color(16#00000000),
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Win#widget.image, Xi, Yi, Width, Height),
    set_font_color(W#widget.font_color),
    draw_text(Win, Ascent, Text, TxW, TxH, Xi, Yi,
	      Width, Height, Halign, Valign),
    draw_v_tabs(Win, W, I+1, Ascent, TextDims,
		Xi, Yi+Height, Width, Height, Halign, Valign);
draw_v_tabs(_Win, _W, _I, _Ascent, [], 
	    _Xi, Yi, _Width, _Height, _Halign, _Valign) ->
    Yi.

%% same as draw_v_tabs
draw_items(Win, W, I, Ascent, [{{TxW,TxH},Text}|TextDims],
	  Xi, Yi, Width, Height, Halign, Valign) ->
    if I =:= W#widget.value ->
	    Color = color_sub(?TABS_COLOR, 16#00333333),
	    epx_gc:set_fill_color(Color),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Win#widget.image, Xi, Yi, Width, Height);
       true ->
	    ok
    end,
    epx_gc:set_foreground_color(16#00000000),
    epx_gc:set_fill_style(none),
    if I =:= 1 ->
	    epx_gc:set_border_style([]);
       true ->
	    epx_gc:set_border_style([ntop])
    end,
    epx:draw_rectangle(Win#widget.image, Xi, Yi, Width, Height),
    set_font_color(W#widget.font_color),
    draw_text(Win, Ascent, Text, TxW, TxH, Xi, Yi,
	      Width, Height, Halign, Valign),
    draw_items(Win, W, I+1, Ascent, TextDims,
	      Xi, Yi+Height, Width, Height, Halign, Valign);
draw_items(_Win, _W, _I, _Ascent, [], 
	  _Xi, Yi, _Width, _Height, _Halign, _Valign) ->
    Yi.
    
    
draw_text(Win, Ascent, Text, TxW, TxH, X, Y, Width, Height, Halign, Valign) ->
    Xd = case Halign of
	     left  -> 0;
	     right -> Width - TxW;
	     center -> (Width-TxW) div 2
	 end,
    Yd = case Valign of
	     top -> 0;
	     bottom -> Height - TxH;
	     center -> (Height-TxH) div 2
	 end,
    %% draw aligned text
    X1 = X + Xd,
    Y1 = Y + Yd + Ascent,
    epx:draw_string(Win#widget.image, X1, Y1, Text).

draw_background(Win, X, Y, W=#widget {width=Width, height=Height}) ->
    draw_background(Win, X, Y, Width, Height, W).

draw_background(Win, X, Y, Width, Height, 
		W = #widget {min=Min, max=Max}) ->
    if W#widget.color2 =/= undefined,
       Min =/= undefined, Max =/= undefined ->
	    draw_split_background(Win,X,Y,Width,Height,W);
       W#widget.image2 =/= undefined,
       Min =/= undefined, Max =/= undefined ->
	    draw_split_background(Win,X,Y,Width,Height,W);
       W#widget.animation2 =/= undefined,
       Min =/= undefined, Max =/= undefined ->
	    draw_split_background(Win,X,Y,Width,Height,W);
       true ->
	    #widget {color = Color, image = Image, animation = Anim,
		     frame = Frame } = W,
	    if is_integer(W#widget.shadow_x),
	       is_integer(W#widget.shadow_y) ->
		    case W#widget.state of
			normal ->
			    Xs = W#widget.shadow_x,
			    Ys = W#widget.shadow_y,
			    draw_one_background(Win,W#widget{fill=[blend]},
						X+Xs,Y+Ys,
						Width,Height,
						1, {85,0,0,0},
						Image, Anim, Frame),
%%			    epx:pixmap_filter_area(Win#widget.image,
%%						   Win#widget.image,
%%						   {5,1,<<1,1,1,1,1>>},
%%						   X, Y,
%%						   X, Y, Width, Height),

			    draw_one_background(Win, W, X, Y, Width, Height, 
						1, Color, Image, Anim, Frame);
			active ->
			    Xs = W#widget.shadow_x,
			    Ys = W#widget.shadow_y,
			    Xi = Xs bsr 1,
			    Yi = Ys bsr 1,
			    draw_one_background(Win,W#widget{fill=[blend]},
						X+Xs,Y+Xs,
						Width,Height,
						1, {200,0,0,0},
						Image, Anim, Frame),
			    draw_one_background(Win, W, X+Xi,Y+Yi,
						Width, Height,
						1, Color, Image, Anim, Frame);
			_ ->
			    draw_one_background(Win, W, X, Y, Width, Height, 
						1, Color, Image, Anim, Frame)
		    end;
	       true ->
		    draw_one_background(Win, W, X, Y, Width, Height, 
					1, Color, Image, Anim, Frame)
	    end
    end.

-ifdef(NOT_USED).
draw_split_background(Win,X,Y,W=#widget{width=Width,height=Height}) ->
    draw_split_background(Win,X,Y,Width,Height,W).
-endif.

draw_split_background(Win,X,Y,Width,Height,
		      W=#widget {orientation = horizontal}) ->
    #widget {color = Color, image = Image, 
	     animation = Anim, frame = Frame} = W,
    #widget {color2 = Color2, image2 = Image2, 
	     animation2 = Anim2, frame2 = Frame2} = W,
    R = value_proportion(W),
    draw_one_background(Win, W, X, Y, 
			trunc(R*Width), Height, 
			1, Color, Image, Anim, Frame),
    draw_one_background(Win, W, X + trunc(R*Width), Y,
			Width - trunc(R*Width), Height, 
			2, Color2, Image2, Anim2, Frame2);
draw_split_background(Win,X,Y,Width,Height,
		      W=#widget {orientation = vertical}) ->
    #widget {color = Color, image = Image,
	     animation = Anim, frame = Frame } = W,
    #widget {color2 = Color2, image2 = Image2,
	     animation2 = Anim2, frame2 = Frame2} = W,
    R = value_proportion(W),
    Y0 = Y + Height - 1,
    %% Bottom part
    draw_one_background(Win, W, X, trunc(Y0*(1-R) + Y*R),
			Width, trunc(R*(Y0-Y)) + 1,
			1, Color, Image, Anim, Frame),
    %% Top part
    draw_one_background(Win, W, X, Y,
			Width, trunc((1-R)*(Y0-Y)),
			2, Color2, Image2, Anim2, Frame2).

draw_one_background(Win,W,X,Y,Width,Height,N,Color,Image,Anim,Frame) ->
    %% optionally draw background color
    if Color =:= undefined ->
	    ok;
       true ->
	    %% lager:debug("draw_one_background: color = ~p\n", [Color]),
	    epx_gc:set_fill_style(W#widget.fill),  %% fill, fill2!
	    set_color(W, Color),
	    if W#widget.type =:= button ->
		    Rw = optional(W#widget.round_w, 8),
		    Rh = optional(W#widget.round_h, 8),
		    epx:draw_roundrect(Win#widget.image,X,Y,Width,Height,Rw,Rh);
	       W#widget.round_w =/= undefined; W#widget.round_h =/= undefined ->
		    Rw = optional(W#widget.round_w, 0),
		    Rh = optional(W#widget.round_h, 0),
		    epx:draw_roundrect(Win#widget.image,X,Y,Width,Height,Rw,Rh);
	       true ->
		    epx:draw_rectangle(Win#widget.image,X,Y,Width,Height)
	    end
    end,
    %% optionally draw image possibly with rect as background
    if is_record(Image, epx_pixmap) ->
	    ?dbg("drawing image ~p", [Image]),
	    IWidth  = epx:pixmap_info(Image,width),
	    IHeight = epx:pixmap_info(Image,height),
	    if IWidth =:= Width, IHeight =:= Height ->
		    epx:pixmap_copy_area(Image,
					 Win#widget.image,
					 0, 0, X, Y, Width, Height,
					 [blend]);
	       true ->
		    epx:pixmap_scale_area(Image,
					  Win#widget.image,
					  0, 0, X, Y,
					  IWidth, IHeight,
					  Width, Height,
					  [blend])
	    end;
       true ->
	    ok
    end,
    %% optionally draw animation possibly with image as background
    if is_record(Anim, epx_animation) ->
	    lager:debug("drawing animation ~p", [Anim]),
	    AWidth  = epx:animation_info(Anim,width),
	    AHeight = epx:animation_info(Anim,height),
	    Count = epx:animation_info(Anim, count),
	    Frame0 = if is_number(Frame) -> Frame;
			true -> 0
		     end,
	    Frame1 = clamp(Frame0, 0, Count-1),
	    %% lager:debug("draw frame: ~w", [Frame1]),
	    if AWidth =:= Width, AHeight =:= Height ->
		    epx:animation_draw(Anim, round(Frame1),
				       Win#widget.image, epx_gc:current(),
				       X,Y);
	       true ->
		    AFormat = epx:animation_info(Anim,pixel_format),
		    TmpImage = create_tmp_pixmap(AFormat,AWidth,AHeight),
		    epx:animation_draw(Anim, round(Frame1),
				       TmpImage, epx_gc:current(),
				       0,0),
		    epx:pixmap_scale_area(TmpImage,
					  Win#widget.image,
					  0, 0, X, Y,
					  AWidth, AHeight,
					  Width, Height,
					  [blend])
	    end,
	    %% fixme: animation step?
	    update_animation(W, N, Frame0+1, Count);
       true ->
	    ok
    end.

update_animation(W, 1, Frame, Count) ->
    case W#widget.animate of
	continuous ->
	    put(animations, true),
	    widget_store(W#widget { frame = fmod(Frame,Count)});
	sequence ->
	    if Frame >= Count ->
		    widget_store(W#widget { animate = undefined });
	       true ->
		    put(animations, true),
		    widget_store(W#widget { frame = Frame})
	    end;
	undefined ->
	    W
    end;
update_animation(W, 2, Frame, Count) ->
    case W#widget.animate2 of
	continuous ->
	    put(animations, true),
	    widget_store(W#widget { frame2 = fmod(Frame, Count) });
	sequence ->
	    if Frame >= Count ->
		    widget_store(W#widget { animate2 = undefined });
	       true ->
		    put(animations, true),
		    widget_store(W#widget { frame2 = Frame})
	    end;
	undefined ->
	    W
    end.

optional(undefined, Default) -> Default;
optional(Value, _Default) -> Value.

fmod(A, B) when is_integer(A), is_integer(B), B =/= 0 ->
    A rem B;
fmod(A, B) when is_number(A), is_number(B), B =/= 0 ->
    AB = abs(A / B),
    C = (AB - trunc(AB))*abs(B),
    if A < 0 -> -C;
       true -> C
    end.    
    

%% A bit ugly but may be efficient?
create_tmp_pixmap(Format, Width, Height) ->
    Pixmap =
	case get(tmp_pixmap) of
	    undefined ->
		epx:pixmap_create(Width, Height, Format);
	    Pixmap0 ->
		F = epx:pixmap_info(Pixmap0,pixel_format),
		W  = epx:pixmap_info(Pixmap0,width),
		H = epx:pixmap_info(Pixmap0,height),
		if Format =:= F, Width =< W, Height =< H ->
			Pixmap0;
		   true ->
			epx:pixmap_create(Width, Height, Format)
		end
	end,
    epx:pixmap_fill(Pixmap, 0),
    put(tmp_pixmap, Pixmap),
    Pixmap.

draw_border(_Win,_X,_Y,_W, undefined) ->
    ok;
draw_border(_Win,_X,_Y,_W, 0) ->
    ok;
draw_border(Win,X,Y,W,Border) ->
    %% fixme: calculate size from border thickness
    epx_gc:set_border_color(W#widget.border_color),
    epx_gc:set_border_width(Border),
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Win#widget.image,
		       X, Y,
		       W#widget.width, W#widget.height).

draw_focus(Win,X,Y,W) when W#widget.edit, W#widget.state =:= focus ->
    Bw = optional(W#widget.border, 0) + 2,
    epx_gc:set_border_color(16#009f9f9f),
    epx_gc:set_border_width(2),
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Win#widget.image,
		       X-Bw, Y-Bw,
		       W#widget.width+2*Bw, W#widget.height+2*Bw);
draw_focus(_Win,_X,_Y,_W) ->
    ok.

draw_value_bar(Win,X,Y,W,TopImage) ->
    #widget { min=Min, max=Max, value=Value} = W,
    if is_number(Min),is_number(Max),is_number(Value) ->
	    R = value_proportion(W),
	    if is_record(TopImage, epx_pixmap) ->
		    draw_topimage(Win,X,Y,W,TopImage,R);
	       true ->
		    draw_value_marker(Win,X,Y,W,R)
	    end;
       true ->
	    ok
    end.

value_proportion(W) ->
    #widget { min=Min, max=Max, value=Value} = W,
    Delta = abs(Max - Min),
    if Delta == 0 -> 0.5;
       true  -> (clamp(Value,Min,Max)-min(Min,Max))/Delta
    end.

draw_topimage(Win,Xw,Yw, W, TopImage, R) ->
    lager:debug("drawing topimage ~p, orientation ~p, r ~p", 
		[W#widget.topimage, W#widget.orientation, R]),
    Width = epx:pixmap_info(TopImage,width),
    Height = epx:pixmap_info(TopImage,height),
    {X,Y} = case W#widget.orientation of
		horizontal ->
		    X0 = Xw,
		    X1 = Xw + W#widget.width - 1,
		    Xv = trunc(X0*(1-R) + X1*R),
		    {Xv - (Width div 2),
		     Yw + (W#widget.height- Height) div 2};
		vertical ->
		    Y0 = Yw + W#widget.height - 1,
		    Y1 = Yw,
		    Yv = trunc(Y0*(1-R) + Y1*R),
		    {Xw + (W#widget.width - Width) div 2,
		     (Yv - (Height div 2))}
	    end,
    epx:pixmap_copy_area(TopImage,
			 Win#widget.image,
			 0, 0, X, Y, Width, Height,
			 [blend]).

draw_value_marker(Win,Xw,Yw,W, R) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(16#00000000),
    M = 3,    %% marker width/height
    case W#widget.orientation of
	horizontal ->
	    X = trunc(Xw + R*((W#widget.width-M)-1)),
	    Y =  Yw + 2,
	    epx:draw_rectangle(Win#widget.image,
			       X, Y, M, W#widget.height-4);
	vertical ->
	    X = Xw + 2,
	    Y0 = Yw + W#widget.height - 1,
	    Y1 = Yw,
	    Y = trunc(Y0*(1-R) + Y1*R),
	    epx:draw_rectangle(Win#widget.image,
			       X, Y - (M div 2), W#widget.width-4, M)
	    
    end.

user({M,F,As},Type,Event,Widget,Window,XY) ->
    apply(M,F,[Type,Event,Widget,Window,XY|As]);
user(undefined,_Type,_Event,_Widget,_Window,_XY) ->
    undefined.

parse_color(Color) when is_integer(Color) ->
    Color band 16#ffffffff;
parse_color({R,G,B}) when is_integer(R), is_integer(G), is_integer(B) ->
    (16#ff bsl 24) bor ((R band 16#ff) bsl 16)
	bor ((G band 16#ff) bsl 8) bor (B band 16#ff);
parse_color({A,R,G,B}) when is_integer(A),is_integer(R),
			    is_integer(G), is_integer(B) ->
    ((A band 16#ff) bsl 24) bor ((R band 16#ff) bsl 16)
	bor ((G band 16#ff) bsl 8) bor (B band 16#ff);
parse_color(Name) when is_list(Name); is_atom(Name) ->
    case epx_color:from_name(Name) of
	false ->
	    lager:error("no such color ~s", [Name]),
	    false;
	{R,G,B} ->
	    (16#ff bsl 24)+(R bsl 16)+(G bsl 8)+B
    end.

%% set font color (alpha is masked)
%%set_font_color(Color) when is_atom(Color); is_list(Color) ->
%%    {R,G,B} = epx_color:from_name(Color),
%%    Color1 = (R bsl 16) + (G bsl 8) + B,
%%    epx_gc:set_foreground_color(Color);
set_font_color(Color) ->
    epx_gc:set_foreground_color(Color band 16#ffffff).


%% set foreground / fillcolor also using animatation state
set_color(W, Color0) ->
    Color = case W#widget.state of 
		active ->
		    if is_integer(W#widget.shadow_x),
		       is_integer(W#widget.shadow_y) ->
			    Color0;
		       true ->
			    %% darken color when active
			    color_sub(Color0, 16#00333333)
		    end;
		_ ->
		    Color0
	    end,
    epx_gc:set_foreground_color(Color),
    epx_gc:set_fill_color(Color).

color_add(C1, C2) ->
    <<C3:32>> = color_add_argb(<<C1:32>>, <<C2:32>>),
    C3.

color_sub(C1, C2) ->
    <<C3:32>> = color_sub_argb(<<C1:32>>, <<C2:32>>),
    C3.

color_interpolate(V, C1, C2) ->
    <<C3:32>> = color_interpolate_argb(V, <<C1:32>>, <<C2:32>>),
    C3.

color_add_argb(<<A1,R1,G1,B1>>,<<A2,R2,G2,B2>>) ->
    A = A1 + A2,
    R = R1 + R2,
    G = G1 + G2,
    B = B1 + B2,
    <<(clamp_byte(A)),(clamp_byte(R)),(clamp_byte(G)),(clamp_byte(B))>>.

color_sub_argb(<<A1,R1,G1,B1>>,<<A2,R2,G2,B2>>) ->
    A = A1 - A2,
    R = R1 - R2,
    G = G1 - G2,
    B = B1 - B2,
    <<(clamp_byte(A)),(clamp_byte(R)),(clamp_byte(G)),(clamp_byte(B))>>.

color_interpolate_argb(V, <<A0,R0,G0,B0>>,<<A1,R1,G1,B1>>) 
  when is_float(V), V >= 0.0, V =< 1.0 ->
    A = trunc(A0 + V*(A1-A0)),
    R = trunc(R0 + V*(R1-R0)),
    G = trunc(R1 + V*(G1-G0)),
    B = trunc(B1 + V*(B1-B0)),
    <<(clamp_byte(A)),(clamp_byte(R)),(clamp_byte(G)),(clamp_byte(B))>>.
    
clamp_byte(A) when A > 255 -> 255;
clamp_byte(A) when A < 0  -> 0;
clamp_byte(A) -> A.

%% clamp numbers
clamp(V,Min,Max) when is_number(V),is_number(Min),is_number(Max) ->
    if Min < Max -> min(max(V,Min), Max);
       Min > Max -> max(min(V,Min), Max);
       Min == Max -> Min
    end;
clamp(undefined,Min,Max) ->
    if is_number(Min) -> Min;
       is_number(Max) -> Max;
       true -> undefined
    end;
clamp(V,Min,undefined) when is_number(Min), V < Min -> Min;
clamp(V,undefined,Max) when is_number(Max), V > Max -> Max;
clamp(V,_,_) -> V.

%%
%% Utility to exand environment "variables" in unicode text
%% variables are written as ${var} where var is a encoded atom
%% operating system enviroment is accessed through $(VAR)
%% and application library dir $/app/
%%
text_expand(Text, Env) when is_list(Text) ->
    %% assume unicode character list!
    text_expand_(Text, [], Env);
text_expand(Text, Env) when is_binary(Text) ->
    %% assume utf8 encoded data!
    text_expand_(unicode:characters_to_list(Text), [], Env).

text_expand_([$$,${|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [${,$$], env, Acc, Env);
text_expand_([$$,$(|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [$(,$$], shell, Acc, Env);
text_expand_([$$,$/|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [$/,$$], lib, Acc, Env);
text_expand_([$\\,C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([], Acc, _Env) ->
    lists:reverse(Acc).


text_expand_collect_([$)|Text], Var, _Pre, shell, Acc, Env) ->
    case os:getenv(rev_variable(Var)) of
	false ->
	    text_expand_(Text, Acc, Env);
	Value ->
	    Acc1 = lists:reverse(Value, Acc),
	    text_expand_(Text, Acc1, Env)
    end;
text_expand_collect_([$/|Text], Var, _Pre, lib, Acc, Env) ->
    try erlang:list_to_existing_atom(rev_variable(Var)) of
	App ->
	    case code:lib_dir(App) of
		{error,_} ->
		    text_expand_(Text, Acc, Env);
		Value ->
		    Acc1 = lists:reverse(Value, Acc),
		    text_expand_(Text, Acc1, Env)
	    end
    catch
	error:_ ->
	    text_expand_(Text, Acc, Env)
    end;
text_expand_collect_([$}|Text], Var, _Pre, env, Acc, Env) ->
    try erlang:list_to_existing_atom(rev_variable(Var)) of
	Key ->
	    case lists:keyfind(Key, 1, Env) of
		false ->
		    text_expand_(Text, Acc, Env);
		{_,Val} ->
		    Value = lists:flatten(io_lib:format("~w", [Val])),
		    Acc1 = lists:reverse(Value, Acc),
		    text_expand_(Text, Acc1, Env)
	    end
    catch
	error:_ ->
	    text_expand_(Text, Acc, Env)
    end;
text_expand_collect_([C|Text], Var, Pre, Shell, Acc, Env) ->
    if C >= $a, C =< $z;
       C >= $A, C =< $Z;
       C >= $0, C =< $9;
       C =:= $_; C =:= $@;
       C =:= $\s; C =:= $\t -> %% space and tab allowed in begining and end
	    text_expand_collect_(Text, [C|Var], Pre, Shell, Acc, Env);
       true ->
	    %% char not allowed in variable named
	    text_expand_(Text,  [C | Var ++ Pre ++ Acc], Env)
    end;
text_expand_collect_([], Var, Pre, _Shell, Acc, Env) ->
    text_expand_([],  Var ++ Pre ++ Acc, Env).

rev_variable(Var) ->
    trim_hd(lists:reverse(trim_hd(Var))).

trim_hd([$\s|Cs]) -> trim_hd(Cs);
trim_hd([$\t|Cs]) -> trim_hd(Cs);
trim_hd(Cs) -> Cs.
