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
-export([new/2, delete/1, set/2, get/2, add_callback/3, remove_callback/1]).

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
		       rectangle | ellipse | line | text.

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
	  x = 0   :: integer(),
	  y = 0   :: integer(),
	  z = 0   :: integer(),   %% define the order for overlap
	  width  = 0 :: non_neg_integer(),
	  height = 0 :: non_neg_integer(),
	  text = "",
	  tabs = [],
	  border  :: number(),
	  shadow_x :: number(),
	  shadow_y :: number(),
	  orientation = horizontal :: horizontal|vertical,
	  children_first = true :: boolean(),
	  relative = false :: boolean(),   %% childrens are relative to parent
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
	  backing :: epx:epx_pixmap()
	}).

-record(sub,
	{
	  ref :: reference(),
	  mon :: reference(),
	  id  :: string(),
	  callback :: atom() | function(),
	  signal :: term()
	}).

-record(state, {
	  redraw_timer = undefined,
	  active = [] :: [#widget{}],   %% active widgets pressed
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
	    {reply,{error,enoent},State};
	{ok,W} ->
	    try widget_set(Flags, W) of
		W1 ->
		    widget_store(W1),
		    self() ! refresh,
		    {reply, ok, State}
	    catch
		error:Reason ->
		    {reply, {error,Reason}, State}
	    end
    end;
handle_call({get,ID,Flags},_From,State) ->
    case widget_find(ID) of
	error ->
	    {reply,{error,enoent}, State};
	{ok,W} ->
	    {reply,{ok,widget_get(Flags, W)}, State}
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
			    lager:warn("widget ~p not created ~p\n",
				       [ID, Reason]),
			    {reply, Error, State}
		    end;
		{ok,_W} ->
		    {reply,{error,ealready},State}
	    end;
	{Root,ID1} ->
	    case widget_find(Root) of
		false ->
		    case widget_create(ID,?DEFAULT_WINDOW_ID,Flags,State) of
			{ok,_W} ->
			    {reply, ok, State};
			Error={error,Reason} ->
			    lager:warn("widget ~p not created ~p\n",
				       [ID, Reason]),
			    {reply, Error, State}
		    end;
		{ok,Win} when Win#widget.type =:= window ->
		    case widget_create(ID1,Win#widget.id,Flags,State) of
			{ok,_W} ->
			    {reply, ok, State};
			Error={error,Reason} ->
			    lager:warn("widget ~p not created ~p\n",
				       [ID, Reason]),
			    {reply, Error, State}
		    end;
		{ok,_W} -> %% not under a window, must be under "screen"
		    case widget_create(ID,?DEFAULT_WINDOW_ID,Flags,State) of
			{ok,_W1} ->
			    {reply, ok, State};
			Error={error,Reason} ->
			    lager:warn("widget ~p not created ~p\n",
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
    ?dbg("event: ~p", [Event]),
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
    try widget_set(Flags,W0) of
	W1 ->
	    W2 =
		if W1#widget.type =:= window ->
			window_open(W1);
		   true ->
			W1#widget { window=WID }
		end,
	    Y = if W2#widget.type =:= window ->
			W2#widget.id;
		   true ->
			W2#widget.window++"."++W2#widget.id
		end,
	    tree_db:insert(State#state.wtree,{Y,W2#widget.id}),
	    widget_store(W2),
	    self() ! refresh,
	    {ok,W2}
    catch
	error:Reason ->
	    io:format("widget ~p not created ~p\n",
		      [ID, Reason]),
	    {error,Reason}
    end.

widget_store(W) ->
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

handle_event({key_press,_Sym,_Mod,_Code},_W,State) ->
    {noreply, State};
handle_event({key_release,_Sym,_Mod,_Code},_W,State) ->
    {noreply, State};
handle_event(Event={button_press,Button,Where},Window,State) ->
    case lists:member(left,Button) of
	true ->
	    %% locate an active widget at position (X,Y)
	    WinID = Window#widget.id,
	    lager:debug("Window = ~p\n", [WinID]),
	    case widgets_at_location(Where,{0,0},WinID,State) of
		[] ->
		    {noreply, State};
		Ws ->
		    lager:debug("selected ws=~p", [[Wi#widget.id||{Wi,_}<-Ws]]),
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
	    Ws1 = lists:foldl(
		    fun(Wo={W,_Offs},Ws) ->
			    case widget_member(W, Ws) of
				false -> [Wo|Ws];
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
	    %% locate an active widget at position (X,Y)
	    WinID = Window#widget.id,
	    case widgets_at_location(Where,{0,0},WinID,State) of
		[] ->
		    {noreply, State};
		Ws ->
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
widget_member(W, [{W1,_}|Ws]) ->
    if W#widget.id =:= W1#widget.id -> true;
       true -> widget_member(W, Ws)
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
	    Active = [{W1,XY}|State#state.active],
	    widget_store(W1),
	    self() ! refresh,
	    widgets_event(Ws,Event,Window,State#state { active=Active })
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
	    ?dbg("tab at (~w,~w) = ~w\n", [X,Y,_Tab]),
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
	    [{W,XY}|Acc];
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
widget_event({button_press,_Button,Where},W,XY,Window,State) ->
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
		    ?dbg("tab at (~w,~w) = ~w\n", [X,Y,Value]),
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    W#widget { value = Value }
	    end;
	_ ->
	    {Xi,Yi} = XY,
	    {X,Y,_} = Where,
	    callback_all(W#widget.id,State#state.subs,
			 [{press,1},{x,X-Xi},{y,Y-Yi}]),
	    W#widget { state=selected }
    end;
widget_event({button_release,_Button,Where},W,XY,Window,State) ->
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
	_ ->
	    {Xi,Yi} = XY,
	    {X,Y,_} = Where,
	    callback_all(W#widget.id,State#state.subs,
			 [{press,0},{x,X-Xi},{y,Y-Yi}]),
	    W#widget { state=normal }
    end;
widget_event({motion,_Button,Where},W,XY,_Window,State) ->
    case W#widget.type of
	slider ->
	    case widget_slider_value(W,Where,XY) of
		false -> W;
		Value ->
		    callback_all(W#widget.id,State#state.subs,[{value,Value}]),
		    W#widget { value = Value }
	    end;
	_ ->
	    W
    end;
widget_event(close,W,_XY,_Window,State) ->
    callback_all(W#widget.id,State#state.subs,[{closed,true}]),
    W#widget { state=closed };
widget_event(_Event,W,_XY,_Window,_State) ->
    W.

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
    W = widget_set([{type,window}|Flags], #widget{}),
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

widget_set([Option|Flags], W) ->
    case Option of
	{type,Type} when is_atom(Type) -> 
	    widget_set(Flags, W#widget{type=Type});
	{id,ID} when is_atom(ID); is_list(ID) ->
	    widget_set(Flags, W#widget{id=id_string(ID)});
	{static,Bool} when is_boolean(Bool) -> 
	    widget_set(Flags, W#widget{static=Bool});
	{hidden,Arg} when is_boolean(Arg); Arg =:= none; Arg =:= all -> 
	    widget_set(Flags, W#widget{hidden=Arg});
	{disabled,Arg} when is_boolean(Arg); Arg =:= none; Arg =:= all -> 
	    widget_set(Flags, W#widget{disabled=Arg});
	{x,X} when is_integer(X) ->
	    widget_set(Flags, W#widget{x=X});
	{y,Y} when is_integer(Y) ->
	    widget_set(Flags, W#widget{y=Y});
	{z,Z} when is_integer(Z) ->
	    widget_set(Flags, W#widget{z=Z});
	{shadow_x,X} when is_integer(X) ->
	    widget_set(Flags, W#widget{shadow_x=X});
	{shadow_y,Y} when is_integer(Y) ->
	    widget_set(Flags, W#widget{shadow_y=Y});
	{children_first,Bool} when is_boolean(Bool) ->
	    widget_set(Flags, W#widget{children_first=Bool});
	{relative,Bool} when is_boolean(Bool) ->
	    widget_set(Flags, W#widget{relative=Bool});
	{width,Width} when is_integer(Width), Width>=0 ->
	    widget_set(Flags, W#widget{width=Width});
	{height,Height} when is_integer(Height), Height>=0 ->
	    widget_set(Flags, W#widget{height=Height});
	{text,Text} when is_list(Text) ->
	    widget_set(Flags, W#widget{text=Text});
	{text,Text} when is_atom(Text) ->
	    widget_set(Flags, W#widget{text=atom_to_list(Text)});
	{tabs,Tabs} when is_list(Tabs) ->
	    widget_set(Flags, W#widget{tabs=Tabs});
	{border, Border} when is_integer(Border) ->
	    widget_set(Flags, W#widget{border=Border});
	{orientation, Orientation} when 
	      Orientation =:= horizontal; Orientation =:= vertical ->
	    widget_set(Flags, W#widget{orientation = Orientation });
	{image,File} when is_list(File) ->
	    case epx_image:load(hex:text_expand(File, [])) of
		{ok,Image} ->
		    lager:debug("load image file ~s.",[File]),
		    case Image#epx_image.pixmaps of
			[Pixmap] ->
			    lager:debug("pixmap created ~s.",[File]),
			    widget_set(Flags, W#widget{image=Pixmap});
			_ ->
			    lager:error("no pixmap found in ~s",[File]),
			    widget_set(Flags, W)
		    end;
		Error ->
		    lager:error("unable to load image file ~s:~p",
				[File,Error]),
		    widget_set(Flags, W)
	    end;
	{image,Image} when is_record(Image,epx_pixmap) ->
	    widget_set(Flags, W#widget{image=Image});
	{topimage,File} when is_list(File) ->
	    case epx_image:load(hex:text_expand(File, [])) of
		{ok,Image} ->
		    lager:debug("load image file ~s.",[File]),
		    case Image#epx_image.pixmaps of
			[Pixmap] ->
			    lager:debug("pixmap created ~s.",[File]),
			    widget_set(Flags, W#widget{topimage=Pixmap});
			_ ->
			    lager:error("no pixmap found in ~s",[File]),
			    widget_set(Flags, W)
		    end;
		Error ->
		    lager:error("unable to load image file ~s:~p",
				[File,Error]),
		    widget_set(Flags, W)
	    end;
	{topimage,Image} when is_record(Image,epx_pixmap) ->
	    widget_set(Flags, W#widget{topimage=Image});
	{animation, File} when is_list(File) ->
	    try epx:animation_open(hex:text_expand(File, [])) of 
		Anim ->
		    lager:debug("open animation file ~s.",[File]),
		    widget_set(Flags, W#widget{animation = Anim})	   
	    catch
		error:_Reason ->
		    lager:error("unable to open animation file ~s:~p",
				[File,_Reason]),
		    widget_set(Flags, W)
	    end;	    
	{frame, Frame} when is_integer(Frame) ->
	    widget_set(Flags, W#widget{frame=Frame});
	{frame2, Frame} when is_integer(Frame) ->
	    widget_set(Flags, W#widget{frame2=Frame});
	{animate, Style} when Style =:= continuous; Style =:= sequence ->
	    widget_set(Flags, W#widget{animate=Style});
	{animate2, Style} when Style =:= continuous; Style =:= sequence ->
	    widget_set(Flags, W#widget{animate2=Style});
	{font, Spec} when is_list(Spec) ->
	    case epx_font:match(Spec) of
		false ->
		    lager:error("unable to load font ~p", [Spec]),
		    widget_set(Flags, W);
		{ok,Font} ->
		    widget_set(Flags, W#widget{font=Font})
	    end;
	{font,Font} when is_record(Font,epx_font) ->
	    widget_set(Flags, W#widget{font=Font});
	{color,Color} when is_integer(Color), Color >= 0 ->
	    widget_set(Flags, W#widget{color=Color});
	{color,ColorName} when is_list(ColorName); is_atom(ColorName) ->
	    case epx_color:from_name(ColorName) of
		false ->
		    lager:error("no such color ~s", [ColorName]),
		    widget_set(Flags, W);
		{R,G,B} ->
		    Color = (255 bsl 24)+(R bsl 16)+(G bsl 8)+B,
		    widget_set(Flags, W#widget{color=Color})
	    end;
	{color2,Color} when is_integer(Color), Color >= 0 ->
	    widget_set(Flags, W#widget{color2=Color});
	{color2,ColorName} when is_list(ColorName); is_atom(ColorName) ->
	    case epx_color:from_name(ColorName) of
		false ->
		    lager:error("no such color ~s", [ColorName]),
		    widget_set(Flags, W);
		{R,G,B} ->
		    Color = (255 bsl 24)+(R bsl 16)+(G bsl 8)+B,
		    widget_set(Flags, W#widget{color2=Color})
	    end;
	{font_color,Color} when is_integer(Color), Color>=0 ->
	    widget_set(Flags, W#widget{font_color=Color});
	{font_color,ColorName} when is_list(ColorName); is_atom(ColorName) ->
	    case epx_color:from_name(ColorName) of
		false ->
		    lager:error("no such text color ~s", [ColorName]),
		    widget_set(Flags, W);
		{R,G,B} ->
		    Color = (R bsl 16)+(G bsl 8)+B,
		    widget_set(Flags, W#widget{font_color=Color})
	    end;
	{fill, Style} when is_atom(Style) ->
	    widget_set(Flags, W#widget{fill=Style});
	{events,Es} when is_list(Es) ->
	    widget_set(Flags, W#widget{events=Es});
	{halign,A} when A =:= left;
			A =:= right;
			A =:= center->
	    widget_set(Flags, W#widget{halign=A});
	{valign,A} when A =:= top;
			A =:= bottom;
			A =:= center->
	    widget_set(Flags, W#widget{valign=A});
	{min,Min} when is_number(Min) ->
	    V = clamp(W#widget.value, Min, W#widget.max),
	    widget_set(Flags, W#widget{value=V,min=Min});
	{max,Max} when is_number(Max) ->
	    V = clamp(W#widget.value, W#widget.min, Max),
	    widget_set(Flags, W#widget{value=V,max=Max});
	{value,V} when is_number(V) ->
	    V1 = clamp(V, W#widget.min, W#widget.max),
	    widget_set(Flags, W#widget{value=V1});
	{vscale,V} when is_number(V) ->
	    widget_set(Flags, W#widget{vscale=V});
	{format,F} when is_list(F) ->
	    widget_set(Flags, W#widget{format=F});
	{state, active} when W#widget.state =/= active ->
	    widget_set(Flags, W#widget{state=active, value=1});
	{state, normal} when W#widget.state =/= normal ->
	    widget_set(Flags, W#widget{state=normal, value=0});
	{state, _S} ->
	    lager:debug("state ~s already set ~p", [_S,W#widget.id]),
	    widget_set(Flags, W);
	_ ->
	    lager:debug("option ignored ~p", [Option]),
	    widget_set(Flags, W)
    end;
widget_set([], W) ->
    W.

widget_get(Flags, W) ->
    widget_get(Flags, W, []).

widget_get([Flag|Flags], W, Acc) ->
    case Flag of
	type -> widget_get(Flags, W, [{type,W#widget.type}|Acc]);
	id -> widget_get(Flags, W, [{type,W#widget.id}|Acc]);
	window -> widget_get(Flags, W, [{window,W#widget.window}|Acc]);
	static -> widget_get(Flags, W, [{static,W#widget.static}|Acc]);
	hidden -> widget_get(Flags, W, [{hidden,W#widget.hidden}|Acc]);
	disabled -> widget_get(Flags, W, [{disabled,W#widget.disabled}|Acc]);
	x -> widget_get(Flags, W, [{x,W#widget.x}|Acc]);
	y -> widget_get(Flags, W, [{y,W#widget.y}|Acc]);
	z -> widget_get(Flags, W, [{y,W#widget.z}|Acc]);
	shadow_x -> widget_get(Flags, W, [{shadow_x,W#widget.shadow_x}|Acc]);
	shadow_y -> widget_get(Flags, W, [{shadow_y,W#widget.shadow_y}|Acc]);
	relative -> widget_get(Flags, W, [{relative,W#widget.relative}|Acc]);
	width -> widget_get(Flags, W, [{width,W#widget.width}|Acc]);
	height -> widget_get(Flags, W, [{height,W#widget.height}|Acc]);
	text -> widget_get(Flags, W, [{text,W#widget.text}|Acc]);
	tabs -> widget_get(Flags, W, [{tabs,W#widget.tabs}|Acc]);
	border -> widget_get(Flags, W, [{border,W#widget.border}|Acc]);
	orientation -> widget_get(Flags, W, [{orientation,W#widget.orientation}|Acc]);
	image -> widget_get(Flags, W, [{image,W#widget.image}|Acc]);
	topimage -> widget_get(Flags, W, [{topimage,W#widget.topimage}|Acc]);
	animation -> widget_get(Flags, W, [{animation,W#widget.animation}|Acc]);
	frame -> widget_get(Flags, W, [{frame,W#widget.frame}|Acc]);
	frame2 -> widget_get(Flags, W, [{frame2,W#widget.frame2}|Acc]);
	animate -> widget_get(Flags, W, [{animate,W#widget.animate}|Acc]);
	animate2 -> widget_get(Flags, W, [{animate2,W#widget.animate2}|Acc]);
	font -> widget_get(Flags, W, [{font,W#widget.font}|Acc]);
	color -> widget_get(Flags, W, [{color,W#widget.color}|Acc]);
	color2 -> widget_get(Flags, W, [{color2,W#widget.color2}|Acc]);
	font_color -> widget_get(Flags, W, [{font_color,W#widget.font_color}|Acc]);
	fill -> widget_get(Flags, W, [{fill,W#widget.fill}|Acc]);
	events -> widget_get(Flags, W, [{event,W#widget.events}|Acc]);
	halign -> widget_get(Flags, W, [{halign,W#widget.halign}|Acc]);
	valign -> widget_get(Flags, W, [{valign,W#widget.valign}|Acc]);
	min -> widget_get(Flags, W, [{min,W#widget.min}|Acc]);
	max -> widget_get(Flags, W, [{max,W#widget.max}|Acc]);
	value -> widget_get(Flags, W, [{value,W#widget.value}|Acc]);
	vscale -> widget_get(Flags, W, [{vscale,W#widget.value}|Acc]);
	format -> widget_get(Flags, W, [{format,W#widget.format}|Acc]);
	state -> widget_get(Flags, W, [{state,W#widget.state}|Acc]);
	_ -> widget_get(Flags, W, Acc)
    end;
widget_get([], _W, Acc) ->
    lists:reverse(Acc).

id_string(X) when is_atom(X) ->
    atom_to_list(X);
id_string(X) when is_list(X) ->
    X.

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
    epx:pixmap_copy_to(Win#widget.image, Win#widget.backing),
    epx:pixmap_draw(Win#widget.backing, 
		    Win#widget.win, 0, 0, 0, 0, 
		    Win#widget.width, 
		    Win#widget.height).

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
	    %% io:format("draw_one: ~p\n", [Wid]),
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
draw_widget(W, Win, {X,Y}, _State) ->
    case W#widget.type of
	window ->
	    %% do not draw (yet), we may use this
	    %% to draw multiple/embedded windows in the future
	    ok;

	panel ->
	    epx_gc:draw(
	      fun() ->
		      %% draw_background(Win, W),
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

tabs_item_box(W) ->
    Font = W#widget.font,
    epx_gc:set_font(Font),
    Ascent = epx:font_info(Font, ascent),
    Tabs = W#widget.tabs,
    TextDims = [ {epx_font:dimension(epx_gc:current(), Text),Text} || 
		   Text <- Tabs ],
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
	    case W#widget.type of
		button ->
		    epx:draw_roundrect(Win#widget.image,X,Y,Width,Height,8,8);
		_ ->
		    epx:draw_rectangle(Win#widget.image,X,Y,Width,Height)
	    end
    end,
    %% optionally draw image
    if is_record(Image, epx_pixmap) ->
	    lager:debug("drawing image ~p", [Image]),
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
    %% optionally draw animation
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
draw_border(Win,X,Y,W,_Border) ->
    %% fixme: calculate size from border thickness
    epx_gc:set_foreground_color(16#00000000),
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Win#widget.image,
		       X, Y,
		       W#widget.width, W#widget.height).

draw_value_bar(Win,X,Y,W,TopImage) ->
    #widget { min=Min, max=Max, value=Value} = W,
    if is_number(Min),is_number(Max),is_number(Value) ->
	    R = value_proportion(W),
	    if is_record(TopImage, epx_pixmap) ->
		    draw_topimage(Win,X,Y,W, TopImage, R);
	       true ->
		    draw_value_marker(Win,X,Y, W, R)
	    end;
		    
       true ->
	    ok
		
    end.

value_proportion(W) ->
    #widget { min=Min, max=Max, value=Value} = W,
    Delta = abs(Max - Min),
    if Min < Max ->
		V = if Value < Min -> Min;
		       Value > Max -> Max;
		       true -> Value
		    end,
		(V - Min)/Delta;
	   Min > Max -> %% reversed axis
		V = if Value > Min -> Min;
		       Value < Max -> Max;
		       true -> Value
		    end,
		(V - Max)/Delta;
	   true ->
		0.5
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
