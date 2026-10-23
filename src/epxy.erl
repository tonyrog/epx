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
%%%
%%%    Each epxy window is an epxw process (epxy_window) and the
%%%    widgets live in the widget tree of that window. This server
%%%    maps names to windows and keeps the signal subscriptions.
%%%
%%%    Names: "w" is the window w, "w.btn" is widget "btn" in window
%%%    w. A name that does not start with a window name, "btn" or
%%%    "panel.btn", is a widget in the default window "screen", and
%%%    "screen.btn" is the same as "btn".
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------

-module(epxy).

-behaviour(gen_server).

%% API
-export([start/1, start_link/1, stop/0]).
-export([new/2, delete/1, set/2, get/2, inject/2]).
-export([add_callback/3, remove_callback/1]).

%% User API - maybe called from user type callbacks
-export([widget_set/2, widget_get/2]).

%% Utils
-export([color_add/2, color_interpolate/3, color_add_argb/2,
	 color_interpolate_argb/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("epx/include/epx.hrl").

-define(SERVER, ?MODULE).

-define(DEFAULT_WIDTH,  320).
-define(DEFAULT_HEIGHT, 240).
-define(DEFAULT_WINDOW_ID, "screen").

-define(warning(F,A), io:format((F),(A))).

-record(sub,
	{
	  ref :: reference(),
	  id  :: string(),
	  callback :: atom() | function(),
	  signal :: term()
	}).

-record(win,
	{
	  pid :: pid(),
	  mon :: reference(),
	  window :: epx:epx_window(),     %% the epx window (for inject)
	  widget :: epxy_widget:widget()  %% the window properties
	}).

-record(state, {
	  fps = 30.0 :: number(),     %% animation frames per second
	  windows = #{} :: #{ string() => #win{} },
	  subs = [] :: [#sub{}]
	 }).

new(ID,Flags) ->
    gen_server:call(?SERVER, {new,to_id(ID),Flags}).

delete(ID) ->
    gen_server:call(?SERVER, {delete,to_id(ID)}).

set(ID, Flags) ->
    gen_server:call(?SERVER, {set,to_id(ID),Flags}).

get(ID, Flags) ->
    gen_server:call(?SERVER, {get,to_id(ID),Flags}).

add_callback(ID, Signal, Cb) ->
    gen_server:call(?SERVER, {add_callback, self(), to_id(ID), Signal, Cb}).

remove_callback(Ref) ->
    gen_server:call(?SERVER, {remove_callback, Ref}).

inject(WinID,Event) ->
    gen_server:cast(?SERVER, {inject, to_id(WinID), Event}).

stop() ->
    gen_server:call(?SERVER, stop).

to_id(ID) when is_atom(ID) -> atom_to_list(ID);
to_id(ID) when is_list(ID) -> ID.

%% for user widgets
widget_set(W, Props) -> epxy_widget:set(W, Props).
widget_get(W, Keys) -> epxy_widget:get(W, Keys).

color_add(C1, C2) -> epxy_widget:color_add(C1, C2).
color_interpolate(V, C1, C2) -> epxy_widget:color_interpolate(V, C1, C2).
color_add_argb(C1, C2) -> epxy_widget:color_add_argb(C1, C2).
color_interpolate_argb(V, C1, C2) ->
    epxy_widget:color_interpolate_argb(V, C1, C2).

%%%===================================================================
%%% API
%%%===================================================================

start(Options) ->
    application:ensure_all_started(epx),
    gen_server:start({local, ?SERVER}, ?MODULE, Options, []).

start_link(Options) ->
    application:ensure_all_started(epx),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
    State0 = #state{ fps = Fps },
    case window_create(?DEFAULT_WINDOW_ID,
		       [{static,true},
			{x,50},{y,50},
			{width,Width},{height,Height},
			{color, 16#ffffffff}], State0) of
	{ok, State1} -> {ok, State1};
	{error, Reason} -> {stop, Reason}
    end.

handle_call({new,ID,Flags}, _From, State) ->
    IsWindow = proplists:get_value(type, Flags) =:= window,
    case resolve(ID, State) of
	{window, _} ->
	    {reply, {error,ealready}, State};
	_ when IsWindow ->
	    case window_create(ID, Flags, State) of
		{ok, State1} -> {reply, ok, State1};
		Error -> {reply, Error, State}
	    end;
	{WinName, Name} ->
	    FullID = full_id(WinName, Name),
	    Props = [{id,FullID},{window,WinName}|Flags],
	    Reply = win_call(WinName, State,
			     fun() -> epxw:widget_new(Name, Props) end),
	    case Reply of
		ok -> ok;
		{error,ealready} -> ok;
		{error,Reason} ->
		    ?warning("widget ~p not created ~p\n", [ID, Reason])
	    end,
	    {reply, Reply, State}
    end;
handle_call({set,ID,Flags}, _From, State) ->
    case resolve(ID, State) of
	{window, WinName} ->
	    {reply, ok, window_set(WinName, Flags, State)};
	{WinName, Name} ->
	    {reply, win_call(WinName, State,
			     fun() -> epxw:widget_set(Name, Flags) end),
	     State}
    end;
handle_call({get,ID,Keys},_From,State) ->
    case resolve(ID, State) of
	{window, WinName} ->
	    #win{ widget = W } = maps:get(WinName, State#state.windows),
	    {reply, epxy_widget:get(W, Keys), State};
	{WinName, Name} ->
	    case win_call(WinName, State,
			  fun() -> epxw:widget_get(Name, Keys) end) of
		{error,_} -> {reply, [], State};
		Reply -> {reply, Reply, State}
	    end
    end;
handle_call({delete,ID}, _From, State) ->
    case resolve(ID, State) of
	{window, WinName} ->
	    #win{ widget = W } = maps:get(WinName, State#state.windows),
	    case maps:get(static, W) of
		true -> %% delete the widgets but keep the window
		    win_call(WinName, State,
			     fun() ->
				     [epxw:widget_delete(N) ||
					 N <- epxw:widgets()],
				     ok
			     end),
		    {reply, ok, State};
		false ->
		    {reply, ok, window_delete(WinName, State)}
	    end;
	{WinName, Name} ->
	    {reply, win_call(WinName, State,
			     fun() -> epxw:widget_delete(Name) end),
	     State}
    end;
handle_call({add_callback,Pid,ID,Signal,Cb}, _From, State) ->
    Ref = erlang:monitor(process, Pid),
    Sub = #sub{id=canonical_id(ID),ref=Ref,signal=Signal,callback=Cb},
    {reply, {ok,Ref}, State#state { subs=[Sub|State#state.subs]}};
handle_call({remove_callback,Ref}, _From, State) ->
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {reply, {error, not_found}, State};
	{value, Sub, Subs} ->
	    erlang:demonitor(Sub#sub.ref, [flush]),
	    {reply, ok, State#state { subs=Subs} }
    end;
handle_call(stop,_From,State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

handle_cast({inject, WinID, Event}, State) ->
    case maps:find(WinID, State#state.windows) of
	{ok, #win{ pid = Pid, window = Window }} ->
	    Pid ! #epx_event{ win = Window, data = Event };
	error ->
	    ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({epxy_signal, WinName, Name, Env}, State) ->
    callback_all(full_id(WinName, Name), State#state.subs, Env),
    {noreply, State};
handle_info({epxy_closed, WinName, _Pid}, State) ->
    case maps:find(WinName, State#state.windows) of
	{ok, #win{ widget = #{ static := false }, mon = Mon }} ->
	    erlang:demonitor(Mon, [flush]),
	    callback_all(WinName, State#state.subs,
			 #{event=>closed, xy=>{0,0}}),
	    Ws = maps:remove(WinName, State#state.windows),
	    {noreply, State#state { windows = Ws }};
	_ ->
	    {noreply, State}
    end;
handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	{value, _Sub, Subs} ->
	    {noreply, State#state { subs=Subs} };
	false ->
	    %% a window process died
	    Ws = maps:filter(fun(_, #win{ mon = M }) -> M =/= Ref end,
			     State#state.windows),
	    {noreply, State#state { windows = Ws }}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    maps:foreach(fun(_Name, #win{ pid = Pid }) -> stop_window(Pid) end,
		 State#state.windows),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% "screen.id" names the same widget as "id"
canonical_id(?DEFAULT_WINDOW_ID++"."++ID) when ID =/= [] -> ID;
canonical_id(ID) -> ID.

%% full (external) id of widget Name in window WinName
full_id(?DEFAULT_WINDOW_ID, Name) -> Name;
full_id(WinName, Name) -> WinName ++ "." ++ Name.

%% find the window of an id: {window,WinName} | {WinName,WidgetName}
resolve(ID, #state { windows = Ws }) ->
    case maps:is_key(ID, Ws) of
	true -> {window, ID};
	false ->
	    case string:split(ID, ".") of
		[Root, Name] when Name =/= "" ->
		    case maps:is_key(Root, Ws) of
			true -> {Root, Name};
			false -> {?DEFAULT_WINDOW_ID, ID}
		    end;
		_ ->
		    {?DEFAULT_WINDOW_ID, ID}
	    end
    end.

%% run Fun inside the epxw process of window WinName
win_call(WinName, State, Fun) ->
    case maps:find(WinName, State#state.windows) of
	{ok, #win{ pid = Pid }} ->
	    try epxw:call(Pid, fun(S) -> {Fun(), S} end)
	    catch
		exit:Reason -> {error, Reason}
	    end;
	error ->
	    {error, enoent}
    end.

window_create(Name, Flags, State) ->
    try epxy_widget:new([{id,Name},{type,window}|Flags]) of
	W0 ->
	    W = W0#{ fps => State#state.fps },
	    case epxy_window:start(self(), Name, W) of
		{ok, Pid} ->
		    Mon = erlang:monitor(process, Pid),
		    Window = epxw:call(Pid, fun(S) -> {epxw:window(), S} end),
		    Win = #win{ pid = Pid, mon = Mon, window = Window,
				widget = maps:remove(fps, W) },
		    Ws = (State#state.windows)#{ Name => Win },
		    {ok, State#state { windows = Ws }};
		{error, Reason} ->
		    {error, Reason}
	    end
    catch
	error:Reason ->
	    ?warning("window ~p not created ~p\n", [Name, Reason]),
	    {error, Reason}
    end.

%% window properties, only the color is applied to the open window
window_set(WinName, Flags, State) ->
    Win = #win{ widget = W } = maps:get(WinName, State#state.windows),
    W1 = epxy_widget:set(W, Flags),
    case maps:get(color, W1) =/= maps:get(color, W) of
	true ->
	    Color = argb(maps:get(color, W1)),
	    win_call(WinName, State,
		     fun() ->
			     epxw:profile_set(screen_color, Color),
			     epxw:invalidate()
		     end);
	false ->
	    ok
    end,
    Ws = (State#state.windows)#{ WinName => Win#win{ widget = W1 }},
    State#state { windows = Ws }.

argb(C) when is_integer(C) ->
    {(C bsr 24) band 255, (C bsr 16) band 255, (C bsr 8) band 255, C band 255}.

window_delete(WinName, State) ->
    #win{ pid = Pid, mon = Mon } = maps:get(WinName, State#state.windows),
    erlang:demonitor(Mon, [flush]),
    stop_window(Pid),
    State#state { windows = maps:remove(WinName, State#state.windows) }.

stop_window(Pid) ->
    try gen_server:stop(Pid, normal, 2000)
    catch
	exit:_ -> exit(Pid, kill)
    end.

callback_all(Wid, Subs, Env) when is_map(Env) ->
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
