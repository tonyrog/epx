%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
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
%%%---- END COPYRIGHT ----------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    EPX animation server
%%% @end
%%% Created : 15 Oct 2011 by Tony Rogvall <tony@rogvall.se>

-module(epx_animation).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0]).
-export([add_path/1, open/1, close/1]).
-export([i/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/epx.hrl").
-include_lib("kernel/include/file.hrl").

-define(SERVER, epx_animation_srv).
-define(TABLE,  epx_animation_table).

-record(state,
	{
	  paths=[],
	  tab        %% table of {Name,Animation}
	 }).

%%====================================================================
%% API
%%====================================================================

add_path(Path) ->
    gen_server:call(?SERVER, {add_path,Path}).

open(Name) ->
    gen_server:call(?SERVER, {open, Name}).
close(Name) ->
    gen_server:call(?SERVER, {close, Name}).
    

i() ->
    io:format("~30s ~6s ~6s ~6s ~15s\n",
	      ["Name", "Width", "Height", "Count", "Format"]),
    i(ets:first(?TABLE)).

i(Key) ->
    case ets:lookup(?TABLE, Key) of
	[] ->
	    ok;
	[{_Key,Anim}] ->	   
	    io:format("~30s ~6w ~6w ~6w ~15s\n",
		      [ Key,
			epx:animation_info(Anim, width),
			epx:animation_info(Anim, height),
			epx:animation_info(Anim, count),
			epx:animation_info(Anim, pixel_format)]),
	    i(ets:next(?TABLE, Key))
    end.

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Tab = ets:new(?TABLE, [named_table,set,public]),
    {ok,Cwd} = file:get_cwd(),
    %% always search in driectory we started from and current directory
    {ok, #state{ paths=[Cwd,"."], tab=Tab }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({open,Name}, _From, State) ->
    {reply, load_animation(Name, State), State};
handle_call({close,Name}, _From, State) ->
    ets:delete(?TABLE, Name),
    {reply, ok, State};
handle_call({add_path,Path}, _From, State) ->
    case lists:member(Path, State#state.paths) of
	true -> 
	    {reply, ok, State};
	false ->
	    case file:read_file_info(Path) of
		Err={error,_} ->
		    {reply, Err, State};
		{ok,Info} ->
		    if Info#file_info.type == directory ->
			    Ps = State#state.paths,
			    {reply, ok, State#state { paths=[Path|Ps]}};
		       true ->
			    {reply, {error,enotdir}, State}
		    end
	    end
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

load_animation(Name, State) ->
    case ets:lookup(State#state.tab, Name) of
	[{_,Anim}] ->
	    {ok,Anim};
	[] ->
	    case try_load_animation(Name, State#state.paths) of
		{ok,Anim} ->
		    ets:insert(State#state.tab, {Name,Anim}),
		    {ok,Anim};
		Error ->
		    Error
	    end
    end.

try_load_animation(_Name, []) ->
    {error, enoent};
try_load_animation(Name, [Path|Paths]) ->
    FileName = filename:join(Path,Name),
    io:format("try open ~s\n", [FileName]),
    case file:read_file_info(FileName) of
	{error,_} ->
	    try_load_animation(Name, Paths);
	{ok,Info} ->
	    if Info#file_info.type == regular ->
		    
		    try epx:animation_open(FileName) of
			Anim ->
			    {ok,Anim}
		    catch
			error:_ ->
			    try_load_animation(Name, Paths)
		    end;
	       true ->
		    try_load_animation(Name, Paths)
	    end
    end.
