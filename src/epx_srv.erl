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
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Keep track on EPX backend
%%% @end
%%% Created : 27 Oct 2010 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(epx_srv).

-behaviour(gen_server).

%% API
-export([start/0]).
-export([start_link/0]).
-export([backend/0, backend/1, backend_create/2, backend_set_default/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER,  ?MODULE).
-define(EPX_REG, epx_reg).

-record(state,
	{
	  next_id = 1
	}).

%%%===================================================================
%%% API
%%%===================================================================

%% get the default backend
backend() ->
    backend(default).

backend(ID) when is_integer(ID) ->
    ets:lookup_element(?EPX_REG, ID, 2).

%% create a new backend - return backend {ok,ID}
%% or {error,Reason} when bad arg
backend_create(Name, Opts) ->
    gen_server:call(?SERVER, {backend_create,Name,Opts}).

%% Setup the default backend
backend_set_default(ID) ->
    gen_server:call(?SERVER, {backet_set_default, ID}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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

init([]) ->
    ets:new(?EPX_REG, [set, named_table, protected]),
    %% maybe create the default backend here?
    {ok, #state{}}.

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
handle_call({backend_create,Name,Opts}, _From, State) ->
    try epx:backend_open(Name, epx:dict_from_list(Opts)) of
	Backend ->
	    ID = State#state.next_id,
	    ets:insert(?EPX_REG, {ID, Backend}),
	    if ID =:= 1 ->
		    ets:insert(?EPX_REG, {default, Backend});
	       true ->
		    ok
	    end,
	    {reply, {ok,ID}, State#state { next_id = ID+1 }}
    catch
	error:_ ->
	    {reply, {error,einval}, State}
    end;
handle_call({backend_set_default,ID}, _From, State) ->
    case ets:lookup(?EPX_REG, ID) of
	[] ->
	    {reply, {error,einval}, State};
	[{_,Backend}] ->
	    ets:insert(?EPX_REG, {default, Backend}),
	    {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error,badcall}, State}.

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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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
