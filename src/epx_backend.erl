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
-module(epx_backend).

-behaviour(gen_server).

%% API
-export([start/0]).
-export([start_link/0, start_link/1]).
-export([default/0, backend/1, create/2, set_default/1]).
-export([assumed_backend/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).


-define(SERVER,  epx_backend_srv).
-define(TABLE,   epx_backend_table).

-include("../include/epx.hrl").

-record(state,
	{
	  next_id = 1
	}).

%%%===================================================================
%%% API
%%%===================================================================


%% get the default backend
default() ->
    backend(default).

backend(ID) ->
    ets:lookup_element(?TABLE, ID, 2).

%% create a new backend - return backend {ok,ID}
%% or {error,Reason} when bad arg
create(Name, Opts) ->
    gen_server:call(?SERVER, {create,Name,Opts}).

%% Setup the default backend
set_default(ID) ->
    gen_server:call(?SERVER, {set_default, ID}).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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

init(Args) ->
    put(debug, proplists:get_bool(debug, Args)),
    ?epx_debug("starting ~p", [Args]),
    ets:new(?TABLE, [set, named_table, protected]),
    Dict = epx:dict_from_list(Args),
    BeName = try epx:dict_get(Dict, backend) of
		 Be -> Be
	      catch
		  error:_ ->
		      assumed_backend()
	      end,
    if BeName =:= "none" ->
	    ?epx_info("warning no backend selected", []);
       true ->
	    ok
    end,
    try epx:backend_open(BeName, epx:dict_from_list(Args)) of
	Backend ->
	    ets:insert(?TABLE, {1, Backend}),
	    ets:insert(?TABLE, {default, Backend}),
	    {ok, #state { next_id = 2 }}
    catch
	error:Error ->
	    ?epx_info("backend_open no default backend, failed ~p", [Error]),
	    {ok, #state {}}
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

handle_call({create,Name,Opts}, _From, State) ->
    try epx:backend_open(Name, epx:dict_from_list(Opts)) of
	Backend ->
	    ID = State#state.next_id,
	    ets:insert(?TABLE, {ID, Backend}),
	    if ID =:= 1 ->
		    ets:insert(?TABLE, {default, Backend});
	       true ->
		    ok
	    end,
	    {reply, {ok,ID}, State#state { next_id = ID+1 }}
    catch
	error:Error ->
	    ?epx_warning("backed create failed ~p", [Error]),
	    {reply, {error,einval}, State}
    end;
handle_call({set_default,ID}, _From, State) ->
    case ets:lookup(?TABLE, ID) of
	[] ->
	    {reply, {error,einval}, State};
	[{_,Backend}] ->
	    ets:insert(?TABLE, {default, Backend}),
	    {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error,badcall}, State}.

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
handle_cast(_Request, State) ->
    {noreply, State}.

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

handle_info(_Info, State) ->
    {noreply, State}.

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
terminate(_Reason, _State) ->
    ok.

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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% select 
assumed_backend() ->
    case os:getenv("EPX_BACKEND") of
	false ->
	    case os:type() of
		{unix,darwin} ->
		    case is_backend("macos") of
			true -> "macos";
			false ->
			    case os:getenv("DISPLAY") of
				false ->
				    "none";
				_ ->
				    case is_backend("x11") of
					true -> 
					    %% fixme check if X11 is running
					    "x11";
					false -> "none"
				    end
			    end
		    end;
		{unix,linux} -> 
		    case os:getenv("DISPLAY") of
			false ->
			    case is_backend("fb") of
				true -> "fb";
				false -> "none"
			    end;
			_ ->
			    case is_backend("x11") of
				true ->
				    %% fixme check if X11 is running
				    "x11";
				false -> 
				    "none"
			    end
		    end;
		{unix,_} -> 
		    case is_backend("x11") of
			true -> "x11";
			false -> "none"
		    end;
		_ -> 
		    "none"
	    end;
	Backend ->
	    case is_backend(Backend) of
		true -> Backend;
		false -> "none"
	    end
    end.

is_backend(Name) ->
    lists:member(Name, epx:backend_list()).
