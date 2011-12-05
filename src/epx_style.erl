%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
%%% @doc
%%%    EPX style server
%%% @end
%%% Created : 15 Oct 2011 by Tony Rogvall <tony@rogvall.se>

-module(epx_style).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0]).
-export([new/1, add/2, delete/1, get/1, set/3]).
-export([i/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/epx.hrl").

-define(SERVER, epx_style_srv).
-define(TABLE,  epx_style_table).

-record(state,
	{
	  tab        %% table of {atom(),#epx_gc{}}
	 }).

%%====================================================================
%% API
%%====================================================================

new(Name) ->
    gen_server:call(?SERVER, {new, Name}).

add(Name,Gc) ->
    gen_server:call(?SERVER, {add, Name, Gc}).

delete(Name) ->
    gen_server:call(?SERVER, {delete, Name}).

get(Name) ->
    gen_server:call(?SERVER, {get,Name}).

set(Name,Attr,Value) ->
    gen_server:call(?SERVER, {set,Name,Attr,Value}).


i() ->
    io:format("~10s ~10s ~10s\n",
	      ["Name", "BGColor", "FGColor"]),
    i(ets:first(?TABLE)).

i(Key) ->
    case ets:lookup(?TABLE, Key) of
	[] ->
	    ok;
	[{_Key,Gc}] ->
	    io:format("~10s ~10w ~10w\n",
		      [ Key,
			epx:gc_info(Gc, background_color),
			epx:gc_info(Gc, foreground_color)]),
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
    %% always search in driectory we started from and current directory
    {ok, #state{ tab=Tab } }.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get,Name}, _From, State) when is_atom(Name) ->
    case ets:lookup(State#state.tab, Name) of
	[{_,Gc}] ->
	    {reply,Gc,State};
	[] ->
	    {reply,undefined,State}
    end;
handle_call({set,Name,Attr,Value}, _From, State) when is_atom(Name), 
						      is_atom(Attr) ->
    case ets:lookup(State#state.tab, Name) of
	[{_,Gc}] ->
	    try epx:gc_set(Gc,Attr,Value) of
		_ ->
		    {reply,ok,State}
	    catch
		error:Err ->
		    {reply,{error,Err},State}
	    end;
	[] ->
	    {reply,{error,enoent},State}
    end;
handle_call({new,Name}, _From, State) when is_atom(Name) ->
    case ets:lookup(State#state.tab, Name) of
	[] ->
	    Gc = epx:gc_create(),
	    ets:insert(State#state.tab, {Name,Gc}),
	    {reply, ok, State};
	[{_,_}] ->
	    {reply, ok, State}
    end;
handle_call({add,Name,Gc}, _From, State) when is_atom(Name),
					      is_record(Gc,epx_gc) ->
    case ets:lookup(State#state.tab, Name) of
	[] ->
	    ets:insert(State#state.tab, {Name,Gc}),
	    {reply, ok, State};
	[_] ->
	    {reply,{error,ealready},State}
    end;
handle_call({delete,Name}, _From, State) when is_atom(Name) ->
    ets:delete(State#state.tab, Name),
    {reply, ok, State};    
handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

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
