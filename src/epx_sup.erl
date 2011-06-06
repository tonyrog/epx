%%% File    : epx_sup.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : 
%%% Created : 28 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(epx_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    start_link([]).
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init(Args) ->
    io:format("epx_sup:init: Args=~p\n", [Args]),
    Backend = {epx_backend, {epx_backend, start_link, [Args]},
	       permanent, 5000, worker, [epx_backend]},
    Font    = {epx_font, {epx_font, start_link, []},
	       permanent, 5000, worker, [epx_font]},
    {ok,{{one_for_all,0,300}, [Backend,Font]}}.

