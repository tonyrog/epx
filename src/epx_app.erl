%%% File    : epix_app.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : 
%%% Created : 28 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(epx_app).

-behaviour(application).
-export([start/2,stop/1]).

%% start
start(_Type, _StartArgs) ->
    {ok,Args} = application:get_env(arguments),
    io:format("epx_app: Args=~p\n", [Args]),
    epx_sup:start_link(Args).

%% stop FIXME
stop(_State) ->
  ok.
