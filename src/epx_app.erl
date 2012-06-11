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
%%% File    : epix_app.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : 
%%% Created : 28 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(epx_app).

-behaviour(application).
-export([start/2,stop/1]).

-include("../include/epx.hrl").

%% start
start(_Type, _StartArgs) ->
    Args = application:get_all_env(epx),
    ?epx_debug("Args=~p", [Args]),
    epx_sup:start_link(Args).

%% stop FIXME
stop(_State) ->
  ok.
