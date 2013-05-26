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

-include("../include/epx.hrl").
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
    Debug = proplists:get_bool(debug, Args),
    put(debug, Debug),
    if Debug ->
	    epx:debug(debug);
       true ->
	    ok
    end,
    ?epx_debug("starting ~p", [Args]),
    Backend = {epx_backend, {epx_backend, start_link, [Args]},
	       permanent, 5000, worker, [epx_backend_srv]},
    Font    = {epx_font, {epx_font, start_link, []},
	       permanent, 5000, worker, [epx_font_srv]},
    Anim    = {epx_animation, {epx_animation, start_link, []},
	       permanent, 5000, worker, [epx_animation_srv]},
    Style   = {epx_style, {epx_style, start_link, []},
	       permanent, 5000, worker, [epx_style_srv]},
    {ok,{{one_for_all,0,300}, [Backend,Font,Anim,Style]}}.

