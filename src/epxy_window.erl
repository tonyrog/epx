%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2026, Rogvall Invest AB, <tony@rogvall.se>
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
%%%    epxw callbacks for an epxy window. The widgets live in the
%%%    epxw widget tree, signals and close are sent to the epxy server.
%%% @end
%%%-------------------------------------------------------------------

-module(epxy_window).

-behaviour(epxw).

-export([start/3]).
-export([init/1, draw/3, widget/3, close/1, command/3]).

%% Start the window process for window Name, Win is the window widget
start(Server, Name, Win) ->
    #{ x := X, y := Y, width := W, height := H, color := Color } = Win,
    epxw:start(?MODULE, [Server, Name, maps:get(static, Win)],
	       [{title, Name},
		{x, X}, {y, Y},
		{width, max(1,W)}, {height, max(1,H)},
		{screen_color, argb(Color)},
		{fps, maps:get(fps, Win, 30)},
		{bottom_bar, 0}, {scroll_bar_size, 0}, {status_bar, false}]).

argb(undefined) -> {255,255,255,255};
argb(C) when is_integer(C) ->
    {(C bsr 24) band 255, (C bsr 16) band 255, (C bsr 8) band 255, C band 255}.

init([Server, Name, Static]) ->
    {ok, #{ server => Server, name => Name, static => Static }}.

%% the background is filled by epxw, widgets are drawn after this
draw(_Pixels, _Dirty, State) ->
    State.

widget(Widget, Env, State = #{ server := Server, name := Name }) ->
    Server ! {epxy_signal, Name, Widget, Env},
    State.

%% a static window may not be closed
close(State = #{ server := Server, name := Name, static := Static }) ->
    Server ! {epxy_closed, Name, self()},
    if Static -> {noreply, State};
       true -> State
    end.

%% no scrolling or zoom in epxy windows
command(_Symbol, _Mod, State) ->
    {noreply, State}.
