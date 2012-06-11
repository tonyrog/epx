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
%%% File    : epx_gc.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EPX GC process wrapper
%%% Created : 24 Oct 2006 by Tony Rogvall <tony@PBook.local>

-module(epx_gc).

-export([create/0, clear/0, copy/1]).
-export([push/0, pop/0, current/0]).
-export([draw/1]).

-export([set_item/2, set_item/3]).
-export([set/1, set/2]).
-export([get_item/1, get_item/2]).
-export([get/1, get/2]).

-export([set_background_color/2, set_background_color/1,
	 set_foreground_color/2, set_foreground_color/1,
	 set_fill_style/2,	 set_fill_style/1,
	 set_fill_color/2,	 set_fill_color/1,
	 set_fill_texture/2,	 set_fill_texture/1,

	 set_line_style/2,	  set_line_style/1,
	 set_line_width/2,	  set_line_width/1,
	 set_line_join_style/2,	  set_line_join_style/1,
	 set_line_cap_style/2,	  set_line_cap_style/1,
	 set_line_texture/2,	  set_line_texture/1,

	 set_border_style/2,	  set_border_style/1,
	 set_border_color/2,	  set_border_color/1,
	 set_border_width/2,	  set_border_width/1,
	 set_border_join_style/2, set_border_join_style/1,
	 set_border_cap_style/2,  set_border_cap_style/1,
	 set_border_texture/2,	  set_border_texture/1,
	 set_font/2,              set_font/1
	]).

-export([get_background_color/1,  get_background_color/0,
	 get_foreground_color/1,  get_foreground_color/0,
	 get_fill_style/1,	  get_fill_style/0,
	 get_fill_color/1,	  get_fill_color/0,
	 get_fill_texture/1,	  get_fill_texture/0,

	 get_line_style/1,	  get_line_style/0,
	 get_line_width/1,	  get_line_width/0,
	 get_line_join_style/1,	  get_line_join_style/0,
	 get_line_cap_style/1,	  get_line_cap_style/0,
	 get_line_texture/1,	  get_line_texture/0,

	 get_border_style/1,	  get_border_style/0,
	 get_border_color/1,	  get_border_color/0,
	 get_border_width/1,	  get_border_width/0,
	 get_border_join_style/1, get_border_join_style/0,
	 get_border_cap_style/1,  get_border_cap_style/0,
	 get_border_texture/1,	  get_border_texture/0,
	 get_font/1,              get_font/0
	]).

-define(CURRENT, '_EPX_GC_CURRENT').
-define(STACK,   '_EPX_GC_STACK').

create() ->
    epx:gc_create().

copy(Gc) ->
    epx:gc_copy(Gc).

draw(Fun) ->
    %%  Should implement copy on write!
    push(),
    %% Do NOT pop inside fun, unless intensional ;-)
    try Fun() of
	Res -> Res
    catch
	error:Reason ->
	    io:format("draw: crashed, ~p\n", [erlang:get_stacktrace()]),
	    {error,Reason};
	exit:Reason ->
	    io:format("draw: exit, reason = ~p\n", [erlang:get_stacktrace()]),
	    {error,Reason};
	thrown:Value ->
	    Value
    after 
	pop()
    end.

current() ->
    case erlang:get(?CURRENT) of
	undefined ->
	    Gc = create(),
	    put(?CURRENT, Gc),
	    put(?STACK, []),
	    Gc;
	Gc -> Gc
    end.

%% clear EPX_GC stack
clear() ->
    erase(?CURRENT),
    erase(?STACK).


push() ->
    Gc = current(),
    GcCopy = copy(Gc),  %% fixme copy on write
    Stack = erlang:get(?STACK),
    put(?STACK, [Gc|Stack]),
    put(?CURRENT, GcCopy),
    ok.

pop() ->
    [Gc|Stack] = erlang:get(?STACK),
    put(?CURRENT, Gc),
    put(?STACK, Stack),
    ok.

%%
%% General colors
%%
set_background_color(Color) -> set_background_color(current(),Color).
set_background_color(Gc,Color) -> set_item(Gc,background_color, Color).

set_foreground_color(Color) -> set_foreground_color(current(),Color).
set_foreground_color(Gc,Color) -> set_item(Gc,foreground_color, Color).

%%
%% FILL style/color/texture
%%
set_fill_style(Style) ->  set_fill_style(current(),Style).
set_fill_style(Gc,Style) -> set_item(Gc, fill_style, Style).

set_fill_color(Color) -> set_fill_color(current(),Color).
set_fill_color(Gc,Color) -> set_item(Gc, fill_color, Color).

set_fill_texture(Texture) -> set_fill_texture(current(),Texture).
set_fill_texture(Gc,Texture) -> set_item(Gc, fill_texture, Texture).

%%
%% BORDER style/color/join/cap/texture
%%
set_border_style(Style) ->  set_border_style(current(), Style).
set_border_style(Gc,Style) -> set_item(Gc, border_style, Style).

set_border_color(Color) -> set_border_color(current(),Color).
set_border_color(Gc,Color) -> set_item(Gc, border_color, Color).

set_border_width(Width) -> set_border_width(current(), Width).
set_border_width(Gc,Width) when is_integer(Width), Width >= 0  ->
    set_item(Gc, border_width, Width).

set_border_join_style(Style) -> set_border_join_style(current(),Style).
set_border_join_style(Gc,Style) -> set_item(Gc, border_join_style, Style).

set_border_cap_style(Style) -> set_border_cap_style(current(),Style).
set_border_cap_style(Gc,Style) -> set_item(Gc, border_cap_style, Style).

set_border_texture(Texture) -> set_border_texture(current(),Texture).
set_border_texture(Gc,Texture) -> set_item(Gc, border_texture, Texture).

%%
%% LINE style/join/cap/texture
%%
set_line_style(Style) ->  set_line_style(current(), Style).
set_line_style(Gc,Style) -> set_item(Gc, line_style, Style).

set_line_width(Width) -> set_line_width(current(), Width).
set_line_width(Gc,Width) when is_integer(Width), Width >= 0 ->
    set_item(Gc, line_width, Width).

set_line_join_style(Style) -> set_line_join_style(current(),Style).
set_line_join_style(Gc,Style) -> set_item(Gc, line_joine_style, Style).

set_line_cap_style(Style) -> set_line_cap_style(current(),Style).
set_line_cap_style(Gc,Style) -> set_item(Gc, line_cap_style, Style).

set_line_texture(Texture) ->  set_line_texture(current(),Texture).
set_line_texture(Gc,Texture) -> set_item(Gc, line_texture, Texture).

set_font(Font) -> set_font(current(), Font).
set_font(Gc,Font) -> set_item(Gc,font,Font).

%% GETTERS
%%
%% General colors
%%
get_background_color() -> get_background_color(current()).
get_background_color(Gc) -> get_item(Gc,background_color).

get_foreground_color() -> get_foreground_color(current()).
get_foreground_color(Gc) -> get_item(Gc,foreground_color).

%%
%% FILL style/color/texture
%%
get_fill_style() ->  get_fill_style(current()).
get_fill_style(Gc) -> get_item(Gc, fill_style).

get_fill_color() -> get_fill_color(current()).
get_fill_color(Gc) -> get_item(Gc, fill_color).

get_fill_texture() -> get_fill_texture(current()).
get_fill_texture(Gc) -> get_item(Gc, fill_texture).

%%
%% BORDER style/color/join/cap/texture
%%
get_border_style() ->  get_border_style(current()).
get_border_style(Gc) -> get_item(Gc, border_style).

get_border_color() -> get_border_color(current()).
get_border_color(Gc) -> get_item(Gc, border_color).

get_border_width() -> get_border_width(current()).
get_border_width(Gc) -> get_item(Gc, border_width).

get_border_join_style() -> get_border_join_style(current()).
get_border_join_style(Gc) -> get_item(Gc, border_join_style).

get_border_cap_style() -> get_border_cap_style(current()).
get_border_cap_style(Gc) -> get_item(Gc, border_cap_style).

get_border_texture() -> get_border_texture(current()).
get_border_texture(Gc) -> get_item(Gc, border_texture).

%%
%% LINE style/join/cap/texture
%%
get_line_style() ->  get_line_style(current()).
get_line_style(Gc) -> get_item(Gc, line_style).

get_line_width() -> get_line_width(current()).
get_line_width(Gc) -> get_item(Gc, line_width).

get_line_join_style() -> get_line_join_style(current()).
get_line_join_style(Gc) -> get_item(Gc, line_joine_style).

get_line_cap_style() -> get_line_cap_style(current()).
get_line_cap_style(Gc) -> get_item(Gc, line_cap_style).

get_line_texture() ->  get_line_texture(current()).
get_line_texture(Gc) -> get_item(Gc, line_texture).

get_font() -> get_font(current()).
get_font(Gc) -> get_item(Gc,font).

%%
%% General item functions
%%
set_item(Item,Value) when is_atom(Item) ->
    set_item(current(), Item, Value).

set_item(Gc, Item, Value) when is_atom(Item) ->
    epx:gc_set(Gc,Item,Value).

set(Items) ->
    set(current(), Items).

set(Gc, [{Item,Value}|Items]) ->
    ok = set_item(Gc, Item, Value),
    set(Gc, Items);
set(_Gc, []) ->
    ok.

get_item(Item) when is_atom(Item) ->
    get_item(current(), Item).

get_item(Gc, Item) when is_atom(Item) ->
    epx:gc_get(Gc, Item).

get(Items) when is_list(Items) ->
    get_item_list(current(), Items, []).
    
get(Gc, Items) when is_list(Items) ->
    get_item_list(Gc, Items, []).

get_item_list(Gc, [Item|Items], Acc) ->
    Value = get_item(Gc, Item),
    get_item_list(Gc, Items, [{Item,Value}|Acc]);
get_item_list(_Gc, [], Acc) ->
    lists:reverse(Acc).
