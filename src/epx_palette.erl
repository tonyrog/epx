%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Palette Number => RGB, Name => Number
%%% @end
%%% Created : 22 Mar 2019 by Tony Rogvall <tony@rogvall.se>

-module(epx_palette).
-export([color_to_rgb/1, color_from_name/1]).

-undef(COLOR).
-undef(COLOR_LIST).
-define(COLOR(I,Color32,RGB,HSL,Name), (I) => (RGB)).
-include("xterm.hrl").
color_to_rgb_xterm() ->
    #{ ?COLOR_LIST }.

-undef(COLOR).
-undef(COLOR_LIST).
-define(COLOR(I,Color32,RGB,HSL,Name),
	(Name) => (I),
	(??Name) => (I)
).
-include("xterm.hrl").
color_from_name_xterm() ->
    # { ?COLOR_LIST }.

-undef(COLOR).
-undef(COLOR_LIST).
-define(COLOR(I,Name,RGB),
	(Name) => (I),
	(??Name) => (I)
).
-include("logo.hrl").

color_from_name_logo() ->
    # { ?COLOR_LIST }.

-undef(COLOR).
-undef(COLOR_LIST).
-define(COLOR(I,Name,RGB), (I) => (RGB)).
-include("logo.hrl").
color_to_rgb_logo() ->
    #{ ?COLOR_LIST }.


color_from_name(xterm) -> color_from_name_xterm();
color_from_name(logo) ->  color_from_name_logo().

color_to_rgb(xterm) -> color_to_rgb_xterm();
color_to_rgb(logo) ->  color_to_rgb_logo().

    
