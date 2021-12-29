%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%     epx (color) profile utils
%%% @end
%%% Created :  3 Apr 2019 by Tony Rogvall <tony@rogvall.se>

-module(epx_profile).
-export([color/2]).
-export([color_number/2]).

color(_Scheme, RGB) when is_tuple(RGB) -> 
    RGB;
color(Scheme, Index) when is_integer(Index) ->
    Map = epx_palette:color_to_rgb(Scheme),
    maps:get(Index, Map);
color(Scheme, Name) when is_atom(Name) ->
    Map = epx_palette:color_from_name(Scheme),
    color(Scheme, maps:get(Name, Map));
color(Scheme, Name) when is_list(Name) ->
    Map = epx_palette:color_from_name(Scheme),
    color(Scheme, maps:get(string:lowercase(Name), Map)).

color_number(_Scheme, Index) when is_integer(Index) -> Index;
color_number(_Scheme, RGB) when is_tuple(RGB) -> RGB;
color_number(Scheme, Name) when is_atom(Name) ->
    Map = epx_palette:color_from_name(Scheme),
    maps:get(Name, Map);
color_number(Scheme, Name) when is_list(Name) ->
    Map = epx_palette:color_from_name(Scheme),
    maps:get(string:lowercase(Name), Map).
