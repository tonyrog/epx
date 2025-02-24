%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    Produce efnt from ttf (true type file)
%%% @end
%%% Created : 22 Oct 2023 by Tony Rogvall <tony@rogvall.se>

-module(epx_efnt).

-include("../include/epx.hrl").

-export([from_ttf/2]).
-compile(export_all).

test_efnt(Filename) ->
    Font = epx:font_open(Filename),
    Info = epx:font_info(Font),
    io:format("info for file [~s] = ~p\n", [Filename, Info]),    
    ok = epx:font_load(Font),
    test_efnt_(Font, Info, "0123456789ABCDEF").

test_efnt_(_Font, _Info, []) ->
    ok;
test_efnt_(Font, Info, [C|Cs]) ->
    try epx:glyph_info(Font, C) of
	GInfo ->
	    io:format("glyph: ~w: ~p\n", [C, GInfo]),
	    test_efnt_(Font, Info, Cs)
    catch
	error:_ ->
	    test_efnt_(Font, Info, Cs)
    end.    


%% Create efnt file from ttf file using point size Size
from_ttf(Filename, Size) ->
    Sft = epx:sft_fileopen(Filename, Size),
    Info = epx:sft_info(Sft),
    io:format("info for file [~s] = ~p\n", [Filename, Info]),
    from_sft_(Sft, Info, "0123456789ABCDEF").

from_sft_(_Sft, _Info, []) ->
    ok;
from_sft_(Sft, Info, [C|Cs]) ->
    try epx:sft_glyph_info(Sft, C) of
	GM ->
	    GInfo = [{width,trunc(GM#gmetrics.min_width)},
		     {height,trunc(GM#gmetrics.min_height)},
		     {x, trunc(GM#gmetrics.left_side_bearing)}, %% ???
		     {y, trunc(GM#gmetrics.yoffset)},
		     {dx, trunc(GM#gmetrics.advance_width)},
		     {dy, 0}
		    ],
	    io:format("glyph: ~w: ~p\n", [C, GInfo]),
	    from_sft_(Sft, Info, Cs)
    catch
	error:_ ->
	    from_sft_(Sft, Info, Cs)
    end.

rotate(Fontname) ->
    rotate(Fontname, $g, 20, 45).

rotate(Fontname, Char, Size, Angle) ->
    Sft = epx:sft_open(Fontname, Size),
    Info = epx:sft_info(Sft),
    Ascent = proplists:get_value(ascent, Info),
    Gid = epx:sft_glyph(Sft, Char),
    io:format("info for file [~s] = ~p\n", [Fontname, Info]),
    GM = epx:sft_glyph_info(Sft, Gid),
    GI = [{min_width,GM#gmetrics.min_width},
	  {min_height,GM#gmetrics.min_height},
	  {left_side_bearing,GM#gmetrics.left_side_bearing},
	  {yoffset,GM#gmetrics.yoffset},
	  {advance_width,GM#gmetrics.advance_width}],
    X = 0,
    Y = 0,
    H = proplists:get_value(min_height, GI),
    W = proplists:get_value(min_width, GI),
    Dx = proplists:get_value(advance_width, GI),
    Dy = 0,

    io:format("glyph: ~c: gid=~w, ~p\n", [Char, Gid, GI]),
    T0 = epx_t2d:identity(),
    T1 = epx_t2d:rotate(T0, Angle),
    T2 = epx_t2d:translate(T1, 8, 8),

    Trans = T2,
    Pts0 = [{X,Y},{X+W,Y},{X+W,Y+H}, {X,Y+H}],
    Pts = epx_t2d:points(Trans, Pts0),
    MinX = lists:min([Xi || {Xi,_Y} <- Pts]),
    MinY = lists:min([Yi || {_X,Yi} <- Pts]),
    Pts1 = [{X+abs(MinX),Y+abs(MinY)} || {X,Y} <- Pts],
    WW = lists:max([Xi || {Xi,_Y} <- Pts1]),
    HH = lists:max([Yi || {_X,Yi} <- Pts1]),
    %%
    A0 = [{0,Ascent},{W+Dx,Ascent+Dy}],
    A1 = epx_t2d:points(Trans, A0),

    io:format("pts0 = ~p\n", [Pts0]),
    io:format("pts = ~p\n", [Pts]),
    io:format("pts1 = ~p\n", [Pts1]),
    io:format("W'=~w H'=~w\n", [WW,HH]),

    epx_gc:set_foreground_color({0,0,255,0}),
    %% epx_gc:set_item(fader_value, 0.5),
    Pixmap = epx:pixmap_create(WW, HH, argb),
    epx:pixmap_fill(Pixmap, {0,0,0,0}),
    ok = epx:sft_glyph_render_to(Sft, epx_gc:current(), 
				 Gid, Trans, 0, 0, Pixmap),
    ascii_pixels(Pixmap).

ascii_pixels(Pixmap) ->
    [{width,W}, {height,H}] = epx:pixmap_info(Pixmap, [width,height]),
    lists:foreach(
      fun(Y) ->
	      lists:foreach(
		fun(X) ->
			case epx:pixmap_get_pixels(Pixmap, X, Y, 1, 1) of
			    <<0,0,0,0>> ->
				io:put_chars("-");
			    <<_,0,_,0>> ->
				io:put_chars("X");
			    _Pixel ->
				io:format("~w", [_Pixel])
				%% io:put_chars(" ")
			end
		end, lists:seq(0, W-1)),
	      io:put_chars("\n")
      end, lists:seq(0, H-1)).
	      
    

