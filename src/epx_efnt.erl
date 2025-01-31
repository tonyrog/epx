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
