%%% @doc Tests for generated (ttf -> efnt) fonts and the font cache
%%%
%%% Run: erl -pa ebin -pa test -noshell -eval 'eunit:test(epx_font_cache_tests, [verbose]), halt().'
-module(epx_font_cache_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/epx.hrl").

ttf(Name) ->
    filename:join([code:priv_dir(epx), "truetype", Name]).

tmp(Name) ->
    filename:join(os:getenv("TMPDIR", "/tmp"),
		  "epx_font_cache_test_" ++ os:getpid() ++ "_" ++ Name).

%% restart the font server with a fresh cache directory
setup() ->
    application:ensure_all_started(epx),
    Dir = tmp("cache"),
    os:cmd("rm -rf " ++ Dir),
    application:set_env(epx, font_cache_dir, Dir),
    ok = supervisor:terminate_child(epx_sup, epx_font),
    {ok,_} = supervisor:restart_child(epx_sup, epx_font),
    epx_font:sync(),
    Dir.

cleanup(Dir) ->
    os:cmd("rm -rf " ++ Dir),
    application:unset_env(epx, font_cache_dir),
    ok = supervisor:terminate_child(epx_sup, epx_font),
    {ok,_} = supervisor:restart_child(epx_sup, epx_font).

font_cache_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Dir) ->
	     [?_test(efnt_format()),
	      ?_test(glyphs_like_sft()),
	      ?_test(metrics_like_handmade()),
	      ?_test(match_generates(Dir)),
	      ?_test(bad_glyphs())]
     end}.

%% the generated file loads and has the right info and baseline
efnt_format() ->
    File = tmp("arial20.efnt"),
    try
	ok = epx_efnt:write_ttf(ttf("arial.ttf"), 20, File),
	F = epx:font_open(File),
	ok = epx:font_load(F),
	"Arial" = epx:font_info(F, family_name),
	medium = epx:font_info(F, weight),
	roman = epx:font_info(F, slant),
	200 = epx:font_info(F, point_size),
	75 = epx:font_info(F, resolution_y),
	Asc = epx:font_info(F, ascent),
	Desc = epx:font_info(F, descent),
	?assert(Asc > 15), ?assert(Desc > 2),
	%% 'A' sits on the baseline, 'g' goes below it
	{Atop,Abot} = ink_rows(F, "A"),
	{_,Gbot} = ink_rows(F, "g"),
	Base = 50,
	?assert(Atop < Base), Abot = Base - 1,
	?assert(Gbot > Base)
    after
	file:delete(File)
    end.

%% first and last row with ink when drawing Text at baseline 50
ink_rows(Font, Text) ->
    P = epx:pixmap_create(100, 80, argb),
    epx:pixmap_fill(P, {255,255,255,255}),
    Gc = epx:gc_copy(epx:gc_default()),
    epx:gc_set(Gc, font, Font),
    epx:gc_set(Gc, foreground_color, {0,0,0,0}),
    epx:font_draw_string(P, Gc, 10, 50, Text),
    Rows = [Y || Y <- lists:seq(0,79),
		 lists:any(fun(X) -> epx:pixmap_get_pixel(P,X,Y) =/=
					 {255,255,255,255} end,
			   lists:seq(0,99))],
    {hd(Rows), lists:last(Rows)}.

%% the glyph pixels are the pixels libschrift renders
glyphs_like_sft() ->
    Size = 30,
    Data = epx_efnt:from_ttf(ttf("arial.ttf"), Size),
    File = tmp("arial30.efnt"),
    ok = file:write_file(File, Data),
    try
	F = epx:font_open(File),
	ok = epx:font_load(F),
	Sft = epx:sft_fileopen(ttf("arial.ttf"), Size*75/72),
	lists:foreach(
	  fun(C) ->
		  Gid = epx:sft_glyph(Sft, C),
		  {Bx,By,W,H} = epx:sft_glyph_box(Sft, Gid),
		  Ref = epx:pixmap_get_pixels(
			  epx:sft_glyph_render(Sft, Gid, undefined), 0, 0, W, H),
		  %% draw the efnt glyph as alpha on black
		  P = epx:pixmap_create(W+40, H+40, argb),
		  epx:pixmap_fill(P, {255,0,0,0}),
		  Gc = epx:gc_copy(epx:gc_default()),
		  epx:gc_set(Gc, font, F),
		  epx:gc_set(Gc, foreground_color, {0,255,255,255}),
		  X0 = 20 - Bx,
		  Y0 = 20 - By,
		  epx:font_draw_string(P, Gc, X0, Y0, [C]),
		  Got = << <<G>> || Y <- lists:seq(20, 20+H-1),
				    X <- lists:seq(20, 20+W-1),
				    {_,_,G,_} <- [epx:pixmap_get_pixel(P,X,Y)] >>,
		  MaxDiff = lists:max([abs(A-B) || {A,B} <-
		      lists:zip(binary_to_list(Ref), binary_to_list(Got))]),
		  ?assert(MaxDiff =< 2)
	  end, "AgW@(")
    after
	file:delete(File)
    end.

%% the generated Arial 12 has the metrics of the hand made arial12.efnt
metrics_like_handmade() ->
    File = tmp("arial12.efnt"),
    ok = epx_efnt:write_ttf(ttf("arial.ttf"), 12, File),
    try
	Gen = epx:font_open(File), ok = epx:font_load(Gen),
	Ref = epx:font_open(filename:join([code:priv_dir(epx),"fonts",
					   "arial12.efnt"])),
	ok = epx:font_load(Ref),
	[A,D] = [epx:font_info(Gen,K) || K <- [ascent,descent]],
	[A,D] = [epx:font_info(Ref,K) || K <- [ascent,descent]],
	Text = "The quick brown fox jumps over the lazy dog",
	{Wg,_} = epx_font:dimension(Gen, Text),
	{Wr,_} = epx_font:dimension(Ref, Text),
	%% the hand made fonts use hinted advance widths (wider)
	?assert(abs(Wg - Wr) / Wr < 0.08)
    after
	file:delete(File)
    end.

%% match generates a font once, in the cache dir
match_generates(Dir) ->
    Spec = [{name,"Arial"},{size,17},{weight,bold}],
    Self = self(),
    [spawn(fun() -> Self ! {font, epx_font:match(Spec)} end) ||
	_ <- lists:seq(1,8)],
    Fs = [receive {font, R} -> R after 10000 -> timeout end ||
	     _ <- lists:seq(1,8)],
    [{ok,F}] = lists:usort(Fs),
    bold = epx:font_info(F, weight),
    170 = epx:font_info(F, point_size),
    [_] = filelib:wildcard(filename:join(Dir, "Arial-Bold-17-75-*.efnt")),
    %% an existing efnt in the exact size is used
    {ok,F12} = epx_font:match([{name,"Arial"},{size,12},{slant,roman},
			       {weight,medium},{resolution,75}]),
    "fonts" = filename:basename(filename:dirname(
				  epx:font_info(F12, file_name))),
    false = epx_font:match([{name,"No Such Font"},{size,12}]),
    true = length(epx_font:scalable()) >= 4.

%% glyphs libschrift can not read are left out
bad_glyphs() ->
    Data = epx_efnt:from_ttf(ttf("tgl.0-17-alt.ttf"), 25),
    <<"EFNT", _/binary>> = Data.
