%%% @doc Tests for transformed TrueType text (epx:sft_glyph_draw, epx_text)
%%%
%%% Run: erl -pa ebin -pa test -noshell -eval 'eunit:test(epx_text_tests, [verbose]), halt().'
-module(epx_text_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/epx.hrl").

-define(BG, {255,255,255,255}).

setup() ->
    application:ensure_all_started(epx),
    epx_font:sync().

sft(Size) ->
    epx:sft_fileopen(filename:join([code:priv_dir(epx),"truetype","arial.ttf"]),
		     Size).

canvas() ->
    P = epx:pixmap_create(200, 120, argb),
    epx:pixmap_fill(P, ?BG),
    P.

gc() ->
    Gc = epx:gc_create(),
    epx:gc_set(Gc, foreground_color, {0,0,0,0}),
    Gc.

pixels(P) -> epx:pixmap_get_pixels(P, 0, 0, 200, 120).

%% {MinX,MinY,MaxX,MaxY} of the ink
ink(P) ->
    Ps = [{X,Y} || Y <- lists:seq(0,119), X <- lists:seq(0,199),
		   epx:pixmap_get_pixel(P,X,Y) =/= ?BG],
    Xs = [X || {X,_} <- Ps], Ys = [Y || {_,Y} <- Ps],
    {lists:min(Xs), lists:min(Ys), lists:max(Xs), lists:max(Ys)}.

text_test_() ->
    {setup, fun setup/0,
     [?_test(box()),
      ?_test(pen_space()),
      ?_test(global_transform()),
      ?_test(sub_pixel()),
      ?_test(anchor_center()),
      ?_test(path_straight()),
      ?_test(kerning())]}.

%% the rendered glyph image is the glyph box
box() ->
    Sft = sft(40),
    G = epx:sft_glyph(Sft, $g),
    {_X,Y,W,H} = epx:sft_glyph_box(Sft, G),
    Pix = epx:sft_glyph_render(Sft, G, undefined),
    W = epx:pixmap_info(Pix, width),
    H = epx:pixmap_info(Pix, height),
    ?assert(Y < 0),         %% top above the baseline
    ?assert(Y + H > 0),     %% descender below
    {0,0,0,0} = epx:sft_glyph_box(Sft, epx:sft_glyph(Sft, $\s)).

%% identity transform: pen point at the origin; translate puts it
%% anywhere, the same pixels as the untransformed glyph image
pen_space() ->
    Sft = sft(40),
    G = epx:sft_glyph(Sft, $g),
    {Bx,By,W,H} = epx:sft_glyph_box(Sft, G),
    P = canvas(),
    T = epx_t2d:translate(epx_t2d:identity(), 50, 60),
    ok = epx:sft_glyph_draw(Sft, gc(), G, T, P),
    %% the ink is inside the box placed at the pen point (50,60)
    {X0,Y0,X1,Y1} = ink(P),
    ?assert(X0 >= 50+Bx andalso Y0 >= 60+By andalso
	    X1 =< 50+Bx+W-1 andalso Y1 =< 60+By+H-1),
    ?assert(Y0 < 60 andalso Y1 > 60),  %% g crosses the baseline
    %% same as rendering the glyph image at the box position
    Q = canvas(),
    ok = epx:sft_glyph_render_to(Sft, gc(), G, undefined, 50+Bx, 60+By, Q),
    ?assertEqual(pixels(Q), pixels(P)).

%% the pixmap ltm is applied after the glyph transform
global_transform() ->
    Sft = sft(30),
    G = epx:sft_glyph(Sft, $R),
    P = canvas(),
    T = epx_t2d:rotate_rad(epx_t2d:translate(epx_t2d:identity(), 80, 70), 0.5),
    ok = epx:sft_glyph_draw(Sft, gc(), G, T, P),
    Q = canvas(),
    epx:pixmap_ltm_translate(Q, 80, 70),
    epx:pixmap_ltm_rotate(Q, 0.5),
    ok = epx:sft_glyph_draw(Sft, gc(), G, undefined, Q),
    epx:pixmap_ltm_reset(Q),
    ?assertEqual(pixels(P), pixels(Q)).

%% fractional positions are kept (not rounded to whole pixels)
sub_pixel() ->
    Sft = sft(30),
    G = epx:sft_glyph(Sft, $l),
    Draw = fun(X) ->
		   P = canvas(),
		   T = epx_t2d:translate(epx_t2d:identity(), X, 60),
		   ok = epx:sft_glyph_draw(Sft, gc(), G, T, P),
		   pixels(P)
	   end,
    ?assertNotEqual(Draw(50.0), Draw(50.5)),
    %% a tiny move changes the anti aliasing only a little
    MaxDiff = lists:max([abs(A-B) || {A,B} <- lists:zip(
					       binary_to_list(Draw(50.0)),
					       binary_to_list(Draw(50.01)))]),
    ?assert(MaxDiff =< 8).

%% rotating 180 degrees around the center anchor keeps the glyph center
anchor_center() ->
    Sft = sft(40),
    Center = fun(P) -> {X0,Y0,X1,Y1} = ink(P), {(X0+X1)/2, (Y0+Y1)/2} end,
    Draw = fun(A) ->
		   P = canvas(),
		   epx_text:draw_glyph(P, gc(), Sft, $F,
				       [{x,100},{y,60},{anchor,center},
					{rotate,A}]),
		   P
	   end,
    {Xa,Ya} = Center(Draw(0)),
    {Xb,Yb} = Center(Draw(math:pi())),
    ?assert(abs(Xa-100) =< 1), ?assert(abs(Ya-60) =< 1),
    ?assert(abs(Xa-Xb) =< 1), ?assert(abs(Ya-Yb) =< 1).

%% text on a straight path is the same as the string on a baseline
path_straight() ->
    Sft = sft(24),
    Text = "Wavy",
    P = canvas(),
    epx_text:draw_string(P, gc(), Sft, 10, 60, Text),
    Q = canvas(),
    4 = epx_text:draw_path(Q, gc(), Sft, Text,
			   fun(T) -> {10 + T*180, 60} end, [{samples,4}]),
    ?assertEqual(pixels(P), pixels(Q)),
    %% the advance is the end of the pen
    {X,60.0} = epx_text:draw_string(canvas(), gc(), Sft, 10, 60, Text),
    ?assert(abs(X - 10 - epx_text:advance(Sft, Text)) < 1.0e-6).

%% kerning makes AV narrower
kerning() ->
    Sft = sft(40),
    ?assert(epx_text:advance(Sft, "AV") <
	    epx_text:advance(Sft, "AV", [{kerning,false}])).
