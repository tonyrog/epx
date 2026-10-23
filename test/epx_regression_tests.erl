%%% @doc Regression tests for the defects listed in epx_bugs.md
%%%
%%% Run: erl -pa ebin -pa test -noshell -eval 'eunit:test(epx_regression_tests, [verbose]), halt().'
%%%
%%% Many of the original defects crashed the VM, so a failing test
%%% may show up as a dead emulator rather than an eunit failure.
-module(epx_regression_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/epx_image.hrl").

-define(RED,   {255,255,0,0}).
-define(BLUE,  {255,0,0,255}).
-define(BLACK, {255,0,0,0}).

%% pixel helpers
px(P, X, Y) -> epx:pixmap_get_pixel(P, X, Y).

new(W, H) -> new(W, H, argb).
new(W, H, Fmt) ->
    P = epx:pixmap_create(W, H, Fmt),
    epx:pixmap_fill(P, ?BLACK),
    P.

%% Set of {X,Y} where pixel is not black
painted(P) ->
    W = epx:pixmap_info(P, width),
    H = epx:pixmap_info(P, height),
    [{X,Y} || Y <- lists:seq(0,H-1), X <- lists:seq(0,W-1),
	      px(P,X,Y) =/= ?BLACK].

rows(P) -> lists:usort([Y || {_,Y} <- painted(P)]).
cols(P) -> lists:usort([X || {X,_} <- painted(P)]).

solid_gc() ->
    Gc = epx:gc_create(),
    epx:gc_set(Gc, fill_style, solid),
    epx:gc_set(Gc, fill_color, ?RED),
    epx:gc_set(Gc, foreground_color, ?RED),
    epx:gc_set(Gc, border_color, ?BLUE),
    Gc.

%% 1. sub pixmap has transform and bytes_per_pixel
sub_pixmap_test() ->
    P = new(4, 4),
    S1 = epx:pixmap_sub_pixmap(P, -1, -1, 2, 2),
    ?BLACK = px(S1, 0, 0),
    S2 = epx:pixmap_sub_pixmap(P, 1, 1, 2, 2),
    ok = epx:pixmap_put_pixel(S2, 0, 0, ?RED),
    ?RED = px(P, 1, 1),
    ?assertEqual(4, epx:pixmap_info(S2, bytes_per_pixel)).

%% 2. horizontal scroll by more than the width, and rotate between two
scroll_horizontal_test() ->
    [begin
	 P = new(4, 3),
	 epx:pixmap_put_pixel(P, 0, 0, ?RED),
	 ok = epx:pixmap_scroll(P, P, A, 0, false, ?BLUE),
	 ?BLUE = px(P, 0, 0)
     end || A <- [4, 5, 6, 100, -4, -5, -100]],
    S = new(4, 3),
    D = new(4, 3),
    epx:pixmap_put_pixel(S, 0, 0, ?RED),
    ok = epx:pixmap_scroll(S, D, 1, 0, true, ?BLUE),
    ?RED = px(D, 1, 0),
    ok = epx:pixmap_scroll(S, D, -1, 0, true, ?BLUE),
    ?RED = px(D, 3, 0),
    ok = epx:pixmap_scroll(S, D, 9, 0, true, ?BLUE),
    ?RED = px(D, 1, 0).

%% 3. vertical scroll into a smaller target, in place rotate with padding
scroll_vertical_test() ->
    S = new(4, 100),
    D = new(4, 2),
    epx:pixmap_put_pixel(S, 0, 1, ?RED),
    ok = epx:pixmap_scroll(S, D, 0, 1, false, ?BLUE),
    ?RED = px(D, 0, 0),
    ok = epx:pixmap_scroll(S, D, 0, -1, false, ?BLUE),
    ?BLUE = px(D, 0, 0),
    P = new(3, 100),
    epx:pixmap_put_pixel(P, 2, 0, ?RED),
    ok = epx:pixmap_scroll(P, P, 0, -50, true, ?BLUE),
    ?RED = px(P, 2, 50),
    ok = epx:pixmap_scroll(P, P, 0, 50, true, ?BLUE),
    ?RED = px(P, 2, 0).

%% 4. short data and formats without unpack
put_pixels_test() ->
    P = new(2, 2),
    ?assertError(badarg, epx:pixmap_put_pixels(P,0,0,2,2,argb,
					       <<0:(15*8)>>,[])),
    ?assertError(badarg, epx:pixmap_put_pixels(P,0,0,2,2,argb,
					       <<0:(12*8)>>,[])),
    ok = epx:pixmap_put_pixels(P,0,0,2,2,argb,
			       <<255,255,0,0, 0:(12*8)>>,[]),
    ?RED = px(P, 0, 0),
    [?assertError(badarg, epx:pixmap_put_pixels(P,0,0,1,1,F,<<0:64>>,[])) ||
	F <- [r16, a16, l16, r4g4b4a4, r3g3b2, r2g3b2, r10g10b10a2]].

%% 5. get all pixels binary refers to the resource
get_pixels_all_test() ->
    Bs = [begin
	      P = new(2, 2),
	      epx:pixmap_get_pixels(P, 0, 0, 0, 0)
	  end || _ <- lists:seq(1, 100)],
    erlang:garbage_collect(),
    [?assert(byte_size(B) >= 16) || B <- Bs].

%% 6. copy area with source cut by its clip moves the target too
copy_area_clip_test() ->
    S = new(3, 1),
    T = new(3, 1),
    epx:pixmap_put_pixel(S, 0, 0, ?RED),
    epx:pixmap_copy_area(S, T, -1, 0, 0, 0, 3, 1),
    ?BLACK = px(T, 0, 0),
    ?RED = px(T, 1, 0).

%% 7. huge pixmaps are refused or allocated fully (not 15 bytes)
huge_pixmap_test() ->
    try epx:pixmap_create(65536, 65536, argb) of
	P ->
	    epx:pixmap_put_pixel(P, 0, 100, ?RED),
	    ?RED = px(P, 0, 100)
    catch
	error:_ -> ok
    end.

%% 8. right edge past 2^31-1 does not wrap
fill_area_edge_test() ->
    P = new(3, 1),
    epx:pixmap_fill_area(P, 1, 0, 2147483647, 1, ?RED),
    ?BLACK = px(P, 0, 0),
    ?RED = px(P, 1, 0),
    ?RED = px(P, 2, 0).

%% 9. thick horizontal/vertical lines stay inside the clip and are centered
thick_line_clip_test() ->
    P = new(4, 2),
    Gc = solid_gc(),
    epx:gc_set(Gc, line_width, 9),
    ok = epx:pixmap_draw_line(P, Gc, 0, 1, 3, 1),
    ok = epx:pixmap_draw_line(P, Gc, 3, 0, 3, 1),
    Q = new(4, 4),
    epx:gc_set(Gc, line_width, 3),
    ok = epx:pixmap_draw_line(Q, Gc, 0, -100, 3, -100),
    [] = painted(Q).

%% 10. a thick diagonal line is centered on itself
thick_line_center_test() ->
    P = new(20, 20),
    Gc = solid_gc(),
    epx:gc_set(Gc, line_width, 5),
    ok = epx:pixmap_draw_line(P, Gc, 2, 4, 15, 9),
    Rs = rows(P),
    ?assert(lists:min(Rs) >= 1),
    ?assert(lists:max(Rs) =< 12),
    P2 = new(10, 10),
    epx:gc_set(Gc, line_width, 3),
    ok = epx:pixmap_draw_line(P2, Gc, 3, 3, 4, 4),
    ?RED = px(P2, 3, 3),
    ?RED = px(P2, 4, 4),
    ?assert(lists:min(rows(P2)) >= 2).

%% 11. rectangle border placement
rectangle_border_test() ->
    P = new(20, 20),
    Gc = solid_gc(),
    epx:gc_set(Gc, border_width, 1),
    ok = epx:pixmap_draw_rectangle(P, Gc, 6, 5, 6, 4),
    %% default (outside): fill at 6..11 x 5..8, border around
    ?RED  = px(P, 6, 5),
    ?RED  = px(P, 11, 8),
    ?BLUE = px(P, 5, 4),
    ?BLUE = px(P, 12, 9),
    ?BLACK = px(P, 13, 10),
    Q = new(5, 5),
    epx:gc_set(Gc, border_width, 2),
    epx:gc_set(Gc, border_style, [solid, inside]),
    ok = epx:pixmap_draw_rectangle(Q, Gc, 2, 2, 1, 1),
    [{2,2}] = painted(Q),
    [solid, inside] = epx:gc_get(Gc, border_style),
    epx:gc_set(Gc, border_style, [solid, outside]),
    [solid, outside] = epx:gc_get(Gc, border_style).

%% 12. ellipse ring closed, each pixel blended once, big radius ends
ellipse_ring_test() ->
    P = new(21, 21),
    Gc = solid_gc(),
    epx:gc_set(Gc, fill_style, none),
    epx:gc_set(Gc, border_width, 1),
    epx:gc_set(Gc, border_style, [blend]),
    epx:gc_set(Gc, border_color, {128,0,0,255}),
    ok = epx:pixmap_draw_ellipse(P, Gc, 0, 0, 20, 20),
    Bs = lists:usort([element(4, px(P,X,Y)) || {X,Y} <- painted(P)]),
    ?assertEqual(1, length(Bs)),
    %% every row from 0..20 has border pixels
    ?assertEqual(lists:seq(0,20), rows(P)),
    Big = new(10, 10),
    epx:gc_set(Gc, border_style, [solid]),
    ok = epx:pixmap_draw_ellipse(Big, Gc, 0, 0, 131072, 131072).

%% 13. rounded rectangle symmetric and inside its rectangle
roundrect_test() ->
    P = new(30, 30),
    Gc = solid_gc(),
    ok = epx:pixmap_draw_roundrect(P, Gc, 5, 5, 16, 12, 4, 4),
    Ps = painted(P),
    ?assertEqual(lists:seq(5,20), cols(P)),
    ?assertEqual(lists:seq(5,16), rows(P)),
    %% symmetric top/bottom and left/right
    ?assertEqual(lists:sort(Ps), lists:sort([{X, 21-Y} || {X,Y} <- Ps])),
    ?assertEqual(lists:sort(Ps), lists:sort([{25-X, Y} || {X,Y} <- Ps])),
    Q = new(30, 30),
    ok = epx:pixmap_draw_roundrect(Q, Gc, 5, 5, 10, 10, 100, 100),
    ?assertEqual(lists:seq(5,14), cols(Q)),
    R = new(10, 10),
    epx:gc_set(Gc, border_width, 1),
    ok = epx:pixmap_draw_roundrect(R, Gc, 0, 0, 16777216, 10, 4194304, 2).

%% 14. polygon fill in fill color, empty polygon, bad points
poly_test() ->
    P = new(18, 14),
    Gc = solid_gc(),
    ok = epx:pixmap_draw_poly(P, Gc, [{3,2},{14,2},{14,10},{3,10}]),
    Cs = lists:usort([px(P,X,Y) || {X,Y} <- painted(P)]),
    ?assertEqual([?RED], Cs),
    ?assertEqual(lists:seq(2,10), rows(P)),
    D = new(18, 14),
    ok = epx:pixmap_draw_poly(D, Gc, [{8,1},{14,6},{8,12},{2,6}]),
    ?assertEqual([?RED], lists:usort([px(D,X,Y) || {X,Y} <- painted(D)])),
    ?BLACK = px(D, 0, 6),
    ?RED = px(D, 8, 6),
    ok = epx:pixmap_draw_poly(P, Gc, []),
    epx:gc_set(Gc, fill_style, none),
    ok = epx:pixmap_draw_poly(P, Gc, []),
    ?assertError(badarg, epx:pixmap_draw_poly(P, Gc, [{1}])),
    ?assertError(badarg, epx:pixmap_draw_poly(P, Gc, [{1,2,3},{4,5}])).

%% 15. style none
style_none_test() ->
    Gc = solid_gc(),
    ok = epx:gc_set(Gc, border_style, none),
    ok = epx:gc_set(Gc, line_style, none),
    P = new(5, 5),
    ok = epx:pixmap_draw_point(P, Gc, 1, 1),
    ok = epx:pixmap_draw_line(P, Gc, 0, 0, 4, 3),
    [] = painted(P),
    epx:gc_set(Gc, line_style, []),
    ok = epx:pixmap_draw_line(P, Gc, 0, 0, 4, 3),
    [] = painted(P).

%% png reader: rows were passed to put_pixels with bytes per row as
%% width, palette, 16 bit and 1,2,4 bit gray formats were not converted
png_formats_test() ->
    Tmp = filename:join(os:getenv("TMPDIR", "/tmp"),
			"epx_png_test_"++os:getpid()++".png"),
    try
	%% 3x2 2-bit gray, 2 bits padding at end of each row
	ok = file:write_file(Tmp, png(3, 2, 2, 0, [], [<<2#00011011>>, <<2#11100100>>])),
	{ok, G} = epx_image:load(Tmp),
	[Pg] = G#epx_image.pixmaps,
	[{255,0,0,0},{255,85,85,85},{255,170,170,170}] =
	    [epx:pixmap_get_pixel(Pg, X, 0) || X <- [0,1,2]],
	{255,255,255,255} = epx:pixmap_get_pixel(Pg, 0, 1),
	%% 4x1 4-bit palette with transparency
	Plte = <<255,0,0, 0,255,0, 0,0,255>>,
	ok = file:write_file(Tmp, png(4, 1, 4, 3, [{"PLTE",Plte},{"tRNS",<<128>>}],
				      [<<16#01, 16#2F>>])),
	{ok, I} = epx_image:load(Tmp),
	[Pi] = I#epx_image.pixmaps,
	rgba = epx:pixmap_info(Pi, pixel_format),
	[{128,255,0,0},{255,0,255,0},{255,0,0,255},{0,0,0,0}] =
	    [epx:pixmap_get_pixel(Pi, X, 0) || X <- [0,1,2,3]],
	%% 2x1 16-bit rgb
	ok = file:write_file(Tmp, png(2, 1, 16, 2, [],
				      [<<1,2,3,4,5,6, 255,255,0,0,128,0>>])),
	{ok, R} = epx_image:load(Tmp),
	[Pr] = R#epx_image.pixmaps,
	[{255,1,3,5},{255,255,0,128}] =
	    [epx:pixmap_get_pixel(Pr, X, 0) || X <- [0,1]]
    after
	file:delete(Tmp)
    end.

png(W, H, Depth, ColorType, Chunks, Rows) ->
    Chunk = fun(T, D) ->
		    TD = <<(list_to_binary(T))/binary, D/binary>>,
		    <<(byte_size(D)):32, TD/binary, (erlang:crc32(TD)):32>>
	    end,
    Data = zlib:compress([[0,Row] || Row <- Rows]),
    iolist_to_binary(
      [<<137,$P,$N,$G,$\r,$\n,26,$\n>>,
       Chunk("IHDR", <<W:32,H:32,Depth,ColorType,0,0,0>>),
       [Chunk(T, D) || {T,D} <- Chunks],
       Chunk("IDAT", Data),
       Chunk("IEND", <<>>)]).

%% epxy: a child of a window other than "screen" was stored under its
%% short name so set/get/delete with "win.id" failed with enoent
epxy_names_test_() ->
    {setup,
     fun() -> {ok,_} = epxy:start([]) end,
     fun(_) -> epxy:stop() end,
     fun() ->
	     ok = epxy:new("w", [{type,window},{width,100},{height,50}]),
	     ok = epxy:new("v", [{type,window},{width,100},{height,50}]),
	     ok = epxy:new("w.btn", [{type,button},{x,1}]),
	     ok = epxy:new("v.btn", [{type,button},{x,2}]),
	     ok = epxy:new("btn", [{type,button},{x,3}]),
	     {error,ealready} = epxy:new("w.btn", [{type,button}]),
	     {error,ealready} = epxy:new("screen.btn", [{type,button}]),
	     [{x,1}] = epxy:get("w.btn", [x]),
	     [{x,2}] = epxy:get("v.btn", [x]),
	     [{x,3}] = epxy:get("btn", [x]),
	     [{x,3}] = epxy:get("screen.btn", [x]),
	     ok = epxy:set("w.btn", [{x,4}]),
	     [{x,4}] = epxy:get("w.btn", [x]),
	     [{x,2}] = epxy:get("v.btn", [x]),
	     ok = epxy:delete("w"),
	     [] = epxy:get("w.btn", [x]),
	     [{x,2}] = epxy:get("v.btn", [x])
     end}.
