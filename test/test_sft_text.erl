%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    Draw text using TrueType fonts through libschrift (epx:sft_*)
%%%
%%%    test_sft_text:check()            - headless checks (metrics, orientation)
%%%    test_sft_text:ascii("arial",$g,24) - dump glyph as ascii art
%%%    test_sft_text:png("/tmp/sft.png")  - render sample text to png
%%%    test_sft_text:show()             - render sample text in a window
%%%
%%%   Coordinate conventions (SFT_DOWNWARD_Y):
%%%    gmetrics.yoffset is the offset from the baseline down to the top
%%%    of the glyph box (negative for glyphs above the baseline), so a glyph
%%%    is drawn with its top left corner at
%%%      (PenX + left_side_bearing, BaseLine + yoffset)
%%% @end
-module(test_sft_text).

-export([check/0, ascii/3, png/1, png/0, show/0, show/1]).
-export([draw_string/7, text_width/2]).
-export([sample/1]).

-include("../include/epx.hrl").

-define(DEFAULT_PNG, "sft_text.png").

%%
%% Draw String with Sft on Pixmap with pen starting at X and baseline at Y
%% using foreground color Color (alpha component is coverage, keep 0).
%% Returns the pen position after the string.
%%
draw_string(Pixmap, Sft, Color, X, Y, String, Transform) ->
    Gc = epx:gc_create(),
    epx:gc_set(Gc, foreground_color, color_no_alpha(Color)),
    layout(Sft, String,
	   fun(Gid, GM, PenX) ->
		   GX = floor(PenX + GM#gmetrics.left_side_bearing),
		   GY = Y + GM#gmetrics.yoffset,
		   ok = epx:sft_glyph_render_to(Sft, Gc, Gid, Transform,
						GX, GY, Pixmap)
	   end, X).

%% width in pixels of String
text_width(Sft, String) ->
    layout(Sft, String, fun(_Gid, _GM, _PenX) -> ok end, 0).

%% walk string with advance width and kerning, call Fun for every glyph
layout(Sft, String, Fun, X0) ->
    {X, _} =
	lists:foldl(
	  fun(Char, {PenX, Prev}) ->
		  Gid = epx:sft_glyph(Sft, Char),
		  GM = epx:sft_glyph_info(Sft, Gid),
		  PenX1 = case Prev of
			      undefined -> PenX;
			      _ ->
				  {Kx,_Ky} = epx:sft_glyph_kerning(Sft,Prev,Gid),
				  PenX + Kx
			  end,
		  Fun(Gid, GM, PenX1),
		  {PenX1 + GM#gmetrics.advance_width, Gid}
	  end, {X0, undefined}, String),
    X.

%% the glyph coverage is added to the color alpha (as for epx fonts)
%% so the alpha component must be 0 to get anti aliased text
color_no_alpha({_A,R,G,B}) -> {0,R,G,B};
color_no_alpha({R,G,B}) -> {0,R,G,B};
color_no_alpha(Name) -> color_no_alpha(epx_color:from_name(Name)).

%%
%% Sample text page
%%
sample(Pixmap) ->
    epx:pixmap_fill(Pixmap, white),
    Lines = [{"arial", 12, "Arial 12: The quick brown fox jumps over the lazy dog."},
	     {"arial", 24, "Arial 24: AVAWAY Typography @ gjpqy 0123"},
	     {"times", 32, "Times 32: Hello, World! Wave AVA"},
	     {"times", 48, "Times 48: Ægg @ ÅÄÖ"},
	     {"arial", 64, "agH@jy"}],
    Y = lists:foldl(
	  fun({Font, Size, Text}, Y0) ->
		  Sft = epx:sft_open(Font, Size),
		  Asc = epx:sft_info(Sft, ascent),
		  Desc = epx:sft_info(Sft, descent),
		  Gap = epx:sft_info(Sft, line_gap),
		  BaseLine = round(Y0 + Asc),
		  %% baseline marker
		  epx:pixmap_draw_line(Pixmap, epx_gc:current(),
				       4, BaseLine, 6, BaseLine),
		  draw_string(Pixmap, Sft, black, 10, BaseLine, Text, undefined),
		  round(BaseLine - Desc + Gap)
	  end, 4, Lines),
    %% colored and rotated glyphs
    Sft = epx:sft_open("arial", 40),
    lists:foldl(
      fun(Angle, X) ->
	      T = epx_t2d:rotate(epx_t2d:identity(), Angle),
	      Gid = epx:sft_glyph(Sft, $R),
	      Gc = epx:gc_create(),
	      epx:gc_set(Gc, foreground_color, {0,200,0,0}),
	      ok = epx:sft_glyph_render_to(Sft, Gc, Gid, T, X, Y+10, Pixmap),
	      X + 50
      end, 10, [0, 30, 60, 90, 180, 270]),
    ok.

png() ->
    png(?DEFAULT_PNG).
png(File) ->
    Pixmap = epx:pixmap_create(800, 270, rgb),
    sample(Pixmap),
    epx_image_png:write_file(File, Pixmap).

show() ->
    show(30000).
show(Timeout) ->
    epx:start(),
    W = 800, H = 270,
    Win = epx:window_create(50, 50, W, H, [all]),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(W, H, argb),
    epx:pixmap_attach(Pix),
    sample(Pix),
    epx:pixmap_draw(Pix, Win, 0, 0, 0, 0, W, H),
    epx:sync(Win),
    Loop = fun Loop() ->
		   receive
		       {epx_event,Win,close} -> ok;
		       {epx_event,Win,{key_press,$q,_,_}} -> ok;
		       {epx_event,Win,_} -> Loop()
		   after Timeout -> ok
		   end
	   end,
    Loop(),
    epx:window_detach(Win),
    epx:pixmap_detach(Pix),
    ok.

%%
%% ASCII art of a glyph, the baseline is drawn as a line of '-' between
%% the last pixel row above the baseline and the first row below it.
%% (the glyph box from libschrift includes one extra row/column at the
%%  bottom/right, minWidth/minHeight = bbox max - bbox min + 1)
%%
ascii(Font, Char, Size) ->
    Sft = epx:sft_open(Font, Size),
    Gid = epx:sft_glyph(Sft, Char),
    GM = epx:sft_glyph_info(Sft, Gid),
    Pix = epx:sft_glyph_render(Sft, Gid, undefined),
    {W,H,Rows} = l8_rows(Pix),
    io:format("~s ~w '~ts' gid=~w ~wx~w ~p\n", [Font,Size,[Char],Gid,W,H,GM]),
    BaseRow = -GM#gmetrics.yoffset,  %% row index of baseline
    lists:foldl(
      fun(Row, I) ->
	      Line = [if V > 170 -> $#; V > 85 -> $+; V > 0 -> $.; true -> $\s end
		      || <<V>> <= Row],
	      if I =:= BaseRow -> baseline(W); true -> ok end,
	      io:format("|~s|\n", [Line]),
	      I+1
      end, 0, Rows),
    if BaseRow >= H -> baseline(W); true -> ok end,
    ok.

baseline(W) ->
    io:format("+~s+ baseline\n", [lists:duplicate(W,$-)]).

l8_rows(Pix) ->
    W = epx:pixmap_info(Pix, width),
    H = epx:pixmap_info(Pix, height),
    Bin = epx:pixmap_get_pixels(Pix, 0, 0, W, H),
    {W, H, [binary:part(Bin, Y*W, W) || Y <- lists:seq(0, H-1)]}.

%%
%% Headless checks
%%
check() ->
    [check_font(F) || F <- ["arial", "times"]],
    check_rotate(),
    check_render_to(),
    io:format("all sft checks ok\n"),
    ok.

check_font(Font) ->
    Size = 40,
    Sft = epx:sft_open(Font, Size),
    Asc = epx:sft_info(Sft, ascent),
    Desc = epx:sft_info(Sft, descent),
    true = (Asc > 0.7*Size) andalso (Asc < 1.1*Size),
    true = (Desc < 0) andalso (Desc > -0.4*Size),
    lists:foreach(fun(C) -> check_glyph(Sft, Font, C) end, "AgHL@jy"),
    %% 'H' cap height is about 0.66-0.75 em in arial/times
    {_,HH} = glyph_size(Sft, $H),
    true = (HH >= 0.62*Size) andalso (HH =< 0.78*Size),
    %% kerning: AV should be tighter than the sum of advances (both fonts
    %% have a kern table entry for A,V)
    {Kx,_} = epx:sft_glyph_kerning(Sft, epx:sft_glyph(Sft,$A),
				   epx:sft_glyph(Sft,$V)),
    io:format("~s: ascent=~.2f descent=~.2f H-height=~w kern(AV)=~.2f\n",
	      [Font, Asc, Desc, HH, Kx]),
    true = Kx < 0,
    ok.

glyph_size(Sft, C) ->
    GM = epx:sft_glyph_info(Sft, epx:sft_glyph(Sft, C)),
    {GM#gmetrics.min_width, GM#gmetrics.min_height}.

check_glyph(Sft, Font, C) ->
    Gid = epx:sft_glyph(Sft, C),
    GM = epx:sft_glyph_info(Sft, Gid),
    Pix = epx:sft_glyph_render(Sft, Gid, undefined),
    {W,H,Rows} = l8_rows(Pix),
    %% image size equals the metrics
    W = GM#gmetrics.min_width,
    H = GM#gmetrics.min_height,
    %% ink bounding box must touch (within one pixel) all four sides
    {X0,Y0,X1,Y1} = ink_box(Rows),
    Ok = (X0 =< 1) andalso (Y0 =< 1) andalso (X1 >= W-2) andalso (Y1 >= H-2),
    if not Ok ->
	    io:format("~s '~c' ~wx~w ink box ~p\n", [Font,C,W,H,{X0,Y0,X1,Y1}]),
	    ascii(Font, C, 40),
	    error({bad_ink_box, Font, C});
       true -> ok
    end,
    Bottom = H + GM#gmetrics.yoffset,  %% bottom of glyph relative baseline
    case C of
	$A -> %% sits on baseline, apex at top (not upside down)
	    true = abs(Bottom) =< 1,
	    true = row_ink(lists:nth(Y0+1,Rows)) < row_ink(lists:nth(Y1+1,Rows));
	$H -> true = abs(Bottom) =< 1;
	$L -> %% stem to the left, foot to the right: not mirrored
	    true = abs(Bottom) =< 1,
	    {Left,Right} = half_ink(Rows, W),
	    true = Left > Right,
	    %% top row only has the stem on the left
	    {TL,TR} = half_ink([lists:nth(Y0+1,Rows)], W),
	    true = TL > 0 andalso TR =:= 0;
	$g -> %% descender below baseline, top at about x-height
	    true = Bottom > 0.15*40,
	    true = GM#gmetrics.yoffset < 0;
	$y -> true = Bottom > 0.15*40;
	$j -> true = Bottom > 0.15*40;
	_ -> ok
    end,
    ok.

ink_box(Rows) -> ink_box(Rows, 0).

%% bounding box of pixels with value > Threshold
ink_box(Rows, Threshold) ->
    {_, Box} =
	lists:foldl(
	  fun(Row, {Y, Box0}) ->
		  Xs = [X || {X,V} <- lists:zip(lists:seq(0,byte_size(Row)-1),
						binary_to_list(Row)), V > Threshold],
		  Box1 = case Xs of
			     [] -> Box0;
			     _ ->
				 MinX = lists:min(Xs), MaxX = lists:max(Xs),
				 case Box0 of
				     undefined -> {MinX,Y,MaxX,Y};
				     {A,B,C,_D} -> {min(A,MinX),B,max(C,MaxX),Y}
				 end
			 end,
		  {Y+1, Box1}
	  end, {0, undefined}, Rows),
    Box.

row_ink(Row) -> lists:sum(binary_to_list(Row)).

half_ink(Rows, W) ->
    Half = W div 2,
    lists:foldl(
      fun(Row, {L,R}) ->
	      <<Lb:Half/binary, _:(W rem 2)/binary, Rb/binary>> = Row,
	      {L + row_ink(Lb), R + row_ink(Rb)}
      end, {0,0}, Rows).

%% rotating 90 degrees swaps width and height, and 'L' foot ends up
%% where it should: rotate (x,y)->(-y,x) in y-down coords is clockwise,
%% so the foot (bottom) of the L goes to the left side.
check_rotate() ->
    Sft = epx:sft_open("arial", 40),
    Gid = epx:sft_glyph(Sft, $L),
    GM = epx:sft_glyph_info(Sft, Gid),
    T = epx_t2d:rotate(epx_t2d:identity(), 90),
    Pix = epx:sft_glyph_render(Sft, Gid, T),
    {W,H,Rows} = l8_rows(Pix),
    true = abs(W - GM#gmetrics.min_height) =< 1,
    true = abs(H - GM#gmetrics.min_width) =< 1,
    {X0,Y0,X1,Y1} = ink_box(Rows),
    true = (X0 =< 1) andalso (Y0 =< 1) andalso (X1 >= W-2) andalso (Y1 >= H-2),
    %% the stem is now a horizontal bar at the top and the foot a
    %% vertical bar at the left
    {Upper, Lower} = lists:split(H div 2, Rows),
    TopInk = lists:sum([row_ink(R) || R <- Upper]),
    BotInk = lists:sum([row_ink(R) || R <- Lower]),
    {LeftInk, RightInk} = half_ink(Rows, W),
    io:format("rotate 90: ~wx~w -> ~wx~w top=~w bottom=~w left=~w right=~w\n",
	      [GM#gmetrics.min_width, GM#gmetrics.min_height, W, H,
	       TopInk, BotInk, LeftInk, RightInk]),
    true = TopInk > 2*BotInk,
    true = LeftInk > 1.5*RightInk,
    ok.

%% render a string onto an rgb pixmap with clipping at the left/top edge
%% and compare the ink box with the layout.
check_render_to() ->
    Sft = epx:sft_open("arial", 30),
    Pix = epx:pixmap_create(200, 60, rgb),
    epx:pixmap_fill(Pix, white),
    BaseLine = 40,
    Text = "Hello",
    X1 = draw_string(Pix, Sft, black, 10, BaseLine, Text, undefined),
    true = abs(X1 - 10 - text_width(Sft, Text)) < 0.001,
    {IX0,IY0,IX1,IY1} = rgb_ink_box(Pix),
    {_, HH} = glyph_size(Sft, $H),
    io:format("render_to: ink box ~p pen end ~.1f\n", [{IX0,IY0,IX1,IY1},X1]),
    true = abs(IY1 - (BaseLine-1)) =< 1,        %% bottom on baseline
    true = abs(IY0 - (BaseLine-HH)) =< 1,       %% top at cap height
    true = IX0 >= 10 andalso IX0 =< 14,
    true = IX1 =< X1 andalso IX1 >= X1 - 6,
    %% partly clipped glyph (left and top outside the pixmap) must not
    %% leave streaks to the right of the glyph
    Pix2 = epx:pixmap_create(40, 40, rgb),
    epx:pixmap_fill(Pix2, white),
    Gc = epx:gc_create(),
    epx:gc_set(Gc, foreground_color, {0,0,0,0}),
    ok = epx:sft_glyph_render_to(Sft, Gc, epx:sft_glyph(Sft,$O),
				 undefined, -8, -8, Pix2),
    {_,_,CX1,CY1} = rgb_ink_box(Pix2),
    {OW,OH} = glyph_size(Sft, $O),
    io:format("clipped O ~wx~w: ink box ends at ~w,~w\n", [OW,OH,CX1,CY1]),
    true = CX1 =< OW-8 andalso CY1 =< OH-8,
    ok.

rgb_ink_box(Pix) ->
    W = epx:pixmap_info(Pix, width),
    H = epx:pixmap_info(Pix, height),
    Bin = epx:pixmap_get_pixels(Pix, 0, 0, W, H),
    %% convert to a 'darkness' l8 image
    L8 = << <<(255 - ((R+G+B) div 3))>> || <<R,G,B>> <= Bin >>,
    ink_box([binary:part(L8, Y*W, W) || Y <- lists:seq(0, H-1)], 64).
