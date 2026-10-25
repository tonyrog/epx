%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2026, Tony Rogvall
%%% @doc
%%%    Transformed TrueType text: glyphs, strings and text along a path.
%%%
%%%    Every glyph is drawn with a transform from its pen space to the
%%%    user coordinates of the pixmap (see doc/TEXT.md):
%%%
%%%      pen space   origin at the pen point on the baseline, x to the
%%%                  right, y down, pixels at the font size
%%%      user space  the coordinates you draw with on the pixmap
%%%      device      the pixmap pixels, device = pixmap ltm * user
%%%
%%%    The functions build the pen space -> user space transform from
%%%    a position, an anchor and a local transform, and the pixmap
%%%    transform (epx:pixmap_ltm_*) is the global transform on top.
%%% @end
%%% Created : 25 Oct 2026 by Tony Rogvall <tony@rogvall.se>

-module(epx_text).

-export([open/2]).
-export([glyph/2, anchor/3, glyph_transform/3, draw_glyph/5]).
-export([layout/3, advance/2, advance/3]).
-export([draw_string/6, draw_string/7]).
-export([draw_glyphs/6]).
-export([draw_path/6, path_table/2, path_length/1, path_point/2]).
-export([transform/1]).

-include("../include/epx.hrl").
-include("../include/epx_t2d.hrl").

-type sft() :: epx:epx_sft().
-type t2d() :: #t2d{}.
-type anchor() :: pen | baseline_center | center | {number(),number()}.
-type glyph_info() :: #{ index := non_neg_integer(),
			 char := integer(),
			 glyph := integer(),
			 pen := number(),       %% pen x along the string
			 advance := number(),
			 width := number(),     %% advance of the whole string
			 box := {integer(),integer(),integer(),integer()} }.
-export_type([glyph_info/0, anchor/0]).

%% Open a TrueType font at Size pixels per em. Name is a file name, a
%% family name known by the font server ("Arial", "Times New Roman")
%% or a file in priv/truetype without .ttf ("times").
-spec open(Name::string(), Size::number()) -> sft().
open(Name, Size) ->
    epx:sft_fileopen(find_ttf(Name), Size).

find_ttf(Name) ->
    case filelib:is_regular(Name) of
	true -> Name;
	false ->
	    Scalable = try epx_font:scalable() catch _:_ -> [] end,
	    Match = [{Style =/= "Regular", File} ||
			{Family, Style, File} <- Scalable,
			string:equal(Family, Name, true) orelse
			    string:equal(Family++" "++Style, Name, true)],
	    case lists:sort(Match) of
		[{_, File}|_] -> File;
		[] -> filename:join([code:priv_dir(epx), "truetype",
				     Name ++ ".ttf"])
	    end
    end.

%% Glyph id of a character
-spec glyph(Sft::sft(), Char::integer()) -> integer().
glyph(Sft, Char) ->
    epx:sft_glyph(Sft, Char).

%% The anchor point in pen space
%%   pen              (0,0) the pen point on the baseline
%%   baseline_center  middle of the advance on the baseline
%%   center           center of the glyph box
%%   {X,Y}            any point in pen space
-spec anchor(Sft::sft(), Glyph::integer(), Anchor::anchor()) ->
	  {number(),number()}.
anchor(_Sft, _Glyph, pen) -> {0, 0};
anchor(Sft, Glyph, baseline_center) ->
    GM = epx:sft_glyph_info(Sft, Glyph),
    {GM#gmetrics.advance_width/2, 0};
anchor(Sft, Glyph, center) ->
    {X,Y,W,H} = epx:sft_glyph_box(Sft, Glyph),
    {X + W/2, Y + H/2};
anchor(_Sft, _Glyph, {X,Y}) -> {X, Y}.

%% Transform from local options:
%%   {rotate, Radians}, {scale, S | {Sx,Sy}}, {transform, T2D}
%% The result is T2D * Rotate * Scale (scale is done first).
-spec transform(Opts::list()) -> t2d().
transform(Opts) ->
    T0 = case proplists:get_value(transform, Opts) of
	     undefined -> epx_t2d:identity();
	     T -> T
	 end,
    T1 = case proplists:get_value(rotate, Opts) of
	     undefined -> T0;
	     A -> epx_t2d:rotate_rad(T0, A)
	 end,
    case proplists:get_value(scale, Opts) of
	undefined -> T1;
	{Sx,Sy} -> epx_t2d:scale(T1, Sx, Sy);
	S -> epx_t2d:scale(T1, S, S)
    end.

%% Pen space -> user space transform that puts the anchor of the glyph
%% at {x,y} and applies the local transform around the anchor:
%%
%%   T = Translate(X,Y) * Local * Translate(-Ax,-Ay)
%%
%% Options: {x,X}, {y,Y}, {anchor,A} (pen) and the options of transform/1
-spec glyph_transform(Sft::sft(), Glyph::integer(), Opts::list()) -> t2d().
glyph_transform(Sft, Glyph, Opts) ->
    X = proplists:get_value(x, Opts, 0),
    Y = proplists:get_value(y, Opts, 0),
    {Ax,Ay} = anchor(Sft, Glyph, proplists:get_value(anchor, Opts, pen)),
    T0 = epx_t2d:translate(epx_t2d:identity(), X, Y),
    T1 = epx_t2d:compose(transform(Opts), T0),
    epx_t2d:translate(T1, -Ax, -Ay).

%% Draw one character, see glyph_transform/3 for the options
-spec draw_glyph(Pixmap::epx:epx_pixmap(), Gc::epx:epx_gc(), Sft::sft(),
		 Char::integer(), Opts::list()) -> ok.
draw_glyph(Pixmap, Gc, Sft, Char, Opts) ->
    Glyph = glyph(Sft, Char),
    epx:sft_glyph_draw(Sft, Gc, Glyph, glyph_transform(Sft, Glyph, Opts),
		       Pixmap).

%% Lay out a string on a baseline starting at pen x = 0.
%% Options: {kerning, true|false} (true), {spacing, Px} (0) extra space
%% between characters.
-spec layout(Sft::sft(), Text::string(), Opts::list()) -> [glyph_info()].
layout(Sft, Text, Opts) ->
    Kerning = proplists:get_value(kerning, Opts, true),
    Spacing = proplists:get_value(spacing, Opts, 0),
    Gs = [{C, glyph(Sft, C)} || C <- Text],
    {Infos, Width} = layout_(Sft, Gs, undefined, 0, 0, Kerning, Spacing, []),
    [I#{ width => Width } || I <- Infos].

layout_(Sft, [{C,G}|Gs], Prev, Pen0, I, Kerning, Spacing, Acc) ->
    Kern = if Kerning, Prev =/= undefined ->
		   {Kx,_Ky} = epx:sft_glyph_kerning(Sft, Prev, G),
		   Kx;
	      true -> 0
	   end,
    Pen = Pen0 + Kern,
    GM = epx:sft_glyph_info(Sft, G),
    Adv = GM#gmetrics.advance_width,
    Info = #{ index => I, char => C, glyph => G, pen => Pen,
	      advance => Adv, box => epx:sft_glyph_box(Sft, G) },
    Next = case Gs of
	       [] -> Pen + Adv;
	       _ -> Pen + Adv + Spacing
	   end,
    layout_(Sft, Gs, G, Next, I+1, Kerning, Spacing, [Info|Acc]);
layout_(_Sft, [], _Prev, Pen, _I, _Kerning, _Spacing, Acc) ->
    {lists:reverse(Acc), Pen}.

%% Advance width of a string (with kerning)
-spec advance(Sft::sft(), Text::string()) -> number().
advance(Sft, Text) ->
    advance(Sft, Text, []).
advance(_Sft, [], _Opts) -> 0;
advance(Sft, Text, Opts) ->
    #{ width := W } = hd(layout(Sft, Text, Opts)),
    W.

%% Draw glyphs placed by Fun. Fun(GlyphInfo) returns the pen space ->
%% user space transform of that glyph, or skip. GlyphInfo has the pen
%% position of the glyph along the string (see layout/3), so Fun maps a
%% straight baseline onto anything.
-spec draw_glyphs(Pixmap::epx:epx_pixmap(), Gc::epx:epx_gc(), Sft::sft(),
		  Text::string(),
		  Fun::fun((glyph_info()) -> t2d() | skip),
		  Opts::list()) -> ok.
draw_glyphs(Pixmap, Gc, Sft, Text, Fun, Opts) ->
    lists:foreach(
      fun(Info = #{ glyph := G }) ->
	      case Fun(Info) of
		  skip -> ok;
		  T -> epx:sft_glyph_draw(Sft, Gc, G, T, Pixmap)
	      end
      end, layout(Sft, Text, Opts)).

%% Draw a string with its baseline origin at (X,Y).
%% Options: {align, left|center|right} (left), {kerning,B}, {spacing,Px}
%% and {rotate,A}, {scale,S}, {transform,T} for the whole string, done
%% around (X,Y). Return the pen position after the string (user space).
-spec draw_string(Pixmap::epx:epx_pixmap(), Gc::epx:epx_gc(), Sft::sft(),
		  X::number(), Y::number(), Text::string()) -> {number(),number()}.
draw_string(Pixmap, Gc, Sft, X, Y, Text) ->
    draw_string(Pixmap, Gc, Sft, X, Y, Text, []).

-spec draw_string(Pixmap::epx:epx_pixmap(), Gc::epx:epx_gc(), Sft::sft(),
		  X::number(), Y::number(), Text::string(), Opts::list()) ->
	  {number(),number()}.
draw_string(Pixmap, Gc, Sft, X, Y, Text, Opts) ->
    Layout = layout(Sft, Text, Opts),
    Width = case Layout of [] -> 0; [#{ width := W }|_] -> W end,
    Start = case proplists:get_value(align, Opts, left) of
		left -> 0;
		center -> -Width/2;
		right -> -Width
	    end,
    %% string transform: Translate(X,Y) * Local * Translate(Start,0)
    T0 = epx_t2d:compose(transform(Opts),
			 epx_t2d:translate(epx_t2d:identity(), X, Y)),
    StrT = epx_t2d:translate(T0, Start, 0),
    lists:foreach(
      fun(#{ glyph := G, pen := Pen }) ->
	      epx:sft_glyph_draw(Sft, Gc, G, epx_t2d:translate(StrT, Pen, 0),
				 Pixmap)
      end, Layout),
    epx_t2d:point(StrT, {Width, 0}).

%% Draw text along the path Fun(T) -> {X,Y}, T from 0.0 to 1.0.
%% Each glyph is placed with the middle of its advance on the path and
%% turned along the path (the chord over the glyph advance).
%% Options:
%%   {align, left|center|right}  where on the path the text is (left)
%%   {offset, S}                 extra distance along the path (0)
%%   {baseline, Dy | center}     baseline Dy pixels above the path (0),
%%                               center puts the path through the middle
%%                               of the text
%%   {samples, N}                path samples (256)
%%   {kerning, B}, {spacing, Px}
%% Glyphs outside the path are not drawn. Returns the number of glyphs
%% drawn.
-spec draw_path(Pixmap::epx:epx_pixmap(), Gc::epx:epx_gc(), Sft::sft(),
		Text::string(),
		PathFun::fun((float()) -> {number(),number()}),
		Opts::list()) -> non_neg_integer().
draw_path(Pixmap, Gc, Sft, Text, PathFun, Opts) ->
    Table = path_table(PathFun, proplists:get_value(samples, Opts, 256)),
    Length = path_length(Table),
    Layout = layout(Sft, Text, Opts),
    Width = case Layout of [] -> 0; [#{ width := W }|_] -> W end,
    Offset = proplists:get_value(offset, Opts, 0),
    Start = Offset + case proplists:get_value(align, Opts, left) of
			 left -> 0;
			 center -> (Length - Width)/2;
			 right -> Length - Width
		     end,
    Dy = case proplists:get_value(baseline, Opts, 0) of
	     center ->
		 Asc = epx:sft_info(Sft, ascent),
		 Desc = epx:sft_info(Sft, descent),  %% negative
		 -(Asc + Desc)/2;
	     D -> D
	 end,
    lists:foldl(
      fun(#{ glyph := G, pen := Pen, advance := Adv }, N) ->
	      S = Start + Pen + Adv/2,
	      if S < 0; S > Length ->
		      N;
		 true ->
		      {X,Y} = path_point(Table, S),
		      Angle = path_angle(Table, S, max(Adv/2, 1.0)),
		      %% T = Translate(X,Y) * Rotate(Angle) * Translate(-Adv/2,-Dy)
		      T0 = epx_t2d:translate(epx_t2d:identity(), X, Y),
		      T1 = epx_t2d:rotate_rad(T0, Angle),
		      T = epx_t2d:translate(T1, -Adv/2, -Dy),
		      ok = epx:sft_glyph_draw(Sft, Gc, G, T, Pixmap),
		      N + 1
	      end
      end, 0, Layout).

%% Sample Fun(T) for T in 0..1 into a table of
%% {S, X, Y} where S is the distance along the path
-spec path_table(PathFun::fun((float()) -> {number(),number()}),
		 N::pos_integer()) -> tuple().
path_table(PathFun, N) when N >= 1 ->
    Ps = [PathFun(I/N) || I <- lists:seq(0, N)],
    {_, Rows} = lists:foldl(
		  fun({X,Y}, {undefined, Acc}) ->
			  {{X,Y,0.0}, [{0.0,X,Y}|Acc]};
		     ({X,Y}, {{X0,Y0,S0}, Acc}) ->
			  S = S0 + math:sqrt((X-X0)*(X-X0)+(Y-Y0)*(Y-Y0)),
			  {{X,Y,S}, [{S,X,Y}|Acc]}
		  end, {undefined, []}, Ps),
    list_to_tuple(lists:reverse(Rows)).

-spec path_length(Table::tuple()) -> float().
path_length(Table) ->
    element(1, element(tuple_size(Table), Table)).

%% point at distance S along the path (clamped to the ends)
-spec path_point(Table::tuple(), S::number()) -> {float(),float()}.
path_point(Table, S) ->
    N = tuple_size(Table),
    I = find_segment(Table, S, 1, N),
    {S0,X0,Y0} = element(I, Table),
    {S1,X1,Y1} = element(min(I+1, N), Table),
    R = if S1 > S0 -> max(0.0, min(1.0, (S - S0)/(S1 - S0)));
	   true -> 0.0
	end,
    {X0 + R*(X1-X0), Y0 + R*(Y1-Y0)}.

%% direction of the chord from S-H to S+H
path_angle(Table, S, H) ->
    {Xa,Ya} = path_point(Table, S - H),
    {Xb,Yb} = path_point(Table, S + H),
    if Xa == Xb, Ya == Yb -> 0.0;
       true -> math:atan2(Yb - Ya, Xb - Xa)
    end.

%% binary search for the last sample with distance =< S
find_segment(_Table, _S, Lo, Hi) when Hi - Lo =< 1 ->
    Lo;
find_segment(Table, S, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case element(1, element(Mid, Table)) =< S of
	true -> find_segment(Table, S, Mid, Hi);
	false -> find_segment(Table, S, Lo, Mid)
    end.
