%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    Produce efnt from ttf (true type file)
%%%
%%%    The glyphs are rendered with libschrift (epx:sft_*) and stored
%%%    as an a8 (anti aliased) efnt font that epx:font_open/1 can load.
%%%    See include/epx_efnt.h for the file format.
%%% @end
%%% Created : 22 Oct 2023 by Tony Rogvall <tony@rogvall.se>

-module(epx_efnt).

-include("../include/epx.hrl").

-export([from_ttf/2, from_ttf/3]).
-export([write_ttf/3, write_ttf/4]).
-export([ttf_info/1]).
%% debug
-export([test_efnt/1, rotate/1, rotate/4, ascii_pixels/1]).

-define(HEADER_SIZE, 96).          %% sizeof(epx_font_file_t)
-define(EPX_FORMAT_A8, 16#a187).
-define(ALIGN, 16).                %% EPX_PIXMAP_ALIGNMENT

-define(WEIGHT_MEDIUM, 1).
-define(WEIGHT_BOLD, 2).
-define(SLANT_ROMAN, 1).
-define(SLANT_ITALIC, 2).
-define(WIDTH_NORMAL, 1).
-define(SPACING_PROPORTIONAL, 1).
-define(SPACING_MONOSPACED, 2).

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

%% Family name, style name, weight and slant of a ttf file
-spec ttf_info(File::string()) ->
	  #{ family := string(), style := string(),
	     weight := medium|bold, slant := roman|italic }.
ttf_info(File) ->
    Sft = epx:sft_fileopen(File, 12),
    Family = epx:sft_info(Sft, family_name),
    Style = epx:sft_info(Sft, subfamily_name),
    Words = string:lexemes(string:lowercase(Style), " -"),
    Weight = case lists:any(fun(W) -> lists:member(W, ["bold","black",
							"heavy","semibold",
							"demibold"])
			    end, Words) of
		 true -> bold;
		 false -> medium
	     end,
    Slant = case lists:any(fun(W) -> lists:member(W, ["italic","oblique"])
			   end, Words) of
		true -> italic;
		false -> roman
	    end,
    #{ family => Family, style => Style, weight => Weight, slant => Slant }.

%% Create efnt data from ttf file using point size Size at 75 dpi
-spec from_ttf(File::string(), Size::number()) -> binary().
from_ttf(File, Size) ->
    from_ttf(File, Size, []).

%% Options:
%%   {resolution, Dpi}       (75)
%%   {range, {First,Last}}   ({32,255}) code points in the font
%%   {default, Char}         ($?) glyph used for missing code points
-spec from_ttf(File::string(), Size::number(), Opts::list()) -> binary().
from_ttf(File, Size, Opts) ->
    Res = proplists:get_value(resolution, Opts, 75),
    {First,Last} = proplists:get_value(range, Opts, {32,255}),
    Default = proplists:get_value(default, Opts, $?),
    PixelSize = Size*Res/72,
    Sft = epx:sft_fileopen(File, PixelSize),
    #{ family := Family, weight := Weight, slant := Slant } = ttf_info(File),
    Ascent = ceil(epx:sft_info(Sft, ascent)),
    Descent = ceil(-epx:sft_info(Sft, descent)),
    Glyphs = [try render_glyph(Sft, C)
	      catch error:_ -> missing  %% glyph libschrift can not handle
	      end || C <- lists:seq(First, Last)],
    Advances = lists:usort([Dwx || {_,_,W,_,Dwx,_} <- Glyphs, W > 0]),
    Spacing = case Advances of
		  [_] -> ?SPACING_MONOSPACED;
		  _ -> ?SPACING_PROPORTIONAL
	      end,
    %% string table: foundry and family, glyphs are not named
    Strings = [str("epx"), str(Family)],
    FamilyOffset = iolist_size(hd(Strings)),
    StringTab = pad(iolist_to_binary(Strings)),
    %% glyph table and offset table (offsets relative to data start)
    OffsetTabStart = byte_size(StringTab),
    OffsetTabLen = 4*(Last-First+1),
    GlyphTabStart = OffsetTabStart + align(OffsetTabLen),
    {Offsets, GlyphData} = glyph_table(Glyphs, GlyphTabStart, [], []),
    OffsetTab = pad(<< <<O:32/little>> || O <- Offsets >>),
    GlyphTab = iolist_to_binary(GlyphData),
    Header = <<"EFNT",
	       0:32/little,              %% foundry_offset
	       FamilyOffset:32/little,
	       %% font info
	       (weight(Weight)):32/little,
	       (slant(Slant)):32/little,
	       ?WIDTH_NORMAL:32/little,
	       0:32/little,              %% style
	       Spacing:32/little,
	       ?EPX_FORMAT_A8:32/little,
	       (round(PixelSize)):32/little,
	       (round(Size*10)):32/little,  %% point size * 10
	       Res:32/little, Res:32/little,
	       Descent:32/little,
	       Ascent:32/little,
	       First:32/little, Last:32/little, Default:32/little,
	       0:32/little, (byte_size(StringTab)):32/little,
	       OffsetTabStart:32/little, OffsetTabLen:32/little,
	       GlyphTabStart:32/little, (byte_size(GlyphTab)):32/little>>,
    ?HEADER_SIZE = byte_size(Header),
    <<Header/binary, StringTab/binary, OffsetTab/binary, GlyphTab/binary>>.

%% Create efnt file from ttf
write_ttf(TTF, Size, File) ->
    write_ttf(TTF, Size, File, []).
write_ttf(TTF, Size, File, Opts) ->
    file:write_file(File, from_ttf(TTF, Size, Opts)).

weight(bold) -> ?WEIGHT_BOLD;
weight(_) -> ?WEIGHT_MEDIUM.

slant(italic) -> ?SLANT_ITALIC;
slant(_) -> ?SLANT_ROMAN.

%% string table entry <<Len,Chars,0>>
str(S) ->
    B = unicode:characters_to_binary(S, unicode, latin1),
    <<(byte_size(B)), B/binary, 0>>.

align(N) -> (N + ?ALIGN - 1) band (bnot (?ALIGN - 1)).

pad(Bin) ->
    Pad = align(byte_size(Bin)) - byte_size(Bin),
    <<Bin/binary, 0:Pad/unit:8>>.

%% {Code, Width, Height, Xoffs, Dwx, Rows} | missing
render_glyph(Sft, C) ->
    case epx:sft_glyph(Sft, C) of
	0 when C =/= 0 ->
	    missing;
	Gid ->
	    GM = epx:sft_glyph_info(Sft, Gid),
	    W = GM#gmetrics.min_width,
	    H = GM#gmetrics.min_height,
	    Xoffs = floor(GM#gmetrics.left_side_bearing),
	    Dwx = round(GM#gmetrics.advance_width),
	    %% efnt: glyph top is at baseline - yoffs - height
	    Yoffs = -(GM#gmetrics.yoffset + H),
	    Rows = if W > 0, H > 0 ->
			   Pix = epx:sft_glyph_render(Sft, Gid, undefined),
			   Data = epx:pixmap_get_pixels(Pix, 0, 0, W, H),
			   RowPad = align(W) - W,
			   [<<Row/binary, 0:RowPad/unit:8>> ||
			       <<Row:W/binary>> <= Data];
		      true ->
			   []
		   end,
	    if W > 0, H > 0 -> {Xoffs, Yoffs, W, H, Dwx, Rows};
	       true -> {0, 0, 0, 0, Dwx, []}
	    end
    end.

glyph_table([missing|Gs], Pos, Offs, Acc) ->
    glyph_table(Gs, Pos, [0|Offs], Acc);
glyph_table([{Xoffs,Yoffs,W,H,Dwx,Rows}|Gs], Pos, Offs, Acc) ->
    G = [<<0:32/little, W:16/little, H:16/little,
	   Xoffs:16/little-signed, Yoffs:16/little-signed,
	   Dwx:16/little-signed, 0:16/little-signed>> | Rows],
    glyph_table(Gs, Pos + iolist_size(G), [Pos|Offs], [G|Acc]);
glyph_table([], _Pos, Offs, Acc) ->
    {lists:reverse(Offs), lists:reverse(Acc)}.

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
    Pts1 = [{Xp+abs(MinX),Yp+abs(MinY)} || {Xp,Yp} <- Pts],
    WW = lists:max([Xi || {Xi,_Y} <- Pts1]),
    HH = lists:max([Yi || {_X,Yi} <- Pts1]),
    %%
    A0 = [{0,Ascent},{W+Dx,Ascent+Dy}],
    _A1 = epx_t2d:points(Trans, A0),

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
	      
    

