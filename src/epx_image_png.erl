%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%     png image
%%% @end
%%% Created : 15 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_image_png).

-behaviour(epx_image).
-export([magic/1, mime_type/0, extensions/0,
	 read_info/1, write_info/2,
	 read/2, write/2]).
-export([write_data/5, write_data/6, write_data/8, write_data/9]).
-export([format_data/4, format_data/7, format_data/8]).
-export([format_pixel_data/2]).
-export([write_file/2]).
%% -define(debug, true).

-include("../include/epx_image.hrl").
-include("dbg.hrl").

-import(lists, [reverse/1]).
-import(epx_image, [attribute/3, set_attribute/3]).
-export([filter/4]).

-define(MAGIC, 137,$P,$N,$G,$\r,$\n,26,$\n).

-define(IHDR, "IHDR"). %% image header
-define(PLTE, "PLTE"). %% palette
-define(IDAT, "IDAT"). %% image data
-define(IEND, "IEND"). %% image trailer

-define(bKGD, "bKGD"). %% background color
-define(cHRM, "cHRM"). %% primary chromaticites and white point
-define(gAMA, "gAMA"). %% Image gamma
-define(hIST, "hIST"). %% Image histogram
-define(pHYs, "pHYs"). %% Physical pixel dimensions
-define(sBIT, "sBIT"). %% Significant bits
-define(tEXt, "tEXt"). %% Textual data
-define(tIME, "tIME"). %% Image last modification time
-define(tRNS, "tRNS"). %% Transparency
-define(zTXt, "zTXt"). %% Compressed textual data

magic(<<?MAGIC, _/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/png".

extensions() -> [ ".png" ].
    
read_info(Fd) ->
    case file:read(Fd, 8) of
	{ok, << ?MAGIC >> } ->
	    scan_info(Fd, #epx_image { type = ?MODULE }, true);
	{ok, _} ->
	    {error, bad_magic};
	Error ->
	    Error
    end.

scan_info(Fd, IMG, First) ->
    case read_chunk_hdr(Fd) of
	{ok, Length, Type} ->
	    scan_info(Fd, IMG, First, Type, Length);
	Error -> 
	    Error
    end.

scan_info(Fd, IMG, true, ?IHDR, Length) ->
    case read_chunk_crc(Fd,Length) of
	{ok,  <<Width:32, Height:32, BitDepth:8,
		ColorType:8, CompressionMethod:8,
		FilterMethod:8, InterlaceMethod:8, _/binary >>} ->
	    scan_info(Fd, IMG#epx_image {
			    width = Width,
			    height = Height,
			    depth = BitDepth,
			    format = format(ColorType,BitDepth),
			    order  = left_to_right,
			    attributes = 
			    [ {'ColorType', ColorType},
			      {'Compression', CompressionMethod},
			      {'Filter', FilterMethod },
			      {'Interlace', InterlaceMethod }]}, false);
	Error -> Error
    end;
scan_info(Fd, IMG, false, ?tEXt, Length) ->
    case read_chunk_crc(Fd, Length) of
	{ok, Chunk} ->
	    {Key,<<Value/binary>>} = keyword(Chunk),
	    scan_info(Fd,update_txt(IMG,Key,Value), false);
	Error -> Error
    end;
scan_info(Fd, IMG, false, ?zTXt, Length) ->
    case read_chunk_crc(Fd, Length) of
	{ok, Chunk} ->
	    {Key,<<0,CBin/binary>>} = keyword(Chunk),
	    Value = zlib:uncompress(CBin),
	    scan_info(Fd, update_txt(IMG,Key,Value), false);
	Error -> Error
    end;
scan_info(Fd, IMG, false, ?bKGD, Length) ->
    CT = attribute(IMG, 'ColorType', undefined),
    case read_chunk_crc(Fd, Length) of
	{ok, <<Index:8>>} when CT=:=3 ->
	    scan_info(Fd, set_attribute(IMG, 'Background', Index), false);
	{ok, <<Gray:16>>} when CT=:=0; CT=:=4 ->
	    scan_info(Fd, set_attribute(IMG, 'Background', Gray), false);
	{ok, <<R:16,G:16,B:16>>} when CT=:=2; CT=:=6 ->
	    scan_info(Fd, set_attribute(IMG, 'Background', {R,G,B}), false);
	{ok, _Data} ->
	    ?dbg("bKGD other=~p\n", [_Data]),
	    scan_info(Fd, IMG, false);
	Error -> Error
    end;
scan_info(Fd, IMG, false, ?tIME, Length) ->
    case read_chunk_crc(Fd, Length) of
	{ok, <<Year:16, Mon:8, Day:8, H:8, M:8, S:8>>} ->
	    scan_info(Fd, IMG#epx_image { mtime = {{Year,Mon,Day},
						   {H,M,S}} }, false);
	{ok, _Data} ->
	    ?dbg("tIME other=~p\n", [_Data]),
	    scan_info(Fd, IMG, false);
	Error -> Error
    end;
scan_info(Fd, IMG, false, ?pHYs, Length) ->
    case read_chunk_crc(Fd, Length) of
	{ok, <<X:32, Y:32, Unit:8>>} ->
	    U = case Unit of
		    0 -> unknown;
		    1 -> meter;
		    _ -> {unit,Unit}
		end,
	    scan_info(Fd, set_attribute(IMG,'Physical',{X,Y,U}),false);
	{ok, _Data} ->
	    ?dbg("pHYs other=~p\n", [_Data]),
	    scan_info(Fd, IMG, false);
	Error -> Error
    end;
scan_info(_Fd, IMG, false, ?IEND, 0) ->
    {ok, IMG};
scan_info(Fd, IMG, false, _Type, Length) ->
    ?dbg("~s skipped=~p\n", [_Type,Length]),
    skip_chunk(Fd, Length),
    scan_info(Fd, IMG, false).

%% Update txt attributes
update_txt(IMG,Key,Value) ->
    ?dbg("update_txt: key=~p, value=~p\n", [Key,Value]),
    case Key of
	'Comment' ->
	    IMG#epx_image { comment = binary_to_list(Value) };
	_ ->
	    As = [{Key,Value} | IMG#epx_image.attributes],
	    IMG#epx_image { attributes = As }
    end.


%% determine the epx_image format
bytes_per_row(l1,W) -> (W+7) div 8;
bytes_per_row(l2,W) -> (W+3) div 4;
bytes_per_row(l4,W) -> (W+1) div 2;
bytes_per_row(l8,W) -> W;
bytes_per_row(l16,W) -> W*2;
bytes_per_row(r8g8b8,W) -> W*3;
bytes_per_row(r16g16b16,W) -> W*6;
bytes_per_row(palette1,W) -> (W+7) div 8;
bytes_per_row(palette2,W) -> (W+3) div 4;
bytes_per_row(palette4,W) -> (W+1) div 2;
bytes_per_row(palette8,W) -> W;
bytes_per_row(l8a8,W) -> W*2;
bytes_per_row(l16a16,W) -> W*4;
bytes_per_row(r8g8b8a8,W) -> W*4;
bytes_per_row(r16g16b16a16,W) -> W*8.

%% bytes per pixel
bpp(l1) -> 1;
bpp(l2) -> 1;
bpp(l4) -> 1;
bpp(l8) -> 1;
bpp(l16) -> 2;
bpp(r8g8b8) -> 3;
bpp(r16g16b16) -> 6;
bpp(palette1) -> 1;
bpp(palette2) -> 1;
bpp(palette4) -> 1;
bpp(palette8) -> 1;
bpp(l8a8)  -> 2;
bpp(l16a16) -> 4;
bpp(r8g8b8a8) -> 4;
bpp(r16g16b16a16) -> 8.


format(0, 1)  -> l1;
format(0, 2)  -> l2;
format(0, 4)  -> l4;
format(0, 8)  -> l8;
format(0, 16) -> l16;
format(2, 8)  -> r8g8b8;
format(2, 16) -> r16g16b16;
format(3, 1)  -> palette1;
format(3, 2)  -> palette2;
format(3, 4)  -> palette4;
format(3, 8)  -> palette8;
format(4, 8)  -> l8a8;
format(4, 16) -> l16a16;
format(6, 8)  -> r8g8b8a8;
format(6, 16) -> r16g16b16a16.

%% return ColorType and BitDepth from format
color_type(_,l1) -> {0, 1};
color_type(_,l2) -> {0, 2};
color_type(_,l4) -> {0, 4};
color_type(_,l8) -> {0, 8};
color_type(_,l16) -> {0, 16};
color_type(Bpp,l) -> {0, Bpp};
color_type(_,r8g8b8) -> {2, 8};
color_type(_,rgb) -> {2, 8};
color_type(_,r16g16b16) -> {2, 16};
color_type(_,palette1) -> {3, 1};
color_type(_,palette2) -> {3, 2};
color_type(_,palette4) -> {3, 4};
color_type(_,palette8) -> {3, 8};
color_type(_,l8a8) -> {4, 8};
color_type(_,l16a16) -> {4, 16};
color_type(_,r8g8b8a8) -> {6, 8};
color_type(_,rgba) -> {6, 8};
color_type(_,r16g16b16a16) -> {6, 16}.

%% scan for a keyword, null terminated (max 79 characters or fail)
keyword(Bin) ->
    keyword(Bin, 79, []).

keyword(<<0,Bin/binary>>,_I,Key) ->
    {lists:reverse(Key), Bin};
keyword(<<C,Bin/binary>>,I,Key) when I >= 0 ->
    keyword(Bin,I-1,[C|Key]).

%% read palette
plte(<<R,G,B, Data/binary>>) ->
    [{R,G,B} | plte(Data)];
plte(<<>>) -> [].

write_info(Fd, IMG) ->
    [Pixmap] = IMG#epx_image.pixmaps,
    Width  = epx:pixmap_info(Pixmap, width),
    Height = epx:pixmap_info(Pixmap, height),
    Format = epx:pixmap_info(Pixmap, pixel_format),
    Bpp    = epx:pixmap_info(Pixmap, bits_per_pixel),
    MAGIC = <<?MAGIC>>,
    {ColorType,BitDepth} = color_type(Bpp,Format),
    CompressionMethod = 0,
    FilterMethod = 0,
    InterlaceMethod = 0,
    IHDR = png_chunk(<<"IHDR">>,<< Width:32, Height:32,
				   BitDepth:8,ColorType:8,
				   CompressionMethod:8,FilterMethod:8, 
				   InterlaceMethod:8 >>),
    file:write(Fd, <<MAGIC/binary, IHDR/binary>>).


read(Fd, IMG) ->
    file:position(Fd, 8), %% skip magic
    Z = zlib:open(),
    zlib:inflateInit(Z),
    Resp = read_image(Fd, [], {undefined,undefined}, Z),
    zlib:close(Z),
    case Resp of
	{ok, _Binary, _Palette} when
	      element(2,hd(IMG#epx_image.attributes)) =/= 0,
	      false ->
	    {error, interlace_not_supported};
	{ok, Binary, {Palette,Trns}} ->
	    case attribute(IMG, 'Interlace', 0) of
		0 ->
		    {ok,Pixmap} = create_pixmap(IMG, Binary, Palette, Trns),
		    {ok, IMG#epx_image { pixmaps = [Pixmap],
					 palette = Palette }};
		_ ->
		    {error, interlace_not_supported}
	    end;
	Error -> Error
    end.

%% The pixmap is created in a format epx supports, rows in other png
%% formats (palette, 16 bit and 1,2,4 bit gray) are converted.
%% A palette with transparency (tRNS) is expanded to r8g8b8a8
create_pixmap(IMG, Bin, Palette, Trns) ->
    Width = IMG#epx_image.width,
    PngFormat = IMG#epx_image.format,
    PixFormat = case pixmap_format(PngFormat) of
		    r8g8b8 when is_binary(Trns), Palette =/= undefined ->
			r8g8b8a8;
		    PixFormat0 -> PixFormat0
		end,
    Pix0 = epx:pixmap_create(Width,IMG#epx_image.height,PixFormat),
    Bpp = bpp(PngFormat),
    BytesPerRow = bytes_per_row(PngFormat,Width),
    Convert = row_converter(PngFormat, Width, Palette, Trns),
    raw_data(Bin,Pix0,0,Bpp,<<>>,PixFormat,Width,BytesPerRow,Convert).

raw_data(Bin,Pix,Ri,Bpp,Row0,Format,Width,BytesPerRow,Convert) ->
    case Bin of
	<<Filter:8,Row:BytesPerRow/binary,Bin1/binary>> ->
	    Row1 = filter(Filter,Bpp,Row,Row0), %% Filter method=0 assumed
	    epx:pixmap_put_pixels(Pix,0,Ri,Width,1,Format,Convert(Row1)),
	    raw_data(Bin1,Pix,Ri+1,Bpp,Row1,Format,Width,BytesPerRow,Convert);
	_ ->
	    {ok, Pix}
    end.

pixmap_format(l8) -> l8;
pixmap_format(l8a8) -> l8a8;
pixmap_format(r8g8b8) -> r8g8b8;
pixmap_format(r8g8b8a8) -> r8g8b8a8;
pixmap_format(l16a16) -> l8a8;
pixmap_format(r16g16b16) -> r8g8b8;
pixmap_format(r16g16b16a16) -> r8g8b8a8;
pixmap_format(palette1) -> r8g8b8;
pixmap_format(palette2) -> r8g8b8;
pixmap_format(palette4) -> r8g8b8;
pixmap_format(palette8) -> r8g8b8;
pixmap_format(_) -> l8.  %% l1,l2,l4,l16

row_converter(Format, Width, Palette, Trns) ->
    case Format of
	l8 -> fun(Row) -> Row end;
	l8a8 -> fun(Row) -> Row end;
	r8g8b8 -> fun(Row) -> Row end;
	r8g8b8a8 -> fun(Row) -> Row end;
	l16 -> fun high_bytes/1;
	l16a16 -> fun high_bytes/1;
	r16g16b16 -> fun high_bytes/1;
	r16g16b16a16 -> fun high_bytes/1;
	l1 -> fun(Row) -> gray_row(Row, 1, Width) end;
	l2 -> fun(Row) -> gray_row(Row, 2, Width) end;
	l4 -> fun(Row) -> gray_row(Row, 4, Width) end;
	palette1 -> palette_row(1, Width, Palette, Trns);
	palette2 -> palette_row(2, Width, Palette, Trns);
	palette4 -> palette_row(4, Width, Palette, Trns);
	palette8 -> palette_row(8, Width, Palette, Trns)
    end.

high_bytes(Row) ->
    << <<H>> || <<H:8,_:8>> <= Row >>.

gray_row(Row, Bits, Width) ->
    Max = (1 bsl Bits) - 1,
    << <<((V*255) div Max)>> || <<V:Bits>> <= first_bits(Row,Bits*Width) >>.

palette_row(Bits, Width, Palette, Trns) ->
    Colors = palette_colors(palette_list(Palette), Trns),
    N = tuple_size(Colors),
    Black = if is_binary(Trns) -> <<0,0,0,0>>; true -> <<0,0,0>> end,
    fun(Row) ->
	    << <<(palette_color(I, Colors, N, Black))/binary>> ||
		<<I:Bits>> <= first_bits(Row, Bits*Width) >>
    end.

palette_list(undefined) -> [];
palette_list(Palette) -> Palette.

%% tRNS hold alpha for the first entries, the rest are opaque
palette_colors(Palette, Trns) when is_binary(Trns) ->
    Alpha = binary_to_list(Trns) ++ lists:duplicate(length(Palette), 255),
    list_to_tuple([<<R,G,B,A>> ||
		      {{R,G,B},A} <- lists:zip(Palette,
					       lists:sublist(Alpha,
							     length(Palette)))]);
palette_colors(Palette, _) ->
    list_to_tuple([<<R,G,B>> || {R,G,B} <- Palette]).

palette_color(I, Colors, N, _Black) when I < N -> element(I+1, Colors);
palette_color(_I, _Colors, _N, Black) -> Black.

%% skip padding bits at end of row
first_bits(Row, NBits) ->
    <<Bits:NBits/bitstring, _/bitstring>> = Row,
    Bits.

filter(0,_,Row,_Prior) -> Row;
filter(1, Bpp, Row, Prior) -> filter_sub(Row,Prior,Bpp);
filter(2, Bpp, Row,Prior) -> filter_up(Row,Prior,Bpp);
filter(3, Bpp, Row,Prior) -> filter_avg(Row,Prior,Bpp);
filter(4, Bpp, Row,Prior) -> filter_paeth(Row,Prior,Bpp).

%%
%% Raw(x) = Sub(x) + Raw(x-bpp)  [ Sub(x) = Raw(x) - Raw(x-bpp) ]
%%    
filter_sub(Sub,_Prior,Bpp) ->
    Rn = lists:duplicate(Bpp, 0),
    Rm = [],
    filter_sub(Sub, 0, size(Sub), [], Rn, Rm).

filter_sub(_Sub, X, X, Acc, _, _) ->
    list_to_binary(reverse(Acc));
filter_sub(Sub, X, N, Acc, [Rxb|Rn], Rm) ->
    <<_:X/binary, Sx:8, _/binary>> = Sub,
    Rx = (Sx + Rxb) band 16#ff,
    if Rn == [] ->
	    filter_sub(Sub,X+1,N,[Rx|Acc],reverse([Rx|Rm]),[]);
       true ->
	    filter_sub(Sub,X+1,N,[Rx|Acc],Rn,[Rx|Rm])
    end.
%%
%% Raw(x) = Up(x) + Prior(x) [ Up(x) = Raw(x) - Prior(x) ]
%% 
filter_up(Up, Prior, _Bpp) ->
    filter_up(Up, Prior, 0, size(Up), []).
    
filter_up(_Up, _Prior, X, X, Acc) ->
    list_to_binary(reverse(Acc));
filter_up(Up, Prior, X, N, Acc) ->
    <<_:X/binary,Ux:8,_/binary>> = Up,
    Px = case Prior of
	     <<_:X/binary,Pi,_/binary>> -> Pi;
	     _ -> 0
	 end,
    Rx = (Ux + Px) band 16#ff,
    filter_up(Up,Prior,X+1,N,[Rx|Acc]).

%%
%% Raw(x) = Avarage(x) + floor((Raw(x-bpp)+Prior(x))/2)
%%    [ Avarage(x) = Raw(x) - floor((Raw(x-bpp)+Prior(x))/2) ]
%%

filter_avg(Avg, Prior,Bpp) ->
    Rn = lists:duplicate(Bpp, 0),
    Rm = [],
    filter_avg(Avg, Prior,  0, size(Avg), [], Rn, Rm).

filter_avg(_Avg,_Prior, X, X, Acc, _, _) ->
    list_to_binary(reverse(Acc));
filter_avg(Avg, Prior, X, N, Acc, [Rxb|Rn], Rm) ->
    <<_:X/binary, Ax:8, _/binary>> = Avg,
    Px = case Prior of
	     <<_:X/binary,Pi,_/binary>> -> Pi;
	     _ -> 0
	 end,
    Rx = (Ax + ((Rxb+Px) div 2)) band 16#ff,
    if Rn == [] ->
	    filter_avg(Avg,Prior,X+1,N,[Rx|Acc],reverse([Rx|Rm]),[]);
       true ->
	    filter_avg(Avg,Prior,X+1,N,[Rx|Acc],Rn,[Rx|Rm])
    end.

%%
%% Paeth(x) = Raw(x) -
%%            PaethPredictor(Raw(x-bpp),Prior(x),Prior(x-bpp))
%%
%% Raw(x) = Paeth(x) + PaethPredictor(Raw(x-bpp),Prior(x),Prior(x-bpp))
%%
filter_paeth(Pae,Prior,Bpp) ->
    Pn = Rn = lists:duplicate(Bpp, 0),
    Pm = Rm = [],
    filter_pae(Pae, Prior, 0, size(Pae), [], Rn, Rm, Pn, Pm).


filter_pae(_Pae, _Prior, X, X, Acc, _Rn, _Rm, _Pn, _Pm) ->
    list_to_binary(reverse(Acc));
filter_pae(Pae, Prior, X, N, Acc, [Rxb|Rn], Rm, [Pxb|Pn], Pm) ->
    <<_:X/binary, PAx:8, _/binary>> = Pae,
    Px = case Prior of
	     <<_:X/binary,Pi,_/binary>> -> Pi;
	     _ -> 0
	 end,
    Rx = (PAx + paethPredictor(Rxb, Px, Pxb)) band 16#ff,
    if Rn == [] ->
	    filter_pae(Pae,Prior,X+1,N,[Rx|Acc],
		       reverse([Rx|Rm]),[],
		       reverse([Px|Pm]),[]);
       true ->
	    filter_pae(Pae,Prior,X+1,N,[Rx|Acc],
		       Rn,[Rx|Rm],
		       Pn,[Px|Pm])
    end.

-define(dabs(X,Y),
	if (X) > (Y) -> (X) - (Y);
	   true -> (Y) - (X)
	end).
		
paethPredictor(A,B,C) ->
    P = A + B - C,
    PA = ?dabs(P,A),
    PB = ?dabs(P,B),
    PC = ?dabs(P,C),
    if PA =< PB, PA =< PC -> A;
       PB =< PC -> B;
       true -> C
    end.
	    
        
write(Fd, IMG) ->
    [Pixmap] = IMG#epx_image.pixmaps,
    write_pixmap(Fd, Pixmap).

write_file(Filename, Pixmap) ->
    {ok, Fd} = file:open(Filename, [write, binary]),
    try write_pixmap(Fd, Pixmap) of
	Res -> Res
    after
	file:close(Fd)
    end.
	     

write_pixmap(Fd, Pixmap) ->
    Width  = epx:pixmap_info(Pixmap, width),
    Height = epx:pixmap_info(Pixmap, height),
    Format = epx:pixmap_info(Pixmap, pixel_format),
    Bpp    = epx:pixmap_info(Pixmap, bits_per_pixel),
    PixelData = get_pixel_data(Pixmap,Width,Height),
    write_data(Fd,PixelData,Width,Height,Format,Bpp).

write_data(Fd, PixelData, Width, Height, Format) ->
    write_data(Fd, PixelData, Width, Height, Format, 0).

write_data(Fd, PixelData, Width, Height, Format, Bpp) ->
    write_data(Fd, PixelData, Width, Height, Format, Bpp, 0, 0, 0).

write_data(Fd, PixelData, Width, Height, Format,
	   CompressionMethod, FilterMethod, InterlaceMethod) ->
    write_data(Fd, PixelData, Width, Height, Format, 0,
	       CompressionMethod, FilterMethod, InterlaceMethod).
    
write_data(Fd, PixelData, Width, Height, Format, Bpp,
	   CompressionMethod, FilterMethod, InterlaceMethod) ->
    PngData = format_data(PixelData, Width, Height, Format, Bpp,
			  CompressionMethod, FilterMethod, InterlaceMethod),
    file:write(Fd, PngData).

format_data(PixelData, Width, Height, Format) ->
    format_data(PixelData, Width, Height, Format, 0).

format_data(PixelData, Width, Height, Format, Bpp) ->
    format_data(PixelData, Width, Height, Format, Bpp, 0,0,0).

format_data(PixelData, Width, Height, Format,
	    CompressionMethod, FilterMethod, InterlaceMethod) ->
    format_data(PixelData, Width, Height, Format, 0, 
		CompressionMethod, FilterMethod, InterlaceMethod).

format_data(PixelData, Width, Height, Format, Bpp,
	    CompressionMethod, FilterMethod, InterlaceMethod) ->
    {ColorType,BitDepth} = color_type(Bpp, Format), 
    MAGIC = <<?MAGIC>>,
    IHDR = png_chunk(<<"IHDR">>,<< Width:32, Height:32,
				   BitDepth:8,ColorType:8,
				   CompressionMethod:8,FilterMethod:8, 
				   InterlaceMethod:8 >>),
    IDAT = png_chunk(<<"IDAT">>, PixelData),
    IEND = png_chunk(<<"IEND">>, <<>>),
    <<MAGIC/binary, IHDR/binary, IDAT/binary, IEND/binary>>.

%% use this function if pixel data is packed like R,G,B
format_pixel_data(Data,RowBytes) ->
    zlib:compress([[<<0>>,Row] || <<Row:RowBytes/binary>> <= Data ]).

get_pixel_data(Pixmap,W,H) ->
    Pixels = get_pixels(Pixmap,W,H),
    zlib:compress(Pixels).

get_pixels(Pixmap,W,H) ->
    get_pixels(Pixmap,0,0,W,H).

get_pixels(Pixmap,X,Y,W,H) when Y < H ->
    Row = epx:pixmap_get_pixels(Pixmap,X,Y,W,1),
    [<<0>>,Row | get_pixels(Pixmap,X,Y+1,W,H)];
get_pixels(_Pixmap,_X,_Y,_W,_H) ->
    [].

%% format a png chunk
png_chunk(Type, Bin) ->
    Length = byte_size(Bin),
    CRC = erlang:crc32(<<Type/binary, Bin/binary>>),
    <<Length:32, Type/binary, Bin/binary, CRC:32>>.

read_image(Fd, Acc, Palette, Z) ->
    case read_chunk_hdr(Fd) of
	{ok, Length, ?IDAT} ->
	    case read_chunk_crc(Fd, Length) of
		{ok, CBin} ->
		    Blocks = zlib:inflate(Z, CBin),
		    read_image(Fd, [Blocks|Acc], Palette, Z);
		Error -> Error
	    end;
	{ok, _Length, ?IEND} ->
	    zlib:inflateEnd(Z),
	    {ok, list_to_binary(reverse(Acc)), Palette};

	{ok, Length, ?PLTE} ->
	    case read_chunk_crc(Fd, Length) of
		{ok, Chunk} ->
		    {_, Trns} = Palette,
		    read_image(Fd, Acc, {plte(Chunk),Trns}, Z);
		Error ->
		    Error
	    end;
	{ok, Length, ?tRNS} ->
	    case read_chunk_crc(Fd, Length) of
		{ok, Chunk} ->
		    {Plte, _} = Palette,
		    read_image(Fd, Acc, {Plte,Chunk}, Z);
		Error ->
		    Error
	    end;
	{ok, Length, _} ->
	    skip_chunk(Fd, Length),
	    read_image(Fd, Acc, Palette, Z)
    end.

%%
%% Given chunk header read chunk and check crc
%%
read_chunk_crc(Fd, Length) ->
    file:position(Fd, {cur,-4}),
    LengthWithType = Length+4,
    case file:read(Fd, LengthWithType+4) of
	{ok,<<TypeChunk:LengthWithType/binary, CRC:32>>} ->
	    case valid_crc32(TypeChunk, CRC) of
		true -> 
		    <<_:32, Chunk/binary>> = TypeChunk,
		    {ok, Chunk};
		false ->
		    {error, bad_crc}
	    end;
	{ok,_} ->
	    {error, bad_chunk};
	Error ->
	    Error
    end.

%%
%% Read the chunk header
%%    

read_chunk_hdr(Fd) ->
    case file:read(Fd, 8) of
	{ok, <<Length:32, Type:4/binary>>} ->
	    Tag = binary_to_list(Type),
	    ?dbg("chunk: type = ~p, length=~p\n", [Tag,Length]),
	    {ok, Length, Tag};
	Error ->
	    Error
    end.


skip_chunk(Fd, Length) ->
    file:position(Fd, {cur,Length+4}).

valid_crc32(Binary, CRC32) ->
    %% Z = zlib:open(),
    %% Value = zlib:crc32(Z, Binary),
    Value = erlang:crc32(Binary),
    %% zlib:close(Z),
    ?dbg("crc check: ~p == ~p\n", [CRC32, Value]),
    CRC32 == Value.

