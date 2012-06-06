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
%%%    BMP Image processing
%%% @end
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(epx_image_bmp).

-behaviour(epx_image).
-export([magic/1, mime_type/0, extensions/0,
	 read_info/1, write_info/2,
	 read/2, write/2]).

-include("../include/epx_image.hrl").
-include("dbg.hrl").

-import(lists, [reverse/1]).

-record(bmp_file_header,
	{
	  magic,   %% 2 bytes, "BM"/"BA", "CI", "CP", "IC", PT"
	  size,    %% 4 bytes
	  app1,    %% 2 bytes
	  app2,    %% 2 bytes
	  offset   %% 4 bytes
	 }).

-record(bmp_dib_header,
	{
	  hdr_size, %% :32 size of this header 
	  width,    %% :32
	  height,   %% :32
	  planes,   %% :16,
	  bpp,      %% :16   bit per pixel
	  comp,     %% :32   compression method
	  size,     %% :32   image size
	  hres,     %% :32   pixel / meter
	  vres,     %% :32   pixel / meter
	  colors,   %% :32   number of entries in palette
	  important %% :32   important colors
	 }).


magic(<<$B,$M, _/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/bmp".

extensions() -> [".bmp" ].


read_info(Fd) ->
    case read_file_header(Fd) of
	{ok, #bmp_file_header { magic = "BM" }} ->
	    case read_dib_header(Fd) of
	        {ok, DIB=#bmp_dib_header {}} ->
		    As = [{'Compression', DIB#bmp_dib_header.comp},
			  {'ImageSize', DIB#bmp_dib_header.size},
			  {'VRes', DIB#bmp_dib_header.vres},
			  {'HRes', DIB#bmp_dib_header.hres},
			  {'ImportantColors', DIB#bmp_dib_header.important}],
		    {ok, #epx_image  { type      = ?MODULE,
				       width     = DIB#bmp_dib_header.width,
				       height    = DIB#bmp_dib_header.height,
				       depth     = DIB#bmp_dib_header.bpp,
				       format    = format(DIB),
				       alignment = 4,
				       order = left_to_right,
				       attributes = As
				      }};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

read_file_header(Fd) ->
    case file:read(Fd, 14) of
	{ok, <<M1:8,M2:8,
	      FileSz:32/little, 
	      App1:16/little, 
	      App2:16/little, 
	      Offset:32/little>>} ->
	    {ok, #bmp_file_header { magic = [M1,M2],
				    size = FileSz,
				    app1 = App1,
				    app2 = App2,
				    offset = Offset }};
	{ok, _} ->
	    {error, bad_magic};
	Error ->
	    Error
    end.

read_dib_header(Fd) ->
    case file:read(Fd, 4) of
	{ok, <<HSize:32/little>>} ->
	    case file:read(Fd, HSize-4) of
		{ok,<<Width:32/little, 
		     Height:32/little, 
		     Planes:16/little,
		     BitsPerPixel:16/little, 
		     Compression:32/little, 
		     ImageSize:32/little,
		     HRes:32/little,
		     VRes:32/little,
		     ColorsUsed:32/little, 
		     ImportantColors:32/little>> } ->
		    {ok, #bmp_dib_header { hdr_size=HSize,
					   width = Width,
					   height = Height,
					   planes = Planes,
					   bpp = BitsPerPixel,
					   comp = Compression,
					   size = ImageSize,
					   hres = HRes,
					   vres = VRes,
					   colors = ColorsUsed,
					   important = ImportantColors }};
		{ok, _} ->
		    {error, bad_magic};
		Error ->
		    Error
	    end;
	{ok, _} ->
	    {error, truncated};
	Error ->
	    Error
    end.


%% determine the epx_image format
bits_per_row(gray1,W) -> W;
bits_per_row(gray2,W) -> W*2;
bits_per_row(gray4,W) -> W*4;
bits_per_row(gray8,W) -> W*8;
bits_per_row(r8g8b8,W) -> W*24;
bits_per_row(b8g8r8,W) -> W*24;
bits_per_row(palette1,W) -> W;
bits_per_row(palette4,W) -> W*4;
bits_per_row(palette8,W) -> W*8.

format(DIB) ->
    format(DIB#bmp_dib_header.colors, DIB#bmp_dib_header.bpp).

format(0, 1)  -> gray1;
format(0, 4)  -> gray4;
format(0, 8)  -> gray8;
format(0, 24) -> b8g8r8;
format(_, 1)  -> palette1;
format(_, 4)  -> palette4;
format(_, 8)  -> palette8.

write_info(_Fd, _IMG) ->
    ok.

read(Fd, IMG) ->
    file:position(Fd, 0),
    case read_file_header(Fd) of
	{ok, FH=#bmp_file_header { magic = "BM" }} ->
	    case read_dib_header(Fd) of
	        {ok, DIB=#bmp_dib_header {}} ->
		    As = [{'Compression', DIB#bmp_dib_header.comp},
			  {'ImageSize', DIB#bmp_dib_header.size},
			  {'VRes', DIB#bmp_dib_header.vres},
			  {'HRes', DIB#bmp_dib_header.hres},
			  {'ImportantColors', DIB#bmp_dib_header.important}],
		    Palette = read_palette(Fd, DIB#bmp_dib_header.colors),
		    IMG1 = IMG#epx_image { palette = Palette, attributes = As },
		    %% io:format("Offset=~p\n", [FH#bmp_file_header.offset]),
		    %% {ok,Pos} = file:position(Fd, {cur,0}),
		    file:position(Fd, FH#bmp_file_header.offset),
		    %% io:format("Offset=~p\n", [FH#bmp_file_header.offset]),
		    %% io:format("Position=~p\n", [Pos]),
		    case read_pixels(Fd, IMG1) of
			{ok,PIX} ->
			    {ok, IMG1#epx_image { pixmaps = [PIX] }};
			Error -> Error
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

read_palette(_Fd, 0) ->
    undefined;
read_palette(Fd,  Sz) ->
    case file:read(Fd, Sz*4) of
	{ok, Bin} -> 
	    rd_palette(Bin, [], Sz)
    end.

rd_palette(_Bin, Map, 0) -> 
    reverse(Map);
rd_palette(<<R:8,G:8,B:8,_:8, Bin/binary>>, Map, I) ->
    rd_palette(Bin, [{R,G,B} | Map], I-1).


%% save image
write(_Fd, _IMG) ->
    ok.

%% Read all rows
read_pixels(Fd, IMG) ->
    Width = IMG#epx_image.width,
    Height = IMG#epx_image.height,
    BitsPerRow = bits_per_row(IMG#epx_image.format, Width),
    BytesPerRow = 4*((BitsPerRow + 31) div 32),
    Pixmap = epx:pixmap_create(Width, Height, IMG#epx_image.format),
    read_pixels(Fd, Pixmap, Height, BytesPerRow, Width, IMG#epx_image.format).


read_pixels(_Fd, Pixmap, I, _BytesPerRow, _Width, _Format) when I =< 0 ->
    {ok,Pixmap};
read_pixels(Fd, Pixmap, I, BytesPerRow, Width, Format) ->
    Ri = I - 1,
    case file:read(Fd, BytesPerRow) of
	{ok,Row} ->
	    epx:pixmap_put_pixels(Pixmap,0,Ri,Width,1,Format,Row),
	    read_pixels(Fd, Pixmap, Ri, BytesPerRow, Width, Format);
	Error ->
	    Error
    end.






    
