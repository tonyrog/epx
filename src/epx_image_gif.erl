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
%%%    GIF image processing
%%% @end
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(epx_image_gif).

-behaviour(epx_image).
-export([magic/1, mime_type/0, extensions/0,
	 read_info/1, write_info/2,
	 read/2, write/2]).

-include("../include/epx_image.hrl").

%% -define(debug, true).
-include("dbg.hrl").

-import(lists, [reverse/1]).


-define(APP_EXTENSION, 16#ff).
-define(COM_EXTENSION, 16#fe).
-define(CTL_EXTENSION, 16#f9).
-define(TXT_EXTENSION, 16#01).

-define(EXTENSION, 16#21).   %% $!
-define(IMAGE,   16#2c).     %% $,
-define(TRAILER, 16#3b).     %% $;

%% Read magic info check MAGIC type and width and height (depth)
%% of image
-define(MAGIC87, $G,$I,$F,$8,$7,$a).
-define(MAGIC89, $G,$I,$F,$8,$9,$a).

magic(<<?MAGIC87,_/binary>>) -> true;
magic(<<?MAGIC89,_/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/gif".

extensions() -> [ ".gif" ].
    

read_info(Fd) ->
    case file:read(Fd, 10) of
	{ok, <<?MAGIC87,
	      Width:16/little-unsigned-integer,
	      Height:16/little-unsigned-integer,_/binary>>} ->
	    {ok,#epx_image { type = ?MODULE,
			   width = Width,
			   height = Height,
			   format = palette8,
			   order = left_to_right,
			   depth = 8 }};
	{ok, <<?MAGIC89,
	      Width:16/little-unsigned-integer,
	      Height:16/little-unsigned-integer,_/binary>>} ->
	    {ok,#epx_image { type = ?MODULE,
			   width = Width,
			   height = Height,
			   format = palette8,
			   order = left_to_right,
			   depth  = 8 }};
	{ok, _} ->
	    {error, bad_magic};
	Error ->
	    Error
    end.

write_info(Fd, IMG) ->
    %% Should version be configurable?
    file:write(Fd, <<?MAGIC89,
		    (IMG#epx_image.width):16/little-unsigned-integer,
		    (IMG#epx_image.height):16/little-unsigned-integer>>).


%% The Grammar.
%% <GIF Data Stream> ::=     Header <Logical Screen> <Data>* Trailer
%%
%% <Logical Screen> ::=      Logical Screen Descriptor [Global Color Table]
%%
%% <Data> ::=                <Graphic Block>  |
%%                           <Special-Purpose Block>
%%
%% <Graphic Block> ::=  [Graphic Control Extension] <Graphic-Rendering Block>
%%
%% <Graphic-Rendering Block> ::=  <Table-Based Image>  |
%%                                Plain Text Extension
%%
%% <Table-Based Image> ::=   Image Descriptor [Local Color Table] Image Data
%%
%% <Special-Purpose Block> ::=    Application Extension  |
%%                                Comment Extension

read(Fd,IMG) ->
    file:position(Fd, 6),
    case file:read(Fd, 7) of
	{ok, <<_Width:16/little, _Hight:16/little,
	      Map:1, _Cr:3, Sort:1, Pix:3,
	      Background:8,
	      AspectRatio:8>>} ->
	    Palette = read_palette(Fd, Map =:= 1, Pix+1),
	    ?dbg("sizeof(palette)=~p Map=~w, Cr=~w, Sort=~w, Pix=~w\n", 
		 [tuple_size(Palette),Map,_Cr,Sort,Pix]),
	    ?dbg("Background=~w, AspectRatio=~w\n",
		 [Background, AspectRatio]),
	    As = [{'Background',Background},
		  {'AspectRatio',AspectRatio},
		  {'Sort',Sort} | IMG#epx_image.attributes],
	    IMG1 = IMG#epx_image { palette = Palette, attributes = As},
	    read_data(Fd, IMG1, []);
	Error ->
	    Error
    end.

read_data(Fd, IMG, As) ->
    case file:read(Fd, 1) of
	{ok, <<?EXTENSION>>} ->
	    ?dbg("Extension\n",[]),
	    read_extension(Fd, IMG, As);
	{ok, <<?IMAGE>>} ->
	    ?dbg("Image\n",[]),
	    case file:read(Fd, 9) of
		{ok, <<Left:16/little, Top:16/little,
		       Width:16/little, Height:16/little,
		       Map:1, Interlaced:1, Sort:1,_:2, Pix:3>>} ->
		    Palette = read_palette(Fd, Map =:= 1, Pix+1),
		    As1 = [{'Interlaced', Interlaced},{'Sort', Sort}|
			   IMG#epx_image.attributes],
		    Pixmap = epx:pixmap_create(Width,Height,rgba),
		    Background = proplists:get_value('Background',As1,0),

		    Palette1 = select_palette(Palette,IMG#epx_image.palette),
		    Ps = {Palette1,
			  proplists:get_value('Transparent',As1,false),
			  proplists:get_value('TransparentColor',As1,
					      Background),
			  Background},
		    case read_pixels(Fd,Pixmap,Ps,Width,Height,
				     Interlaced =:= 1) of
			{ok, Pixmap1} ->
			    %% ?dbg("Pixmap = ~p\n", [Pixmap2]),
			    Pxs = if Top =:= 0, Left =:= 0 ->
					  IMG#epx_image.pixmaps ++ [Pixmap1];
				     true ->
					  IMG#epx_image.pixmaps ++ 
					      [{Pixmap1,Top,Left}]
				  end,
			    read_data(Fd, IMG#epx_image { pixmaps = Pxs },
				      []);
			Error -> Error
		    end;
		Error -> Error
	    end;
	{ok, <<?TRAILER>>} ->
	    ?dbg("Trailer\n",[]),
	    {ok, IMG};
	Error -> 
	    Error
    end.


read_extension(Fd,IMG,As) ->
    case file:read(Fd, 1) of
	{ok,<<?COM_EXTENSION>>} ->
	    read_comment(Fd,IMG,As);
	{ok,<<?APP_EXTENSION>>} ->
	    read_app(Fd,IMG,As);
	{ok,<<?CTL_EXTENSION>>} ->
	    read_ctl(Fd,IMG,As);
	{ok,<<?TXT_EXTENSION>>} ->
	    read_txt(Fd,IMG,As);
	{ok, _} ->
	    read_blocks(Fd), %% skip
	    read_data(Fd,IMG,As)
    end.


read_ctl(Fd,IMG,As) ->
    ?dbg("Control block\n",[]),
    case read_block(Fd) of
	{ok, <<_:3, DisposalMethod:3, UserInput:1, Transparent:1,
	      DelayTime:16/unsigned-little,
	      TransparentColor:8>>} ->
	    case read_block(Fd) of
		terminator ->
		    As1 = [{'TransparentColor', TransparentColor},
			   {'DelayTime', DelayTime},
			   {'UserInput', UserInput},
			   {'Transparent', Transparent =/= 0 },
			   {'DisposalMethod', DisposalMethod} |
			   IMG#epx_image.attributes ],
		    read_data(Fd,IMG#epx_image { attributes = As1},
			      As);
		{ok,_} ->
		    {error, bad_ctl_block};
		Error -> Error
	    end;
	Error -> Error
    end.


read_comment(Fd,IMG,As) ->
    ?dbg("Comment block\n",[]),
    case read_blocks(Fd) of
	{ok, Comment} ->
	    read_data(Fd, 
		      IMG#epx_image { comment = binary_to_list(Comment)},
		      As);
	Error -> Error
    end.


read_txt(Fd,IMG,As) ->
    ?dbg("Text block\n",[]),
    case read_block(Fd) of
	{ok, <<GridLeft:16/little, GridTop:16/little,
	       _GridWidth:16/little, _GridHeight:16/little,
	       CellWidth:8, CellHeight:8,
	      Foreground:8, Background:8>>} ->
	    case read_blocks(Fd) of
		{ok,Bin} ->
		    As1 = 
			[{'TextGridLeftPosition', GridLeft},
			 {'TextGridTopPosition', GridTop},
			 {'CharacterCellWidth', CellWidth},
			 {'CharacterCellHeight', CellHeight},
			 {'TextForegroundColorIndex', Foreground},
			 {'TextBackgroundColorIndex', Background},
			 {'Text', binary_to_list(Bin)} | As],
		    read_data(Fd,IMG,As1);
		Error ->
		    Error
	    end;
	terminator ->
	    {error, bad_txt_block};
	Error -> Error
    end.
		    
read_app(Fd,IMG,As) ->
    ?dbg("Application block\n",[]),
    case read_block(Fd) of
	{ok, <<Ident:8/binary, AuthCode:3/binary>>} ->
	    case read_blocks(Fd) of
		{ok, AppData} ->
		    As1 =
			[{'ApplicationIdentifier', binary_to_list(Ident)},
			 {'ApplicationAuthenticationCode',
			  binary_to_list(AuthCode)},
			 {'ApplicationData', AppData} | 
			 IMG#epx_image.attributes],
		    read_data(Fd,IMG#epx_image { attributes = As1},
			      As);
		Error ->
		    Error
	    end;
	terminator ->
	    {error, bad_app_block};
	Error ->
	     Error
    end.
	
%%
%% Read One block
%% return 
%%        {ok, Block}
%%        terminator
%%      | {error,Reason}
%%
%%      
%%
read_block(Fd) ->
    case file:read(Fd, 1) of
	{ok, <<0>>} -> 
	    terminator;
	{ok, <<Size>>} ->
	    file:read(Fd, Size);
	Error ->
	    Error
    end.


%%
%% Read a list of blocks
%%
read_blocks(Fd) ->
    read_blocks(Fd,<<>>).

read_blocks(Fd,Acc) ->
    case read_block(Fd) of
	{ok,Bin} -> read_blocks(Fd,<<Acc/binary,Bin/binary>>);
	terminator -> {ok, Acc};
	Error -> Error
    end.



read_palette(_Fd, false, _Pixel) -> 
    undefined;
read_palette(Fd, true, Pixel) ->
    Sz = (1 bsl Pixel),
    case file:read(Fd, Sz*3) of
	{ok, Bin} -> 
	    rd_palette(Bin, [], Sz)
    end.

rd_palette(_Bin, Map, 0) -> 
    list_to_tuple(reverse(Map));
rd_palette(<<R:8,G:8,B:8, Bin/binary>>, Map, I) ->
    rd_palette(Bin, [{R,G,B} | Map], I-1).

write(_Fd, _IMG) ->
    exit(nyi).

-ifdef(__not_defined__).

write(Fd, IMG) ->
    write_info(Fd, IMG),
    Palette = IMG#epx_image.palette,
    Background = attribute('Background',IMG#epx_image.attributes,0),
    AspectRatio = attribute('AspectRatio', IMG#epx_image.attributes,0),
    if is_tuple(Palette) ->
	    PLen = tuple_size(Palette),
	    ColorRes = if PLen > 0, PLen =< 256 ->
			       trunc(math:log(PLen)/math:log(2))+1;
			  PLen > 0 ->
			       8;
			  true -> 
			       1
		       end,
	    Map = 1,
	    Cr = ColorRes - 1,
	    Sort = 0,
	    Pix = ColorRes - 1,
	    file:write(Fd, <<Map:1, Cr:3, Sort:1, Pix:3>>),
	    file:write(Fd, <<Background:8, AspectRatio:8>>),	    
	    write_palette(Fd, IMG#epx_image.palette, Pix+1);
       true ->
	    Map = 0,
	    Cr = 0,
	    Sort = 0,
	    Pix = 0,
	    file:write(Fd, <<Map:1, Cr:3, Sort:1, Pix:3>>),
	    file:write(Fd, <<Background:8, AspectRatio:8>>)	    
    end,
    write_data(Fd, IMG),
    file:write(Fd, <<?TRAILER>>).


write_palette(Fd, Map, Pixel) ->
    wr_palette(Fd, Map, (1 bsl Pixel)).

wr_palette(_Fd, _, 0) -> ok;
wr_palette(Fd, [{R,G,B}|Map], I) ->
    file:write(Fd, <<R:8, G:8, B:8>>),
    wr_palette(Fd, Map, I-1);
wr_palette(Fd, [], I) ->
    file:write(Fd, <<0:8, 0:8, 0:8>>),
    wr_palette(Fd, [], I-1).

write_data(Fd, IMG) ->
    write_pixmaps(Fd, IMG, IMG#epx_image.pixmaps).

write_pixmaps(Fd, IMG, [Pm|Pms]) ->
    DisposalMethod = attribute('DisposalMethod',IMG#epx_image.attributes, 0),
    UserInput = attribute('UserInput', IMG#epx_image.attributes, 0),
    DelayTime = attribute('DelayTime', IMG#epx_image.attributes, 0),
    Transparent = attribute('Transparent', IMG#epx_image.attributes, 0),
    TransparentColor = attribute('TransparentColor', 
				 IMG#epx_image.attributes, 0),
    file:write(Fd, <<?EXTENSION, ?CTL_EXTENSION>>),
    write_blocks(Fd, <<0:3, DisposalMethod:3, 
		      UserInput:1, Transparent:1, 
		      DelayTime:16/unsigned-little,
		      TransparentColor:8>>),
    write_image(Fd, IMG, Pm),
    write_pixmaps(Fd, IMG, Pms);
write_pixmaps(_Fd, _IMG, []) ->
    ok.


write_image(Fd, IMG, Pm) ->
    file:write(Fd, <<?IMAGE>>),
    Left = 0, %% pixmap.left)
    Top = 0,  %% pixmap.top
    Width = epx:pixmap_info(Pm, width),
    Height = epx:pixmap_info(Pm, height),
    file:write(Fd,
	       <<Left:16/little,Top:16/little,
		 Width:16/little, Height:16/little>>),
    Palette = IMG#epx_image.palette,
    Interlaced = attribute('Interlaced', IMG#epx_image.attributes, 0),
    %% Special code for none compressed data!!!
    Inline     = attribute('Inline', IMG#epx_image.attributes, 0),
    if is_list(Palette) ->
	    PLen = length(Palette),
	    ColorRes = if PLen > 0, PLen =< 256 ->
			       trunc(math:log(PLen)/math:log(2))+1;
			  PLen > 0 ->
			       8;
			  true -> 
			       1
		       end,
	    Sort = 0,
	    Pix = ColorRes - 1,
	    Map = 1,
	    file:write(Fd, <<Map:1, Interlaced:1, Sort:1, 0:2, Pix:3>>),
	    write_palette(Fd, Palette, Pix+1);
       true ->
	    Sort = 0,
	    Pix = 0,
	    Map = 0,
	    file:write(Fd, <<Map:1, Interlaced:1, Sort:1, 0:2, Pix:3>>)
    end,
    write_pixels(Fd,
		 Pm#ei_pixmap.pixels, 
		 Pm#ei_pixmap.width, 
		 Pm#ei_pixmap.height, Interlaced, Inline).

write_pixels(Fd, Pixels, Width, Height, Interlaced, Inline) ->
    Bin = collect_pixels(Pixels, Width, Height, Interlaced),
    {LZWCodeSize, Bin1} = 
	if Inline == 1 ->
		%% FIXME: check that all pixels are 7 bit !!!!!
		{7,<<128, Bin/binary, 129>>};
	   true ->
		lzw:compress_gif(Bin)
	end,
    ?dbg("compress: orig_size=~w, size=~w codesize=~w\n",
	 [size(Bin), size(Bin1), LZWCodeSize]),
    file:write(Fd, <<LZWCodeSize>>),
    write_blocks(Fd, Bin1).

%%
%% Fixme check that all rows are present and
%% implement interlaced order
%%
collect_pixels(Rows, Width, Height, Interlaced) ->
    SortedRows = lists:sort(Rows),
    if Interlaced == 1 ->
	    collect_interlaced(SortedRows,Width,Height,[],[],[],[]);
       true ->
	    collect_raw(SortedRows,Width,Height,[])
    end.

collect_raw([{Ri,Row} | Rows], Width, Height,Acc) when Ri < Height ->
    Sz = size(Row),
    R = if Sz > Width ->
		%% remove pixels
		<<Bin:Width/binary, _/binary>> = Row,
		Bin;
	   Sz < Width ->
		%% add pixels
		<<Row/binary, 
		 (list_to_binary(lists:duplicate(Width-Sz,0)))/binary>>;
	   true ->
		Row
	end,
    collect_raw(Rows, Width, Height, [R | Acc]);
collect_raw([{_Ri,_Row} | Rows], Width, Height, Acc) ->
    %% ignore line out of range
    collect_raw(Rows, Width, Height, Acc);
collect_raw([], _Width, _Height, Acc) ->
    list_to_binary(reverse(Acc)).

collect_interlaced([{Ri,Row}|Rows],Width,Height,R1,R2,R3,R4) ->
    case Ri band 7 of
	0 -> collect_interlaced(Rows,Width,Height,[Row|R1],R2,R3,R4);
	1 -> collect_interlaced(Rows,Width,Height,R1,R2,R3,[Row|R4]);
	2 -> collect_interlaced(Rows,Width,Height,R1,R2,[Row|R3],R4);
	3 -> collect_interlaced(Rows,Width,Height,R1,R2,R3,[Row|R4]);
	4 -> collect_interlaced(Rows,Width,Height,R1,[Row|R2],R3,R4);
	5 -> collect_interlaced(Rows,Width,Height,R1,R2,R3,[Row|R4]);
	6 -> collect_interlaced(Rows,Width,Height,R1,R2,[Row|R3],R4);
	7 -> collect_interlaced(Rows,Width,Height,R1,R2,R3,[Row|R4])
    end;
collect_interlaced([],_Width,_Height,R1,R2,R3,R4) ->
    list_to_binary([reverse(R1),reverse(R2),reverse(R3),reverse(R4)]).

    
    
write_blocks(Fd, Bin) ->
    write_blocks(Fd, Bin, 0, size(Bin)).

write_blocks(Fd, Bin, Pos, Size) ->
    Sz = Size - Pos,
    if Sz > 255 ->
	    <<_:Pos/binary, Block:255/binary, _/binary>> = Bin,
	    file:write(Fd, <<255, Block/binary>>),
	    write_blocks(Fd, Bin, Pos+255, Size);
       true ->
	    <<_:Pos/binary, Block:Sz/binary, _/binary>> = Bin,
	    file:write(Fd, <<Sz, Block/binary>>),
	    file:write(Fd, <<0>>)
    end.
-endif.


read_pixels(Fd,Pixmap,Ps,Width,Height,Interlaced) ->
    case file:read(Fd, 1) of
	{ok, <<LZWCodeSize>>} ->
	    case read_image(Fd,LZWCodeSize,Width,Height) of
		{ok,Data} ->
		    if Interlaced ->
			    interlaced_data(Data,Pixmap,Ps,Width,Height);
		       true ->
			    raw_data(Data,Pixmap,Ps,0,Width)
		    end;
		Error ->
		     Error
	    end;
	Error ->
	    Error
    end.

read_image(Fd, LZWCodeSize, _Width, _Height) ->
    case read_blocks(Fd) of
	{ok,Bin} ->
	    ?dbg("LZWCodeSize=~p compressed=~p\n", [LZWCodeSize, size(Bin)]),
	    {ok,lzw:decompress_gif(Bin, LZWCodeSize)};
	Error ->
	    Error
    end.


%%
%% Read raw data
%%
raw_data(Bin,Pixmap,Ps,Ri,Width) ->
    case Bin of
	<<Row:Width/binary, Bin1/binary>> ->
	    Pixels = unpack_pixels(Row, Ps),
	    epx:pixmap_put_pixels(Pixmap,0,Ri,Width,1,rgba,Pixels),
	    raw_data(Bin1,Pixmap,Ps,Ri+1,Width);
	<<>> ->
	    {ok, Pixmap}
    end.

%% Read interlaced data
%%
%% 0  R1a
%% 1                 R4a
%% 2            R3a           
%% 3                 R4b
%% 4       R2a
%% 5                 R4c
%% 6            R3b
%% 7                 R4d
%% ...

interlaced_data(Bin0,Pixmap,Ps,Width,Height) ->
    Bin1 = raster_data(Bin0,Pixmap,Ps,Height,0,8, Width),
    Bin2 = raster_data(Bin1,Pixmap,Ps,Height,4,8, Width),
    Bin3 = raster_data(Bin2,Pixmap,Ps,Height,2,4, Width),
    _Bin4 = raster_data(Bin3,Pixmap,Ps,Height,1,2, Width),
    {ok, Pixmap}.

raster_data(Bin,_Pixmap,_Ps,Height,Ri,_Rs,_Width) when Ri >= Height ->
    Bin;
raster_data(Bin,Pixmap,Ps,Height,Ri,Rs,Width) ->
    <<Row:Width/binary, Bin1/binary>> = Bin, 
    Pixels = unpack_pixels(Row, Ps),
    epx:pixmap_put_pixels(Pixmap,0,Ri,Width,1,rgba,Pixels),
    raster_data(Bin1,Ps,Pixmap,Height,Ri+Rs,Rs,Width).


unpack_pixels(Data,{Palette,Transparent,Ti,Bi}) ->
    unpack_pixels(Data,Palette,Transparent,Ti,Bi).

unpack_pixels(<<I,Is/binary>>,Palette,Transparent,Ti,Bi) ->
    if Transparent, I =:= Ti ->
	    {R,G,B} = element(I+1,Palette),
	    [R,G,B,0 | unpack_pixels(Is,Palette,Transparent,Ti,Bi)];
       true ->
	    {R,G,B} = element(I+1,Palette),
	    [R,G,B,255 | unpack_pixels(Is,Palette,Transparent,Ti,Bi)]
    end;
unpack_pixels(<<>>, _Palette, _Transparent, _Ti, _Bi) ->
    [].

select_palette(undefined,P) -> P;
select_palette(P,_) -> P.
