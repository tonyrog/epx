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
%%%   XPM image processing
%%% @end
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(epx_image_xpm).

-behaviour(epx_image).
-export([magic/1, mime_type/0, extensions/0,
	 read_info/1, write_info/2,
	 read/2, write/2]).

-include("../include/epx_image.hrl").
-include("dbg.hrl").

-export([scan_xpm/1]).

-import(lists,[reverse/1]).
-define(upper(C), (C)>=$A, (C)=<$Z).
-define(lower(C), (C)>=$a, (C)=<$z).
-define(digit(C), (C)>=$0, (C)=<$9).

-define(ID1(C), ?upper(C); ?lower(C); C==$_).
-define(ID(C), ?upper(C); ?lower(C); ?digit(C); C==$_).

%% Read magic info check MAGIC type and width and height (depth)
%% of image

magic(<<"/* XPM */\n", _Data/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/x-xpixmap".

extensions() -> [".xpm"].


read_info(Fd) ->
    case file:read(Fd, 10) of
	{ok,Bin} ->
	    case magic(Bin) of
		true ->
		    case scan_xpm(Fd,fun({string,_}) -> false;
					(_) -> true
				     end) of
			{ok,Ts} ->
			    [{string,Fmt}|_] = reverse(Ts),
			    {ok,[Width,Height,_NColors,_CharPerPixel],_Rest} = 
				io_lib:fread("~d ~d ~d ~d",Fmt),
			    {ok,#epx_image {
			       type   = ?MODULE,
			       width  = Width,
			       height = Height,
			       format = palette8,
			       order  = left_to_right,
			       depth  = 8 }};
			Error ->
			    Error
		    end;
		false ->
		    {error, bad_magic}
	    end;
	Error ->
	    Error
    end.


write_info(_Fd, _IMG) ->
    ok.


read(Fd, IMG) ->
    case scan_xpm(Fd) of
	{ok,Ts} ->
	    case parse_xpm(Ts) of
		{ok,Strings} ->
		    tr_rows(IMG,Strings);
		Error -> Error
	    end;
	Error -> Error
    end.
    
write(_Fd,_IMG) ->
    ok.

tr_rows(IMG,[Fmt|Data]) ->
    %% fixme: handle x_spot and y_spot
    {ok,[Width,Height,NColors,CharPerPixel],_Rest} = 
	io_lib:fread("~d ~d ~d ~d",Fmt),
    {ColorDict,Data1} = tr_colors(Data,NColors,dict:new(),CharPerPixel),
    %% fixme handle paletted pixmaps ?
    Pix0 = epx:pixmap_create(Width,Height,rgba),
    case tr_pixels(Data1,ColorDict,CharPerPixel,Pix0,0,Width,Height) of
	{ok, Pix1} ->
	    {ok, IMG#epx_image { pixmaps = [ Pix1 ] }};
	Error  ->
	    Error
    end.

tr_pixels([Line|Ls],ColorDict,Cpp,Pix,Y,Width,Height) when Y =/= Height ->
    tr_line(Line,Pix,0,Y,ColorDict,Cpp),
    tr_pixels(Ls,ColorDict,Cpp,Pix,Y+1,Width,Height);
tr_pixels([],_ColorDict,_Cpp,Pix,_Height,_Width,_Height) ->
    {ok, Pix};
tr_pixels(_,_ColorDict,_Cpp,_Pix,_Y,_Width,_Height) ->
    {error, bad_data}.

tr_line([],Pix,_X,_Y,_ColorDict,_Cpp) ->
    Pix;
tr_line(Line,Pix,X,Y,ColorDict,Cpp) ->
    {Chars,Line1} = lists:split(Cpp, Line),
    Kvs = dict:fetch(Chars, ColorDict),
    case lists:keysearch(c, 1, Kvs) of
	{value,{_,none}} ->
	    epx:pixmap_put_pixel(Pix,X,Y,{0,0,0,0}),
	    tr_line(Line1,Pix,X+1,Y,ColorDict,Cpp);
	{value,{_,Color}} ->
	    epx:pixmap_put_pixel(Pix,X,Y,Color),
	    tr_line(Line1,Pix,X+1,Y,ColorDict,Cpp);	    
	false ->
	    io:format("c color not defined in ~p\n", [Kvs]),
	    tr_line(Line1,Pix,X+1,Y,ColorDict,Cpp)
    end.

tr_colors(Data, 0, Dict, _Cpp) ->
    {Dict, Data};
tr_colors([Color|Cs], N, Dict, Cpp) ->
    {Chars,Color1} = lists:split(Cpp, Color),
    Kvs = string:tokens(Color1, " \t"),
    Dict1 = dict:store(Chars, tr_kv_spec(Kvs), Dict),
    tr_colors(Cs, N-1, Dict1, Cpp).

tr_kv_spec(["c",Color|Kvs]) ->
    [tr_color(c,Color) | tr_kv_spec(Kvs)];
tr_kv_spec(["m",Color|Kvs]) ->
    [tr_color(m,Color) | tr_kv_spec(Kvs)];
tr_kv_spec(["g4",Color|Kvs]) ->
    [tr_color(g4,Color) | tr_kv_spec(Kvs)];
tr_kv_spec(["g",Color|Kvs]) ->
    [tr_color(g,Color) | tr_kv_spec(Kvs)];
tr_kv_spec(["s",Name|Kvs]) ->
    [tr_color(s,Name) | tr_kv_spec(Kvs)];
tr_kv_spec([]) ->
    [].

tr_color(s, Name) ->
    {s, Name};
tr_color(Key, none) ->
    {Key, none};
tr_color(Key, "#"++Hex) ->
    Color = erlang:list_to_integer(Hex, 16),
    R = (Color bsr 16) band 16#ff,
    G = (Color bsr 8) band 16#ff,
    B = (Color bsr 0) band 16#ff,
    {Key, {R,G,B}};
tr_color(Key, "%"++Hex) ->
    Color = erlang:list_to_integer(Hex, 16),
    H = (Color bsr 16) band 16#ff,
    S = (Color bsr 8) band 16#ff,
    V = (Color bsr 0) band 16#ff,
    {Key, epx_color:hsv_to_rgb(H,S,V)};
tr_color(Key, Name) ->
    {Key, epx_color:from_name(Name)}.

%% parse the xpm file 
parse_xpm([{id,"static"},{id,"char"},'*',{id,_Name},'[', ']','=', '{' | Ts]) ->
    parse_xpm(Ts, []);
parse_xpm(Ts) ->
    {error,{bad_format,Ts}}.

parse_xpm([{string,Str},',' | Ts], Acc) ->
    parse_xpm(Ts, [Str|Acc]);
parse_xpm([{string,Str},'}',';'], Acc) ->
    {ok, reverse([Str|Acc])};
parse_xpm(['}', ';'], Acc) ->
    {ok, reverse(Acc)};
parse_xpm(Ts,_) ->
    {error, {bad_format,Ts}}.

scan_xpm(Fd) ->
    scan_xpm(Fd, fun(_) -> true end).

scan_xpm(Fd, While) ->
    more(Fd, fun(Cs) -> scan_xpm(Cs,Fd,[],While) end, fun() -> {ok,[]} end).

%% scan xpm (C subset) tokens
scan_xpm([C|Cs], Fd, Ts, W) ->
    if C == $\s; C == $\t; C == $\n; C == $\r ->
	    scan_xpm(Cs,Fd,Ts,W);
       C == $" ->
	    scan_string(Cs,Fd,Ts,W);
       ?ID1(C) ->
	    scan_id(Cs,Fd,[C],Ts,W);
       C == $/ ->
	    scan_comment(Cs,Fd,Ts,W);
       C == $*; C == $[; C == $]; C == ${; C == $};
       C == $,; C == $=; C == $; ->
	    next(Cs,Fd,list_to_atom([C]),Ts,W);
       true ->
	    {error,{bad_format,[C|Cs]}}
    end;
scan_xpm([],Fd,Ts,W) ->
    more(Fd, fun(Cs) -> scan_xpm(Cs,Fd,Ts,W) end,
	 fun() -> {ok,reverse(Ts)} end).
	    

%% seen /
scan_comment([$*|Cs],Fd,Ts,W) -> scan_c1(Cs,Fd,Ts,W);
scan_comment([C|Cs],Fd,Ts,W)  -> scan_xpm([C|Cs],Fd,['/'|Ts],W);
scan_comment([],Fd,Ts,W) ->
    more(Fd, fun(Cs) -> scan_comment(Cs,Fd,Ts,W) end,
	 fun() -> {error,bad_format} end).
%% seen /*
scan_c1([$*|Cs],Fd,Ts,W) -> scan_c2(Cs,Fd,Ts,W);
scan_c1([_|Cs],Fd,Ts,W) ->  scan_c1(Cs,Fd,Ts,W);
scan_c1([],Fd,Ts,W) ->
    more(Fd, fun(Cs) -> scan_c1(Cs,Fd,Ts,W) end,fun() -> {error,comment} end).

%% seen /*... *
scan_c2([$/|Cs],Fd,Ts,W) -> scan_xpm(Cs,Fd,Ts,W);
scan_c2([C|Cs],Fd,Ts,W) -> scan_c1([C|Cs],Fd,Ts,W);
scan_c2([],Fd,Ts,W) ->
    more(Fd, fun(Cs) -> scan_c2(Cs,Fd,Ts,W) end,fun() -> {error,comment} end).
	 
scan_string(Cs,Fd,Ts,W) ->
    scan_string(Cs,Fd,[],Ts,W).
    
scan_string([$\\,C | Cs],Fd,Str,Ts,W) ->
    scan_string(Cs,Fd,[C,$\\|Str],Ts,W);
scan_string([$" | Cs],Fd,Str,Ts,W) ->
    next(Cs,Fd,{string,reverse(Str)},Ts,W);
scan_string([C|Cs],Fd, Str, Ts,W) ->
    scan_string(Cs,Fd,[C|Str],Ts,W);
scan_string(Cs1,Fd,Str,Ts,W) ->
    more(Fd, fun(Cs) -> scan_string(Cs1++Cs,Fd,Str,Ts,W) end,
	 fun() -> {error,string} end).

scan_id([C|Cs],Fd,ID,Ts,W) when ?ID(C) ->
    scan_id(Cs,Fd,[C|ID],Ts,W);
scan_id([],Fd,ID,Ts,W) ->
    more(Fd, fun(Cs) -> scan_id(Cs,Fd,ID,Ts,W) end,
	 fun() -> next([],Fd,{id,reverse(ID)},Ts,W) end); 
scan_id(Cs,Fd,ID,Ts,W) ->
    next(Cs,Fd,{id,reverse(ID)},Ts,W).


next(Cs,Fd,Token,Ts,While) ->
    case While(Token) of
	true -> scan_xpm(Cs,Fd,[Token|Ts],While);
	false -> {ok,reverse([Token|Ts])}
    end.


more(Fd,Fun,Eof) ->
    case file:read(Fd,64) of
	{ok,Data} -> 
	    Fun(binary_to_list(Data));
	eof -> 
	    Eof();
	Error -> 
	    Error
    end.


