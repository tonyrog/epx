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
%%%   Undefined format catch module
%%% @end
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(epx_image_undef).

-behaviour(epx_image).
-export([magic/1, mime_type/0, extensions/0,
	 read_info/1, write_info/2,
	 read/2, read/4, write/2]).

-include("../include/epx_image.hrl").

magic(_) -> false.

mime_type() -> "".

extensions() -> [].

    
read_info(_Fd) ->
    {error, bad_magic}.

write_info(_Fd, _IMG) ->
    {error, bad_image}.

read(_Fd,_IMG) ->
    {error, bad_image}.

read(_Fd,_IMG,_PixFun,_PixSt) ->
    {error, bad_image}.

write(_Fd,_IMG) ->
    {error, bad_image}.


