%%% File    : ei_undef.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : Undefined format catch module
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


