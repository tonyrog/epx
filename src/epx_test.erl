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
%%%    Minor test functions
%%% @end
%%% Created : 27 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_test).

-export([test/0, test/1]).
-export([test_pixels/0, test_pixels/3]).

%%
%% Simple test
%%
test() ->
    test([]).

test(Opts) ->
    case lists:reverse(epx:backend_list()) of
	[] ->
	    ok;
	[Backend|_] ->
	    B = epx:backend_open(Backend, epx:dict_from_list(Opts)),
	    W1 = epx:window_create(40, 40, 100, 100),
	    W2 = epx:window_create(140, 140, 100, 100),
	    P1 = epx:pixmap_create(100, 100, argb),
	    %% pixmap_fill(P1, {255,0,0}),
	    epx:window_attach(W1, B),
	    epx:window_attach(W2, B),
	    %% pixmap_attach(P1, B),
	    loop(P1,W1,W2),
	    epx:pixmap_detach(P1)
    end.

loop(_P1,undefined,undefined) ->
    ok;
loop(P1,W1,W2) ->
    receive
	{epx_event, W1, close} ->
	    io:format("Got window1 close\n", []),
	    epx:window_detach(W1),
	    loop(P1,undefined, W2);
	{epx_event, W2, close} ->
	    io:format("Got window2 close\n", []),
	    epx:window_detach(W2),
	    loop(P1,W1, undefined);
	{epx_event, W1, Event} ->
	    io:format("Got window1 event ~p\n", [Event]),
	    loop(P1,W1,W2);
	{epx_event, W2, Event} ->
	    io:format("Got window2 event ~p\n", [Event]),
	    loop(P1,W1,W2);
	{epx_event, W, Event} ->
	    io:format("Got other window ~w event ~p\n", [W,Event]),
	    loop(P1,W1,W2)
    end.
%%
%% Test pixel operations
%% We use a pixmap either to small to use SIMD (8x8) or
%% a pixmap big enough to use SIMD (32x32)
%%
test_pixels() ->
    test_pixels(8,8,argb),
    test_pixels(8,8,rgba),
    test_pixels(32,32,argb),
    test_pixels(32,32,rgba),
    ok.

test_pixels(Width,Height,Format) ->
    Src = epx:pixmap_create(Width,Height,Format),
    Dst = epx:pixmap_create(Width,Height,Format),

    %% Test fill
    epx:pixmap_fill(Src, {0,0,0,0}),  %% transparent
    check_pixels(Src, {0,0,0,0}),
    epx:pixmap_fill(Src, {255,50,100,200}),
    check_pixels(Src, {255,50,100,200}),

    %% Test alpha_area blend Src and Dst using fixed alpha value
    %% A = Parameter
    %% Ad = A*As + (1-A)*Ad,
    %% Rd = A*Rs + (1-A)*Rd,
    %% Gd = A*Gs + (1-A)*Gd,
    %% Bd = A*Bs + (1-A)*Bd,
    epx:pixmap_fill(Src, {255,16,64,128}),
    epx:pixmap_fill(Dst, {0,0,0,0}),
    epx:pixmap_alpha_area(Src,Dst,0.5,0,0,0,0,Width,Height),
    check_pixels(Dst, {255,8,32,64}),

    epx:pixmap_fill(Src, {255,16,64,128}),
    epx:pixmap_fill(Dst, {255,8,32,64}),
    epx:pixmap_alpha_area(Src,Dst,0.5,0,0,0,0,Width,Height),
    check_pixels(Dst, {255,8+4,32+16,64+32}),

    %% Test fade area, Dst pixels are scaled using a Scaled (fade)
    %% src alpha value, Dst alpha is also affected, Src is "normally"
    %% an alpha map
    %% A = As * Fade   (Fade 0 - 1.0) (actually 8 bit 0.8 fix point)
    %% Ad=(1-A)*As,
    %% Rd=(1-A)*Rs,
    %% Gd=(1-A)*Gs,
    %% Bd=(1-A)*Bs
    %%
    epx:pixmap_fill(Src, {128,100,100,100}),
    epx:pixmap_fill(Dst, {255,16,32,64}),
    epx:pixmap_fade_area(Src, Dst, 0.5, 0,0,0,0,Width,Height),
    check_pixels(Dst, {255,8+4,32+16,64+32}),

    %% Test shadow area (luminance should probably be scaled by As?)
    %% G = luminance({_As,Rs,Gs,Bs}) = (R*299 + G*587 + B*114)/1000
    %% Ad=(1-G)*Ad,
    %% Rd=(1-G)*Rd,
    %% Gd=(1-G)*Gd,
    %% Bd=(1-G)*Bd
    %% 
    epx:pixmap_fill(Src, {128,99,150,99}),
    %% G = (299*99 + 587*150 + 114*99) div 1000 = 128
    epx:pixmap_fill(Dst, {255,16,32,64}),
    epx:pixmap_shadow_area(Src, Dst, 0,0,0,0,Width,Height),
    check_pixels(Dst, {255,8,16,32}),

    ok.



    
%% Check that all pixels have the Color
check_pixels(Pixmap, Color) ->
    true == lists:all(fun(C) -> C =:= Color end,
		      [ epx:pixmap_get_pixel(Pixmap,X,Y) ||
			  X <- lists:seq(0, epx:pixmap_info(Pixmap,width)-1),
			  Y <- lists:seq(0, epx:pixmap_info(Pixmap,height)-1)]).

    
