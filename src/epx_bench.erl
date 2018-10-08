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
%%%  Some minor benchmarks
%%% @end
%%% Created : 27 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_bench).

-export([benchmark/0, benchmark/2]).
-export([loop_wr/1, loop_rd/1, loop_ref/1, loop_nif_ref/1, dummy/0]).

%%
%% Benchmark primitive operations
%%
benchmark() ->
    benchmark(loop_rd, 10).
benchmark(F, N) ->
    Pixmap = epx:pixmap_create(64,64,argb),
    benchmark:run(?MODULE, F, [Pixmap], N).

loop_wr(Pixmap) ->  loop_wr(Pixmap, 1000000).
loop_rd(Pixmap) ->  loop_rd(Pixmap, 1000000).
loop_ref(Pixmap) ->  loop_ref(Pixmap, 1000000).
loop_nif_ref(Pixmap) -> loop_nif_ref(Pixmap, 1000000).
     
loop_wr(Pixmap, I) when I > 0 ->
    epx:pixmap_put_pixel(Pixmap, 1, 1, 16#01020304, 0),
    loop_wr(Pixmap, I-1);
loop_wr(_Pixmap, _I) ->
    ok.

loop_rd(Pixmap, I) when I > 0 ->
    epx:pixmap_get_pixel(Pixmap, 1, 1),
    loop_rd(Pixmap, I-1);
loop_rd(_Pixmap, _I) ->
    ok.

loop_nif_ref(Pixmap, I) when I > 0 ->
    epx:pixmap_info(Pixmap, width),
    loop_nif_ref(Pixmap, I-1);
loop_nif_ref(_Pixmap, _I) ->
    ok.

loop_ref(Pixmap, I) when I > 0 ->
    epx:dummy(),
    loop_ref(Pixmap, I-1);
loop_ref(_Pixmap, _I) ->
    ok.

dummy() ->
    ok.
