%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
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
    epx:pixmap_put_pixel(Pixmap, 1, 1, 0, 16#01020304),
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
