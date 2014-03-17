%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Idea on how to test pixel functions
%%% @end
%%% Created :  7 Mar 2013 by Tony Rogvall <tony@rogvall.se>

-module(test_pixel).

-compile(export_all).

-include("../include/epx.hrl").

%% enough height & width to get simd working
-define(HEIGHT, 127).
-define(WIDTH,  127).
%% -define(HEIGHT, 1).
%% -define(WIDTH,  1).

format_list() ->
    [argb,abgr,rgba,bgra,rgb,bgr].
%%     xarg,xbgr,rgbx,bgrx].

operation_list() ->
    [fill, fill_blend,
     copy0, sum, blend, shadow, shadow_blend,
     alpha, fade, color, color_blend,
     clear,
     src,dst,
     src_over,dst_over,
     src_in, dst_in,
     src_out, dst_out,
     src_atop, dst_atop,
     'xor', copy, add, sub,
     src_blend, dst_blend
    ].

accel_list() ->
    case epx:simd_info(accel) of
	{none,List} -> [none|List];
	{Accel,List} -> [none,Accel|List]
    end.

test() ->
    test(100).

test(N) ->
    lists:foreach(
      fun(Accel) ->
	      epx:simd_set(Accel),
	      lists:foreach(
		fun(Format) ->
			test_format(Format,N)
		end, format_list())
      end, accel_list()).


test_format(Format,N) ->
    test_format(Format,Format,N).

test_format(PixelFormatA,PixelFormatB,N) ->
    test_format(PixelFormatA,PixelFormatB,N,operation_list()).

test_format(FormatA,FormatB,N,Operations) 
  when is_atom(FormatA), is_atom(FormatB) ->
    A = epx:pixmap_create(?WIDTH, ?HEIGHT, FormatA),
    B = epx:pixmap_create(?WIDTH, ?HEIGHT, FormatB),
    %% regression_(A,B),
    [test_random(Operation,A,B,N) || Operation <- Operations].

test_random(_Operation,_A,_B, 0) ->
    ok;
test_random(Operation,A,B,I) ->
    %% io:format("~w: ~w ~w ~w\n", [I,Operation,A,B]),
    PixelFormatA = epx:pixmap_info(A, epx_pixel_format),
    PixelFormatB = epx:pixmap_info(B, epx_pixel_format),
    Ap = random_pixel(PixelFormatA),
    Bp = random_pixel(PixelFormatB),
    X  = random(0, ?WIDTH-1),
    Y  = random(0, ?HEIGHT-1),
    Fader = random(0,255),        %% mapped to (0..1)
    %% Fader = random:uniform(),  %% (0..1) used as fader/alpha ...
    Color = random_argb(),     %% used in color/color_blend
    Vector = {Operation,X,Y,Ap,Bp,Fader,Color},
    case test_vector(A,B,Vector) of
	{C,C} ->
	    test_random(Operation, A, B, I-1);
	{C,D} ->
	    {Accel,_} = epx:simd_info(accel),
	    io:format("~s ~s ~s ~p.\n%%  read=~w / expect=~w\n", 
		      [Accel,
		       PixelFormatA#epx_pixel_format.name,
		       PixelFormatB#epx_pixel_format.name,
		       Vector,C,D]),
	    test_random(Operation, A, B, I-1)
    end.

test_vector(A,B,{Operation,X,Y,Ap,Bp,Fader,Color}) ->
    epx:pixmap_put_pixel(A, X, Y, Ap),
    epx:pixmap_put_pixel(B, X, Y, Bp),
    area(Operation, A, B, Fader, Color),
    AF = epx:pixmap_info(A,epx_pixel_format),
    BF = epx:pixmap_info(B,epx_pixel_format),
    Cp0 = epx:pixmap_get_pixel(B, X, Y),
    Cp = if BF#epx_pixel_format.alpha -> Cp0;
	    true -> setelement(1, Cp0, 255)  %% ignore alpha!
	 end,
    case calc_pixel(Operation,Ap,Bp,Fader,Color,AF,BF) of
	Cp -> {Cp,Cp};
	Dp -> {Cp,Dp}
    end.

regression() ->
    regression(argb).

regression(Format) ->
    regression(Format,Format).

regression(PixelFormatA,PixelFormatB) ->
    A = epx:pixmap_create(?WIDTH, ?HEIGHT, PixelFormatA#epx_pixel_format.name),
    B = epx:pixmap_create(?WIDTH, ?HEIGHT, PixelFormatB#epx_pixel_format.name),
    regression_(A,B).

%%
%% Put regression vectors here!
%%
regression_(A,B) ->
    regress_vector(A,B,{alpha,0,0,{45,186,219,27},{167,95,44,80},255,{188,232,78,139}}),
    regress_vector(A,B,{fade,0,0,{161,19,123,180},{49,149,173,54},255,{240,183,105,223}}),
    ok.

regress_vector(A,B,Vector) ->
    case test_vector(A,B,Vector) of
	{C,C} -> ok;
	{C,D} -> 
	    io:format("~p.\n%%  read=~w / expect=~w\n", [Vector,C,D]),
	    error
    end.

perf() ->
    perf(argb).

perf(Format) when is_atom(Format) ->
    perf(Format,Format).

perf(AFormat,BFormat) when is_atom(AFormat), is_atom(BFormat) ->
    A = epx:pixmap_create(?WIDTH, ?HEIGHT, AFormat),
    B = epx:pixmap_create(?WIDTH, ?HEIGHT, BFormat),
    [perf_random(Operation, A, B, 1000) ||
	Operation <- operation_list()].

perf_random(Operation, A, B, N) ->
    T0 = os:timestamp(),
    perf_loop(Operation, A, B, 0.5, {127,127,127,127}, N),
    T1 = os:timestamp(),
    {Operation, (N/timer:now_diff(T1, T0))*1000000}.

perf_loop(_Operation, _A, _B, _F, _C, 0) ->
    ok;
perf_loop(Operation, A, B, F, C, I) ->
    area(Operation, A, B, F, C),
    perf_loop(Operation, A, B, F, C, I-1).

area(fill, _A, B, _F, C) ->
    epx:pixmap_fill(B, C);
area(fill_blend, _A, B, _F, C) ->
    epx:pixmap_fill(B, C, [blend]);
area(copy0, A, B, _F, _C) ->
    epx:pixmap_copy_area(A, B, 0, 0, 0, 0, ?WIDTH, ?HEIGHT, []);
area(sum, A, B, _F, _C) ->
    epx:pixmap_copy_area(A, B, 0, 0, 0, 0, ?WIDTH, ?HEIGHT, [sum]);
area(blend, A, B, _F, _C) ->
    epx:pixmap_copy_area(A, B, 0, 0, 0, 0, ?WIDTH, ?HEIGHT, [blend]);
area(shadow, A, B, _F, _C) ->
    epx:pixmap_shadow_area(A, B, 0, 0, 0, 0, ?WIDTH, ?HEIGHT, []);
area(shadow_blend, A, B, _F, _C) ->
    epx:pixmap_shadow_area(A, B, 0, 0, 0, 0, ?WIDTH, ?HEIGHT, [blend]);
area(alpha, A, B, F, _C) ->
    epx:pixmap_alpha_area(A, B, F, 0, 0, 0, 0, ?WIDTH, ?HEIGHT);
area(fade,  A, B, F, _C) ->
    epx:pixmap_fade_area(A, B, F, 0, 0, 0, 0, ?WIDTH, ?HEIGHT);
area(color, A, B, F, C)  ->
    epx:pixmap_add_color_area(A, B, F, C, 0, 0, 0, 0, ?WIDTH, ?HEIGHT,
			      []);
area(color_blend, A, B, F, C) ->
    epx:pixmap_add_color_area(A, B, F, C, 0, 0, 0, 0, ?WIDTH, ?HEIGHT,
			      [blend]);
%% generic operation
area(Operation, A, B, _F, _C) ->
    epx:pixmap_operation_area(A, B, Operation, 0, 0, 0, 0, ?WIDTH, ?HEIGHT).

calc_pixel(fill,_S,_D,_F,C,_AF,BF) ->
    set_alpha(C,BF);
calc_pixel(fill_blend,_S,D,_F,C,_AF,BF) ->
    P = case element(1,C) of
	    255 -> C;   %% special condition in nif
	    A  -> pixel_blend(A,C,D)
	end,
    set_alpha(P,BF);
calc_pixel(copy0,S,_D,_F,_C,_AF,BF) ->
    set_alpha(S,BF);
calc_pixel(sum,S,D,_F,_C,_AF, BF) ->
    set_alpha(pixel_add(S,D), BF);
calc_pixel(blend,S,D,_F,_C,AF,BF) ->
    P = if AF#epx_pixel_format.alpha ->
		pixel_blend(element(1,S),S,D);
	   true -> S
	end,
    set_alpha(P,BF);
calc_pixel(shadow,S,D,_F,_C,_AF,BF) ->
    G = luminance(S),
    set_alpha(pixel_shadow(G, D), BF);

calc_pixel(shadow_blend,S,D,_F,_C,_AF,BF) ->
    Sa = element(1,S),
    P  = if Sa =:= 0 -> D;
	    Sa =:= 255 ->
		 G = luminance(S),
		 pixel_shadow(G, D);
	    true ->
		 D1 = pixel_blend(Sa,S,D),
		 G = luminance(D1),
		 pixel_shadow(G, D1)
	 end,
    set_alpha(P, BF);
calc_pixel(alpha,S,D,F,_C,_AF,_BF) ->
    Fu = fix_8_8(F),
    {_,R,G,B} = pixel_blend(Fu,S,D),
    {element(1,D),R,G,B};
calc_pixel(fade,S,D,F,_C,AF,BF) ->
    Fu = fix_8_8(F),
    P = if not AF#epx_pixel_format.alpha ->
		pixel_blend(Fu,S,D);
	   true ->
		Sa = element(1,S),
		if Fu =:= 0 -> D;
		   Fu =:= 255,Sa =:= 255 -> S;
		   Fu =:= 255 -> pixel_blend(Sa,S,D);
		   %% Sa =:= 255 -> pixel_blend(Fu,S,D);
		   true ->
			A = (Sa*Fu) div 256,
			pixel_blend(A,S,D)
		end
	end,
    set_alpha(P, BF);
calc_pixel(color,S,_D,F,Color,_AF,BF) ->
    S1 = pixel_add(Color, S),
    A = (element(1,S1)*fix_8_8(F)) div 256,
    S2 = setelement(1,S1,A),
    set_alpha(pixel_shadow(A, S2),BF);
calc_pixel(color_blend,S,D,F,Color,_AF,BF) ->
    S1 = pixel_add(Color, S),
    A = (element(1,S1)*fix_8_8(F)) div 256,
    set_alpha(pixel_blend(A, S1, D), BF);
calc_pixel(clear,_S,_D,_F,_C,_AF,BF) ->
    set_alpha({0,0,0,0}, BF);
calc_pixel(src,S,_D,_F,_C,_AF,BF)  ->
    set_alpha(pixel_scale(element(1,S), S),BF);
calc_pixel(dst,_S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_scale(element(1,D), D),BF);
calc_pixel(src_over,S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_over(S, D), BF);
calc_pixel(dst_over,S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_over(D, S),BF);
calc_pixel(src_in,S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_in(S, D),BF);
calc_pixel(dst_in,S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_in(D, S),BF);
calc_pixel(src_out,S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_out(S, D),BF);
calc_pixel(dst_out,S,D,_F,_C,_AF,BF) -> 
    set_alpha(pixel_out(D, S),BF);
calc_pixel(src_atop,S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_atop(S, D),BF);
calc_pixel(dst_atop,S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_atop(D, S),BF);
calc_pixel('xor',S,D,_F,_C,_AF,BF) ->    
    set_alpha(pixel_xor(S, D),BF);
calc_pixel('copy',S, D,_F,_C,_AF,BF) ->
    set_alpha(pixel_copy(S, D), BF);
calc_pixel('add',S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_add(S, D),BF);
calc_pixel('sub',S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_sub(S, D),BF);
calc_pixel(src_blend,S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_blend(element(1,S), S, D),BF);
calc_pixel(dst_blend,S,D,_F,_C,_AF,BF) ->
    set_alpha(pixel_blend(element(1,D), D, S),BF).

set_alpha({A,R,G,B},DstFormat) ->
    {Am,Rm,Gm,Bm} = pixel_mask(DstFormat),
    if DstFormat#epx_pixel_format.alpha ->
	    {A band Am, R band Rm, G band Gm, B band Bm};
       true ->
	    {255, R band Rm, G band Gm, B band Bm}
    end.
    
%% pixel operation 
pixel_blend(A, {A0,R0,G0,B0}, {A1,R1,G1,B1}) ->
    {blend(A,A0,A1),blend(A,R0,R1),blend(A,G0,G1),blend(A,B0,B1)}.

pixel_shadow(A,{A0,R0,G0,B0}) ->
    {shadow(A,A0),shadow(A,R0),shadow(A,G0),shadow(A,B0)}.

pixel_scale(A, {_A0,R0,G0,B0}) ->
    {A,scale(A,R0),scale(A,G0),scale(A,B0)}.

pixel_over({A0,R0,G0,B0}, {A1,R1,G1,B1}) ->
    A = (A0*(255-A1) + (A1*256)) div 256,
    {A, over(A0,R0,A1,R1), over(A0,G0,A1,G1), over(A0,B0,A1,B1)}.

pixel_in({A0,R0,G0,B0}, {A1,_R1,_G1,_B1}) ->
    A = (A0*A1) div 256,
    {A,scale(A,R0),scale(A,G0),scale(A,B0)}.

pixel_out({A0,R0,G0,B0}, {A1,_R1,_G1,_B1}) ->
    A = (A0*(255-A1)) div 256,
    {A,scale(A,R0),scale(A,G0),scale(A,B0)}.

pixel_atop({A0,R0,G0,B0}, {A1,R1,G1,B1}) ->
    {A1, atop(A0,R0,A1,R1), atop(A0,G0,A1,G1), atop(A0,B0,A1,B1)}.

pixel_xor({A0,R0,G0,B0}, {A1,R1,G1,B1}) ->
    A = (((A0 + A1) * 256) - 2*A1*A0) div 256,
    {A, pxor(A0,R0,A1,R1), pxor(A0,G0,A1,G1), pxor(A0,B0,A1,B1)}.

pixel_copy(S, _D) ->
    S.

pixel_add({A0,R0,G0,B0}, {A1,R1,G1,B1}) ->
    {add(A0,A1), add(R0,R1), add(G0,G1), add(B0,B1)}.

pixel_sub({A0,R0,G0,B0}, {A1,R1,G1,B1}) ->
    {sub(A0,A1), sub(R0,R1), sub(G0,G1), sub(B0,B1)}.

shadow(A, D) ->    
    blend(A,0,D).

blend(A,S,D) ->
    (A*(S-D) + (D*256)) div 256.

scale(A, S) ->
    (A*S) div 256.

over(A1, C1, A2, C2) ->
    blend(A1, C1, ((A2*C2) div 256)).

atop(A1, C1, A2, C2) ->
    A = (C1*A2) div 256,
    B = (C2*A2) div 256,
    blend(A1, A, B).

pxor(A1, C1, A2, C2) ->
    A = (C1*(255-A2)) div 256,
    B = (C2*A2) div 256,
    blend(A1, A, B).

add(A, B) ->
    C = A + B,
    if C > 255 -> 255;
       true -> C
    end.

sub(A, B) ->
    C = A - B,
    if C < 0 -> 0;
       true -> C
    end.

fix_8_8(F) when is_float(F) ->
    if F >= 1.0 -> 255;
       F =< 0.0 -> 0;
       true -> trunc(F*256)
    end;
fix_8_8(F) when is_integer(F) ->
    if F > 255 -> 255;
       F < 0 -> 0;
       true -> F
    end.

luminance({R,G,B}) ->
    luminance(R,G,B);
luminance({_A,R,G,B}) ->
    luminance(R,G,B).

luminance(R,G,B) ->
    (R*299 + G*587 + B*114) div 1000.

has_alpha(Format) ->
    Format#epx_pixel_format.alpha.

pixel_mask(Format) ->
    case Format#epx_pixel_format.format of
	rgb5   -> { 255, 16#f8, 16#f8, 16#f8 };	
	rgb8   -> { 255, 255, 255, 255 };
	rgb565 -> { 255, 16#f8, 16#fc, 16#f8 };
	gray   -> { 255, 255, 255, 255};
	alpha  -> { 255, 0, 0, 0   };
	red    -> { 255, 255, 0, 0 };
	green  -> { 255, 0, 255, 0 };
	blue   -> { 255, 0, 0, 255 }
    end.

random_argb() ->
    { r8(8), r8(8), r8(8), r8(8) }.

random_pixel(Format) ->
    A = if Format#epx_pixel_format.alpha -> r8(8);
	   true -> 255
	end,
    case Format#epx_pixel_format.format of
	rgb5    -> { A band 16#80, r8(5), r8(5), r8(5) };
	rgb8    -> { A, r8(8), r8(8), r8(8) };
	rgb565  -> { 255, r8(5), r8(6), r8(5) };
	gray    -> G=r8(8), { A, G, G, G};
	alpa    -> { r8(8), 0, 0, 0   };
	red     -> { 255, r8(8), 0, 0 };
	green   -> { 255, 0, r8(8), 0 };
	blue    -> { 255, 0, 0, r8(8) }
    end.

r8(8) -> random(0,255);
r8(5) -> random(0,31) bsl 3;
r8(6) -> random(0,63) bsl 2.
	    
random(A, B) when is_integer(A), A =< B ->   
    random:uniform((B - A) + 1) - 1 + A.
