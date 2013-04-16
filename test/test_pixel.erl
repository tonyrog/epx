%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Idea on how to test pixel functions
%%% @end
%%% Created :  7 Mar 2013 by Tony Rogvall <tony@rogvall.se>

-module(test_pixel).

-compile(export_all).

%% enough height & width to get simd working
-define(HEIGHT, 127).
-define(WIDTH,  127).
%% -define(HEIGHT, 1).
%% -define(WIDTH,  1).


test() ->
    test(argb,1000).

test(Format,N) ->
    test(Format,Format,N).

test(PixelFormatA,PixelFormatB,N) ->
    test(PixelFormatA,PixelFormatB,N,
	 [copy0, sum, blend, shadow, shadow_blend,
	  alpha, fade, color, color_blend,
	  clear,
	  src,dst,
	  src_over,dst_over,
	  src_in, dst_in,
	  src_out, dst_out,
	  src_atop, dst_atop,
	  'xor', copy, add, sub,
	  src_blend, dst_blend
	 ]).

test(PixelFormatA,PixelFormatB,N,Operations) ->
    A = epx:pixmap_create(?WIDTH, ?HEIGHT, PixelFormatA),
    B = epx:pixmap_create(?WIDTH, ?HEIGHT, PixelFormatB),
    regression_(A,B),
    [test_random(Operation,A,B,N) ||
	Operation <- Operations].

test_random(_Operation,_A,_B, 0) ->
    ok;
test_random(Operation,A,B,I) ->
    PixelFormatA = epx:pixmap_info(A, pixel_format),
    PixelFormatB = epx:pixmap_info(B, pixel_format),
    Ap = random_pixel(PixelFormatA),
    Bp = random_pixel(PixelFormatB),
    X  = random(0, ?WIDTH-1),
    Y  = random(0, ?HEIGHT-1),
    Fader = random(0,255),        %% mapped to (0..1)
    %% Fader = random:uniform(),  %% (0..1) used as fader/alpha ...
    Color = random_pixel(argb),     %% used in color/color_blend
    Vector = {Operation,X,Y,Ap,Bp,Fader,Color},
    case test_vector(A,B,Vector) of
	{C,C} ->
	    test_random(Operation, A, B, I-1);
	{C,D} ->
	    io:format("~p.\n%%  read=~w / expect=~w\n", [Vector,C,D]),
	    test_random(Operation, A, B, I-1)
    end.

test_vector(A,B,{Operation,X,Y,Ap,Bp,Fader,Color}) ->
    epx:pixmap_put_pixel(A, X, Y, Ap),
    epx:pixmap_put_pixel(B, X, Y, Bp),
    area(Operation, A, B, Fader, Color),
    Cp = epx:pixmap_get_pixel(B, X, Y),
    AF = epx:pixmap_info(A,pixel_format),
    BF = epx:pixmap_info(B,pixel_format),
    case calc_pixel(Operation,Ap,Bp,Fader,Color,AF,BF) of
	Cp -> {Cp,Cp};
	Dp -> {Cp,Dp}
    end.

regression() ->
    regression(argb).

regression(Format) ->
    regression(Format,Format).

regression(PixelFormatA,PixelFormatB) ->
    A = epx:pixmap_create(?WIDTH, ?HEIGHT, PixelFormatA),
    B = epx:pixmap_create(?WIDTH, ?HEIGHT, PixelFormatB),
    regression_(A,B).

regression_(_A,_B) ->
%%    regress_vector(A,B,{alpha,0,0,{45,186,219,27},{167,95,44,80},255,{188,232,78,139}}),
%%    regress_vector(A,B,{fade,0,0,{161,19,123,180},{49,149,173,54},255,{240,183,105,223}}),
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

perf(Format) ->
    perf(Format,Format).

perf(AFormat,BFormat) ->
    A = epx:pixmap_create(?WIDTH, ?HEIGHT, AFormat),
    B = epx:pixmap_create(?WIDTH, ?HEIGHT, BFormat),
    [perf_random(Operation, A, B, 1000) ||
	Operation <- [copy0, sum, blend, shadow, shadow_blend,
		      alpha, fade, color, color_blend,
		      clear,
		      src,dst,
		      src_over,dst_over,
		      src_in, dst_in,
		      src_out, dst_out,
		      src_atop, dst_atop,
		      'xor', copy, add, sub,
		      src_blend, dst_blend
		      ]].

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



calc_pixel(copy0,S,_D,_F,_C,_AF,BF) ->
    set_alpha(S,BF);
calc_pixel(sum,S,D,_F,_C,_AF, BF) ->
    set_alpha(pixel_add(S,D), BF);
calc_pixel(blend,S,D,_F,_C,AF,BF) ->
    P = case has_alpha(AF) of
	    true -> pixel_blend(element(1,S),S,D);
	    false -> S
	end,
    set_alpha(P,BF);
calc_pixel(shadow,S,D,_F,_C,_AF,BF) ->
    G = luminance(S),
    set_alpha(pixel_shadow(G, D), BF);
calc_pixel(shadow_blend,_S={0,_,_,_},D,_F,_C,_AF,BF) ->
    set_alpha(D,BF);
calc_pixel(shadow_blend,S={255,_,_,_},D,_F,_C,_AF,BF) ->
    G = luminance(S),
    P = pixel_shadow(G, D),
    set_alpha(P, BF);
calc_pixel(shadow_blend,S,D,_F,_C,_AF,BF) ->
    D1 = pixel_blend(element(1,S),S,D),
    G = luminance(D1),
    P = pixel_shadow(G, D1),
    set_alpha(P, BF);
calc_pixel(alpha,S,D,F,_C,_AF,BF) ->
    P = case fix_8_8(F) of
	    255 -> D;
	    Fu  ->
		{_,R,G,B} = pixel_blend(Fu,S,D),
		{element(1,D),R,G,B}
	end,
    set_alpha(P, BF);
calc_pixel(fade,S,D,F,_C,_AF,BF) ->
    Sa = element(1,S),
    P = case fix_8_8(F) of
	    0   -> D;
	    255 -> 
		if Sa =:= 255 -> S;
		   true -> pixel_blend(Sa,S,D)
		end;
	    Fu ->
		A = (Sa*Fu) div 256,
		pixel_blend(A,S,D)
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
    P = {A band Am, R band Rm, G band Gm, B band Bm},
    case has_alpha(DstFormat) of
	true -> P;
	false -> setelement(1,P,255)
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
    %% fixme: return complex pixel_format!
    case Format of
	rgb       -> false;
	bgr       -> false;
	r8g8b8    -> false;
	b8g8r8    -> false;
	argb      -> true;
	abgr      -> true;
	rgba      -> true;
	bgra      -> true;
	'565'     -> false;
	'565BE'   -> false;
	'565LE'   -> false;
	r5g6b5    -> false;
	r5g6b5BE  -> false;
	r5g6b5LE  -> false;
	b5g6r5    -> false;
	b5g6r5BE  -> false;
	b5g6r5LE  -> false;
	'1555'     -> true;
	'a1r5g5b5' -> true;
	gray8a8    -> true;
	a8l8       -> true;
	gray8      -> false;
	alpha8     -> true;
	a8         -> true;
	r8         -> false;
	g8         -> false;
	b8         -> false
    end.


pixel_mask(Format) ->
    case Format of
	rgb       -> { 255, 255, 255, 255 };
	bgr       -> { 255, 255, 255, 255 };
	r8g8b8    -> { 255, 255, 255, 255 };
	b8g8r8    -> { 255, 255, 255, 255 };

	argb      -> { 255, 255, 255, 255};
	abgr      -> { 255, 255, 255, 255};
	rgba      -> { 255, 255, 255, 255};
	bgra      -> { 255, 255, 255, 255};

	'565'     -> { 255, 16#f8, 16#fc, 16#f8 };
	'565BE'   -> { 255, 16#f8, 16#fc, 16#f8 };
	'565LE'   -> { 255, 16#f8, 16#fc, 16#f8 };

	r5g6b5    -> { 255, 16#f8, 16#fc, 16#f8 };
	r5g6b5BE  -> { 255, 16#f8, 16#fc, 16#f8 };
	r5g6b5LE  -> { 255, 16#f8, 16#fc, 16#f8 };

	b5g6r5    -> { 255, 16#f8, 16#fc, 16#f8 };
	b5g6r5BE  -> { 255, 16#f8, 16#fc, 16#f8 };
	b5g6r5LE  -> { 255, 16#f8, 16#fc, 16#f8 };

	'1555'     -> { 255, 16#f8, 16#f8, 16#f8 };	
	'a1r5g5b5' -> { 255, 16#f8, 16#f8, 16#f8 };	

	gray8a8    -> { 255, 255, 255, 255};
	a8l8       -> { 255, 255, 255, 255};
	gray8      -> { 255, 255, 255, 255};
	alpha8     -> { 255, 0, 0, 0   };
	a8         -> { 255, 0, 0, 0   };
	r8         -> { 255, 255, 0, 0 };
	g8         -> { 255, 0, 255, 0 };
	b8         -> { 255, 0, 0, 255 }
    end.

random_pixel(Format) ->
    case Format of
	rgb     -> { 255, r8(), r8(), r8() };
	bgr     -> { 255, r8(), r8(), r8() };
	r8g8b8  -> { 255, r8(), r8(), r8() };
	b8g8r8  -> { 255, r8(), r8(), r8() };

	argb -> { r8(), r8(), r8(), r8()};
	abgr -> { r8(), r8(), r8(), r8()};
	rgba -> { r8(), r8(), r8(), r8()};
	bgra -> { r8(), r8(), r8(), r8()};

	'565'     -> { 255, r8(5), r8(6), r8(5) };
	'565BE'   -> { 255, r8(5), r8(6), r8(5) };
	'565LE'   -> { 255, r8(5), r8(6), r8(5) };

	r5g6b5    -> { 255, r8(5), r8(6), r8(5) };
	r5g6b5BE  -> { 255, r8(5), r8(6), r8(5) };
	r5g6b5LE  -> { 255, r8(5), r8(6), r8(5) };

	b5g6r5    -> { 255, r8(5), r8(6), r8(5) };
	b5g6r5BE  -> { 255, r8(5), r8(6), r8(5) };
	b5g6r5LE  -> { 255, r8(5), r8(6), r8(5) };

	'1555'     -> { r8(1), r8(5), r8(5), r8(5) };	
	'a1r5g5b5' -> { r8(1), r8(5), r8(5), r8(5) };	

	gray8a8    -> G=r8(), { r8(), G, G, G};
	a8l8       -> G=r8(), { r8(), G, G, G};
	gray8      -> G=r8(), { 255, G, G, G};

	alpha8     -> { r8(), 0, 0, 0 };

	a8   -> { r8(), 0, 0, 0   };
	r8   -> { 255, r8(), 0, 0 };
	g8   -> { 255, 0, r8(), 0 };
	b8   -> { 255, 0, 0, r8() }
    end.

r8() -> r8(8).

r8(8) -> random(0,255);
r8(5) -> random(0,31) bsl 3;
r8(6) -> random(0,63) bsl 2.
	    
random(A, B) when is_integer(A), A =< B ->   
    random:uniform((B - A) + 1) - 1 + A.
