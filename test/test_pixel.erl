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

regression() ->
    regression(argb).

regression(Format) ->
    A = epx:pixmap_create(?WIDTH, ?HEIGHT, Format),
    B = epx:pixmap_create(?WIDTH, ?HEIGHT, Format),
    regression(A,B).

test() ->
    test(argb).

test(Format) ->
    A = epx:pixmap_create(?WIDTH, ?HEIGHT, Format),
    B = epx:pixmap_create(?WIDTH, ?HEIGHT, Format),
    regression(A,B),
    [test_random(Operation, A, B, 100) ||
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

test_random(_Operation, _A, _B, 0) ->
    ok;
test_random(Operation, A, B, I) ->
    Ap = random_argb(),
    Bp = random_argb(),
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
	    io:format("~p.\n%%  read=~w / expect=~w\n", [Vector,C,D]),
	    test_random(Operation, A, B, I-1)
    end.

test_vector(A,B,{Operation,X,Y,Ap,Bp,Fader,Color}) ->
    epx:pixmap_put_pixel(A, X, Y, Ap),
    epx:pixmap_put_pixel(B, X, Y, Bp),
    area(Operation, A, B, Fader, Color),
    Cp = epx:pixmap_get_pixel(B, X, Y),
    case calc_pixel(Operation, Ap, Bp, Fader, Color) of
	Cp -> {Cp,Cp};
	Dp -> {Cp,Dp}
    end.

regression(A,B) ->
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

perf(Format) ->
    A = epx:pixmap_create(?WIDTH, ?HEIGHT, Format),
    B = epx:pixmap_create(?WIDTH, ?HEIGHT, Format),
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



calc_pixel(copy0,S,_D,_F,_C) ->
    S;
calc_pixel(sum,S,D,_F,_C) ->
    pixel_add(S,D);
calc_pixel(blend,S,D,_F,_C) ->
    pixel_blend(element(1,S),S,D);
calc_pixel(shadow,S,D,_F,_C) ->
    G = luminance(S),
    pixel_shadow(G, D);
calc_pixel(shadow_blend,_S={0,_,_,_},D,_F,_C) ->
    D;
calc_pixel(shadow_blend,S={255,_,_,_},D,_F,_C) ->
    G = luminance(S),
    pixel_shadow(G, D);
calc_pixel(shadow_blend,S,D,_F,_C) ->
    D1 = pixel_blend(element(1,S),S,D),
    G = luminance(D1),
    pixel_shadow(G, D1);
calc_pixel(alpha,S,D,F,_C) ->
    case fix_8_8(F) of
	255 -> D;
	Fu  ->
	    {_,R,G,B} = pixel_blend(Fu,S,D),
	    {element(1,D),R,G,B}
    end;
calc_pixel(fade,S,D,F,_C) ->
    Sa = element(1,S),
    case fix_8_8(F) of
	0   -> D;
	255 -> pixel_blend(Sa,S,D);
	Fu ->
	    A = (Sa*Fu) div 256,
	    pixel_blend(A,S,D)
    end;
calc_pixel(color,S,_D,F,Color) ->
    S1 = pixel_add(Color, S),
    A = (element(1,S1)*fix_8_8(F)) div 256,
    S2 = setelement(1,S1,A),
    pixel_shadow(A, S2);
calc_pixel(color_blend,S,D,F,Color) ->
    S1 = pixel_add(Color, S),
    A = (element(1,S1)*fix_8_8(F)) div 256,
    pixel_blend(A, S1, D);

calc_pixel(clear,_S,_D,_F,_C) ->
    {0,0,0,0};
calc_pixel(src,S,_D,_F,_C)  ->     pixel_scale(element(1,S), S);
calc_pixel(dst,_S,D,_F,_C) ->      pixel_scale(element(1,D), D);
calc_pixel(src_over,S,D,_F,_C) ->  pixel_over(S, D);
calc_pixel(dst_over,S,D,_F,_C) ->  pixel_over(D, S);
calc_pixel(src_in,S,D,_F,_C) ->    pixel_in(S, D);
calc_pixel(dst_in,S,D,_F,_C) ->    pixel_in(D, S);
calc_pixel(src_out,S,D,_F,_C) ->   pixel_out(S, D);
calc_pixel(dst_out,S,D,_F,_C) ->   pixel_out(D, S);
calc_pixel(src_atop,S,D,_F,_C) ->  pixel_atop(S, D);
calc_pixel(dst_atop,S,D,_F,_C) ->  pixel_atop(D, S);
calc_pixel('xor',S,D,_F,_C) ->     pixel_xor(S, D);
calc_pixel('copy',S, D,_F,_C) ->    pixel_copy(S, D);
calc_pixel('add',S,D,_F,_C) ->     pixel_add(S, D);
calc_pixel('sub',S,D,_F,_C) ->     pixel_sub(S, D);
calc_pixel(src_blend,S,D,_F,_C) -> pixel_blend(element(1,S), S, D);
calc_pixel(dst_blend,S,D,_F,_C) -> pixel_blend(element(1,D), D, S).
    
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


random_rgb() ->
    { 255, random(0,255), random(0,255), random(0,255) }.

random_argb() ->
    { random(0,255), random(0,255), random(0,255), random(0,255) }.

random(A, B) when is_integer(A), A =< B ->   
    random:uniform((B - A) + 1) - 1 + A.
