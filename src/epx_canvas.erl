%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Canvas operations
%%% @end
%%% Created :  2 Jan 2021 by Tony Rogvall <tony@rogvall.se>

-module(epx_canvas).

-export([create/0]). %% clear/0, copy/1]).
%% primitives
-export([line/4, line_params/2]).
-export([ellipse/3, ellipse/4]).
-export([set_color/3, set_operation/3, set_params/5, set_params/8]).
%% composite
-export([intersect/4, intersect/5, intersect/6, intersect/3]).
-export([union/4, union/5, union/6, union/3]).
-export([line_segment/5]).
-export([triangle/5, rectangle/6, polygon/3]).
-export([ellipse_r/5]).
-export([circle/4]).
-export([circle_arc/6]).
-export([circle_border/5]).
-export([ellipse_border/5]).
-export([ellipse_border_r/6]).

create() ->	 
    epx:canvas_create().

%% wrapper
set_color(Canvas, Elem, Color) ->
    epx:canvas_set_color(Canvas, Elem, Color).

%% wrapper
set_operation(Canvas, Elem, Operation) ->
    epx:canvas_set_operation(Canvas, Elem, Operation).

set_params(Canvas, Elem, D, E, F) ->
    epx:canvas_set_params(Canvas, Elem, D, E, F).

set_params(Canvas, Elem, A, B, C, D, E, F) ->
    epx:canvas_set_params(Canvas, Elem, A, B, C, D, E, F).

%% Ax + By = C
%% S(x,y) = Ax + By - C = 0
%% S(x+1,y) = Ax+A+By-C = S(x,y) + Delta(x,y) = 0
%% Delta(x,y) = A
%% 
%% Delta = A => when |S|<A use 1/A as anti-alias effect?
%%
%% Line defined passing the points (X1,Y1) - (X2,Y2)
%% Dx + Ey = F 
%% y = -D/Ex + F/E
%% 
%% -D/E = (y2-y1)/(x2-x1)
%%  D = -(y2-y1)
%%  E = (x2-x1)
%%  F = (x1y2 - x2y1)
%%

line(Canvas,P1,P2,Opts) ->
    {D,E,F} = line_params(P1,P2),
    line_(Canvas, D, E, F, Opts).

line_params({X1,Y1},{X2,Y2}) ->
    D = (Y1-Y2),
    E = (X2-X1),
    F = (X1*Y2 - X2*Y1),
    {D, E, F};
line_params({X1,Y1},A) when is_number(A) ->  %% A in radians
    X2 = X1 + math:cos(A),
    Y2 = Y1 + math:sin(A),
    D = (Y1-Y2),
    E = (X2-X1),
    F = (X1*Y2 - X2*Y1),
    {D, E, F}.

line_(Canvas,D,E,F,Opts) ->
    Color = get_color(Opts),
    I = epx:canvas_line(Canvas,D,E,F),
    set_color(Canvas, I, Color),
    I.

triangle(Canvas,P0,P1,P2,Opts) ->
    intersect(Canvas,
	      line(Canvas,P0,P1,#{}),
	      line(Canvas,P1,P2,#{}),
	      line(Canvas,P2,P0,#{}), Opts).

rectangle(Canvas,P0,P1,P2,P3,Opts) ->
    intersect(Canvas,
	      line(Canvas,P0,P1,#{}),
	      line(Canvas,P1,P2,#{}),
	      line(Canvas,P2,P3,#{}),
	      line(Canvas,P3,P0,#{}), Opts).

polygon(Canvas,Xs,Opts) ->
    XYs = lines(Canvas,Xs,hd(Xs),[]),
    intersect(Canvas,XYs,Opts).

lines(Canvas,[P1|Xs=[P2|_]], P0, Acc) ->
    lines(Canvas,Xs, P0, [line(Canvas,P1,P2,#{})|Acc]);
lines(Canvas,[Pn], P0, Acc) ->
    lists:reverse([line(Canvas,Pn,P0,#{})|Acc]).

line_segment(Canvas,{X1,Y1},{X2,Y2},W,Opts) ->
    Dx = X2-X1,
    Dy = Y2-Y1,
    A = math:atan2(Dy, Dx),
    W2 = W/2,
    A0 = (A - math:pi()/2),
    X3 = X1+W2*math:cos(A0),
    Y3 = Y1+W2*math:sin(A0),
    X4 = X2+W2*math:cos(A0),
    Y4 = Y2+W2*math:sin(A0),
    %% io:format("(~w,~w) - (~w,~w)\n", [X3,Y3,X4,Y4]),
    A1 = (A0 + math:pi()),
    X5 = X1+W2*math:cos(A1),
    Y5 = Y1+W2*math:sin(A1),
    X6 = X2+W2*math:cos(A1),
    Y6 = Y2+W2*math:sin(A1),
    %% io:format("(~w,~w) - (~w,~w)\n", [X5,Y5,X6,Y6]),
    %% fixme add end-caps
    P0 = {X4,Y4},
    P1 = {X3,Y3},
    P2 = {X5,Y5},
    P3 = {X6,Y6},
    intersect(Canvas,
	      line(Canvas,P0,P1,#{}),
	      line(Canvas,P1,P2,#{}),
	      line(Canvas,P2,P3,#{}),
	      line(Canvas,P3,P0,#{}),
	      Opts).

%% ellipse (x=0, y=0)
%% 
%% Ax^2 + Bxy + Cy^2 = F
%% A = a^2*sin(t)^2 + b^2*cos(t)^2
%% C = 2(b^2 - a^2)*sin(t)*cos(t)
%% B = a^2*cos(t)^2 + b^2*sin(t)^2
%% F = -a^2*b^2
%% a = hor axis, b = ver axis
%%
%%  S(x,y) = Ax2 + Bxy + Cy2 - F = 0
%%  S(x+1,y) = S(x,y) + Delta(x,y)
%%
%%  Delta(x,y) = A + By + 2Ax
%% 
ellipse(Canvas,{EA,EB},Opts) ->
    Theta = maps:get(theta, Opts, 0),
    Ct = if Theta =:= 0 -> 1.0; true -> math:cos(Theta) end,
    St = if Theta =:= 0 -> 0.0; true -> math:sin(Theta) end,
    EA2 = EA*EA,
    EB2 = EB*EB,
    St2 = St*St,
    Ct2 = Ct*Ct,
    A = EA2*St2 + EB2*Ct2,
    B = 2*(EB2 - EA2)*St*Ct,
    C = EA2*Ct2 + EB2*St2,
    F2 = -EA2*EB2,
    Color = get_color(Opts),
    I = epx:canvas_quad(Canvas, A, B, C, 0.0, 0.0, F2),
    epx:canvas_set_color(Canvas, I, Color),
    I.

%%
%%  S(x,y) = Ax2 + Bxy Cy2 + Dx + Ey + F = 0
%%  S(x+1,y) = S(x,y) + Delta(x,y)
%%  Delta(x,y) = A+D+By+2Ax
%%

ellipse(Canvas,{X0,Y0},AB,Opts)
  when X0 == 0, Y0 == 0 -> ellipse(Canvas,AB,Opts);
ellipse(Canvas,{X0,Y0},{EA,EB},Opts) ->
    Theta = maps:get(theta, Opts, 0),
    Ct = if Theta =:= 0 -> 1.0; true -> math:cos(Theta) end,
    St = if Theta =:= 0 -> 0.0; true -> math:sin(Theta) end,
    EA2 = EA*EA,
    EB2 = EB*EB,
    St2 = St*St,
    Ct2 = Ct*Ct,
    A = EA2*St2 + EB2*Ct2,
    B = 2*(EB2 - EA2)*St*Ct,
    C = EA2*Ct2 + EB2*St2,
    D = -2*A*X0 - B*Y0,
    E = -B*X0 - 2*C*Y0,
    F1 = A*X0*X0 + B*X0*Y0 + C*Y0*Y0,
    F2 = -EA2*EB2,
    Color = get_color(Opts),
    I = epx:canvas_quad(Canvas, A, B, C, D, E, F1+F2),
    epx:canvas_set_color(Canvas, I, Color),
    I.

ellipse_r_param({X0,Y0},{X1,Y1}, R) ->
    Dx = X1-X0,
    Dy = Y1-Y0,
    Theta = math:atan2(-Dy,Dx),
    F = math:sqrt(Dx*Dx + Dy*Dy)/2,
    Xc = (X0+X1)/2,
    Yc = (Y0+Y1)/2,
    A = R/2,
    B = math:sqrt(F*F - A*F),
    {{Xc,Yc},{A,B},Theta}.

ellipse_r(Canvas,P0,P1,R,Opts) ->
    {Pc,AB,Theta} = ellipse_r_param(P0,P1,R),
    ellipse(Canvas, Pc, AB, Opts#{ theta => Theta }).

circle(Canvas,Pc,R,Opts) ->
    ellipse(Canvas,Pc,{R,R},Opts).

circle_arc(Canvas,Pc,R,A1,A2,Opts) ->
    C1 = ellipse(Canvas,Pc,{R,R},Opts),
    L1 = line(Canvas,Pc,A1,Opts),
    L2 = line(Canvas,Pc,A2,Opts),
    intersect(Canvas, C1, L1, L2, Opts).
    

circle_border(Canvas,Pc,R,W,Opts) ->
    BorderColor = get_argb(border_color, Opts, black),
    Inner = ellipse(Canvas,Pc,{R,R},Opts),
    Outer = ellipse(Canvas,Pc,{R+W,R+W},Opts#{ color => BorderColor }),
    epx:canvas_over(Canvas, Inner, Outer).

ellipse_border(Canvas,Pc,AB={A,B},W,Opts) when W >= 0 ->
    BorderColor = get_argb(border_color, Opts, black),
    ABw = {A+W,B+W},
    Inner = ellipse(Canvas,Pc,AB,Opts),
    Outer = ellipse(Canvas,Pc,ABw,Opts#{ color => BorderColor }),
    epx:canvas_over(Canvas, Inner, Outer).

ellipse_border_r(Canvas,P0,P1,R,W,Opts) when W >= 0 ->
    BorderColor = get_argb(border_color, Opts, black),
    {Pc,AB={A,B},Theta} = ellipse_r_param(P0, P1, R),
    ABw = {A+W,B+W},
    Opts1 = Opts#{ theta => Theta },
    Inner = ellipse(Canvas,Pc,AB,Opts1),
    Outer = ellipse(Canvas,Pc,ABw,Opts1#{ color => BorderColor }),
    epx:canvas_over(Canvas, Inner, Outer).

%% joint operations
intersect(Canvas,A,B,Opts) ->
    Color = get_color(Opts),
    I = epx:canvas_and(Canvas, A, B),
    epx:canvas_set_color(Canvas, I, Color),
    I.

intersect(Canvas,A,B,C,Opts) ->
    Color = get_color(Opts),
    I1 = epx:canvas_and(Canvas, B, C),
    I = epx:canvas_and(Canvas, A, I1),
    epx:canvas_set_color(Canvas, I, Color),
    I.    

intersect(Canvas,A,B,C,D,Opts) ->
    Color = get_color(Opts),
    I1 = epx:canvas_and(Canvas, A, B),
    I2 = epx:canvas_and(Canvas, C, D),
    I = epx:canvas_and(Canvas, I1, I2),
    epx:canvas_set_color(Canvas, I, Color),
    I.

intersect(Canvas,[A], Opts) ->
    Color = get_color(Opts),
    set_color(Canvas, A, Color);
intersect(Canvas,[A,B], Opts) -> intersect(Canvas,A,B,Opts);
intersect(Canvas,[A,B,C], Opts) -> intersect(Canvas,A,B,C,Opts);
intersect(Canvas,[A,B,C,D], Opts) -> intersect(Canvas,A,B,C,D,Opts);
intersect(Canvas,List, Opts) when is_list(List) ->
    Color = get_color(Opts),
    L = length(List),
    {List1,List2} = lists:split(L div 2, List),
    Opts1 = maps:remove(color, Opts),
    I1 = intersect(Canvas, List1, Opts1),
    I2 = intersect(Canvas, List2, Opts1),
    I = epx:canvas_and(Canvas, I1, I2),
    epx:canvas_set_color(Canvas, I, Color),
    I.


union(Canvas,A,B,Opts) ->
    Color = get_color(Opts),
    I = epx:canvas_or(Canvas, A, B),
    epx:canvas_set_color(Canvas, I, Color),
    I.

union(Canvas,A,B,C,Opts) ->
    Color = get_color(Opts),
    I1 = epx:canvas_or(Canvas, B, C),
    I = epx:canvas_or(Canvas, A, I1),
    epx:canvas_set_color(Canvas, I, Color),
    I.    

union(Canvas,A,B,C,D,Opts) ->
    Color = get_color(Opts),
    I1 = epx:canvas_or(Canvas, A, B),
    I2 = epx:canvas_or(Canvas, C, D),
    I = epx:canvas_or(Canvas, I1, I2),
    epx:canvas_set_color(Canvas, I, Color),
    I.

union(Canvas,[A], Opts) ->
    Color = get_color(Opts),
    set_color(Canvas, A, Color);
union(Canvas,[A,B], Opts) -> union(Canvas,A,B,Opts);
union(Canvas,[A,B,C], Opts) -> union(Canvas,A,B,C,Opts);
union(Canvas,[A,B,C,D], Opts) -> union(Canvas,A,B,C,D,Opts);
union(Canvas,List, Opts) when is_list(List) ->
    Color = get_color(Opts),
    L = length(List),
    {List1,List2} = lists:split(L div 2, List),
    Opts1 = maps:remove(color, Opts),
    I1 = union(Canvas, List1, Opts1),
    I2 = union(Canvas, List2, Opts1),
    I = epx:canvas_or(Canvas, I1, I2),
    epx:canvas_set_color(Canvas, I, Color),
    I.


get_argb(Key, Opts, Default) ->
    argb(maps:get(Key, Opts, Default)).

get_argb(Key, Opts) ->
    get_argb(Key, Opts, {0,0,0,0}).

get_color(Opts) ->
    get_argb(color, Opts).

argb(Name) when is_atom(Name); is_list(Name) ->
    {R,G,B} = epx_color:from_name(Name),
    {255,R,G,B};
argb({R,G,B}) ->
    {255,R,G,B}; 
argb(Color={_A,_R,_G,_B}) -> 
    Color.
