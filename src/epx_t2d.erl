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
%%%   2D transformations
%%% @end
%%% Created : 16 Oct 2011 by Tony Rogvall <tony@rogvall.se>

-module(epx_t2d).

-export([identity/0, translate/3, scale/3, rotate/2, compose/2]).
-export([deg_norm/1, deg_to_rad/1]).
-export([x/3, y/3, dx/3, dy/3, height/2, width/2, 
	 dimension/3, dimension/2, delta/3, delta/2,
	 point/3, point/2, points/2, 
	 rectangle/5, rectangle/2, rectangle_to_poly/2
	]).

-include("../include/epx_t2d.hrl").

-type(t2d() :: #t2d{}).
-export_type([t2d/0]).

-define(xp(T,X,Y), (X)*(T)#t2d.sx + (Y)*(T)#t2d.ry + (T)#t2d.tx).
-define(yp(T,X,Y), (X)*(T)#t2d.rx + (Y)*(T)#t2d.sy + (T)#t2d.ty).
-define(wp(T,W), abs((W)*(T)#t2d.sx)).
-define(hp(T,H), abs((H)*(T)#t2d.sy)).

dx(T, X, Y) -> X*T#t2d.sx + Y*T#t2d.ry.
dy(T, X, Y) -> X*T#t2d.rx + Y*T#t2d.sy.

x(T, X, Y) -> ?xp(T,X,Y).
y(T, X, Y) -> ?yp(T,X,Y).

height(T, H) -> ?hp(T,H).
width(T, W) ->  ?wp(T,W).

dimension(T, {W, H}) ->
    dimension(T, W, H).

dimension(T, W, H) ->
    {?hp(T,W), ?wp(T,H)}.


point(T, {X, Y}) ->
    point(T, X, Y).
point(T, X, Y) ->
    {?xp(T,X,Y), ?yp(T,X,Y)}.

points(T, XYs) ->
    #t2d { sx=Sx,ry=Ry,rx=Rx,sy=Sy,tx=Tx,ty=Ty } = T,
    [{ X*Sx + Y*Ry + Tx, X*Rx + Y*Sy + Ty } || {X,Y} <- XYs].
	      
delta(T, {Dx,Dy}) ->
    delta(T, Dx, Dy).

delta(T, Dx, Dy) ->
    {Dx*T#t2d.sx + Dy*T#t2d.ry,
     Dx*T#t2d.rx + Dy*T#t2d.sy }.

rectangle(T, {X, Y, W, H}) ->
    rectangle(T, X, Y, W, H).    

rectangle(T, X, Y, W, H) ->
    {?xp(T,X,Y), ?yp(T,X,Y), W*T#t2d.sx, H*T#t2d.sy}.

rectangle_to_poly(T, {X,Y,W,H}) ->
    points(T, [{X,Y},{X+W,Y},{X+W,Y+H},{X,Y+H}]).

%%
%% Get the identity transformation
%%

identity() ->
    #t2d {}.

%% @doc
%% Compose - Transform "matrix"
%%  | a b e | | sx  ry  tx |   | a*sx+b*rx  a*ry+b*sy a*tx+b*ty+e |
%%  | c d f |*| rx  sy  ty | = | c*sx+d*rx  c*ry+d*sy c*tx+d*ty+f |
%%  | 0 0 1 | | 0   0   1  |   | 0          0         1           |
%% @end
-spec compose(t2d(), t2d()) -> t2d().
compose(T, S) ->
    A = S#t2d.sx,
    B = S#t2d.ry,
    C = S#t2d.rx,
    D = S#t2d.sy,
    E = S#t2d.tx,
    F = S#t2d.ty,

    Sx = A*T#t2d.sx + B*T#t2d.rx,
    Ry = A*T#t2d.ry + B*T#t2d.sy,
    Tx = A*T#t2d.tx + B*T#t2d.ty + E,

    Rx = C*T#t2d.sx + D*T#t2d.rx,
    Sy = C*T#t2d.ry + D*T#t2d.sy,
    Ty = C*T#t2d.tx + D*T#t2d.ty + F,
    T#t2d { sx=Sx, rx=Rx, ry=Ry, sy=Sy, tx=Tx, ty=Ty}.

%% @doc
%% Translate - Transform "matrix"
%%  | sx  ry  tx |   | 1 0 x |     | sx ry sx*x+ry*y+tx |
%%  | rx  sy  ty | * | 0 1 y |  =  | rx sy rx*x+sy*y+ty |
%%  | 0   0   1  |   | 0 0 1 |     | 0  0  1            |
%% @end
-spec translate(t2d(), number(), number()) -> t2d().
translate(T,Tx,Ty) when is_number(Tx), is_number(Ty) ->
    Tx1 = T#t2d.sx*Tx + T#t2d.ry*Ty + T#t2d.tx,
    Ty1 = T#t2d.rx*Tx + T#t2d.sy*Ty + T#t2d.ty,
    T#t2d { tx=Tx1, ty=Ty1}.

%% @doc
%%  Scale - Transform "matrix"
%%  | sx  ry  tx |   | x   0   0 |    | sx*x ry*y tx |
%%  | rx  sy  ty | * | 0   y   0 | =  | rx*x sy*y ty |
%%  | 0   0   1  |   | 0   0   1 |    | 0    0    1  |
%% @end
-spec scale(t2d(), number(), number()) -> t2d().
scale(T, Sx, Sy) when is_number(Sx), is_number(Sy) ->
    Sx1 = T#t2d.sx*Sx,
    Ry1 = T#t2d.ry*Sy,
    Rx1 = T#t2d.rx*Sx,
    Sy1 = T#t2d.sy*Sy,
    T#t2d { sx=Sx1, ry=Ry1, rx=Rx1, sy=Sy1 }.

%% @doc
%% Rotate - Transform "matrix"
%%  | sx  ry  tx |   | c  -s   0 |     | sx*c+ry*s  -sx*s+ry*c tx |
%%  | rx  sy  ty | * | s   c   0 |  =  | rx*c+sy*s  -rx*s+sy*c ty |
%%  | 0   0   1  |   | 0   0   1 |     | 0          0          1  |
%% @end
-spec rotate(t2d(), number()) -> t2d().
rotate(T, Angle) ->
    A = deg_to_rad(deg_norm(Angle)),
    C = math:cos(A),
    S = math:sin(A),

    Sx =  T#t2d.sx*C + T#t2d.ry*S,  Ry = -T#t2d.sx*S + T#t2d.ry*C,
    Rx =  T#t2d.rx*C + T#t2d.sy*S,  Sy = -T#t2d.rx*S + T#t2d.sy*C,

    T#t2d { sx=Sx, rx=Rx, ry=Ry, sy=Sy }.

%% @doc
%% Normalize angle to -180..180
%% @end
deg_norm(A) when is_number(A) ->
    N = trunc(A / 360),
    if A < 0 ->
	    A - ((N-1)*360);
       true ->
	    A - N*360
    end.

%% @doc
%% Convert degrees to radians
%% @end

deg_to_rad(Deg) ->
    Deg * (math:pi()/180.0).

