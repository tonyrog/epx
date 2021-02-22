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
%%%    Rectangular routines
%%% @end
%%% Created :  4 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_rect).

-export([union/2, union/1]).
-export([intersect/2, intersect/1]).
-export([is_subrect/2]).
-export([add_point/2]).
-export([contains/2]).

%% union of two rectangles
union(undefined, R) -> R;
union(R, undefined) -> R;
union({X1,Y1,W1,H1}, {X2,Y2,W2,H2}) ->
    Xl = erlang:min(X1,X2),
    Xr = erlang:max(X1+W1, X2+W2),
    Yt = erlang:min(Y1,Y2),
    Yb = erlang:max(Y1+H1, Y2+H2),
    {Xl, Yt, Xr-Xl, Yb-Yt}.

union([R]) -> R;
union([R|Rs]) ->
    union(R,union(Rs)).

intersect(undefined, R) -> R;
intersect(R, undefined) -> R;
intersect({X1,Y1,W1,H1}, {X2,Y2,W2,H2}) ->
    Xl = erlang:max(X1,X2),
    Xr = erlang:min(X1+W1, X2+W2),
    if Xl >= Xr ->
	    {0,0,0,0};
       true ->
	    Yt = erlang:max(Y1,Y2),
	    Yb = erlang:min(Y1+H1, Y2+H2),
	    if Yt >= Yb ->
		    {0,0,0,0};
	       true ->
		    {Xl, Yt, Xr-Xl, Yb-Yt}
	    end
    end.

intersect([R]) -> R;
intersect([R|Rs]) ->
    intersect(R,intersect(Rs)).

%% is Rect A a proper sub-rect of B (intersect(A,B) == A)
is_subrect(_A, undefined) -> true;
is_subrect({Xa,Ya,Wa,Ha}, {Xb,Yb,Wb,Hb}) ->
    (Xa >= Xb) andalso ((Xa+Wa) =< (Xb+Wb)) andalso
    (Ya >= Yb) andalso ((Ya+Ha) =< (Yb+Hb)).

%% Add Point P to rectangle A = union(A, {Px,Py,1,1})
add_point(undefined, {Xp,Yp}) -> {Xp,Yp,1,1};
add_point(undefined, {Xp,Yp,_Zp}) -> {Xp,Yp,1,1};
add_point(A, {Xp,Yp}) -> union(A, {Xp,Yp,1,1});
add_point(A, {Xp,Yp,_Zp}) -> union(A, {Xp,Yp,1,1}).

%% return true if Point is inside the Rect, false otherwise
-spec contains(Rect::undefined|epx:epx_rect(), 
	       Point::epx:epx_point2d()|epx:epx_point3d()) -> boolean.

contains({X,Y,W,H}, {Xp,Yp}) ->
    (Xp >= X) andalso (Xp < X+W) andalso (Yp >= Y) andalso (Yp < Y+H);
contains({X,Y,W,H}, {Xp,Yp,_Zp}) ->
    (Xp >= X) andalso (Xp < X+W) andalso (Yp >= Y) andalso (Yp < Y+H);
contains(undefined, _) ->
    false.
