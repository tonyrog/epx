%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%    Rectangular routines
%%% @end
%%% Created :  4 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_rect).

-compile(export_all).

%% union of two rectangles
union(undefined, R) -> R;
union(R, undefined) -> R;
union({X1,Y1,W1,H1}, {X2,Y2,W2,H2}) ->
    Xl = erlang:min(X1,X2),
    Xr = erlang:max(X1+W1-1, X2+W2-1),
    Yt = erlang:min(Y1,Y2),
    Yb = erlang:max(Y1+H1-1, Y2+H2-1),
    {Xl, Yt, Xr-Xl+1, Yb-Yt+1}.

union([R]) -> R;
union([R|Rs]) ->
    union(R,union(Rs)).

intersect(undefined, R) -> R;
intersect(R, undefined) -> R;
intersect({X1,Y1,W1,H1}, {X2,Y2,W2,H2}) ->
    Xl = erlang:max(X1,X2),
    Xr = erlang:min(X1+W1-1, X2+W2-1),
    if Xl > Xr ->
	    {0,0,0,0};
       true ->
	    Yt = erlang:max(Y1,Y2),
	    Yb = erlang:min(Y1+H1-1, Y2+H2-1),
	    if Yt > Yb ->
		    {0,0,0,0};
	       true ->
		    {Xl, Yt, Xr-Xl+1, Yb-Yt+1}
	    end
    end.

intersect([R]) -> R;
intersect([R|Rs]) ->
    intersect(R,intersect(Rs)).

%% is Rect A a proper sub-rect of B (intersect(A,B) == A)
is_subrect(_A, undefined) -> true;
is_subrect({Xa,Ya,Wa,Ha}, {Xb,Yb,Wb,Hb}) ->
    (Xa >= Xb) andalso ((Xa+Wa-1) =< (Xb+Wb-1)) andalso
    (Ya >= Yb) andalso ((Ya+Ha-1) =< (Yb+Hb-1)).

%% Add Point P to rectangle A = union(A, {Px,Py,1,1})
add_point(undefined, {Xp,Yp}) ->
    {Xp,Yp,1,1};
add_point({Xa,Ya,Wa,Ha}, {Xp,Yp}) ->
    Xl = erlang:min(Xa, Xp),
    Xr = erlang:max(Xa+Wa-1, Xp+1),
    Yt = erlang:min(Ya, Yp),
    Yb = erlang:max(Ya+Ha-1, Yp+1),
    {Xl, Yt, Xr-Xl+1, Yb-Yt+1}.    
    


       


    

