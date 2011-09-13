%%% File    : epx_view.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : View managment
%%%               Handle a view matrix stack
%%% Created :  4 Jun 2009 by Tony Rogvall <tony@rogvall.se>

-module(epx_view).

-export([push/1, pop/1]).
-export([put_pixel/4, put_pixels/6, get_pixel/3]).
-export([draw_point/3, draw_line/5,  draw_rectangle/5, draw_ellipse/5]).
-export([draw/2]).
-export([set_clip/5]).
-export([moveto/3, lineto/3, turnto/2]).
-export([move/2, move/3, line/2, line/3, turn/2]).

-export([identity/1, translate/3, scale/3, rotate/2]).
-export([deg_norm/1]).
-export([set_orientation/2, get_orientation/1]).
-export([set_position/2, get_position/1]).
-export([x/3, y/3, height/2, width/2, point/2, rect/2]).

-record(t2d,
	{
	  sx=1.0, ry=0.0,
	  rx=0.0, sy=1.0,
	  tx=0.0, ty=0.0
	 }).

t2d_dxf(Vm, X, Y) ->
    X*Vm#t2d.sx + Y*Vm#t2d.ry.

t2d_dyf(Vm, X, Y) ->
    X*Vm#t2d.rx + Y*Vm#t2d.sy.

t2d_xf(Vm, X, Y) ->
    X*Vm#t2d.sx + Y*Vm#t2d.ry + Vm#t2d.tx.

t2d_yf(Vm, X, Y) ->
    X*Vm#t2d.rx + Y*Vm#t2d.sy + Vm#t2d.ty.

t2d_hf(Vm, H) ->
    H*Vm#t2d.sy.

t2d_wf(Vm, W) ->
    W*Vm#t2d.sx.


identity(Pixmap) ->
    set_vm(Pixmap, #t2d{}).

%% Get view matrix
get_vm(Pixmap) ->
    case get({'EPX_VM',Pixmap}) of
	undefined ->
	    identity(Pixmap);
	Vm ->
	    Vm
    end.

%% Set a view matrix for the Pixmap
set_vm(Pixmap, Vm) ->
    put({'EPX_VM',Pixmap},Vm),
    Vm.


get_position(Pixmap) ->
    case get({'EPX_XY',Pixmap}) of
	undefined -> {0,0};
	Pos -> Pos
    end.

set_position(Pixmap,Pos={_X,_Y}) ->
    put({'EPX_XY',Pixmap},Pos),
    Pos.

-define(PI, 3.141592653589793).

deg_norm(A) when is_number(A) ->
    N = trunc(A / 360),
    if A < 0 ->
	    A - ((N-1)*360);
       true ->
	    A - N*360
    end.

deg_to_rad(Deg) ->
    Deg * (?PI/180.0).

get_orientation(Pixmap) ->
    case get({'EPX_A',Pixmap}) of
	undefined -> 0.0;
	Angle -> Angle
    end.

set_orientation(Pixmap, Deg) when is_number(Deg) ->
    Deg1 = deg_norm(Deg),
    put({'EPX_A',Pixmap}, Deg1),
    Deg1.


set_clip(Pixmap,X,Y,W,H) ->
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    Wi = t2d_w(Vm, W),
    Hi = t2d_h(Vm, H),
    epx:pixmap_set_clip(Pixmap, {Xi, Yi, Wi, Hi}).



push(Pixmap) ->
    Vm    = get_vm(Pixmap),
    Clip  = epx:pixmap_info(Pixmap, clip),
    Pos   = get_position(Pixmap),
    Deg   = get_orientation(Pixmap),
    Elem = {Vm,Clip,Pos,Deg},
    case get({'EPX_VM_STACK',Pixmap}) of
	undefined ->
	    put({'EPX_VM_STACK',Pixmap}, [Elem]);
	Stack ->
	    put({'EPX_VM_STACK',Pixmap}, [Elem|Stack])
    end.

pop(Pixmap) ->
    [{Vm,Clip,Pos,Deg}|Stack] = get({'EPX_VM_STACK',Pixmap}),
    put({'EPX_VM_STACK',Pixmap}, Stack),
    set_vm(Pixmap, Vm),
    set_position(Pixmap, Pos),
    set_orientation(Pixmap, Deg),
    case Clip of
	undefined -> Vm;
	Clip -> epx:pixmap_set_clip(Pixmap,Clip), Vm
    end.


draw(Pixmap,Fun) ->
    %%  Should implement copy on write!?
    push(Pixmap),
    %% Do NOT pop inside fun, unless intensional ;-)
    try Fun() of
	Res -> Res
    catch
	error:Reason ->
	    io:format("draw: crashed, ~p\n", [erlang:get_stacktrace()]),
	    {error,Reason};
	exit:Reason ->
	    io:format("draw: exit, reason = ~p\n", [erlang:get_stacktrace()]),
	    {error,Reason};
	thrown:Value ->
	    Value
    after 
	pop(Pixmap)
    end.


t2d_x(Vm, X, Y) ->
    round(t2d_xf(Vm, X, Y)).

t2d_y(Vm, X, Y) ->
    round(t2d_yf(Vm, X, Y)).

t2d_h(Vm, H) ->
    round(t2d_hf(Vm, H)).

t2d_w(Vm, W) ->
    round(t2d_wf(Vm, W)).

%%t2d_hi(Vm, H) ->
%%    round(H*(1/Vm#t2d.sy)).

%%t2d_wi(Vm, W) ->
%%    round(W*(1/Vm#t2d.sx)).

x(Pixmap, X, Y) ->
    t2d_x(get_vm(Pixmap), X, Y).

y(Pixmap, X, Y) ->
    t2d_y(get_vm(Pixmap), X, Y).

point(Pixmap, {X,Y}) ->
    Vm = get_vm(Pixmap),
    X1 = t2d_x(Vm, X,Y),    
    Y1 = t2d_y(Vm, X,Y),
    {X1,Y1}.

width(Pixmap, W) ->
    t2d_w(get_vm(Pixmap), W).

height(Pixmap, H) ->
    t2d_h(get_vm(Pixmap), H).

rect(Pixmap, {X,Y,W,H}) ->
    Vm = get_vm(Pixmap),
    X1 = t2d_x(Vm, X, Y),    
    Y1 = t2d_y(Vm, X, Y),
    W1 = t2d_w(Vm, W),
    H1 = t2d_h(Vm, H),
    {X1,Y1,W1,H1}.

%%
%% Translate - Transform "matrix"
%%  | sx  ry  tx |   | 1 0 x |     | sx ry sx*x+ry*y+tx |
%%  | rx  sy  ty | * | 0 1 y |  =  | rx sy rx*x+sy*y+ty |
%%  | 0   0   1  |   | 0 0 1 |     | 0  0  1            |
%%
translate(Pixmap,X,Y) when is_number(X), is_number(Y) ->
    Vm = get_vm(Pixmap),
    Tx = Vm#t2d.tx + Vm#t2d.sx*X + Vm#t2d.ry*Y,
    Ty = Vm#t2d.ty + Vm#t2d.rx*X + Vm#t2d.sy*Y,
    set_vm(Pixmap, Vm#t2d { tx=Tx, ty=Ty}).


%% Scale - Transform "matrix"
%%  | sx  ry  tx |   | x   0   0 |    | sx*x ry*y tx |
%%  | rx  sy  ty | * | 0   y   0 | =  | rx*x sy*y ty |
%%  | 0   0   1  |   | 0   0   1 |    | 0    0    1  |
%%
scale(Pixmap, X, Y) when is_number(X), is_number(Y) ->
    Vm = get_vm(Pixmap),
    Sx = Vm#t2d.sx*X,
    Rx = Vm#t2d.rx*X,
    Ry = Vm#t2d.ry*Y,
    Sy = Vm#t2d.sy*Y,
    set_vm(Pixmap, Vm#t2d { sx=Sx, rx=Rx, ry=Ry, sy=Sy}).

%% Rotate - Transform "matrix"
%%  | sx  ry  tx |   | c  -s   0 |     | sx*c+ry*s  -sx*s+ry*c tx |
%%  | rx  sy  ty | * | s   c   0 |  =  | rx*c+sy*s  -rx*s+sy*c ty |
%%  | 0   0   1  |   | 0   0   1 |     | 0          0          1  |
%%
    
rotate(Pixmap, Angle) ->
    A = deg_to_rad(deg_norm(Angle)),
    C = math:cos(A),
    S = math:sin(A),
    Vm = get_vm(Pixmap),
    Sx =  Vm#t2d.sx*C + Vm#t2d.ry*S,
    Ry = -Vm#t2d.sx*S + Vm#t2d.ry*C,
    Rx = Vm#t2d.rx*C + Vm#t2d.sy*S,
    Sy = -Vm#t2d.rx*S + Vm#t2d.sy*C,
    set_vm(Pixmap, Vm#t2d { sx=Sx, rx=Rx, ry=Ry, sy=Sy}).

%%
%% moveto/lineto interface
%%
moveto(Pixmap,X0,Y0) when is_number(X0), is_number(Y0) ->
    Vm = get_vm(Pixmap),
    X1 = t2d_xf(Vm, X0, Y0),
    Y1 = t2d_yf(Vm, X0, Y0),
    set_position(Pixmap, {X1,Y1}).
    
lineto(Pixmap,X,Y) when is_number(X), is_number(Y) ->
    Vm = get_vm(Pixmap),
    {X0,Y0} = get_position(Pixmap),
    X1 = t2d_xf(Vm, X, Y),
    Y1 = t2d_yf(Vm, X, Y),
    epx:draw_line(Pixmap, trunc(X0), trunc(Y0), trunc(X1), trunc(Y1)),
    set_position(Pixmap, {X1,Y1}).

turnto(Pixmap,Deg) when is_number(Deg) ->
    set_orientation(Pixmap, Deg). 

move(Pixmap,Dx0,Dy0) when is_number(Dx0), is_number(Dy0) ->
    Vm = get_vm(Pixmap),
    {X0,Y0} = get_position(Pixmap),
    X1 = X0 + t2d_dxf(Vm, Dx0, Dy0),
    Y1 = Y0 + t2d_dyf(Vm, Dx0, Dy0),
    set_position(Pixmap, {X1,Y1}).

line(Pixmap,Dx0,Dy0) when is_number(Dx0), is_number(Dy0) ->
    Vm = get_vm(Pixmap),
    {X0,Y0} = get_position(Pixmap),
    X1 = X0 + t2d_dxf(Vm, Dx0, Dy0),
    Y1 = Y0 + t2d_dyf(Vm, Dx0, Dy0),
    epx:draw_line(Pixmap,trunc(X0),trunc(Y0),trunc(X1), trunc(Y1)),
    set_position(Pixmap, {X1,Y1}).

move(Pixmap,Len) when is_number(Len) ->
    A = deg_to_rad(get_orientation(Pixmap)),
    move(Pixmap, math:cos(A)*Len, math:sin(A)*Len).

line(Pixmap,Len) when is_number(Len) ->
    A = deg_to_rad(get_orientation(Pixmap)),
    line(Pixmap, math:cos(A)*Len, math:sin(A)*Len).

 turn(Pixmap,Deg) when is_number(Deg) ->
    set_orientation(Pixmap, get_orientation(Pixmap) + Deg).

%%
%% Pixmap interface
%%
draw_point(Pixmap, X, Y) ->
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    epx:draw_point(Pixmap, Xi, Yi).

draw_line(Pixmap, X0, Y0, X1, Y1) ->
    Vm = get_vm(Pixmap),
    Xi0 = t2d_x(Vm, X0, Y0),
    Yi0 = t2d_y(Vm, X0, Y0),
    Xi1 = t2d_x(Vm, X1, Y1),
    Yi1 = t2d_y(Vm, X1, Y1),
    epx:draw_line(Pixmap, Xi0, Yi0, Xi1, Yi1).

draw_rectangle(Pixmap, X, Y, W, H) ->    
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    %% should really draw polygon!
    Wi = t2d_w(Vm, W),
    Hi = t2d_h(Vm, H),
    epx:draw_rectangle(Pixmap,Xi,Yi,Wi,Hi).

draw_ellipse(Pixmap, X, Y, A, B) ->
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    %% should really draw rotated ellipse!
    Ai = t2d_w(Vm, A),
    Bi = t2d_h(Vm, B),
    epx:draw_ellipse(Pixmap,Xi,Yi,Ai,Bi).

put_pixel(Pixmap,X,Y,Pixel) ->
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    epx:pixmap_put_pixel(Pixmap,Xi,Yi,Pixel).

put_pixels(Pixmap, X, Y, W, H, Ps) ->
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    %% Wi = t2d_w(Vm, W),
    %% Hi = t2d_h(Vm, W),
    epx:pixmap_put_pixels(Pixmap,Xi,Yi,W,H,Ps).    

get_pixel(Pixmap,X,Y) ->
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    epx:pixmap_get_pixel(Pixmap,Xi,Yi).
