%%% File    : epx_view.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : View managment
%%%               Handle a view matrix stack
%%% Created :  4 Jun 2009 by Tony Rogvall <tony@rogvall.se>

-module(epx_view).

-export([push/1, pop/1]).
-export([put_pixel/4, put_pixels/6, get_pixel/3]).
-export([draw_point/3, draw_line/5,  draw_rectangle/5, draw_ellipse/5]).
-export([set_clip/5]).
-export([moveto/3, lineto/3]).
-export([identity/1, translate/3, scale/3, rotate/2]).
-export([angle/1]).
-export([x/3, y/3, height/2, width/2]).

-record(t2d,
	{
	  sx=1.0, ry=0.0,
	  rx=0.0, sy=1.0,
	  tx=0.0, ty=0.0
	 }).


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

get_pos(Pixmap) ->
    case get({'EPX_XY',Pixmap}) of
	undefined -> {0,0};
	Pos -> Pos
    end.

set_clip(Pixmap,X,Y,W,H) ->
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    Wi = t2d_w(Vm, W),
    Hi = t2d_h(Vm, H),
    epx:pixmap_set_clip(Pixmap, Xi, Yi, Wi, Hi).

push(Pixmap) ->
    Vm   = get_vm(Pixmap),
    Clip = epx:pixmap_info(Pixmap, clip),
    Pos = get_pos(Pixmap),
    case get({'EPX_VM_STACK',Pixmap}) of
	undefined ->
	    put({'EPX_VM_STACK',Pixmap}, [{Vm,Clip,Pos}]);
	Stack ->
	    put({'EPX_VM_STACK',Pixmap}, [{Vm,Clip,Pos}|Stack])
    end.

pop(Pixmap) ->
    [{Vm,Clip,Pos}|Stack] = get({'EPX_VM_STACK',Pixmap}),
    put({'EPX_VM_STACK',Pixmap}, Stack),
    put({'EPX_VM',Pixmap}, Vm),
    put({'EPX_XY',Pixmap}, Pos),
    case Clip of
	undefined -> Vm;
	{X,Y,W,H} -> epx:pixmap_set_clip(Pixmap,X,Y,W,H), Vm
    end.

t2d_x(Vm, X, Y) ->
    round(X*Vm#t2d.sx + Y*Vm#t2d.ry + Vm#t2d.tx).

t2d_y(Vm, X, Y) ->
    round(X*Vm#t2d.rx + Y*Vm#t2d.sy + Vm#t2d.ty).

t2d_h(Vm, H) ->
    round(H*Vm#t2d.sy).

t2d_w(Vm, W) ->
    round(W*Vm#t2d.sx).

%%t2d_hi(Vm, H) ->
%%    round(H*(1/Vm#t2d.sy)).

%%t2d_wi(Vm, W) ->
%%    round(W*(1/Vm#t2d.sx)).

x(Pixmap, X, Y) ->    
    t2d_x(get_vm(Pixmap), X, Y).

y(Pixmap, X, Y) ->    
    t2d_y(get_vm(Pixmap), X, Y).

width(Pixmap, W) ->
    t2d_w(get_vm(Pixmap), W).

height(Pixmap, H) ->
    t2d_h(get_vm(Pixmap), H).

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
-define(PI, 3.141592653589793).

angle(A) when is_number(A) ->
    N = trunc(A / 360),
    if A < 0 ->
	    A - ((N-1)*360);
       true ->
	    A - N*360
    end.
    
rotate(Pixmap, Angle) ->
    A = angle(Angle),
    C = math:cos(A* (?PI/180.0)),
    S = math:sin(A* (?PI/180.0)),
    Vm = get_vm(Pixmap),
    Sx =  Vm#t2d.sx*C + Vm#t2d.ry*S,
    Ry = -Vm#t2d.sx*S + Vm#t2d.ry*C,
    Rx = Vm#t2d.rx*C + Vm#t2d.sy*S,
    Sy = -Vm#t2d.rx*S + Vm#t2d.sy*C,
    set_vm(Pixmap, Vm#t2d { sx=Sx, rx=Rx, ry=Ry, sy=Sy}).
%%
%% moveto/lineto interface
%%
moveto(Pixmap,X,Y) ->
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    put({'EPIX_XY',Pixmap}, {Xi,Yi}).

lineto(Pixmap,X,Y) ->
    Vm = get_vm(Pixmap),
    {X0,Y0} = get_pos(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    put({'EPX_XY',Pixmap}, {Xi,Yi}),
    epx:pixmap_draw_line(Pixmap, X0, Y0, Xi, Yi).

%%
%% Pixmap interface
%%
draw_point(Pixmap, X, Y) ->
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    epx:pixmap_draw_point(Pixmap, Xi, Yi).

draw_line(Pixmap, X0, Y0, X1, Y1) ->
    Vm = get_vm(Pixmap),
    Xi0 = t2d_x(Vm, X0, Y0),
    Yi0 = t2d_y(Vm, X0, Y0),
    Xi1 = t2d_x(Vm, X1, Y1),
    Yi1 = t2d_y(Vm, X1, Y1),
    epx:pixmap_draw_line(Pixmap, Xi0, Yi0, Xi1, Yi1).

draw_rectangle(Pixmap, X, Y, W, H) ->    
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    Wi = t2d_w(Vm, W),
    Hi = t2d_h(Vm, H),
    epx:pixmap_draw_rectangle(Pixmap,Xi,Yi,Wi,Hi).

draw_ellipse(Pixmap, X, Y, A, B) ->
    Vm = get_vm(Pixmap),
    Xi = t2d_x(Vm, X, Y),
    Yi = t2d_y(Vm, X, Y),
    Ai = t2d_w(Vm, A),
    Bi = t2d_h(Vm, B),
    epx:pixmap_draw_ellipse(Pixmap,Xi,Yi,Ai,Bi).

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
