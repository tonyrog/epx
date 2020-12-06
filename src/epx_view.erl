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
%%% File    : epx_view.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : View managment
%%%               Handle a view matrix stack
%%% Created :  4 Jun 2009 by Tony Rogvall <tony@rogvall.se>

-module(epx_view).

-export([push/1, pop/1]).
-export([put_pixel/3,put_pixel/4]).
-export([put_pixels/3, put_pixels/6]).
-export([get_pixel/2, get_pixel/3]).
-export([draw_point/2, draw_point/3]).
-export([draw_line/3, draw_line/5]).
-export([draw_rectangle/2, draw_rectangle/5]).
-export([draw_ellipse/2, draw_ellipse/5]).
-export([draw/2]).
-export([show/2, show/3]).
-export([set_clip/5]).
-export([moveto/3, lineto/3, turnto/2]).
-export([move/2, move/3, line/2, line/3, turn/2]).

-export([identity/1, translate/3, scale/3, rotate/2]).
-export([set_orientation/2, get_orientation/1]).
-export([set_position/2, get_position/1]).
-export([x/3, y/3, height/2, width/2, point/2, rectangle/2]).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

identity(Pixmap) ->
    set_vm(Pixmap, epx_t2d:identity()).

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

get_orientation(Pixmap) ->
    case get({'EPX_A',Pixmap}) of
	undefined -> 0.0;
	Angle -> Angle
    end.

set_orientation(Pixmap, Deg) when is_number(Deg) ->
    Deg1 = epx_t2d:deg_norm(Deg),
    put({'EPX_A',Pixmap}, Deg1),
    Deg1.


set_clip(Pixmap,X,Y,W,H) ->
    Vm = get_vm(Pixmap),
    {X1,Y1,W1,H1} = epx_t2d:rectangle(Vm,X,Y,W,H),
    epx:pixmap_set_clip(Pixmap,X1,Y1,W1,H1).

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
	?EXCEPTION(error,Reason,Stacktrace) ->
	    io:format("draw: crashed, ~p\n", [?GET_STACK(Stacktrace)]),
	    {error,Reason};
	?EXCEPTION(exit,Reason,Stacktrace) ->
	    io:format("draw: exit, reason = ~p\n", [?GET_STACK(Stacktrace)]),
	    {error,Reason};
	thrown:Value ->
	    Value
    after 
	pop(Pixmap)
    end.

x(Pixmap, X, Y) ->
    epx_t2d:x(get_vm(Pixmap), X, Y).

y(Pixmap, X, Y) ->
    epx_t2d:y(get_vm(Pixmap), X, Y).

point(Pixmap, Point) ->
    epx_t2d:point(get_vm(Pixmap), Point).

width(Pixmap, W) ->
    epx_t2d:width(get_vm(Pixmap), W).

height(Pixmap, H) ->
    epx_t2d:height(get_vm(Pixmap), H).

rectangle(Pixmap, Rect) ->
    epx_t2d:rectangle(get_vm(Pixmap),Rect).

translate(Pixmap,X,Y) when is_number(X), is_number(Y) ->
    T1 = epx_t2d:translate(get_vm(Pixmap),X,Y),
    set_vm(Pixmap, T1).

scale(Pixmap, X, Y) when is_number(X), is_number(Y) ->
    T1 = epx_t2d:scale(get_vm(Pixmap), X, Y),
    set_vm(Pixmap, T1).

rotate(Pixmap, Angle) ->
    T1 = epx_t2d:rotate(get_vm(Pixmap), Angle),
    set_vm(Pixmap, T1).

%%
%% moveto/lineto interface
%%
moveto(Pixmap,X0,Y0) when is_number(X0), is_number(Y0) ->
    Vm = get_vm(Pixmap),
    NewPosition = epx_t2d:point(Vm,X0,Y0),
    set_position(Pixmap, NewPosition).
    
lineto(Pixmap,X,Y) when is_number(X), is_number(Y) ->
    Vm = get_vm(Pixmap),
    {X0,Y0} = get_position(Pixmap),
    {X1,Y1} = epx_t2d:point(Vm,X,Y),
    epx:draw_line(Pixmap, X0, Y0, X1, Y1),
    set_position(Pixmap, {X1,Y1}).

turnto(Pixmap,Deg) when is_number(Deg) ->
    set_orientation(Pixmap, Deg). 

move(Pixmap,Dx0,Dy0) when is_number(Dx0), is_number(Dy0) ->
    Vm = get_vm(Pixmap),
    {X0,Y0} = get_position(Pixmap),
    {X1,Y1} = epx_t2d:delta(Vm,Dx0,Dy0),
    set_position(Pixmap, {X0+X1,Y0+Y1}).

line(Pixmap,Dx0,Dy0) when is_number(Dx0), is_number(Dy0) ->
    Vm = get_vm(Pixmap),
    {X0,Y0} = get_position(Pixmap),
    {X1,Y1} = epx_t2d:delta(Vm,Dx0,Dy0),
    X = X0 + X1,
    Y = Y0 + Y1,
    epx:draw_line(Pixmap,X0,Y0,X,Y),
    set_position(Pixmap, {X,Y}).

move(Pixmap,Len) when is_number(Len) ->
    A = epx_t2d:deg_to_rad(get_orientation(Pixmap)),
    move(Pixmap, math:cos(A)*Len, math:sin(A)*Len).

line(Pixmap,Len) when is_number(Len) ->
    A = epx_t2d:deg_to_rad(get_orientation(Pixmap)),
    line(Pixmap, math:cos(A)*Len, math:sin(A)*Len).

turn(Pixmap,Deg) when is_number(Deg) ->
    set_orientation(Pixmap, get_orientation(Pixmap) + Deg).

%%
%% Pixmap interface
%%
draw_point(Pixmap, {X, Y}) ->
    draw_point(Pixmap, X, Y).
draw_point(Pixmap, X, Y) ->
    Vm = get_vm(Pixmap),
    {X1,Y1} = epx_t2d:point(Vm,X,Y),
    epx:draw_point(Pixmap,X1,Y1).

draw_line(Pixmap, {X0,Y0}, {X1,Y1}) ->
    draw_line(Pixmap, X0, Y0, X1, Y1).
draw_line(Pixmap, X0, Y0, X1, Y1) ->
    Vm = get_vm(Pixmap),
    {X2,Y2} = epx_t2d:point(Vm,X0,Y0),
    {X3,Y3} = epx_t2d:point(Vm,X1,Y1),
    epx:draw_line(Pixmap,X2,Y2,X3,Y3).

draw_rectangle(Pixmap, {X, Y, W, H}) ->    
    draw_rectangle(Pixmap, X, Y, W, H).
draw_rectangle(Pixmap, X, Y, W, H) ->    
    Vm = get_vm(Pixmap),
    {X1,Y1} = epx_t2d:point(Vm,X,Y),
    %% should really draw polygon!
    {W1,H1} = epx_t2d:dimension(Vm,W,H),
    epx:draw_rectangle(Pixmap,X1,Y1,W1,H1).

draw_ellipse(Pixmap, {X, Y, A, B}) ->
    draw_ellipse(Pixmap, X,Y, A,B).
draw_ellipse(Pixmap, X, Y, A, B) ->
    Vm = get_vm(Pixmap),
    {X1,Y1} = epx_t2d:point(Vm,X,Y),
    %% should really draw rotated ellipse!
    {A1,B1} = epx_t2d:dimension(Vm,A,B),
    epx:draw_ellipse(Pixmap,X1,Y1,A1,B1).

put_pixel(Pixmap,{X,Y},Pixel) ->
    put_pixel(Pixmap,X,Y,Pixel).
put_pixel(Pixmap,X,Y,Pixel) ->
    Vm = get_vm(Pixmap),
    {X1,Y1} = epx_t2d:point(Vm,X,Y),
    epx:pixmap_put_pixel(Pixmap,X1,Y1,Pixel).

put_pixels(Pixmap, {X, Y, W, H}, Ps) ->
    put_pixels(Pixmap, X, Y, W, H, Ps).
put_pixels(Pixmap, X, Y, W, H, Ps) ->
    Vm = get_vm(Pixmap),
    {X1,Y1} = epx_t2d:point(Vm,X,Y),
    %% fixme: scale etc..
    epx:pixmap_put_pixels(Pixmap,X1,Y1,W,H,Ps).    

get_pixel(Pixmap,{X,Y}) ->
    get_pixel(Pixmap,X,Y).
get_pixel(Pixmap,X,Y) ->
    Vm = get_vm(Pixmap),
    {X1,Y1} = epx_t2d:point(Vm,X,Y),
    epx:pixmap_get_pixel(Pixmap,X1,Y1).

%% utility to setup window and draw som content 
show(Fun, WindowRect) ->
    show(Fun, WindowRect, [configure]).

show(Fun, {X,Y,W,H}, Events) ->
    epx:start(),
    Win = epx:window_create(X,Y,W,H,Events),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(W,H,argb),
    epx:pixmap_fill(Pix, white),
    epx:pixmap_attach(Pix),
    epx:pixmap_draw(Pix,Win,0,0,0,0,W, H),
    %%  prepare epx_view
    identity(Pix),
    draw(Pix, fun() -> Fun(Pix) end),
    epx:pixmap_draw(Pix,Win,0,0,0,0,W, H),
    (fun DRAW() ->
	     receive
		 {epx_event,Win, close} ->
		     ok;
		 {epx_event,_Win,{configure, _Rect}} ->
		     epx:pixmap_fill(Pix, white),
		     identity(Pix),
		     draw(Pix, fun() -> Fun(Pix) end),
		     epx:pixmap_draw(Pix,Win,0,0,0,0,W, H),
		     DRAW();
		 %% fixme: handle zoom(+/-) scroll(arrows)
		 Event ->
		     io:format("epx_view: got ~w\n", [Event]),
		     DRAW()
	     end

     end)(),
    epx:window_detach(Win),
    epx:pixmap_detach(Pix),
    ok.
