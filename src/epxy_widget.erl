%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2026, Rogvall Invest AB, <tony@rogvall.se>
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
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Widget library (the widgets of epxy)
%%%
%%%    A widget is a map, see defaults/0 for the keys. All functions
%%%    are free of process state; the caller keeps the widgets and
%%%    acts on the effects returned by event/3.
%%%
%%%    Positions (XY) passed to draw/3, event/3 and hit/3 are the
%%%    absolute top left corner of the widget, see pos/2.
%%% @end
%%%-------------------------------------------------------------------

-module(epxy_widget).

-export([new/1, set/2, get/2, keys/0, defaults/0]).
-export([draw/3, event/3, hit/3, pos/2, bounds/2]).
-export([tab_at_location/3, is_animating/1, menu_dimension/1]).
-export([parse_color/1, text_expand/2]).
%% Utils
-export([color_add/2, color_sub/2, color_interpolate/3, color_add_argb/2,
	 color_interpolate_argb/3]).

-include("../include/epx.hrl").
-include("../include/epx_image.hrl").

-type widget_type() :: window | panel | button | switch | slider | value |
		       menu | rectangle | ellipse | line | text | user.
-type widget() :: #{ atom() => term() }.
-type xy() :: {number(), number()}.
-type effect() :: {signal, Env::map()} | motion_on | motion_off.
-export_type([widget/0, widget_type/0, effect/0]).

-define(dbg(F,A), ok).
%%-define(dbg(F,A), io:format((F),(A))).
-define(error(F,A), io:format((F),(A))).
-define(warning(F,A), io:format((F),(A))).

-define(TABS_X_OFFSET, 10).  %% should scale with size!
-define(TABS_Y_OFFSET, 10).  %% should scale with size!
-define(TABS_X_PAD, 16).      %% should scale with font size!
-define(TABS_Y_PAD, 8).      %% should scale with font size!
-define(TABS_COLOR, 16#ffcccccc).  %% configure this

-define(DEFAULT_FONT, [{name,"Arial"},{size,12}]).

defaults() ->
    #{
      id => undefined,     %% (structured) name of widget
      type => undefined,   %% widget_type()
      window => undefined, %% id of base window (if type != window)
      state  => normal,    %% normal,active
      focus  => false,     %% has key focus
      static => false,     %% object may not be deleted
      hidden => false,     %% show/hiden false,true,all,none
      disabled => false,   %% true | false | none | all | rest
      edit => false,       %% allow edit of text fields
      relative => true,    %% childrens are relative to parent
      manual => false,     %% manual control over state (release)
      user => undefined,   %% mfa when type=user
      filter => undefined, %% mfa for event filter
      children_first => true, %% draw/select children first
      last => false,       %% draw last (ontop)
      x => 0, y => 0,
      z => 0,              %% define the order for overlap
      width => 0, height => 0,
      max_length => 0,     %% 0=no limit
      text => "",
      tabs => [],
      items => [],         %% menu items
      border => undefined,
      border_color => 16#00000000,
      shadow_x => undefined, shadow_y => undefined,
      round_w => undefined, round_h => undefined,
      orientation => horizontal,
      image => undefined, image2 => undefined, topimage => undefined,
      animation => undefined, animation2 => undefined,
      frame => undefined, frame2 => undefined,
      color => undefined, color2 => undefined,
      active_color => undefined,
      font_color => 16#00000000,
      active_font_color => undefined,
      fill => none,
      events => undefined,
      halign => center,    %% text alignment left|right|center
      valign => center,    %% text alignment top|bottom|center
      min => undefined, max => undefined, %% type=value|slider
      format => "~w",      %% io_lib:format format
      value => 0,          %% type=value|slider
      vscale => undefined, %% scale value (multiplier)
      animate => undefined,  %% animation state.
      animate2 => undefined, %% animation state of second animation.
      font => undefined,   %% type=text|button|value
      win => undefined,    %% type=window
      backing => undefined,
      user_data => undefined
     }.

keys() ->
    maps:keys(defaults()).

%% Create a widget from properties, raise error on failure
-spec new(Props::[{atom(),term()}]) -> widget().
new(Props) ->
    W = set((defaults())#{ font => default_font() }, Props),
    case W of
	#{ type := user } ->
	    case user(W,init,undefined,undefined,undefined) of
		W1 when is_map(W1) -> W1;
		_ -> W
	    end;
	_ ->
	    W
    end.

default_font() ->
    case get({?MODULE,default_font}) of
	undefined ->
	    Font = case epx_font:match(?DEFAULT_FONT) of
		       false -> undefined;
		       {ok,F} -> F
		   end,
	    put({?MODULE,default_font}, Font),
	    Font;
	Font -> Font
    end.

%% Set properties, invalid keys and values are ignored
-spec set(W::widget(), Props::[{atom(),term()}]) -> widget().
set(W, Props) ->
    W1 = set_(W, Props, false),
    update(W1).

set_(W, [{Key,Value}|Ps], Clamp) ->
    case maps:is_key(Key, W) of
	false ->
	    ?dbg("key ~p not found, ignored\n", [Key]),
	    set_(W, Ps, Clamp);
	true ->
	    Clamp1 = Clamp orelse (Key =:= value) orelse
		(Key =:= min) orelse (Key =:= max),
	    case validate(Key, Value) of
		true ->
		    set_(W#{ Key => Value }, Ps, Clamp1);
		{true,Value1} ->
		    set_(W#{ Key => Value1 }, Ps, Clamp1);
		false ->
		    ?dbg("value ~p for key ~p not valid, ignored\n",
			 [Value,Key]),
		    set_(W, Ps, Clamp1)
	    end
    end;
set_(W, [], true) ->
    #{ value := V, min := Min, max := Max } = W,
    W#{ value => clamp(V, Min, Max) };
set_(W, [], false) ->
    W.

-spec get(W::widget(), Keys::[atom()]) -> [{atom(),term()}].
get(W, Keys) ->
    [{K,maps:get(K,W)} || K <- Keys, maps:is_key(K, W)].

%% Check automatic adjustments needed after create/set
update(W = #{ type := menu }) ->
    {Width,Height} = menu_dimension(W),
    W#{ width => Width, height => Height };
update(W) ->
    W.

%% return true|false|{true,Value'}
validate(type,Type) ->
    lists:member(Type,[window,panel,button,switch,slider,value,menu,
		       rectangle,ellipse,line,text,user]);
validate(id,ID) when is_atom(ID) -> {true, atom_to_list(ID)};
validate(id,ID) -> is_list(ID);
validate(window,ID) when is_atom(ID) -> {true, atom_to_list(ID)};
validate(window,ID) -> is_list(ID);
validate(user,{M,F,As}) ->
    is_atom(M) andalso is_atom(F) andalso is_list(As);
validate(filter,{M,F,As}) ->
    is_atom(M) andalso is_atom(F) andalso is_list(As);
validate(static,Arg) -> is_boolean(Arg);
validate(hidden,Arg) -> lists:member(Arg,[true,false,none,all]);
validate(disabled,Arg) -> lists:member(Arg,[true,false,none,all,rest]);
validate(edit,Arg) -> is_boolean(Arg);
validate(focus,Arg) -> is_boolean(Arg);
validate(x,Arg) -> is_number(Arg);
validate(y,Arg) -> is_number(Arg);
validate(z,Arg) -> is_number(Arg);
validate(shadow_x,Arg) -> is_number(Arg);
validate(shadow_y,Arg) -> is_number(Arg);
validate(round_w,Arg) -> is_number(Arg);
validate(round_h,Arg) -> is_number(Arg);
validate(children_first,Arg) -> is_boolean(Arg);
validate(last,Arg) -> is_boolean(Arg);
validate(relative,Arg) -> is_boolean(Arg);
validate(manual,Arg) -> is_boolean(Arg);
validate(width,Arg) -> is_number(Arg) andalso (Arg >= 0);
validate(height,Arg) -> is_number(Arg) andalso (Arg >= 0);
validate(max_length,Arg) -> is_integer(Arg) andalso (Arg >= 0);
validate(text,Arg) when is_list(Arg) -> true;
validate(text,Arg) when is_atom(Arg) -> {true,atom_to_list(Arg)};
validate(tabs,Arg) -> is_list(Arg);
validate(items,Arg) -> is_list(Arg);
validate(border,Arg) -> is_number(Arg);
validate(border_color,Arg) -> validate_color(Arg);
validate(orientation,Arg) -> lists:member(Arg,[horizontal,vertical]);
validate(image,Arg) -> validate_image(Arg);
validate(image2,Arg) -> validate_image(Arg);
validate(topimage,Arg) -> validate_image(Arg);
validate(animation,Arg) -> validate_animation(Arg);
validate(animation2,Arg) -> validate_animation(Arg);
validate(frame,Arg) -> is_integer(Arg);
validate(frame2,Arg) -> is_integer(Arg);
validate(animate,Arg) -> lists:member(Arg,[continuous,sequence,undefined]);
validate(animate2,Arg) -> lists:member(Arg,[continuous,sequence,undefined]);
validate(font,Arg) -> validate_font(Arg);
validate(color,Arg) -> validate_color(Arg);
validate(color2,Arg) -> validate_color(Arg);
validate(active_color,Arg) -> validate_color(Arg);
validate(font_color,Arg) -> validate_color(Arg);
validate(active_font_color,Arg) -> validate_color(Arg);
validate(fill,Arg) -> lists:member(Arg,[solid,blend,sum,aalias,textured,none]);
validate(events,Arg) when is_list(Arg) ->
    lists:all(fun(E) -> validate_event(E) end, Arg);
validate(events,Arg) -> validate_event(Arg);
validate(halign,Arg) -> lists:member(Arg,[left,right,center]);
validate(valign,Arg) -> lists:member(Arg,[top,bottom,center]);
validate(min,Arg) -> is_number(Arg);
validate(max,Arg) -> is_number(Arg);
validate(value,Arg) -> is_number(Arg);
validate(vscale,Arg) -> is_number(Arg);
validate(format,Arg) -> is_list(Arg);
validate(state,Arg) -> lists:member(Arg,[active,normal]);
validate(user_data,_Arg) -> true;
validate(_,_Arg) -> false.

validate_event(Arg) ->
    lists:member(Arg,[key_press,key_release,motion,button_press,button_release,
		      focus_in,focus_out,focus,enter,leave,configure,resize,
		      crossing,button,left,middle,right,wheel,wheel_up,
		      wheel_down,wheel_left,wheel_right,close,destroyed,
		      all,none]).

validate_color(Arg) ->
    case parse_color(Arg) of
	false -> false;
	C -> {true,C}
    end.

validate_font(Arg) when is_list(Arg) ->
    case epx_font:match(Arg) of
	false ->
	    ?error("unable to load font ~p\n", [Arg]),
	    false;
	{ok,Font} ->
	    {true,Font}
    end;
validate_font(Arg) when is_record(Arg,epx_font) -> true;
validate_font(_) -> false.

validate_animation(Arg) when is_list(Arg) ->
    File = text_expand(Arg, []),
    try epx:animation_open(File) of
	Anim ->
	    ?dbg("open animation file ~s.\n",[File]),
	    {true,Anim}
    catch
	error:_Reason ->
	    ?error("unable to open animation file ~s:~p\n",
		   [File,_Reason]),
	    false
    end;
validate_animation(_) ->
    false.

validate_image(Arg) when is_list(Arg) ->
    File = text_expand(Arg, []),
    case epx_image:load(File) of
	{ok,Image} ->
	    ?dbg("load image file ~s.\n",[File]),
	    case Image#epx_image.pixmaps of
		[Pixmap] ->
		    {true,Pixmap};
		_ ->
		    ?warning("no pixmap found in ~s\n",[File]),
		    false
	    end;
	Error ->
	    ?error("unable to load image file ~s:~p\n",
		   [File,Error]),
	    false
    end;
validate_image(Arg) when is_record(Arg,epx_pixmap) -> true;
validate_image(_) -> false.

%%%===================================================================
%%% Geometry
%%%===================================================================

%% absolute position given the absolute position of the parent
-spec pos(W::widget(), ParentXY::xy()) -> xy().
pos(#{ relative := true, x := X, y := Y }, {Xo,Yo}) -> {X+Xo, Y+Yo};
pos(#{ x := X, y := Y }, _XY) -> {X, Y}.

%% rectangle covering everything the widget may draw
-spec bounds(W::widget(), XY::xy()) -> {number(),number(),number(),number()}.
bounds(W = #{ width := Width, height := Height }, {X,Y}) ->
    #{ border := B, shadow_x := Sx, shadow_y := Sy } = W,
    M = 4 + optional(B, 0) + max(abs(optional(Sx,0)), abs(optional(Sy,0)))*2,
    {X-M, Y-M, Width+2*M, Height+2*M}.

%% Check if Pos hits the widget at XY, return the widget (that a user
%% widget may update) or false
-spec hit(W::widget(), Pos::xy(), XY::xy()) -> false | {true, widget()}.
hit(#{ disabled := D }, _Pos, _XY) when D =:= true; D =:= all ->
    false;
hit(W, Pos={Xi,Yi}, XY={X,Y}) ->
    #{ width := Width, height := Height } = W,
    case in_rect(Xi,Yi,X,Y,Width,Height) of
	true ->
	    %% Option to allow select even outside bounding box?
	    case W of
		#{ type := user } ->
		    case user(W,select,user_pos(Pos),undefined,XY) of
			W1 when is_map(W1) -> {true, W1};
			_ -> false
		    end;
		_ ->
		    {true, W}
	    end;
	false ->
	    case topimage_at_location(W,Pos,XY,maps:get(topimage,W)) of
		true -> {true, W};
		false -> false
	    end
    end.

%% Check if (X,Y) is within any of the panel tabs
tab_at_location(W,{Xi,Yi},_XY={Xw,Yw}) ->
    {_Ascent,TextDims,MaxW,MaxH} = tabs_item_box(W),
    N = length(TextDims),
    Width =  (MaxW+?TABS_X_PAD),
    Height = (MaxH+?TABS_Y_PAD),
    case maps:get(orientation, W) of
	horizontal ->
	    Xoffs = (maps:get(width,W) - (Width*N)) div 2,
	    X0 = Xw + Xoffs,
	    Y0 = Yw + ?TABS_Y_OFFSET,
	    case in_rect(Xi,Yi,X0,Y0,Width*N,Height) of
		false -> 0;
		true  -> ((Xi-X0) div Width)+1
	    end;
	vertical ->
	    Yoffs = (maps:get(height,W) - (Height*N)) div 2,
	    Y0 = Yw + Yoffs,
	    X0 = Xw + ?TABS_X_OFFSET,
	    case in_rect(Xi,Yi,X0,Y0,Width,Height*N) of
		false -> 0;
		true -> ((Yi-Y0) div Height)+1
	    end
    end.

item_at_location(W,{Xi,Yi},_XY={Xw,Yw}) ->
    case length(maps:get(items,W)) of
	0 -> 0;
	N ->
	    #{ width := Width, height := Height } = W,
	    case in_rect(Xi,Yi,Xw,Yw,Width,Height) of
		false -> 0;
		true ->
		    H = Height div N,
		    ((Yi-Yw) div H)+1
	    end
    end.

in_rect(X,Y,Xr,Yr,Wr,Hr) ->
    (X >= Xr) andalso (X < Xr + Wr) andalso (Y >= Yr) andalso (Y < Yr + Hr).

%% Check if (X,Y) hit inside a topimage (used in slider)
topimage_at_location(_W,_Pos,_XY,undefined) ->
    false;
topimage_at_location(W=#{orientation:=horizontal},{X,Y},{Xi,Yi},Image) ->
    Height = epx:pixmap_info(Image,height),
    #{ width := Ww, height := Wh } = W,
    Y1 = Yi + (Wh - Height) div 2,
    Y2 = Yi + (Wh + Height) div 2,
    (X >= Xi) andalso (X < Xi + Ww) andalso (Y >= Y1) andalso (Y =< Y2);
topimage_at_location(W=#{orientation:=vertical},{X,Y},{Xi,Yi},Image) ->
    Width = epx:pixmap_info(Image,width),
    #{ width := Ww, height := Wh } = W,
    X1 = Xi + (Ww - Width) div 2,
    X2 = Xi + (Ww + Width) div 2,
    (Y >= Yi) andalso (Y < Yi + Wh) andalso (X >= X1) andalso (X =< X2).

menu_dimension(W) ->
    {_Ascent,TextDims,MaxW,MaxH} = menu_item_box(W),
    case maps:get(state, W) of
	active ->
	    N = length(TextDims),
	    {MaxW+?TABS_X_PAD,(MaxH+?TABS_Y_PAD)*N};
	_ ->
	    {MaxW+?TABS_X_PAD,MaxH+?TABS_Y_PAD}
    end.

is_animating(#{ animate := A1, animate2 := A2 }) ->
    (A1 =/= undefined) orelse (A2 =/= undefined).

%%%===================================================================
%%% Events
%%%===================================================================

%% Handle an event for widget at absolute position XY
%% Event positions are {X,Y} (integers) in the same coordinates as XY.
%% Return the updated widget and a list of effects:
%%   {signal, Env}  - signal subscribers, Env is a map with at least event
%%   motion_on      - the widget want motion events (drag)
%%   motion_off     - no more motion events needed
-spec event(Event::term(), W::widget(), XY::xy()) -> {widget(), [effect()]}.
event(Event, W, XY) ->
    case maps:get(filter, W) of
	undefined -> %% include
	    event_(Event, W, XY);
	Filter ->
	    try user_(Filter,filter,user_pos(Event),W,
		       #{ type => window, id => maps:get(window,W),
			  image => undefined },XY) of
		true -> event_(Event, W, XY);
		_ -> {W, []} %% discard
	    catch
		error:_ -> {W, []} %% discard
	    end
    end.

sig(Env) -> {signal, Env}.

event_(Event={button_press,_Button,Where},W,XY) ->
    case maps:get(type,W) of
	button ->
	    {W#{ state => active, value => 1 },
	     [sig(#{event=>button_press, value=>1, xy=>XY})]};
	switch ->
	    {WState,Value} =
		case maps:get(state,W) of
		    active -> {normal,0};
		    _ -> {active,1}
		end,
	    {W#{ frame => Value, state => WState, value => Value },
	     [sig(#{event=>changed, value=>Value, xy=>XY})]};
	slider ->
	    case slider_value(W,Where,XY) of
		false ->
		    {W, []};
		Value ->
		    {W#{ state => active, value => Value },
		     [motion_on, sig(#{event=>changed,value=>Value,xy=>XY})]}
	    end;
	panel ->
	    case tab_at_location(W,Where,XY) of
		0 ->
		    {W, []};
		Value ->
		    {W#{ value => Value },
		     [sig(#{event=>changed,value=>Value,xy=>XY})]}
	    end;
	menu ->
	    case item_at_location(W,Where,XY) of
		0 ->
		    {W, []};
		_ ->
		    W1 = W#{ state => active, last => true },
		    {Mw,Mh} = menu_dimension(W1),
		    {W1#{ width => Mw, height => Mh }, [motion_on]}
	    end;
	user ->
	    {user_event(Event, W, XY), []};
	text ->
	    W1 = case W of
		     #{ focus := true } -> W;
		     #{ edit := true } -> W#{ focus => true };
		     _ -> W
		 end,
	    {Xi,Yi} = XY,
	    {X,Y} = Where,
	    {W1, [sig(#{event=>button_press,x=>X-Xi,y=>Y-Yi,xy=>XY})]};
	_ ->
	    {Xi,Yi} = XY,
	    {X,Y} = Where,
	    {W#{ state => active },
	     [sig(#{event=>button_press,x=>X-Xi,y=>Y-Yi,xy=>XY})]}
    end;
event_(Event={button_release,_Button,Where},W,XY) ->
    Manual = maps:get(manual, W),
    case maps:get(type,W) of
	button ->
	    W1 = if Manual -> W;
		    true -> W#{ state => normal, value => 0 }
		 end,
	    {W1, [sig(#{event=>button_release,value=>0,xy=>XY})]};
	switch ->
	    {W, []};
	slider ->
	    {W#{ state => normal }, [motion_off]};
	panel ->
	    {W, []};
	menu ->
	    Value = maps:get(value, W),
	    W1 = W#{ state => normal, last => false },
	    {Mw,Mh} = menu_dimension(W1),
	    {W1#{ width => Mw, height => Mh },
	     [motion_off, sig(#{event=>changed,value=>Value,xy=>XY})]};
	user ->
	    {user_event(Event, W, XY), []};
	_ -> %% text and others
	    {Xi,Yi} = XY,
	    {X,Y} = Where,
	    W1 = if Manual -> W;
		    true -> W#{ state => normal }
		 end,
	    {W1, [sig(#{event=>button_release,x=>X-Xi,y=>Y-Yi,xy=>XY})]}
    end;
event_(Event={motion,_Button,Where},W,XY) ->
    case maps:get(type,W) of
	slider ->
	    case slider_value(W,Where,XY) of
		false -> {W, []};
		Value ->
		    {W#{ value => Value },
		     [sig(#{event=>motion,value=>Value,xy=>XY})]}
	    end;
	menu ->
	    case item_at_location(W,Where,XY) of
		0 -> {W, []};
		Value -> {W#{ value => Value }, []}
	    end;
	user ->
	    {user_event(Event, W, XY), []};
	_ ->
	    {W, []}
    end;
event_(_Event={key_press,Sym,_Mod,_Code},W=#{ type := text },XY) ->
    Text = maps:get(text, W),
    case Sym of
	$\r ->
	    {W#{ focus => false },
	     [sig(#{event=>changed,value=>Text,xy=>XY}),
	      sig(#{event=>focus_out,xy=>XY})]};
	$\e ->
	    {W#{ focus => false }, [sig(#{event=>focus_out,xy=>XY})]};
	$\t ->
	    {W#{ focus => false }, [sig(#{event=>focus_out,xy=>XY})]};
	$\b -> %% delete backwards
	    Text1 = case Text of
			"" -> "";
			_ -> lists:droplast(Text)
		    end,
	    {W#{ text => Text1 }, []};
	_ when is_integer(Sym), Sym >= $\s, Sym =< $~ ->
	    Max = maps:get(max_length, W),
	    Text1 = if Max =:= 0; length(Text) < Max -> Text ++ [Sym];
		       true -> Text
		    end,
	    {W#{ text => Text1 }, []};
	_ ->
	    {W, []}
    end;
event_(close,W,XY) ->
    {W#{ state => closed }, [sig(#{event=>closed,xy=>XY})]};
event_(Event,W=#{ type := user },XY) ->
    {user_event(Event, W, XY), []};
event_(_Event,W,_XY) ->
    {W, []}.

user_event(Event, W, XY) ->
    case user(W,event,user_pos(Event),undefined,XY) of
	W1 when is_map(W1) -> W1;
	_ -> W
    end.

%% Calculate the slider value given the position
slider_value(W=#{min:=Min,max:=Max,orientation:=horizontal},{X,_Y},{Xi,_Yi}) ->
    Width = maps:get(width,W)-2,
    if is_number(Min), is_number(Max), Width > 1 ->
	    X0 = Xi+1,
	    X1 = X0 + Width - 1,
	    Xv = clamp(X, X0, X1),
	    R = (Xv - X0) / (X1 - X0),
	    trunc(Min + R*(Max - Min));
       true ->
	    false
    end;
slider_value(W=#{min:=Min,max:=Max,orientation:=vertical},{_X,Y},{_Xi,Yi}) ->
    Height = maps:get(height,W)-2,
    if is_number(Min), is_number(Max), Height > 1 ->
	    Y1 = Yi+1,
	    Y0 = Y1 + Height - 1,
	    Yv = clamp(Y, Y1, Y0),
	    R = (Y0 - Yv) / (Y0 - Y1),
	    trunc(Min + R*(Max - Min));
       true ->
	    false
    end.

%% user widgets get positions as {X,Y,Z} (as epx window events)
user_pos({X,Y}) -> {X,Y,0};
user_pos({Tag,Button,{X,Y}}) -> {Tag,Button,{X,Y,0}};
user_pos(Other) -> Other.

%% user callback: apply(M,F,[Type,Event,Widget,Window,XY|As])
%% Window is a window widget with the window id and (when drawing) the
%% pixmap to draw in as image. For init Window is the window id.
user(W, init, Event, _Pixmap, XY) ->
    user_(maps:get(user, W), init, Event, W, maps:get(window, W), XY);
user(W, Type, Event, Pixmap, XY) ->
    Win = #{ type => window, id => maps:get(window, W), image => Pixmap },
    user_(maps:get(user, W), Type, Event, W, Win, XY).

user_({M,F,As},Type,Event,Widget,Pixmap,XY) ->
    apply(M,F,[Type,Event,Widget,Pixmap,XY|As]);
user_(undefined,_Type,_Event,_Widget,_Pixmap,_XY) ->
    undefined.

%%%===================================================================
%%% Drawing
%%%===================================================================

%% Draw widget into Pixmap at absolute position XY. Return the
%% widget, animation frames are stepped.
-spec draw(W::widget(), Pixmap::epx:epx_pixmap(), XY::xy()) -> widget().
draw(#{ hidden := H } = W, _Pix, _XY) when H =:= true; H =:= all ->
    W;
draw(W, Pix, XY={X,Y}) ->
    Res =
	case maps:get(type, W) of
	    window ->
		W;
	    user ->
		epx_gc:draw(fun() -> user(W,draw,undefined,Pix,XY), W end);
	    panel ->
		epx_gc:draw(fun() -> draw_tabs(Pix, X, Y, W), W end);
	    button ->
		epx_gc:draw(fun() -> draw_text_box(Pix, X, Y, W,
						   maps:get(text,W)) end);
	    switch ->
		epx_gc:draw(fun() -> draw_text_box(Pix, X, Y, W,
						   maps:get(text,W)) end);
	    slider ->
		epx_gc:draw(
		  fun() ->
			  W1 = draw_background(Pix, X, Y, W),
			  draw_border(Pix, X, Y, W, maps:get(border,W)),
			  draw_value_bar(Pix, X, Y, W, maps:get(topimage,W)),
			  W1
		  end);
	    value ->
		epx_gc:draw(fun() -> draw_text_box(Pix, X, Y, W,
						   value_text(W)) end);
	    menu ->
		epx_gc:draw(
		  fun() ->
			  W1 = draw_background(Pix, X, Y, W),
			  draw_menu(Pix, X, Y, W),
			  W1
		  end);
	    rectangle ->
		epx_gc:draw(
		  fun() ->
			  W1 = draw_background(Pix, X, Y, W),
			  draw_border(Pix, X, Y, W, maps:get(border,W)),
			  W1
		  end);
	    ellipse ->
		epx_gc:draw(
		  fun() ->
			  epx_gc:set_fill_style(maps:get(fill,W)),
			  set_color(W, maps:get(color,W)),
			  epx:draw_ellipse(Pix, X, Y,
					   maps:get(width,W), maps:get(height,W)),
			  W
		  end);
	    line ->
		epx_gc:draw(
		  fun() ->
			  set_color(W, maps:get(color,W)),
			  epx:draw_line(Pix, X, Y,
					X+maps:get(width,W)-1,
					Y+maps:get(height,W)-1),
			  W
		  end);
	    text ->
		epx_gc:draw(
		  fun() ->
			  draw_focus(Pix, X, Y, W),
			  draw_border(Pix, X, Y, W, maps:get(border,W)),
			  draw_text_box(Pix, X, Y, W, maps:get(text,W))
		  end);
	    _Type ->
		?dbg("bad widget type ~p\n", [_Type]),
		W
	end,
    if is_map(Res) -> Res;
       true -> W   %% draw crashed
    end.

value_text(W) ->
    Value = case maps:get(vscale,W) of
		undefined -> maps:get(value,W);
		Scale -> Scale * maps:get(value,W)
	    end,
    Format = maps:get(format,W),
    if Value =:= undefined ->
	    "-";
       Format =:= undefined ->
	    if is_integer(Value) -> integer_to_list(Value);
	       is_float(Value) -> io_lib_format:fwrite_g(Value);
	       true -> "?"
	    end;
       true ->
	    lists:flatten(io_lib:format(Format,[Value]))
    end.

%% draw widget button/value with centered text
draw_text_box(Pix, X, Y, W, Text) ->
    if is_list(Text), Text =/= "" ->
	    Font = maps:get(font,W),
	    epx_gc:set_font(Font),
	    Ascent = epx:font_info(Font, ascent),
	    {TxW,TxH} = epx_font:dimension(epx_gc:current(), Text),
	    {X1,Y1} = get_coord_xy(W,X,Y),
	    Width = case maps:get(width,W) of 0 -> TxW; Wi -> Wi end,
	    Height = case maps:get(height,W) of 0 -> TxH; Hi -> Hi end,
	    W1 = draw_background(Pix,X,Y,Width,Height,W),
	    FontColor = get_font_color(W, maps:get(state,W) =:= active),
	    set_font_color(FontColor),
	    draw_text(Pix,Ascent,Text,TxW,TxH,
		      X1,Y1,Width,Height,maps:get(halign,W),maps:get(valign,W)),
	    W1;
       true ->
	    draw_background(Pix, X, Y, W)
    end.

get_coord_xy(W,X0,Y0) ->
    case W of
	#{ state := active, shadow_x := Sx, shadow_y := Sy }
	  when is_number(Sx), is_number(Sy) ->
	    {X0+(Sx*2), Y0+(Sy*2)};
	_ ->
	    {X0, Y0}
    end.

%%  Put tabs as a row (horizontal) or column (vertical)
draw_tabs(Pix,X,Y,W) ->
    {Ascent,TextDims,MaxW,MaxH} = tabs_item_box(W),
    N = length(TextDims),
    Width =  (MaxW+?TABS_X_PAD),
    Height = (MaxH+?TABS_Y_PAD),
    Halign = maps:get(halign,W),
    Valign = maps:get(valign,W),
    case maps:get(orientation,W) of
	horizontal ->
	    X0 = X + (maps:get(width,W) - (Width*N)) div 2,
	    Y0 = (Y + ?TABS_Y_OFFSET),
	    set_color(W, ?TABS_COLOR),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Pix, X0, Y0, Width*N, Height),
	    draw_tab_items(Pix, W, 1, Ascent, TextDims, X0, Y0, Width, Height,
			   Halign, Valign, {Width,0}, false);
	vertical ->
	    Y0 =  Y + (maps:get(height,W) - (Height*N)) div 2,
	    X0 = (X + ?TABS_X_OFFSET),
	    set_color(W, ?TABS_COLOR),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Pix, X0, Y0, Width, Height*N),
	    draw_tab_items(Pix, W, 1, Ascent, TextDims, X0, Y0, Width, Height,
			   Halign, Valign, {0,Height}, false)
    end.

%% state=normal menu is drawn as the current item only or
%% blank if value=0, state=active full menu is draw
draw_menu(Pix,X,Y,W) ->
    {Ascent,TextDims0,MaxW,MaxH} = menu_item_box(W),
    Value = maps:get(value,W),
    {N,I,TextDims} =
	case maps:get(state,W) of
	    active -> {length(TextDims0),1,TextDims0};
	    _ ->
		try lists:nth(Value, TextDims0) of
		    TextDim -> {1,Value, [TextDim]}
		catch
		    error:_ -> {1,1,[{{MaxW,MaxH},""}]}
		end
	end,
    Width =  (MaxW+?TABS_X_PAD),
    Height = (MaxH+?TABS_Y_PAD),
    Y0 =  Y + (maps:get(height,W) - (Height*N)) div 2,
    X0 = X,
    set_color(W, ?TABS_COLOR),
    epx_gc:set_fill_style(solid),
    epx:draw_rectangle(Pix, X0, Y0, Width, Height*N),
    draw_tab_items(Pix, W, I, Ascent, TextDims, X0, Y0, Width, Height,
		   maps:get(halign,W), maps:get(valign,W), {0,Height}, true).

tabs_item_box(W) ->
    item_box(W, maps:get(tabs,W)).

menu_item_box(W) ->
    item_box(W, maps:get(items,W)).

item_box(W, Items) ->
    Font = maps:get(font,W),
    epx_gc:set_font(Font),
    Ascent = epx:font_info(Font, ascent),
    TextDims = [ {epx_font:dimension(epx_gc:current(),Text),Text} ||
		   Text <- Items ],
    MaxW = lists:max([0|[Wi || {{Wi,_},_} <- TextDims]]),
    MaxH = lists:max([0|[Hi || {{_,Hi},_} <- TextDims]]),
    {Ascent,TextDims,MaxW,MaxH}.

%% draw tab or menu items, step {Dx,Dy} between items
draw_tab_items(Pix, W, I, Ascent, [{{TxW,TxH},Text}|TextDims],
	       Xi, Yi, Width, Height, Halign, Valign, Step={Dx,Dy}, Menu) ->
    %% darken selected field
    case I =:= maps:get(value,W) of
	true ->
	    Color = color_sub(?TABS_COLOR, 16#00333333),
	    epx_gc:set_fill_color(Color),
	    epx_gc:set_fill_style(solid),
	    epx:draw_rectangle(Pix, Xi, Yi, Width, Height);
	false ->
	    ok
    end,
    epx_gc:set_foreground_color(16#00000000),
    epx_gc:set_fill_style(none),
    if Menu, I =:= 1 -> epx_gc:set_border_style([]);
       Menu -> epx_gc:set_border_style([ntop]);
       true -> ok
    end,
    epx:draw_rectangle(Pix, Xi, Yi, Width, Height),
    set_font_color(maps:get(font_color,W)),
    draw_text(Pix, Ascent, Text, TxW, TxH, Xi, Yi,
	      Width, Height, Halign, Valign),
    draw_tab_items(Pix, W, I+1, Ascent, TextDims,
		   Xi+Dx, Yi+Dy, Width, Height, Halign, Valign, Step, Menu);
draw_tab_items(_Pix, _W, _I, _Ascent, [],
	       _Xi, _Yi, _Width, _Height, _Halign, _Valign, _Step, _Menu) ->
    ok.

draw_text(Pix, Ascent, Text, TxW, TxH, X, Y, Width, Height, Halign, Valign) ->
    Xd = case Halign of
	     left  -> 0;
	     right -> Width - TxW;
	     center -> (Width-TxW) div 2
	 end,
    Yd = case Valign of
	     top -> 0;
	     bottom -> Height - TxH;
	     center -> (Height-TxH) div 2
	 end,
    epx:draw_string(Pix, X + Xd, Y + Yd + Ascent, Text).

%% draw background, return the widget with animation stepped
draw_background(Pix, X, Y, W=#{ width := Width, height := Height }) ->
    draw_background(Pix, X, Y, Width, Height, W).

draw_background(Pix, X, Y, Width, Height, W = #{ min := Min, max := Max }) ->
    Split = (Min =/= undefined) andalso (Max =/= undefined) andalso
	((maps:get(color2,W) =/= undefined) orelse
	 (maps:get(image2,W) =/= undefined) orelse
	 (maps:get(animation2,W) =/= undefined)),
    if Split ->
	    draw_split_background(Pix,X,Y,Width,Height,W);
       true ->
	    #{ color := Color, image := Image, animation := Anim,
	       frame := Frame, shadow_x := Xs, shadow_y := Ys } = W,
	    if is_number(Xs), is_number(Ys) ->
		    State = case W of
				#{ type := menu } -> normal;
				#{ state := S } -> S
			    end,
		    case State of
			normal ->
			    if Xs > 0; Ys > 0 ->
				    draw_one_background(Pix,W#{fill=>[blend]},
							X+Xs,Y+Ys,
							Width,Height,
							1, {85,0,0,0},
							Image, Anim, Frame);
			       true ->
				    ok
			    end,
			    draw_one_background(Pix, W, X, Y, Width, Height,
						1, Color, Image, Anim, Frame);
			active ->
			    if Xs > 0; Ys > 0 ->
				    draw_one_background(Pix,W#{fill=>[blend]},
							X+Xs,Y+Xs,
							Width,Height,
							1, {200,0,0,0},
							Image, Anim, Frame);
			       true ->
				    ok
			    end,
			    Xi = trunc(Xs) bsr 1,
			    Yi = trunc(Ys) bsr 1,
			    draw_one_background(Pix, W, X+Xi,Y+Yi,
						Width, Height,
						1, Color, Image, Anim, Frame);
			_ ->
			    draw_one_background(Pix, W, X, Y, Width, Height,
						1, Color, Image, Anim, Frame)
		    end;
	       true ->
		    draw_one_background(Pix, W, X, Y, Width, Height,
					1, Color, Image, Anim, Frame)
	    end
    end.

draw_split_background(Pix,X,Y,Width,Height,W=#{orientation := horizontal}) ->
    #{ color := Color, image := Image, animation := Anim, frame := Frame,
       color2 := Color2, image2 := Image2, animation2 := Anim2,
       frame2 := Frame2 } = W,
    R = value_proportion(W),
    W1 = draw_one_background(Pix, W, X, Y,
			     trunc(R*Width), Height,
			     1, Color, Image, Anim, Frame),
    draw_one_background(Pix, W1, X + trunc(R*Width), Y,
			Width - trunc(R*Width), Height,
			2, Color2, Image2, Anim2, Frame2);
draw_split_background(Pix,X,Y,Width,Height,W=#{orientation := vertical}) ->
    #{ color := Color, image := Image, animation := Anim, frame := Frame,
       color2 := Color2, image2 := Image2, animation2 := Anim2,
       frame2 := Frame2 } = W,
    R = value_proportion(W),
    Y0 = Y + Height - 1,
    %% Bottom part
    W1 = draw_one_background(Pix, W, X, trunc(Y0*(1-R) + Y*R),
			     Width, trunc(R*(Y0-Y)) + 1,
			     1, Color, Image, Anim, Frame),
    %% Top part
    draw_one_background(Pix, W1, X, Y,
			Width, trunc((1-R)*(Y0-Y)),
			2, Color2, Image2, Anim2, Frame2).

draw_one_background(Pix,W,X,Y,Width,Height,N,Color,Image,Anim,Frame) ->
    %% optionally draw background color
    if Color =:= undefined ->
	    ok;
       true ->
	    epx_gc:set_fill_style(maps:get(fill,W)),  %% fill, fill2!
	    set_color(W, Color),
	    #{ round_w := RW, round_h := RH } = W,
	    case W of
		#{ type := button } ->
		    epx:draw_roundrect(Pix,X,Y,Width,Height,
				       optional(RW, 8),optional(RH, 8));
		_ when RW =/= undefined; RH =/= undefined ->
		    epx:draw_roundrect(Pix,X,Y,Width,Height,
				       optional(RW, 0),optional(RH, 0));
		_ ->
		    epx:draw_rectangle(Pix,X,Y,Width,Height)
	    end
    end,
    %% optionally draw image possibly with rect as background
    if is_record(Image, epx_pixmap) ->
	    IWidth  = epx:pixmap_info(Image,width),
	    IHeight = epx:pixmap_info(Image,height),
	    if IWidth =:= Width, IHeight =:= Height ->
		    epx:pixmap_copy_area(Image, Pix,
					 0, 0, X, Y, Width, Height,
					 [blend]);
	       true ->
		    epx:pixmap_scale_area(Image, Pix,
					  0, 0, X, Y,
					  IWidth, IHeight,
					  Width, Height,
					  [blend])
	    end;
       true ->
	    ok
    end,
    %% optionally draw animation possibly with image as background
    if is_record(Anim, epx_animation) ->
	    AWidth  = epx:animation_info(Anim,width),
	    AHeight = epx:animation_info(Anim,height),
	    Count = epx:animation_info(Anim, count),
	    Frame0 = if is_number(Frame) -> Frame;
			true -> 0
		     end,
	    Frame1 = clamp(Frame0, 0, Count-1),
	    if AWidth =:= Width, AHeight =:= Height ->
		    epx:animation_draw(Anim, round(Frame1),
				       Pix, epx_gc:current(), X,Y);
	       true ->
		    AFormat = epx:animation_info(Anim,pixel_format),
		    TmpImage = create_tmp_pixmap(AFormat,AWidth,AHeight),
		    epx:animation_draw(Anim, round(Frame1),
				       TmpImage, epx_gc:current(), 0,0),
		    epx:pixmap_scale_area(TmpImage, Pix,
					  0, 0, X, Y,
					  AWidth, AHeight,
					  Width, Height,
					  [blend])
	    end,
	    update_animation(W, N, Frame0+1, Count);
       true ->
	    W
    end.

update_animation(W, 1, Frame, Count) ->
    case maps:get(animate, W) of
	continuous -> W#{ frame => fmod(Frame,Count) };
	sequence when Frame >= Count -> W#{ animate => undefined };
	sequence -> W#{ frame => Frame };
	undefined -> W
    end;
update_animation(W, 2, Frame, Count) ->
    case maps:get(animate2, W) of
	continuous -> W#{ frame2 => fmod(Frame,Count) };
	sequence when Frame >= Count -> W#{ animate2 => undefined };
	sequence -> W#{ frame2 => Frame };
	undefined -> W
    end.

optional(undefined, Default) -> Default;
optional(Value, _Default) -> Value.

fmod(A, B) when is_integer(A), is_integer(B), B =/= 0 ->
    A rem B;
fmod(A, B) when is_number(A), is_number(B), B =/= 0 ->
    AB = abs(A / B),
    C = (AB - trunc(AB))*abs(B),
    if A < 0 -> -C;
       true -> C
    end.

%% A bit ugly but may be efficient?
create_tmp_pixmap(Format, Width, Height) ->
    Pixmap =
	case get({?MODULE,tmp_pixmap}) of
	    undefined ->
		epx:pixmap_create(Width, Height, Format);
	    Pixmap0 ->
		F = epx:pixmap_info(Pixmap0,pixel_format),
		W  = epx:pixmap_info(Pixmap0,width),
		H = epx:pixmap_info(Pixmap0,height),
		if Format =:= F, Width =< W, Height =< H ->
			Pixmap0;
		   true ->
			epx:pixmap_create(Width, Height, Format)
		end
	end,
    epx:pixmap_fill(Pixmap, 0),
    put({?MODULE,tmp_pixmap}, Pixmap),
    Pixmap.

draw_border(_Pix,_X,_Y,_W, undefined) ->
    ok;
draw_border(_Pix,_X,_Y,_W, 0) ->
    ok;
draw_border(Pix,X,Y,W,Border) ->
    epx_gc:set_border_color(maps:get(border_color,W)),
    epx_gc:set_border_width(Border),
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Pix, X, Y, maps:get(width,W), maps:get(height,W)).

draw_focus(Pix,X,Y,W=#{ edit := true, focus := true }) ->
    Bw = optional(maps:get(border,W), 0) + 2,
    epx_gc:set_border_color(16#009f9f9f),
    epx_gc:set_border_width(2),
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Pix, X-Bw, Y-Bw,
		       maps:get(width,W)+2*Bw, maps:get(height,W)+2*Bw);
draw_focus(_Pix,_X,_Y,_W) ->
    ok.

draw_value_bar(Pix,X,Y,W,TopImage) ->
    #{ min := Min, max := Max, value := Value } = W,
    if is_number(Min),is_number(Max),is_number(Value) ->
	    R = value_proportion(W),
	    if is_record(TopImage, epx_pixmap) ->
		    draw_topimage(Pix,X,Y,W,TopImage,R);
	       true ->
		    draw_value_marker(Pix,X,Y,W,R)
	    end;
       true ->
	    ok
    end.

value_proportion(#{ min := Min, max := Max, value := Value }) ->
    Delta = abs(Max - Min),
    if Delta == 0 -> 0.5;
       true  -> (clamp(Value,Min,Max)-min(Min,Max))/Delta
    end.

draw_topimage(Pix,Xw,Yw,W,TopImage,R) ->
    Width = epx:pixmap_info(TopImage,width),
    Height = epx:pixmap_info(TopImage,height),
    #{ width := Ww, height := Wh } = W,
    {X,Y} = case maps:get(orientation,W) of
		horizontal ->
		    X0 = Xw,
		    X1 = Xw + Ww - 1,
		    Xv = trunc(X0*(1-R) + X1*R),
		    {Xv - (Width div 2), Yw + (Wh - Height) div 2};
		vertical ->
		    Y0 = Yw + Wh - 1,
		    Y1 = Yw,
		    Yv = trunc(Y0*(1-R) + Y1*R),
		    {Xw + (Ww - Width) div 2, (Yv - (Height div 2))}
	    end,
    epx:pixmap_copy_area(TopImage, Pix, 0, 0, X, Y, Width, Height, [blend]).

draw_value_marker(Pix,Xw,Yw,W,R) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(16#00000000),
    M = 3,    %% marker width/height
    #{ width := Ww, height := Wh } = W,
    case maps:get(orientation,W) of
	horizontal ->
	    X = trunc(Xw + R*((Ww-M)-1)),
	    epx:draw_rectangle(Pix, X, Yw + 2, M, Wh-4);
	vertical ->
	    Y0 = Yw + Wh - 1,
	    Y = trunc(Y0*(1-R) + Yw*R),
	    epx:draw_rectangle(Pix, Xw + 2, Y - (M div 2), Ww-4, M)
    end.

%%%===================================================================
%%% Colors
%%%===================================================================

parse_color(Color) when is_integer(Color) ->
    Color band 16#ffffffff;
parse_color({R,G,B}) when is_integer(R), is_integer(G), is_integer(B) ->
    (16#ff bsl 24) bor ((R band 16#ff) bsl 16)
	bor ((G band 16#ff) bsl 8) bor (B band 16#ff);
parse_color({A,R,G,B}) when is_integer(A),is_integer(R),
			    is_integer(G), is_integer(B) ->
    ((A band 16#ff) bsl 24) bor ((R band 16#ff) bsl 16)
	bor ((G band 16#ff) bsl 8) bor (B band 16#ff);
parse_color(Name) when is_list(Name); is_atom(Name) ->
    case epx_color:from_name(Name) of
	false ->
	    ?error("no such color ~s\n", [Name]),
	    false;
	{R,G,B} ->
	    (16#ff bsl 24)+(R bsl 16)+(G bsl 8)+B
    end;
parse_color(_) ->
    false.

%% set font color (alpha is masked)
set_font_color(Color) ->
    epx_gc:set_foreground_color(Color band 16#ffffff).

%% set foreground / fillcolor also using animatation state
set_color(W, Color0) ->
    State = case W of
		#{ type := menu } -> normal;
		#{ state := S } -> S
	    end,
    Color = case State of
		active ->
		    #{ shadow_x := Sx, shadow_y := Sy,
		       active_color := Active } = W,
		    if is_integer(Active) ->
			    Active;
		       is_number(Sx), is_number(Sy) ->
			    Color0;
		       true ->
			    %% darken color when active
			    color_sub(Color0, 16#00333333)
		    end;
		_ ->
		    Color0
	    end,
    epx_gc:set_foreground_color(Color),
    epx_gc:set_fill_color(Color).

%% get normal or active font color
get_font_color(#{ active_font_color := undefined, font_color := C }, _) -> C;
get_font_color(#{ active_font_color := C }, true) -> C;
get_font_color(#{ font_color := C }, false) -> C.

color_add(C1, C2) ->
    <<C3:32>> = color_add_argb(<<C1:32>>, <<C2:32>>),
    C3.

color_sub(C1, C2) ->
    <<C3:32>> = color_sub_argb(<<C1:32>>, <<C2:32>>),
    C3.

color_interpolate(V, C1, C2) ->
    <<C3:32>> = color_interpolate_argb(V, <<C1:32>>, <<C2:32>>),
    C3.

color_add_argb(<<A1,R1,G1,B1>>,<<A2,R2,G2,B2>>) ->
    <<(clamp_byte(A1+A2)),(clamp_byte(R1+R2)),
      (clamp_byte(G1+G2)),(clamp_byte(B1+B2))>>.

color_sub_argb(<<A1,R1,G1,B1>>,<<A2,R2,G2,B2>>) ->
    <<(clamp_byte(A1-A2)),(clamp_byte(R1-R2)),
      (clamp_byte(G1-G2)),(clamp_byte(B1-B2))>>.

color_interpolate_argb(V, <<A0,R0,G0,B0>>,<<A1,R1,G1,B1>>)
  when is_float(V), V >= 0.0, V =< 1.0 ->
    A = trunc(A0 + V*(A1-A0)),
    R = trunc(R0 + V*(R1-R0)),
    G = trunc(G0 + V*(G1-G0)),
    B = trunc(B0 + V*(B1-B0)),
    <<(clamp_byte(A)),(clamp_byte(R)),(clamp_byte(G)),(clamp_byte(B))>>.

clamp_byte(A) when A > 255 -> 255;
clamp_byte(A) when A < 0  -> 0;
clamp_byte(A) -> A.

%% clamp numbers
clamp(V,Min,Max) when is_number(V),is_number(Min),is_number(Max) ->
    if Min < Max -> min(max(V,Min), Max);
       Min > Max -> max(min(V,Min), Max);
       Min == Max -> Min
    end;
clamp(undefined,Min,Max) ->
    if is_number(Min) -> Min;
       is_number(Max) -> Max;
       true -> undefined
    end;
clamp(V,Min,undefined) when is_number(Min), V < Min -> Min;
clamp(V,undefined,Max) when is_number(Max), V > Max -> Max;
clamp(V,_,_) -> V.

%%%===================================================================
%%% Text expand
%%%===================================================================

%%
%% Utility to exand environment "variables" in unicode text
%% variables are written as ${var} where var is a encoded atom
%% operating system enviroment is accessed through $(VAR)
%% and application library dir $/app/
%%
text_expand(Text, Env) when is_list(Text) ->
    %% assume unicode character list!
    text_expand_(Text, [], Env);
text_expand(Text, Env) when is_binary(Text) ->
    %% assume utf8 encoded data!
    text_expand_(unicode:characters_to_list(Text), [], Env).

text_expand_([$$,${|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [${,$$], env, Acc, Env);
text_expand_([$$,$(|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [$(,$$], shell, Acc, Env);
text_expand_([$$,$/|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [$/,$$], lib, Acc, Env);
text_expand_([$\\,C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([], Acc, _Env) ->
    lists:reverse(Acc).

text_expand_collect_([$)|Text], Var, _Pre, shell, Acc, Env) ->
    case os:getenv(rev_variable(Var)) of
	false ->
	    text_expand_(Text, Acc, Env);
	Value ->
	    Acc1 = lists:reverse(Value, Acc),
	    text_expand_(Text, Acc1, Env)
    end;
text_expand_collect_([$/|Text], Var, _Pre, lib, Acc, Env) ->
    try erlang:list_to_existing_atom(rev_variable(Var)) of
	App ->
	    case code:lib_dir(App) of
		{error,_} ->
		    text_expand_(Text, Acc, Env);
		Value ->
		    Acc1 = lists:reverse(Value, Acc),
		    text_expand_(Text, Acc1, Env)
	    end
    catch
	error:_ ->
	    text_expand_(Text, Acc, Env)
    end;
text_expand_collect_([$}|Text], Var, _Pre, env, Acc, Env) ->
    try erlang:list_to_existing_atom(rev_variable(Var)) of
	Key ->
	    case lists:keyfind(Key, 1, Env) of
		false ->
		    text_expand_(Text, Acc, Env);
		{_,Val} ->
		    Value = lists:flatten(io_lib:format("~w", [Val])),
		    Acc1 = lists:reverse(Value, Acc),
		    text_expand_(Text, Acc1, Env)
	    end
    catch
	error:_ ->
	    text_expand_(Text, Acc, Env)
    end;
text_expand_collect_([C|Text], Var, Pre, Shell, Acc, Env) ->
    if C >= $a, C =< $z;
       C >= $A, C =< $Z;
       C >= $0, C =< $9;
       C =:= $_; C =:= $@;
       C =:= $\s; C =:= $\t -> %% space and tab allowed in begining and end
	    text_expand_collect_(Text, [C|Var], Pre, Shell, Acc, Env);
       true ->
	    %% char not allowed in variable named
	    text_expand_(Text,  [C | Var ++ Pre ++ Acc], Env)
    end;
text_expand_collect_([], Var, Pre, _Shell, Acc, Env) ->
    text_expand_([],  Var ++ Pre ++ Acc, Env).

rev_variable(Var) ->
    trim_hd(lists:reverse(trim_hd(Var))).

trim_hd([$\s|Cs]) -> trim_hd(Cs);
trim_hd([$\t|Cs]) -> trim_hd(Cs);
trim_hd(Cs) -> Cs.
