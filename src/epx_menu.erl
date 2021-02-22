%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%     Menu interface
%%% @end
%%% Created :  3 Apr 2019 by Tony Rogvall <tony@rogvall.se>

-module(epx_menu).

-export([create/2]).
-export([draw/3]).
-export([set_row/2]).
-export([find_row/3]).
-export([command/1]).

-include("../include/epx_menu.hrl").

-type epx_menu() :: #menu_state{}.
-export_type([epx_menu/0]).

create(MenuProfile,MenuItems) ->
    MenuInfo = load_menu_info(MenuProfile),
    epx_gc:set_font(MenuInfo#menu_info.font),
    Geometry = calc_menu_size(MenuItems, MenuInfo),
    #menu_state { profile  = MenuProfile,
		  info     = MenuInfo,
		  geometry = Geometry,
		  items    = MenuItems,
		  row      = -1 }.

load_menu_info(Profile) ->
    {ok,Font} = epx_font:match([{name,Profile#menu_profile.font_name},
				{size,Profile#menu_profile.font_size}]),
    Gc = epx:gc_default(),
    A = epx:font_info(Font, ascent),
    D = epx:font_info(Font, descent),
    epx:gc_set(Gc, font, Font),
    {W,_Y} = epx:font_draw_string(undefined,Gc,0,0,"0"),
    #menu_info { font_name = Profile#menu_profile.font_name,
		 font_size = Profile#menu_profile.font_size,
		 font = Font,
		 gc = Gc,
		 glyph_width = W,
		 glyph_height = A+D,
		 glyph_ascent = A,
		 glyph_descent = D
	       }.

calc_menu_size(Menu,MI) ->
    calc_menu_size_(Menu,MI,[], 0, 0).

calc_menu_size_([{"---", _Accel}|Elems], MI, Acc, W ,H) ->
    calc_menu_size_(Elems, MI, Acc, W, H);
calc_menu_size_([{Text, Accel}|Elems], MI, Acc, W ,H) ->
    {Lw,_} = epx:font_draw_string(undefined,MI#menu_info.gc,0,0,Text),
    {Rw,_} = epx:font_draw_string(undefined,MI#menu_info.gc,0,0,Accel),
    calc_menu_size_(Elems, MI, [{Lw,Rw}|Acc], max(W, Lw+Rw),
		    H + MI#menu_info.glyph_height);
calc_menu_size_([], MI, Acc, W, H) ->
    WExtra=MI#menu_info.left_offset+MI#menu_info.right_offset +
	MI#menu_info.accel_offset,
    HExtra=MI#menu_info.top_offset+MI#menu_info.bottom_offset,
    {lists:reverse(Acc), W+WExtra, H+HExtra}.


draw(_MenuState, _Pixels, undefined) ->
    ok;
draw(MenuState = #menu_state { row=Row}, Pixels, {X,Y}) ->
    MI = MenuState#menu_state.info,
    Profile = MenuState#menu_state.profile,
    Scheme = Profile#menu_profile.scheme,
    Menu = MenuState#menu_state.items,
    epx_gc:set_font(MI#menu_info.font),
    {WHList, W, H} = MenuState#menu_state.geometry,
    epx_gc:set_border_width(2),  %% in profile?
    epx_gc:set_border_color(
      epx_profile:color(Scheme,Profile#menu_profile.border_color)),
    epx_gc:set_fill_color(
      epx_profile:color(Scheme,Profile#menu_profile.background_color)),
    epx_gc:set_fill_style(solid),
    epx:draw_roundrect(Pixels,{X,Y,W,H},4,4), %% profile?
    MenuFontColor = epx_profile:color(Scheme,Profile#menu_profile.font_color),
    TextColor = alpha_color(0,MenuFontColor),
    epx_gc:set_foreground_color(TextColor),
    Top = MI#menu_info.top_offset,
    draw_items(Menu,0,Row,Pixels,MI,WHList,X,Y+Top,W,H),
    ok.

draw_items([{"---",_Accel}|Items],I,Row,Pixels,MI,WHList,X,Y,W,H) ->
    L = MI#menu_info.left_offset,
    R = MI#menu_info.right_offset,
    epx:draw_line(Pixels, X+L, Y, X+W-R, Y),
    draw_items(Items,I,Row,Pixels,MI,WHList,X,Y,W,H);
draw_items([{Text,Accel}|Items],I,Row,Pixels,MI,
		[{_Wl,Wr}|WHList],X,Y,W,H) ->
    Ascent = MI#menu_info.glyph_ascent,
    ItemHeight = MI#menu_info.glyph_height,
    Left = MI#menu_info.left_offset,
    Right = MI#menu_info.right_offset,
    epx:draw_string(Pixels,X+Left,Y+Ascent,Text),
    epx:draw_string(Pixels,X+W-Wr-Right,Y+Ascent,Accel),
    if I =:= Row ->
	    epx_gc:set_border_width(0),
	    epx_gc:set_fill_color({127,100,100,100}),
	    epx_gc:set_fill_style(blend),
	    Rect = {X+Left,Y+1,W-(Left+Right),ItemHeight-2},
	    epx:draw_rectangle(Pixels,Rect),
	    epx_gc:set_fill_style(solid);
       true ->
	    ok
    end,
    draw_items(Items,I+1,Row,Pixels,MI,WHList,X,Y+ItemHeight,W,H);
draw_items([],_I,_Rows,_Pixels, _MI, _WHList, _X, _Y, _W, _H) ->
    ok.

alpha_color(A,{_,R,G,B}) -> {A,R,G,B};
alpha_color(A,{R,G,B}) -> {A,R,G,B};
alpha_color(A,Name) when is_list(Name); is_atom(Name) ->
    alpha_color(A, epx_color:from_name(Name)).

set_row(Menu, Row) ->
    Menu#menu_state { row = Row }.

find_row(Menu, Pt1, Pos) ->
    Row = find_menu_item(Pt1,
			 Menu#menu_state.geometry,
			 Menu#menu_state.info,
			 Pos),
    if Row >= 0, Menu#menu_state.row =/= Row ->
	    {Row,Menu#menu_state { row = Row }};
       true ->
	    {Row,Menu}
    end.

find_menu_item({Mx,My}, {_WHList,W,H}, MI, {X,Y}) ->
    ItemHeight = MI#menu_info.glyph_height,
    Top = MI#menu_info.top_offset,
    Bottom = MI#menu_info.bottom_offset,
    if X < Mx; X >= Mx+W -> -1;
       Y < My+Top; Y >= My+H-Bottom -> -1;
       true ->
	    (Y - (My+Top)) div ItemHeight
    end.

command(Menu) ->
    command_(Menu#menu_state.row, Menu#menu_state.items).

command_(I, _) when I < 0 -> none;
command_(I, [{"---", _Accel}|Elems]) -> command_(I, Elems);
command_(0, [{_Command,Accel}|_]) -> accel(Accel);
command_(I, [_|Elems]) -> command_(I-1, Elems);
command_(_, []) ->  none.

%% Translate menu accelerator
accel(Accel) ->
    accel(Accel, #keymod{}).

accel("Ctrl+"++Accel, Mod) -> accel(Accel, Mod#keymod{ctrl=true});
accel("Alt+"++Accel, Mod) -> accel(Accel, Mod#keymod{alt=true});
accel("Shift+"++Accel, Mod) -> accel(Accel, Mod#keymod{shift=true});
accel("Del",Mod) -> {$\b, Mod};
accel("Esc",Mod) -> {$\e, Mod};
%% allow function code?
accel([Char],Mod) when Char >= $A, Char =< $Z, Mod#keymod.shift ->
    {Char, Mod};
accel([Char],Mod) when Char >= $A, Char =< $Z ->
    {Char+32, Mod};
accel([Char],Mod) ->
    {Char, Mod};
accel([],_Mod) -> none.
