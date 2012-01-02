%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%    epx library interface
%%% @end
%%% Created :  2 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx).

-on_load(init/0).

%%
-export([start/0, start/1]).

%% Pixmap access
-export([pixmap_create/3,pixmap_create/2]).
-export([pixmap_copy/1]).
-export([pixmap_sub_pixmap/5]).
-export([pixmap_info/0, pixmap_info/1, pixmap_info/2]).
-export([pixmap_set_clip/2]).
-export([pixmap_fill/2]).
-export([pixmap_copy_to/2]).
-export([pixmap_flip/1]).
-export([pixmap_scale/4]).
-export([pixmap_get_pixel/3]).
-export([pixmap_get_pixels/5]).
-export([pixmap_put_pixel/4,pixmap_put_pixel/5]).
-export([pixmap_put_pixels/8,pixmap_put_pixels/7]).
-export([pixmap_copy_area/9,pixmap_copy_area/8]).
-export([pixmap_alpha_area/9]).
-export([pixmap_fade_area/9]).
-export([pixmap_shadow_area/9,pixmap_shadow_area/8]).
-export([pixmap_add_color_area/11,pixmap_add_color_area/10]).
-export([pixmap_filter_area/10,pixmap_filter_area/9]).
-export([pixmap_rotate_area/12,pixmap_rotate_area/11]).
-export([pixmap_operation_area/9]).
-export([pixmap_scroll/6]).
-export([pixmap_attach/2, pixmap_attach/1]).
-export([pixmap_detach/1]).
-export([pixmap_draw/8]).
-export([pixmap_draw_point/4]).
-export([pixmap_draw_line/6]).
-export([pixmap_draw_rectangle/6]).
-export([pixmap_draw_ellipse/6]).
%% Animation
-export([animation_open/1]).
-export([animation_copy/6]).
-export([animation_draw/6]).
-export([animation_info/0, animation_info/1, animation_info/2]).

%% Dictionary access
-export([dict_create/0]).
-export([dict_copy/1]).
-export([dict_set/3]).
-export([dict_get/2]).
-export([dict_get_boolean/2]).
-export([dict_get_integer/2]).
-export([dict_get_float/2]).
-export([dict_get_string/2]).
-export([dict_get_binary/2]).
-export([dict_from_list/1]).

%% Grapic context access
-export([gc_create/0]).
-export([gc_default/0]).
-export([gc_copy/1]).
-export([gc_set/3]).
-export([gc_get/2]).
-export([gc_info/0, gc_info/1, gc_info/2]).

%% Font 
-export([font_open/1]).
-export([font_load/1]).
-export([font_unload/1]).
-export([font_map/1]).
-export([font_unmap/1]).
-export([font_info/0, font_info/1, font_info/2]).
-export([font_draw_glyph/5]).
-export([font_draw_string/5]).
-export([font_draw_utf8/5]).

%% Backend
-export([backend_list/0]).
-export([backend_open/2]).
-export([backend_info/2]).
-export([backend_adjust/2]).

%% Window
-export([window_create/4]).
-export([window_create/5]).
-export([window_info/0, window_info/1, window_info/2]).
-export([window_adjust/2]).
-export([window_set_event_mask/2]).
-export([window_enable_events/2]).
-export([window_disable_events/2]).
-export([window_attach/2, window_attach/1]).
-export([window_detach/1]).

%% Utils
-export([draw_point/3, draw_point/2]).
-export([draw_line/5]).
-export([draw_rectangle/5, draw_rectangle/2, draw_rectangle/3]).
-export([draw_ellipse/5]).
-export([draw_char/4]).
-export([draw_string/4]).
-export([draw_utf8/4]).

-import(lists, [map/2]).
%% -define(debug, true).

-ifdef(debug).
-define(VARIANT, "debug").
-else.
-define(VARIANT, "release").
-endif.

-include("../include/epx.hrl").

-export_type([epx_backend/0,
	      epx_window/0,
	      epx_pixmap/0,
	      epx_font/0, 
	      epx_gc/0, 
	      epx_dict/0,
	      epx_animation/0]).

-export_type([epx_rect/0, 
	      epx_pixel_format/0,
	      epx_window_event_flag/0,
	      epx_window_event_flags/0,
	      epx_pixmap_info_key/0,
	      epx_window_info_key/0]).

-opaque epx_backend()   ::  #epx_backend{} | undefined.
-opaque epx_window()    ::  #epx_window{}  | undefined.
-opaque epx_pixmap()    ::  #epx_pixmap{}  | undefined.
-opaque epx_font()      ::  #epx_font{}  | undefined.
-opaque epx_gc()        ::  #epx_gc{}  | undefined.
-opaque epx_dict()      ::  #epx_dict{}  | undefined.
-opaque epx_animation() ::  #epx_animation{}  | undefined.
-type epx_rect() :: { integer(), integer(), 
		      non_neg_integer(), non_neg_integer() }.

-type epx_pixel_format() ::
	%% FIXME fill with more formats
	argb | a8r8g8b8 | 
	rgba | r8g8b8a8 |
	abgr | a8b8g8r8 | 
	bgra | b8g8r8a8 |
	rgb  | r8g8b8 |
	bgr  | b8g8r8 |
	'565' | r5g6b5 | '565BE' | r5g6b5BE |
	'565LE' | r5g6b5LE |
	'1555' | a1r5g5b |
	gray8a8 | gray16 |
	a8 | alpha8 | gray8 |
	efnt2.

-type epx_window_event_flag() :: 
	key_press | key_release | motion |
	button_press | button_release | focus_in | focus_out |
	focos | enter | leave | configure | resize | crossing |
	button | left | middle | right | wheel | wheel_up | wheel_down |
	wheel_left | wheel_right | close | destroyed | all | none.

-type epx_window_event_flags() :: epx_window_event_flag() | 
				 [epx_window_event_flag()].

-type void() :: 'ok'.


init() ->
    Nif = filename:join([code:lib_dir(epx),"lib",?VARIANT,"epx_nif"]),
    io:format("Loading: ~s\n", [Nif]),
    erlang:load_nif(Nif, 0).

%% select 
assumed_backend() ->
    case os:getenv("EPX_BACKEND") of
	false ->
	    case os:type() of
		{unix,darwin} ->
		    "macos";
		{unix,linux} -> 
		    case os:getenv("DISPLAY") of
			false -> "fb";
			_ -> "x11"
		    end;
		{unix,_} -> "x11";
		_ -> "none"
	    end;
	Backend ->
	    Backend
    end.
    
%%
%% This is a simple version of the start function
%% For a more robust version use application:start(epx)
%%
start() ->
    Backend = assumed_backend(),
    start(Backend).

start(Prefered) ->
    case epx_backend:start() of
	{error,{already_started,_Pid}} ->
	    ok;
	{ok,_Pid} ->
	    epx_font:start(),
	    epx_animation:start(),
	    epx_style:start(),
	    List = epx:backend_list(),
	    Name = case lists:member(Prefered, List) of
		       true -> Prefered;
		       false when Prefered =:= "", List =/= [] ->
			   hd(List);
		       false ->
			   "none"
		   end,
	    epx_backend:create(Name, [])
    end.

-spec pixmap_create(Width::non_neg_integer(), Height::non_neg_integer()) ->
			   epx_pixmap().

pixmap_create(Width,Height) ->
    pixmap_create(Width,Height,argb).

-spec pixmap_create(Width::non_neg_integer(), Height::non_neg_integer(),
		    Format::epx_pixel_format()) ->
			   epx_pixmap().

pixmap_create(_Width,_Height,_Format) ->
    erlang:error(nif_not_loaded).

-spec pixmap_copy(Src::epx_pixmap()) -> epx_pixmap().

pixmap_copy(_Src) ->
    erlang:error(nif_not_loaded).

-spec pixmap_sub_pixmap(Src::epx_pixmap(),
			X::integer(), Y::integer(),
			Width::non_neg_integer(), Height::non_neg_integer()) ->
			       epx_pixmap().

pixmap_sub_pixmap(_Src, _X, _Y, _Width, _Height) ->
    erlang:error(nif_not_loaded).

%% pixmap_info:
%%    width           unsigned()
%%    height          unsigned()
%%    bytes_per_row   unsigned()
%%    bits_per_pixel  unsigned()
%%    bytes_per_pixel unsigned()
%%    pixel_format    pixel_format()
%%    parent          epx_pixmap()
%%    clip            epx_rect()
%%    backend         epx_backend()
%%
-type epx_pixmap_info_key() ::
	'width' | 'height' |
	'bytes_per_row' | 'bits_per_pixel' | 'bytes_per_pixel' |
	'pixel_format' | 'parent' | 'clip' | 'backend'.

-type epx_pixmap_info() ::
	{ 'width', non_neg_integer() } |
	{ 'height', non_neg_integer() } |
	{ 'bytes_per_row', non_neg_integer() } |
	{ 'bits_per_pixel', non_neg_integer() } |
	{ 'bytes_per_pixel', non_neg_integer() } |
	{ 'pixel_format', epx_pixel_format() } |
	{ 'parent',  epx_pixmap() } |
	{ 'clip',    epx_rect() } | 
	{ 'backend', epx_backend() }.

-spec pixmap_info() -> [epx_pixmap_info()].
			 
pixmap_info() ->
    [width, height, 
     bytes_per_row, bits_per_pixel, bytes_per_pixel,
     pixel_format, parent, clip, backend].

-spec pixmap_info(Pixmap::epx_pixmap()) -> [epx_pixmap_info()].
			 
pixmap_info(Pixmap) ->
    map(fun(Info) -> {Info,pixmap_info(Pixmap,Info)} end,
	pixmap_info()).

-spec pixmap_info(Pixmap::epx_pixmap(),Key::epx_pixmap_info_key()) ->
			 term().

pixmap_info(_Pixmap, _Key) ->
    erlang:error(nif_not_loaded).

pixmap_set_clip(_Pixmap, _Rect) ->
    erlang:error(nif_not_loaded).

pixmap_fill(_Pixmap, _Color) ->
    erlang:error(nif_not_loaded).
    
pixmap_copy_to(_Src, _Dst) ->
    erlang:error(nif_not_loaded).

pixmap_flip(_Pixmap) ->
    erlang:error(nif_not_loaded).
    
pixmap_scale(_Src, _Dst, _Width, _Height) ->
    erlang:error(nif_not_loaded).    

pixmap_get_pixel(_Pixmap,_X,_Y) ->    
    erlang:error(nif_not_loaded).    

pixmap_get_pixels(_Pixmap,_X,_Y,_W,_H) ->    
    erlang:error(nif_not_loaded).

pixmap_put_pixel(Pixmap,X,Y,Value) ->
    pixmap_put_pixel(Pixmap,X,Y,0,Value).

pixmap_put_pixel(_Pixmap,_X,_Y,_Flags,_Value) ->
    erlang:error(nif_not_loaded).

pixmap_put_pixels(Pixmap,X,Y,Width,Height,Format,Data) ->
    pixmap_put_pixels(Pixmap,X,Y,Width,Height,Format,Data,[]).

pixmap_put_pixels(_Pixmap,_X,_Y,_Width,_Height,_Format,_Data,_Flags) ->
    erlang:error(nif_not_loaded).

pixmap_copy_area(Src,Dst,XSrc,YSrc,XDst,YDst,Width,Height) ->
    pixmap_copy_area(Src,Dst,XSrc,YSrc,XDst,YDst,Width,Height,[]).

pixmap_copy_area(_Src,_Dst,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height,_Flags) ->
    erlang:error(nif_not_loaded).

pixmap_alpha_area(_Src,_Dst,_Alpha,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height) ->
    erlang:error(nif_not_loaded).
    
pixmap_fade_area(_Src,_Dst,_Fade,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height) ->
    erlang:error(nif_not_loaded).

pixmap_shadow_area(Src,Dst,XSrc,YSrc,XDst,YDst,Width,Height) ->
    pixmap_shadow_area(Src,Dst,XSrc,YSrc,XDst,YDst,Width,Height,[]).

pixmap_shadow_area(_Src,_Dst,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height,_Flags) ->
    erlang:error(nif_not_loaded).

pixmap_add_color_area(Src,Dst,Fade,Color,XSrc,YSrc,XDst,YDst,Width,Height) ->
    pixmap_add_color_area(Src,Dst,Fade,Color,XSrc,YSrc,XDst,YDst,
			  Width,Height,[]).
    
pixmap_add_color_area(_Src,_Dst,_Fade,_Color,_XSrc,_YSrc,_XDst,_YDst,
		      _Width,_Height,_Flags) ->
    erlang:error(nif_not_loaded).

pixmap_filter_area(Src,Dst,Filter,XSrc,YSrc,XDst,YDst,Width,Height) ->
    pixmap_filter_area(Src,Dst,Filter,XSrc,YSrc,XDst,YDst,Width,Height,[]).
    
pixmap_filter_area(_Src,_Dst,_Filter,
		   _XSrc,_YSrc,_XDst,_YDst,_Width,_Height,_Flags) ->
    erlang:error(nif_not_loaded).

pixmap_rotate_area(Src,Dst,Angle,XSrc,YSrc,XCSrc,YCSrc,XCDst,YCDst,
		   Width,Height) ->
    pixmap_rotate_area(Src,Dst,Angle,XSrc,YSrc,XCSrc,YCSrc,XCDst,YCDst,
		       Width,Height,[]).

pixmap_rotate_area(_Src,_Dst,_Angle,
		   _XSrc,_YSrc,_XCSrc,_YCSrc,_XCDst,_YCDst,
		   _Width,_Height,_Flags) ->
    erlang:error(nif_not_loaded).

pixmap_operation_area(_Src,_Dst,_Op,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height) ->
    erlang:error(nif_not_loaded).
    
pixmap_scroll(_Src,_Dst,_Horizontal,_Vertical,_Rotate,_FillColor) ->
    erlang:error(nif_not_loaded).

pixmap_attach(Pixmap) ->
    pixmap_attach(Pixmap, epx_backend:default()).

pixmap_attach(_Pixmap, _Backend) ->
    erlang:error(nif_not_loaded).

pixmap_detach(_Pixmap) ->
    erlang:error(nif_not_loaded).

pixmap_draw(_Pixmap, _Win, _SrxX, _SrcY, _DstX, _DstY, _Width, _Height) ->
    erlang:error(nif_not_loaded).

pixmap_draw_point(_Pixmap, _Gc, _X, _Y) ->
    erlang:error(nif_not_loaded).

pixmap_draw_line(_Pixmap, _Gc, _X1, _Y1, _X2, _Y2) ->
    erlang:error(nif_not_loaded).

pixmap_draw_rectangle(_Pixmap, _Gc, _X, _Y, _Width, _Height) ->
    erlang:error(nif_not_loaded).

pixmap_draw_ellipse(_Pixmap, _Gc, _X, _Y, _Width, _Height) ->
    erlang:error(nif_not_loaded).

%%
%% Animation
%%

animation_open(_File) ->
    erlang:error(nif_not_loaded).
    
animation_copy(_Anim, _Index, _Pixmap, _Gx,  _X, _Y) ->
    erlang:error(nif_not_loaded).    

animation_draw(_Anim, _Index, _Pixmap, _Gx,  _X, _Y) ->
    erlang:error(nif_not_loaded).    

animation_info() ->
    [file_name, file_size, count, width, height, pixel_format].

animation_info(Anim) ->
    map(fun(Info) -> {Info,animation_info(Anim,Info)} end,
	animation_info()).    

animation_info(_Anim, _Key) ->
    erlang:error(nif_not_loaded).        

%%
%% Dictionary context
%%
dict_create() ->
    erlang:error(nif_not_loaded).

dict_copy(_Dict) ->
    erlang:error(nif_not_loaded).

dict_set(_Dict, _Key, _Value) ->
    erlang:error(nif_not_loaded).    

dict_get(_Dict, _Key) ->
    erlang:error(nif_not_loaded).

dict_get_boolean(_Dict, _Key) ->
    erlang:error(nif_not_loaded).

dict_get_integer(_Dict, _Key) ->
    erlang:error(nif_not_loaded).

dict_get_float(_Dict, _Key) ->
    erlang:error(nif_not_loaded).

dict_get_string(_Dict, _Key) ->
    erlang:error(nif_not_loaded).

dict_get_binary(_Dict, _Key) ->
    erlang:error(nif_not_loaded).

dict_from_list(List) ->
    Dict = dict_create(),
    dict_from_list(Dict, List).

dict_from_list(Dict, [{Key,Value}|List]) ->
    dict_set(Dict, Key, Value),
    dict_from_list(Dict, List);
dict_from_list(Dict, []) ->
    Dict.

%%
%% Graphic context
%%
gc_create() ->
    erlang:error(nif_not_loaded).    

gc_default() ->
    erlang:error(nif_not_loaded).

gc_copy(_Gc) ->
    erlang:error(nif_not_loaded).

gc_set(_Gc, _Item, _Value) ->
    erlang:error(nif_not_loaded).

gc_get(_Gc, _Item) ->
    erlang:error(nif_not_loaded).    

gc_info() ->
    [fill_style,fill_color,fill_texture,
     line_style,line_join_style,line_cap_style,line_width,line_texture,
     border_style,border_join_style,border_cap_style,
     border_color,border_texture,
     foreground_color,
     background_color,
     fader_value,
     font,
     glyph_delta_x,glyph_delta_y,glyph_fixed_width,glyph_dot_kern].
     

gc_info(Gc) ->
    map(fun(Info) -> {Info,gc_info(Gc,Info)} end,
	gc_info()).        
    
gc_info(Gc, Item) ->
    gc_get(Gc, Item).

%% Font
font_open(_Filename) ->
    erlang:error(nif_not_loaded).    

font_load(_Font) ->
    erlang:error(nif_not_loaded).
font_unload(_Font) ->
    erlang:error(nif_not_loaded).
    
font_map(_Font) ->
    erlang:error(nif_not_loaded).

font_unmap(_Font) ->
    erlang:error(nif_not_loaded).

font_info() ->
    [file_name, file_size, foundry_name, family_name, 
     weight, slant, width, style, spacing, pixel_format,
     pixel_size, point_size, resolution_x, resolution_y,
     descent, ascent ].
     
font_info(Font) ->
    map(fun(Info) -> {Info,font_info(Font,Info)} end,
	font_info()).    

font_info(_Font, _Item) ->
    erlang:error(nif_not_loaded).

font_draw_glyph(_Pixmap,_Gc,_X, _Y, _C) ->
    erlang:error(nif_not_loaded).

font_draw_string(_Pixmap, _Gc, _X, _Y, _String) ->
    erlang:error(nif_not_loaded).    

font_draw_utf8(_Pixmap,_Gc, _X, _Y, _IOList) ->
    erlang:error(nif_not_loaded).

%% Backend
backend_list() ->
    erlang:error(nif_not_loaded).    

backend_open(_Name, _Dict) ->
    erlang:error(nif_not_loaded).

backend_info(_Backend, _Item) ->
    erlang:error(nif_not_loaded).
    
backend_adjust(_Backend, _Dict) ->
    erlang:error(nif_not_loaded).

%% Window
-spec window_create(X::integer(), Y::integer(),
		    Width::non_neg_integer(),Height::non_neg_integer()) ->
			   epx_window().
		    
window_create(_X,_Y,_Width,_Height) ->
    erlang:error(nif_not_loaded).

-spec window_create(X::integer(), Y::integer(),
		    Width::non_neg_integer(),Height::non_neg_integer(),
		    Flags::epx_window_event_flags()) ->
			   epx_window().

window_create(_X,_Y,_Width,_Height,_Mask) ->
    erlang:error(nif_not_loaded).

%%
%% window_info(Window::epx_window(), Item) 
%%   Item =
%%        x        integer()
%%      | y        integer()
%%      | width    unsigned()
%%      | height   unsigned()
%%      | backend  epx_backend()
%%      | event_mask all|none|[event_flag]
%%
-type epx_window_info_key() ::
	'x' | 'y' | 'width' | 'height' | 'backend' | 'event_mask'.

-type epx_window_info() ::
	{ 'x', integer() } |
	{ 'y', integer() } |
	{ 'width', non_neg_integer() } |
	{ 'height', non_neg_integer() } |
	{ 'backend', non_neg_integer() } |
	{ 'event_mask', epx_window_event_flags() }.

-spec window_info() -> [epx_window_info_key()].

window_info() ->
    [x, y, width, height, backend, event_mask].    

-spec window_info(Window::epx_window()) ->    
			 [epx_window_info()].

window_info(Window) ->
    map(fun(Info) -> {Info,window_info(Window,Info)} end,
	window_info()).

-spec window_info(Window::epx_window(),Item::epx_window_info_key()) ->
			 term().

window_info(_Window, _Item) ->
    erlang:error(nif_not_loaded).

window_adjust(_Window, _Dict) ->
    erlang:error(nif_not_loaded).

-spec window_set_event_mask(Window::epx_window(), 
			    Events::epx_window_event_flags()) ->
				   void().
    
window_set_event_mask(_Window, _Events) ->
    erlang:error(nif_not_loaded).
    
-spec window_enable_events(Window::epx_window(), 
			   Events::epx_window_event_flags()) ->
				  void().

window_enable_events(_Window, _Events) ->
    erlang:error(nif_not_loaded).

-spec window_disable_events(Window::epx_window(), 
			    Events::epx_window_event_flags()) ->
				   void().

window_disable_events(_Window, _Events) ->
    erlang:error(nif_not_loaded).

window_attach(Window) ->
    window_attach(Window, epx_backend:default()).

window_attach(_Window, _Backend) ->
    erlang:error(nif_not_loaded).    
    
window_detach(_Window) ->
    erlang:error(nif_not_loaded).

%% UTILS
draw_point(Pixmap,{X,Y}) ->
    draw_point(Pixmap,X,Y).

draw_point(Pixmap,Gc,{X,Y}) ->
    pixmap_draw_point(Pixmap,Gc,X,Y);
draw_point(Pixmap,X,Y) ->
    pixmap_draw_point(Pixmap,epx_gc:current(),X,Y).

draw_line(Pixmap, X1, Y1, X2, Y2) ->
    pixmap_draw_line(Pixmap,epx_gc:current(),X1,Y1,X2,Y2).

draw_rectangle(Pixmap, Gc, {X, Y, Width, Height}) ->
    pixmap_draw_rectangle(Pixmap, Gc, X, Y, Width, Height).

draw_rectangle(Pixmap, {X,Y,Width,Height}) ->
    draw_rectangle(Pixmap, X, Y, Width, Height).

draw_rectangle(Pixmap, X, Y, Width, Height) ->
    pixmap_draw_rectangle(Pixmap,epx_gc:current(), X, Y, Width, Height).

draw_ellipse(Pixmap, X, Y, Width, Height) ->
    pixmap_draw_ellipse(Pixmap,epx_gc:current(), X, Y, Width, Height).

draw_char(Pixmap, X, Y, C) ->
    font_draw_glyph(Pixmap,epx_gc:current(), X, Y, C).

draw_string(Pixmap, X, Y, String) ->
    font_draw_string(Pixmap,epx_gc:current(),X, Y, String).

draw_utf8(Pixmap, X, Y, IOList) ->
    font_draw_utf8(Pixmap,epx_gc:current(), X, Y, IOList).
