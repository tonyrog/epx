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
%%%    epx library interface
%%% @end
%%% Created :  2 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx).

-on_load(init/0).

%%
-export([start/0]).
%% set epx debug level
-export([debug/1]).
%% simd
-export([simd_info/0,simd_info/1,simd_set/1,simd_info_/1]).
-export([simd_info_keys/0]).
%% Pixmaps
-export([pixmap_create/3,pixmap_create/2]).
-export([pixmap_copy/1]).
-export([pixmap_sub_pixmap/5]).
-export([pixmap_info/1, pixmap_info/2, pixmap_info_/2]).
-export([pixmap_info_keys/0]).
-export([pixmap_set_clip/2]).
-export([pixmap_fill/2, pixmap_fill/3]).
-export([pixmap_fill_area/6, pixmap_fill_area/7]).
-export([pixmap_copy_to/2]).
-export([pixmap_flip/1]).
-export([pixmap_scale/4]).
-export([pixmap_scale_area/6,pixmap_scale_area/7]).
-export([pixmap_scale_area/11]).
-export([pixmap_get_pixel/3]).
-export([pixmap_interp_pixel/3]).
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
-export([sync/1, sync/2]).
-export([pixmap_draw_point/4]).
-export([pixmap_draw_line/6]).
-export([pixmap_draw_triangles/3]).
-export([pixmap_draw_triangle/8]).
-export([pixmap_draw_rectangle/6]).
-export([pixmap_draw_fan/4]).
-export([pixmap_draw_strip/3]).
-export([pixmap_draw_poly/3]).
-export([pixmap_draw_poly/5]).
-export([pixmap_draw_ellipse/6]).
-export([pixmap_draw_roundrect/8]).
-export([pixmap_ltm_translate/3]).
-export([pixmap_ltm_scale/3]).
-export([pixmap_ltm_rotate/2]).
-export([pixmap_ltm_reset/1]).
%% Bitmaps
-export([bitmap_create/2]).
-export([bitmap_copy/1]).
-export([bitmap_copy_area/8]).
-export([bitmap_put_bit/4]).
-export([bitmap_get_bit/3]).
-export([bitmap_fill/2]).
-export([bitmap_draw_rectangle/6]).
-export([bitmap_fill_rectangle/6]).
-export([bitmap_draw_ellipse/6]).
-export([bitmap_fill_ellipse/6]).
-export([bitmap_draw/10]).
-export([bitmap_info/1, bitmap_info/2, bitmap_info_/2]).
-export([bitmap_info_keys/0]).
-export([bitmap_set_clip/2]).
%% Animation
-export([animation_open/1]).
-export([animation_copy/6]).
-export([animation_draw/6]).
-export([animation_info/1, animation_info/2, animation_info_/2]).
-export([animation_info_keys/0]).

%% Dictionary access
-export([dict_create/0]).
-export([dict_copy/1]).
-export([dict_set/3]).
-export([dict_get/2]).
-export([dict_is_key/2]).
-export([dict_first/1]).
-export([dict_next/2]).
-export([dict_get_boolean/2]).
-export([dict_get_integer/2]).
-export([dict_get_float/2]).
-export([dict_get_string/2]).
-export([dict_get_binary/2]).
-export([dict_from_list/1]).
-export([dict_info/1, dict_info/2]).
-export([dict_info_keys/0]).

%% Grapic context access
-export([gc_create/0]).
-export([gc_default/0]).
-export([gc_copy/1]).
-export([gc_set/3]).
-export([gc_get/2]).
-export([gc_info/1, gc_info/2]).
-export([gc_info_keys/0]).

%% Font
-export([font_open/1]).
-export([font_load/1]).
-export([font_unload/1]).
-export([font_map/1]).
-export([font_unmap/1]).
-export([font_info/1, font_info/2, font_info_/2]).
-export([font_info_keys/0]).
-export([font_draw_glyph/5]).
-export([font_draw_string/5]).
-export([font_draw_utf8/5]).

-export([glyph_info/2, glyph_info/3, glyph_info_/3]).
-export([glyph_info_keys/0]).

%% Backend
-export([backend_list/0]).
-export([backend_open_/2, backend_open/2]).
-export([backend_info/1,backend_info/2,backend_info_/2]).
-export([backend_adjust_/2, backend_adjust/2]).
-export([backend_info_keys/0]).

%% Window
-export([window_create/4]).
-export([window_create/5]).
-export([window_info/1, window_info/2, window_info_/2]).
-export([window_info_keys/0]).
-export([window_adjust_/2, window_adjust/2]).
-export([window_set_event_mask/2]).
-export([window_enable_events/2]).
-export([window_disable_events/2]).
-export([window_attach/2, window_attach/1]).
-export([window_detach/1]).
-export([window_sync/1]).

%% Canvas
-export([canvas_create/0]).
-export([canvas_line/4]).
-export([canvas_quad/7]).
-export([canvas_and/3]).
-export([canvas_or/3]).
-export([canvas_over/3]).
-export([canvas_not/2]).
-export([canvas_set_color/3]).
-export([canvas_set_operation/3]).
-export([canvas_set_params/5]).
-export([canvas_set_params/8]).
-export([canvas_draw/2]).

%% Poly
-export([poly_create/0, poly_create/1, poly_create/2]).
-export([poly_set/2, poly_set/3]).
-export([poly_draw/5]).
-export([poly_info/2, poly_info/3]).

%% Utils
-export([draw_point/3, draw_point/2]).
-export([draw_line/3, draw_line/5]).
-export([draw_triangle/2, draw_triangle/4]).
-export([draw_triangles/2]).
-export([draw_rectangle/2, draw_rectangle/3,
	 draw_rectangle/5, draw_rectangle/6]).
-export([draw_fan/2, draw_fan/3]).
-export([draw_strip/2]).
-export([draw_poly/2]).
-export([draw_roundrect/7, draw_roundrect/4, draw_roundrect/5]).
-export([draw_ellipse/2, draw_ellipse/3, 
	 draw_ellipse/5, draw_ellipse/6]).
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
	      epx_bitmap/0,
	      epx_pixmap/0,
	      epx_font/0,
	      epx_gc/0,
	      epx_dict/0,
	      epx_animation/0,
	      epx_canvas/0,
	      epx_poly/0]).

-export_type([epx_rect/0,
	      point/0,
	      point2d/0,
	      point3d/0,
	      epx_pixel_format/0,
	      epx_window_event_flag/0,
	      epx_window_event_flags/0,
	      epx_pixmap_info_key/0,
	      epx_bitmap_info_key/0,
	      epx_window_info_key/0]).

-type unsigned() :: non_neg_integer().
-type float01() :: float().  %% 0.0 .. 1.0  why can we not write this?
%% 0.8 fixpoint type or float in 0.0..1.0
-type fix8() :: byte() | float01().
-type void() :: 'ok'.
-type coord() :: integer() | float().
-type dim() :: unsigned() | float().
-type point2d() ::  {X::coord(), Y::coord()}.
-type point3d() ::  {X::coord(), Y::coord(), Z::coord()}.
-type point() :: point2d().
-type triangle() :: {P1::point(), P2::point(), P3::point()} |
		    {X1::coord(), Y1::coord(),
		     X2::coord(), Y2::coord(),
		     X3::coord(), Y3::coord()}.

-opaque epx_backend()   ::  #epx_backend{} | undefined.
-opaque epx_window()    ::  #epx_window{}  | undefined.
-opaque epx_bitmap()    ::  #epx_bitmap{}  | undefined.
-opaque epx_pixmap()    ::  #epx_pixmap{}  | undefined.
-opaque epx_font()      ::  #epx_font{}  | undefined.
-opaque epx_gc()        ::  #epx_gc{}  | undefined.
-opaque epx_dict()      ::  #epx_dict{}  | undefined.
-opaque epx_animation() ::  #epx_animation{}  | undefined.
-opaque epx_canvas()    ::  #epx_canvas{} | undefined.
-opaque epx_poly()      ::  #epx_poly{} | undefined.

-type canvas_elem() :: integer().

-type epx_rect() :: { X::coord(), Y::coord(),
		      Width::dim(), Height::dim() }.

-type epx_color3() :: {R::byte(),G::byte(),B::byte()}.
-type epx_color4() :: {A::byte(),R::byte(),G::byte(),B::byte()}.
-type epx_color_name() :: atom().  %% x11 color names

-type epx_color() :: epx_color3() |
		     epx_color4() |
		     epx_color_name() |
		     unsigned().

-type epx_flag() :: solid | blend | sum | aalias |
		    textured | nfirst | nlast | none.
-type epx_flags() :: [epx_flag()] | unsigned().

-type join_style() :: miter | round | bevel.

-type cap_style() :: none | butt | round | projecting.

-type fill_style() :: solid | blend | sum | aalias | textured | none.

-type line_style() :: solid | blend | sum | aalias | textured |
		      dashed | nfirst | nlast | none.

-type border_style() :: solid | blend | sum | aalias | textured |
			dashed | ntop | nright | nbottom | nleft |
			none.

-type epx_pixel_format_simple() ::
	argb | rgba | abgr | bgra | rgb | bgr.

-type epx_pixel_format() ::
	epx_pixel_format_simple() | atom() | string() |
	#epx_pixel_format {}.

-type epx_pixmap_operation() ::
	clear | src | dst | src_over | dst_over | src_in | dst_in |
	src_out | dst_out | src_atop | dst_atop | 'xor' | copy |
	add | sub | src_blend | dst_blend.


-type epx_window_event_flag() ::
	key_press | key_release | motion |
	button_press | button_release |
	focus_in | focus_out |
	enter | leave |
	configure | resize |
	left | middle | right | wheel_up | wheel_down |
	wheel_left | wheel_right | close | destroyed.

-type epx_window_xevent_flag() ::
	epx_window_event_flag() | wheel | button | all | none.


-type epx_window_event_flags() :: epx_window_event_flag() |
				 [epx_window_event_flag()].

-type epx_window_xevent_flags() :: epx_window_xevent_flag() |
				   [epx_window_xevent_flag()].

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join([code:priv_dir(epx),"epx_nif"]),
    erlang:load_nif(Nif, 0).

%%
%%
%%
start() ->
    application:load(?MODULE), %% make sure command line env is loaded
    application:ensure_all_started(?MODULE).

%% @doc
%%  Set epx internal debug logging
%% @end

-type epx_debug_level() :: debug | info | notice | warning | error |
			   critical | alert | emergency | none.

-spec debug(Level::epx_debug_level()) -> void().


debug(_Level) ->
    ?nif_stub().

-type epx_simd_info_key() ::
	'accel' |
	'cpu_vendor_name' |
	'cpu_features' |
	'cpu_cache_line_size' |
	'function'.

-type epx_accel_type() ::
	none |
	emu  |
	altivec |
	mmx  |
	sse2 |
	neon.

-type epx_simd_info() ::
	{ 'cpu_vendor_name', string() } |
	{ 'cpu_brand_string', string() } |
	{ 'cpu_features', string() } |
	{ 'cpu_cache_line_size', unsigned() } |
	{ 'accel',     {epx_accel_type(),[epx_accel_type()]}} |
	{ 'functions', [{Name::atom(), [epx_pixel_format()]}]}.

%% @doc
%%   Get a list of all simd info keys
%% @end
-spec simd_info_keys() -> [epx_simd_info_key()].

simd_info_keys() ->
    ['accel',
     'cpu_vendor_name',
     'cpu_brand_string',
     'cpu_features',
     'cpu_cache_line_size',
     'functions'].

%% @doc
%%   Get all available information about SIMD support
%% @end
-spec simd_info() -> [epx_simd_info()].

simd_info() ->
    simd_info(simd_info_keys()).

-spec simd_info(Keys::[epx_simd_info_key()]) -> [epx_simd_info()].

simd_info(Keys) when is_list(Keys) ->
    [{K,simd_info_(K)} || K <- Keys];
simd_info(K) when is_atom(K) ->
    simd_info_(K).


%% @doc
%%   Get information about SIMD support
%% @end

-spec simd_info_(Key::epx_simd_info_key()) -> term().

simd_info_(_Info) ->
    ?nif_stub().

%% @doc
%%   Set current acceleration type
%% @end
-spec simd_set(Accel::epx_accel_type()) -> void().

simd_set(_Accel) ->
    ?nif_stub().

%% @doc
%%   Create a pixmap of size WidthxHeight using the pixel format 'argb'
%% @end
-spec pixmap_create(Width::dim(), Height::dim()) ->
			   epx_pixmap().
pixmap_create(Width,Height) ->
    pixmap_create(Width,Height,argb).

%% @doc
%%   Create a pixmap of size WidthxHeight with the giiven pixel format
%% @end
-spec pixmap_create(Width::dim(), Height::dim(),
		    Format::epx_pixel_format()) ->
			   epx_pixmap().

pixmap_create(_Width,_Height,_Format) ->
    ?nif_stub().

%% @doc
%%   Create a new pixmap with all pixels from Src
%% @end
-spec pixmap_copy(Src::epx_pixmap()) -> epx_pixmap().

pixmap_copy(_Src) ->
    ?nif_stub().


%% @doc
%%   Create a sub-pixmap from Src refering to pixels in the
%%   rectangluar area (X,Y,Width,Height). The sub-pixmap share
%%   all pixels with the Src pixmap and is is not SMP protected,
%%   if multiple writers are used, keep cachline distance to the
%%   neighbouring pixels to be safe.
%% @end
-spec pixmap_sub_pixmap(Src::epx_pixmap(),
			X::coord(), Y::coord(),
			Width::dim(), Height::dim()) ->
			       epx_pixmap().

pixmap_sub_pixmap(_Src, _X, _Y, _Width, _Height) ->
    ?nif_stub().

-type epx_pixmap_info_key() ::
	'width' |
	'height' |
	'bytes_per_row' |
	'bits_per_pixel' |
	'bytes_per_pixel' |
	'pixel_format' |
	'parent' |
	'clip' |
	'backend'.

-type epx_pixmap_info() ::
	{ 'width', unsigned() } |
	{ 'height', unsigned() } |
	{ 'bytes_per_row', unsigned() } |
	{ 'bits_per_pixel', unsigned() } |
	{ 'bytes_per_pixel', unsigned() } |
	{ 'pixel_format', epx_pixel_format() } |
	{ 'parent',  epx_pixmap() } |
	{ 'clip',    epx_rect() } |
	{ 'backend', epx_backend() }.

%% @doc
%%   Return available pixmap information elements
%% @end
-spec pixmap_info_keys() -> [epx_pixmap_info()].

pixmap_info_keys() ->
    [width,
     height,
     bytes_per_row,
     bits_per_pixel,
     bytes_per_pixel,
     pixel_format,
     parent,
     clip,
     backend].

%% @doc
%%   Get all available pixmap information
%% @end
-spec pixmap_info(Pixmap::epx_pixmap()) -> [epx_pixmap_info()].

pixmap_info(Pixmap) ->
    pixmap_info(Pixmap, pixmap_info_keys()).

pixmap_info(Pixmap, Keys) when is_list(Keys) ->
    [{K,pixmap_info_(Pixmap,K)} || K <- Keys];
pixmap_info(Pixmap, K) when is_atom(K) ->
    pixmap_info_(Pixmap, K).

%% @doc
%%   Get specific pixmap information
%% @end
-spec pixmap_info_(Pixmap::epx_pixmap(),Key::epx_pixmap_info_key()) ->
			  term().

pixmap_info_(_Pixmap, _Key) ->
    ?nif_stub().

%% @doc
%%   Set the clipping Rectangle, pixels drawn outside the clipping
%%   rectangle are ignored.
%% @end
-spec pixmap_set_clip(Pixmap::epx_pixmap(), Rect::epx_rect()) -> void().

pixmap_set_clip(_Pixmap, _Rect) ->
    ?nif_stub().

%% @doc
%%   Fill the rectangle Dst with Color
%% @end
-spec pixmap_fill(Dst::epx_pixmap(), Color::epx_color()) -> void().

pixmap_fill(Dst, Color) ->
    pixmap_fill(Dst, Color, []).

%% @doc
%%   Fill and blend the rectangle Dst with Color
%% @end
-spec pixmap_fill(Dst::epx_pixmap(), Color::epx_color(), Flags::epx_flags()) ->
			 void().

pixmap_fill(_Dst, _Color, _Flags) ->
    ?nif_stub().

%% @doc
%%   Fill the rectangle Dst with Color
%% @end
-spec pixmap_fill_area(Dst::epx_pixmap(),
		       X::coord(),Y::coord(),
		       Width::dim(),Height::dim(),
		       Color::epx_color()) -> void().

pixmap_fill_area(Dst, X, Y, Width, Height, Color) ->
    pixmap_fill_area(Dst, X, Y, Width, Height, Color, []).

%% @doc
%%   Fill and blend the rectangle Dst with Color
%% @end
-spec pixmap_fill_area(Dst::epx_pixmap(), 
		       X::coord(),Y::coord(),
		       Width::dim(),Height::dim(),
		       Color::epx_color(), Flags::epx_flags()) ->
			      void().

pixmap_fill_area(_Dst, _X, _Y, _Width, _Height, _Color, _Flags) ->
    ?nif_stub().

%% @doc
%%   Copy pixles from `Src' pixmap to `Dst' pixmap, ignoring clip rectangle
%% @end
-spec pixmap_copy_to(Src::epx_pixmap(),Dst::epx_pixmap()) -> void().

pixmap_copy_to(_Src, _Dst) ->
    ?nif_stub().

%% @doc
%%  Inline, flip pixmap vertically, that is swap top and bottom rows
%% @end
-spec pixmap_flip(Pixmap::epx_pixmap()) -> void().

pixmap_flip(_Pixmap) ->
    ?nif_stub().

%% @doc
%%  Scale 'Src' pixmap to size ('Width' and 'Height') and put the result
%%  in the 'Dst' pixmap.
%% @end
-spec pixmap_scale(Src::epx_pixmap(),Dst::epx_pixmap(),
		   Width::dim(), Height::dim()) -> void().
pixmap_scale(_Src, _Dst, _Width, _Height) ->
    ?nif_stub().

%% @doc
%%  Scale 'Src' pixmap to size ('Width' and 'Height') and put the result
%%  in the 'Dst' pixmap at offset 'XDst', 'YDst'.
%% @end
-spec pixmap_scale_area(Src::epx_pixmap(),Dst::epx_pixmap(),
			XDst::coord(),YDst::coord(),
			Width::dim(), Height::dim()) -> void().
pixmap_scale_area(_Src, _Dst, _XDst, _YDst, _Width, _Height) ->
    pixmap_scale_area(_Src, _Dst, _XDst, _YDst, _Width, _Height, []).

%% @doc
%%  Scale 'Src' pixmap to size ('Width' and 'Height') and put the result
%%  in the 'Dst' pixmap at offset 'XDst', 'YDst'.
%% @end
-spec pixmap_scale_area(Src::epx_pixmap(),Dst::epx_pixmap(),
			XDst::coord(),YDst::coord(),
			Width::dim(), Height::dim(),
			Flags::epx_flags()) -> void().
pixmap_scale_area(_Src, _Dst, _XDst, _YDst, _Width, _Height, _Flags) ->
    WSrc = pixmap_info(_Src, width),
    HSrc = pixmap_info(_Src, height),
    pixmap_scale_area(_Src, _Dst, 0, 0, _XDst, _YDst,
		      WSrc, HSrc, _Width, _Height, _Flags).

%% @doc
%%  Scale 'Src' pixmap rectangle (XSrc,YSrc, into destination rectangle 
%%  to size ('Width' and 'Height') and put the result
%%  in the 'Dst' pixmap at offset 'XDst', 'YDst'.
%% @end
-spec pixmap_scale_area(Src::epx_pixmap(),Dst::epx_pixmap(),
			XSrc::coord(),YSrc::coord(),
			XDst::coord(),YDst::coord(),
			WSrc::dim(),HSrc::dim(),
			WDst::dim(), HDst::dim(),
			Flags::epx_flags()) -> void().
pixmap_scale_area(_Src, _Dst,
		  _XSrc, _YSrc, _XDst, _YDst,
		  _WSrc, _HSrc, _WDst, _HDst,
		  _Flags) ->
    ?nif_stub().

%% @doc
%%   Read the pixel value at position ('X','Y') in pixmap 'Src', return
%%   a pixel in {A,R,G,B} form or {255,0,0,0} (black) if position is
%%   outside the pixmap.
%% @end
-spec pixmap_get_pixel(Src::epx_pixmap(), X::coord(), Y::coord()) ->
			      epx_color4().
pixmap_get_pixel(_Pixmap,_X,_Y) ->
    ?nif_stub().

%% @doc
%%   Read the interpolated pixel value at position ('X','Y') in pixmap 
%%   'Src', return a pixel in {A,R,G,B} form or {255,0,0,0} (black) 
%%   if position is outside the pixmap.
%% @end
-spec pixmap_interp_pixel(Src::epx_pixmap(), X::coord(), Y::coord()) ->
			      epx_color4().
pixmap_interp_pixel(_Pixmap,_X,_Y) ->
    ?nif_stub().

%% @doc
%%  Read the pixels in the rectangle given by ('X','Y','Width','Height')
%%  return the pixels data in a "native" form as a binary.
%% @end
-spec pixmap_get_pixels(Src::epx_pixmap(), X::coord(), Y::coord(),
			Width::dim(), Height::dim()) ->
			       binary().
pixmap_get_pixels(_Pixmap,_X,_Y,_W,_H) ->
    ?nif_stub().

%% @doc
%%  Write the pixel value to position ('X','Y') in the pixmap 'Dst'
%% @end
-spec pixmap_put_pixel(Dst::epx_pixmap(), X::coord(), Y::coord(),
		       Color::epx_color()) -> void().
pixmap_put_pixel(Dst,X,Y,Color) ->
    pixmap_put_pixel(Dst,X,Y,Color,[]).

%% @doc
%%  Write the pixel value to position ('X','Y') in the pixmap 'Dst'
%%  using the flags in 'Flags'.
%% @end
-spec pixmap_put_pixel(Dst::epx_pixmap(), X::coord(), Y::coord(),
		       Color::epx_color(),Flags::epx_flags()) -> void().
pixmap_put_pixel(_Dst,_X,_Y,_Color,_Flags) ->
    ?nif_stub().

%% @doc
%%  Write the raw pixels in Data described by Format, into
%%  the rectangular area given by ('X','Y','Width','Height') in the
%%  pixmap 'Dst'.
%% @end
-spec pixmap_put_pixels(Dst::epx_pixmap(),X::coord(),Y::coord(),
			Width::dim(),Height::dim(),
			Format::epx_pixel_format(), Data::iolist()) ->
			       void().

pixmap_put_pixels(Dst,X,Y,Width,Height,Format,Data) ->
    pixmap_put_pixels(Dst,X,Y,Width,Height,Format,Data,[]).

%% @doc
%%  Write the raw pixels in Data described by Format, into
%%  the rectangular area given by ('X','Y','Width','Height') using
%%  the flags in 'Flags' in the pixmap 'Dst'.
%% @end
-spec pixmap_put_pixels(Dst::epx_pixmap(),X::coord(),Y::coord(),
			Width::dim(),Height::dim(),
			Format::epx_pixel_format(), Data::iolist(),
			Flags::epx_flags()) ->
			       void().
pixmap_put_pixels(_Dst,_X,_Y,_Width,_Height,_Format,_Data,_Flags) ->
    ?nif_stub().

%% @doc
%%  Copy pixels from the area ('XSrc','YSrc','Width','Height') in 'Src' pixmap
%%  to the area ('XDst','YDst','Width','Height') in the 'Dst' pixmap. The pixels
%%  are clipped according to the 'Dst' clip rectangle.
%% @end

-spec pixmap_copy_area(Src::epx_pixmap(),Dst::epx_pixmap(),
		       XSrc::coord(),YSrc::coord(),
		       XDst::coord(),YDst::coord(),
		       Width::dim(),Height::dim()) ->
			      void().

pixmap_copy_area(Src,Dst,XSrc,YSrc,XDst,YDst,Width,Height) ->
    pixmap_copy_area(Src,Dst,XSrc,YSrc,XDst,YDst,Width,Height,[]).

%% @doc
%%  Copy pixels from the area ('XSrc','YSrc','Width','Height') in 'Src' pixmap
%%  to the area ('XDst','YDst','Width','Height') in the 'Dst' pixmap. The pixels
%%  are clipped according to the 'Dst' clip rectangle. The pixels in
%%  'Src' are mixed with 'Dst' according to the flags in 'Flags'.
%% @end

-spec pixmap_copy_area(Src::epx_pixmap(),Dst::epx_pixmap(),
		       XSrc::coord(),YSrc::coord(),
		       XDst::coord(),YDst::coord(),
		       Width::dim(),Height::dim(),
		       Flags::epx_flags()) -> void().

pixmap_copy_area(_Src,_Dst,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height,_Flags) ->
    ?nif_stub().

%% @doc
%%   Blend 'Src' rectangle ('XSrc','YSrc','Width','Height') with
%%   'Dst' rectangle ('XDst','YDst','Width','Height') using a fixed
%%   alpha value of 'Alpha'. If 'Alpha' is 1.0 it means 'Src' pixels only and
%%   an 'Alpha' of 0.0 means using 'Dst' pixels only.<br/>
%%   The blending formula used is: ('Alpha'*('Src'-'Dst')+('Dst' bsl 8)) bsr 8.
%% @end

-spec pixmap_alpha_area(Src::epx_pixmap(),Dst::epx_pixmap(),
			Alpha::fix8(),
			XSrc::coord(),YSrc::coord(),
			XDst::coord(),YDst::coord(),
			Width::dim(),Height::dim()) -> void().

pixmap_alpha_area(_Src,_Dst,_Alpha,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height) ->
    ?nif_stub().

%% @doc
%%  Blend 'Src' rectangle ('XSrc','YSrc','Width','Heght') to 'Dst'
%%  rectangle ('XDst','YDst','Width','Height') fade using Fade
%%  as a blending scale factor.
%% @end
-spec pixmap_fade_area(Src::epx_pixmap(),Dst::epx_pixmap(),
		       Fade::fix8(),
		       XSrc::coord(),YSrc::coord(),
		       XDst::coord(),YDst::coord(),
		       Width::dim(),Height::dim()) -> void().
pixmap_fade_area(_Src,_Dst,_Fade,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height) ->
    ?nif_stub().

%% @doc
%%  Shadow 'Src' rectangle ('XSrc','YSrc','Width','Heght') to
%%  'Dst' rectangle ('XDst','YDst','Width','Height').
%%  This function will blend the pixels from source with
%%  the luminance value as alpha.
%% @end
-spec pixmap_shadow_area(Src::epx_pixmap(),Dst::epx_pixmap(),
			 XSrc::coord(),YSrc::coord(),
			 XDst::coord(),YDst::coord(),
			 Width::dim(),Height::dim()) -> void().

pixmap_shadow_area(Src,Dst,XSrc,YSrc,XDst,YDst,Width,Height) ->
    pixmap_shadow_area(Src,Dst,XSrc,YSrc,XDst,YDst,Width,Height,[]).

-spec pixmap_shadow_area(Src::epx_pixmap(),Dst::epx_pixmap(),
			 XSrc::coord(),YSrc::coord(),
			 XDst::coord(),YDst::coord(),
			 Width::dim(),Height::dim(),
			 Flags::epx_flags()) -> void().
pixmap_shadow_area(_Src,_Dst,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height,_Flags) ->
    ?nif_stub().

pixmap_add_color_area(Src,Dst,Fade,Color,XSrc,YSrc,XDst,YDst,Width,Height) ->
    pixmap_add_color_area(Src,Dst,Fade,Color,XSrc,YSrc,XDst,YDst,
			  Width,Height,[]).

pixmap_add_color_area(_Src,_Dst,_Fade,_Color,_XSrc,_YSrc,_XDst,_YDst,
		      _Width,_Height,_Flags) ->
    ?nif_stub().

pixmap_filter_area(Src,Dst,Filter,XSrc,YSrc,XDst,YDst,Width,Height) ->
    pixmap_filter_area(Src,Dst,Filter,XSrc,YSrc,XDst,YDst,Width,Height,[]).

pixmap_filter_area(_Src,_Dst,_Filter,
		   _XSrc,_YSrc,_XDst,_YDst,_Width,_Height,_Flags) ->
    ?nif_stub().

%% @doc
%%    Rotate the 'Src' pixels in rectangle ('XSrc','YSrc','Width','Height')
%%    around the point ('XCSrc','YCSrc') with an of 'Angle' radians.
%%    The result is placed in the pixmap 'Dst' at center poistion
%%    ('XCDest','YCDest'). The pixels are blended according to 'Flags'
%% @end

-spec pixmap_rotate_area(Src::epx_pixmap(),Dst::epx_pixmap(),
			 Angle::float(),
			 XSrc::coord(),YSrc::coord(),
			 XCSrc::coord(),YCSrc::coord(),
			 XCDst::coord(),YCDst::coord(),
			 Width::dim(),Height::dim(),
			 Flags::epx_flags()) -> void().

pixmap_rotate_area(_Src,_Dst,_Angle,
		   _XSrc,_YSrc,_XCSrc,_YCSrc,_XCDst,_YCDst,
		   _Width,_Height,_Flags) ->
    ?nif_stub().

pixmap_rotate_area(Src,Dst,Angle,XSrc,YSrc,XCSrc,YCSrc,XCDst,YCDst,
		   Width,Height) ->
    pixmap_rotate_area(Src,Dst,Angle,XSrc,YSrc,XCSrc,YCSrc,XCDst,YCDst,
		       Width,Height,[]).

%% @doc
%%    Combine two pixmaps using Duff-Porter pixmap operation, and then some.
%%    The possible operations are:
%% <ul>
%% <li> 'clear' </li>
%% <li> 'src' </li>
%% <li> 'dst' </li>
%% <li> 'src_over' </li>
%% <li> 'dst_over' </li>
%% <li> 'src_in' </li>
%% <li> 'dst_in' </li>
%% <li> 'src_out' </li>
%% <li> 'dst_out' </li>
%% <li> 'src_atop' </li>
%% <li> 'dst_atop' </li>
%% <li> 'xor' </li>
%% <li> 'copy' </li>
%% <li> 'add' </li>
%% <li> 'sub' </li>
%% <li> 'src_blend' </li>
%% <li> 'dst_blend' </li>
%% </ul>
%% @end
-spec pixmap_operation_area(Src::epx_pixmap(),Dst::epx_pixmap(),
			    Op::epx_pixmap_operation(),
			    XSrc::coord(),YSrc::coord(),
			    XDst::coord(),YDst::coord(),
			    Width::dim(),Height::dim()) -> void().

pixmap_operation_area(_Src,_Dst,_Op,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height) ->
    ?nif_stub().

-spec pixmap_scroll(Src::epx_pixmap(),Dst::epx_pixmap(),
		    Horizontal::integer(), Vertical::integer(),
		    Rotate::boolean(), FillColor::epx_color()) ->
			   void().

pixmap_scroll(_Src,_Dst,_Horizontal,_Vertical,_Rotate,_FillColor) ->
    ?nif_stub().

-spec pixmap_attach(Pixmap::epx_pixmap()) -> void().

pixmap_attach(Pixmap) ->
    pixmap_attach(Pixmap, epx_backend:default()).

-spec pixmap_attach(Pixmap::epx_pixmap(),Backend::epx_backend()) -> void().

pixmap_attach(_Pixmap, _Backend) ->
    ?nif_stub().

-spec pixmap_detach(Pixmap::epx_pixmap()) -> void().

pixmap_detach(_Pixmap) ->
    ?nif_stub().

%% @doc
%%   Draw pixels from the area ('XSrc','YSrc','Width','Height') in 'Pixmap'
%%   pixmap to the area ('XDst','YDst','Width','Height') on to the 'Window'
%%   window. Both the pixmap and the window must be "attached" for this
%%   operation to succeed.
%% @end
-spec pixmap_draw(Pixmap::epx_pixmap(), Win::epx_window(),
		  XSrc::coord(),YSrc::coord(),
		  XDst::coord(),YDst::coord(),
		  Width::dim(),Height::dim()) -> void().

pixmap_draw(_Pixmap, _Win, _XSrx, _YSrc, _XDst, _YDst, _Width, _Height) ->
    ?nif_stub().

%% Send a sync event to the window, the response from the
%% window is to send a synced event back.
-spec window_sync(Win::epx_window()) ->
			 ok.
window_sync(_Win) ->
    ?nif_stub().

sync(Win) ->
    window_sync(Win),
    receive
	{epx_event,Win,synced} ->
	    %% io:format("Got SYNCED\r\n"),
	    ok
    end.

sync(_Pixmap, Win) -> %% previous interface deprecated...
    sync(Win).


pixmap_draw_point(_Pixmap, _Gc, _X, _Y) ->
    ?nif_stub().

pixmap_draw_line(_Pixmap, _Gc, _X1, _Y1, _X2, _Y2) ->
    ?nif_stub().

pixmap_draw_triangle(_Pixmap, _Gc, _X0, _Y0, _X1, _Y1, _X2, _Y2) ->
    ?nif_stub().

-spec pixmap_draw_triangles(Pixmap::epx_pixmap(), Gc::epx_gc(),
			    Triangles::[triangle()]) -> void().
			   
pixmap_draw_triangles(_Pixmap, _Gc, _Triangles) ->
    ?nif_stub().

pixmap_draw_rectangle(_Pixmap, _Gc, _X, _Y, _Width, _Height) ->
    ?nif_stub().

pixmap_draw_ellipse(_Pixmap, _Gc, _X, _Y, _Width, _Height) ->
    ?nif_stub().

pixmap_draw_roundrect(_Pixmap, _Gc, _X, _Y, _Width, _Height, _Rw, _Rh) ->
    ?nif_stub().

-spec pixmap_draw_fan(Pixmap::epx_pixmap(), Gc::epx_gc(), [{X::coord(),Y::coord()}], Closed::boolean()) -> void().
			   
pixmap_draw_fan(_Pixmap, _Gc, _Points, _Closed) ->
    ?nif_stub().

-spec pixmap_draw_strip(Pixmap::epx_pixmap(), Gc::epx_gc(), [{X::coord(),Y::coord()}]) -> void().
			   
pixmap_draw_strip(_Pixmap, _Gc, _Points) ->
    ?nif_stub().

-spec pixmap_draw_poly(Pixmap::epx_pixmap(), Gc::epx_gc(), 
		       Poly::epx_poly()|[{X::coord(),Y::coord()}]) ->
	  void().
pixmap_draw_poly(Pixmap, Gc, Poly) when is_record(Poly, epx_poly) ->
    poly_draw(Poly, Pixmap, Gc, 0, 0);
pixmap_draw_poly(Pixmap, Gc, Points) when is_list(Points) ->
    poly_draw(poly_create(Points), Pixmap, Gc, 0, 0).

-spec pixmap_draw_poly(Pixmap::epx_pixmap(), Gc::epx_gc(),
		       Poly::epx_poly()|[{X::coord(),Y::coord()}],
		       Xoffs::coord(), Yoffs::coord()) -> void().
pixmap_draw_poly(Pixmap, Gc, Poly, Xoffs, Yoffs) 
  when is_record(Poly, epx_poly) ->
    poly_draw(Poly, Pixmap, Gc, Xoffs, Yoffs);
pixmap_draw_poly(Pixmap, Gc, Points, Xoffs, Yoffs) when is_list(Points) ->
    poly_draw(poly_create(Points), Pixmap, Gc, Xoffs, Yoffs).


pixmap_ltm_translate(_Pixmap, _Tx, _Ty) ->
    ?nif_stub().

pixmap_ltm_scale(_Pixmap, _Sx, _Sy) ->
    ?nif_stub().

pixmap_ltm_rotate(_Pixmap, _Radians) ->
    ?nif_stub().

pixmap_ltm_reset(_Pixmap) ->
    ?nif_stub().

%% @doc
%%   Create a bitmap of size WidthxHeight bits
%% @end
-spec bitmap_create(Width::dim(), Height::dim()) ->
	  epx_bitmap().
bitmap_create(_Width,_Height) ->
    ?nif_stub().

%% @doc
%%   Copy bitmap
%% @end
-spec bitmap_copy(Src::epx_bitmap()) ->
	  epx_bitmap().
bitmap_copy(_SrcBitmap) ->
    ?nif_stub().


%% @doc
%%  Copy bits from the area ('XSrc','YSrc','Width','Height') in 'Src' bitmap
%%  to the area ('XDst','YDst','Width','Height') in the 'Dst' bitmap. The bits
%%  are clipped according to the 'Dst' clip rectangle.
%% @end
-spec bitmap_copy_area(Src::epx_bitmap(),Dst::epx_bitmap(),
		       XSrc::coord(),YSrc::coord(),
		       XDst::coord(),YDst::coord(),
		       Width::dim(),Height::dim()) ->
			      void().

bitmap_copy_area(_Src,_Dst,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height) ->
    ?nif_stub().

%% @doc
%%   Put bit at location (x,y)
%% @end

-spec bitmap_put_bit(Bitmap::epx_bitmap(),X::coord(),Y::coord(),
		     Bit::integer()|boolean()) ->
	  ok.
bitmap_put_bit(_Bitmap,_X,_Y,_Bit) ->
    ?nif_stub().

%% @doc
%%   Get bit at location (x,y)
%% @end
-spec bitmap_get_bit(Bitmap::epx_bitmap(),X::coord(),Y::coord()) ->
	  integer().
bitmap_get_bit(_Bitmap,_X,_Y) ->
    ?nif_stub().

%% @doc
%%   Fill bitmap with pattern
%% @end
-spec bitmap_fill(Bitmap::epx_bitmap(),Pat::byte()) -> ok.

bitmap_fill(_Bitmap, _Pat) ->
    ?nif_stub().    

%% @doc
%%   Draw a rectangle
%% @end
-spec bitmap_draw_rectangle(Bitmap::epx_bitmap(),X::coord(),Y::coord(),
			    W::dim(),H::dim(),Bit::integer()) -> ok.

bitmap_draw_rectangle(_Bitmap,_X,_Y,_W,_H,_Val) ->
    ?nif_stub().



%% @doc
%%   Fill a rectangle with BYTE! pattern,  note that a single 1
%%   will not fill the complete rectangle, use 2#11111111 in this case.
%% @end
-spec bitmap_fill_rectangle(Bitmap::epx_bitmap(),X::coord(),Y::coord(),
			    Width::dim(),Height::dim(),
			    Pat::byte()) -> ok.

bitmap_fill_rectangle(_Bitmap,_X,_Y,_Width,_Height,_Pat) ->
    ?nif_stub().

%% @doc
%%   Draw a ellipse
%% @end
-spec bitmap_draw_ellipse(Bitmap::epx_bitmap(),X::coord(),Y::coord(),
			  W::dim(),H::dim(),Pat::byte()) -> ok.

bitmap_draw_ellipse(_Bitmap,_X,_Y,_W,_H,_Pat) ->
    ?nif_stub().

%% @doc
%%   Fill a ellipse with BYTE! pattern,  note that a single 1
%%   will not fill the complete ellipse, use 2#11111111 in this case.
%% @end
-spec bitmap_fill_ellipse(Bitmap::epx_bitmap(),X::coord(),Y::coord(),
			  Width::dim(),Height::dim(),Pat::byte()) -> ok.

bitmap_fill_ellipse(_Bitmap,_X,_Y,_Width,_Height,_Pat) ->
    ?nif_stub().


%% @doc
%%   Draw a bitmap onto a pixmap
%% @end
-spec bitmap_draw(Bitmap::epx_bitmap(),Pixmap::epx_pixmap(),
		  XSrc::coord(),YSrc::coord(),
		  XDst::coord(),YDst::coord(),
		  Width::dim(),Height::dim(),
		  Fg::epx_color(), Bg::epx_color()) ->
	  ok.

bitmap_draw(_Bitmap,_Pixmap,_XSrc,_YSrc,_XDst,_YDst,_Width,_Height,
	    _Fg, _Bg) ->
    ?nif_stub().


-type epx_bitmap_info_key() ::
	'width' |
	'height' |
	'bytes_per_row' |
	'parent' |
	'clip'.

-type epx_bitmap_info() ::
	{ 'width', unsigned() } |
	{ 'height', unsigned() } |
	{ 'bytes_per_row', unsigned() } |
	{ 'parent',  epx_bitmap() } |
	{ 'clip',    epx_rect() }.


%% @doc
%%   Return available bitmap information elements
%% @end
-spec bitmap_info_keys() -> [epx_bitmap_info()].

bitmap_info_keys() ->
    [width,
     height,
     bytes_per_row,
     parent,
     clip].

%% @doc
%%   Get all available bitmap information
%% @end
-spec bitmap_info(Bitmap::epx_bitmap()) -> [epx_bitmap_info()].

bitmap_info(Bitmap) ->
    bitmap_info(Bitmap, bitmap_info_keys()).

bitmap_info(Bitmap, Keys) when is_list(Keys) ->
    [{K,bitmap_info_(Bitmap,K)} || K <- Keys];
bitmap_info(Bitmap, K) when is_atom(K) ->
    bitmap_info_(Bitmap, K).

%% @doc
%%   Get specific pixmap information
%% @end
-spec bitmap_info_(Bitmap::epx_bitmap(),Key::epx_bitmap_info_key()) ->
			  term().

bitmap_info_(_Bitmap, _Key) ->
    ?nif_stub().

%% @doc
%%   Set the clipping Rectangle, bits drawn outside the clipping
%%   rectangle are ignored.
%% @end
-spec bitmap_set_clip(Bitmap::epx_bitmap(), Rect::epx_rect()) -> void().

bitmap_set_clip(_Bitmap, _Rect) ->
    ?nif_stub().

%%
%% Animation
%%

animation_open(_File) ->
    ?nif_stub().

animation_copy(_Anim, _Index, _Pixmap, _Gx,  _X, _Y) ->
    ?nif_stub().

animation_draw(_Anim, _Index, _Pixmap, _Gx,  _X, _Y) ->
    ?nif_stub().

animation_info_keys() ->
    [file_name, file_size, count, width, height, pixel_format].

animation_info(Anim) ->
    animation_info(Anim).

animation_info(Anim, Keys) when is_list(Keys) ->
    [{K,animation_info_(Anim,K)} || K <- Keys];
animation_info(Anim, K) when is_atom(K) ->
    animation_info_(Anim, K).

animation_info_(_Anim, _Key) ->
    ?nif_stub().

-spec canvas_create() -> epx_canvas().
canvas_create() ->
    ?nif_stub().
-spec canvas_line(Canvas::epx_canvas(),D::number(),E::number(),F::number()) ->
	  canvas_elem().
canvas_line(_Cancas,_D,_E,_F) ->
    ?nif_stub().
-spec canvas_quad(Canvas::epx_canvas(),A::number(),B::number(),C::number(),
		  D::number(), E::number(), F::number()) -> canvas_elem().
canvas_quad(_Cancas,_A,_B,_C,_D,_E,_F) ->
    ?nif_stub().
-spec canvas_and(Canvas::epx_canvas(), I::canvas_elem(), J::canvas_elem()) -> 
	  canvas_elem().
canvas_and(_Canvas,_I,_J) ->
    ?nif_stub().
-spec canvas_or(Canvas::epx_canvas(), I::canvas_elem(), J::canvas_elem()) -> 
	  canvas_elem().
canvas_or(_Canvas,_I,_J) ->
    ?nif_stub().
-spec canvas_over(Canvas::epx_canvas(), I::canvas_elem(), J::canvas_elem()) -> 
	  canvas_elem().
canvas_over(_Canvas,_I,_J) ->
    ?nif_stub().
-spec canvas_not(Canvas::epx_canvas(), I::canvas_elem()) -> 
	  canvas_elem().
canvas_not(_Canvas,_I) ->
    ?nif_stub().
-spec canvas_set_color(Canvas::epx_canvas(),I::canvas_elem(),
		       Op::epx_color()) -> ok.
	   
canvas_set_color(_Canvas,_I,_Color) ->
    ?nif_stub().
-spec canvas_set_operation(Canvas::epx_canvas(),I::canvas_elem(),
			   Op::epx_pixmap_operation()) ->
	  ok.
canvas_set_operation(_Canvas,_I,_Operation) ->
    ?nif_stub().

-spec canvas_set_params(Canvas::epx_canvas(),I::canvas_elem(),
			D::number(), E::number(), F::number()) -> ok.
canvas_set_params(_Cancas,_I,_D,_E,_F) ->
    ?nif_stub().

-spec canvas_set_params(Canvas::epx_canvas(),I::canvas_elem(),
			A::number(),B::number(),C::number(),
			D::number(), E::number(), F::number()) -> ok.

canvas_set_params(_Cancas,_I,_A,_B,_C,_D,_E,_F) ->
    ?nif_stub().

-spec canvas_draw(Canvas::epx_canvas(),Pixmap::epx_pixmap()) ->
	  ok.

canvas_draw(_Canvas,_Pixmap) ->
    ?nif_stub().

%% Poly

-spec poly_create() -> epx_poly().
poly_create() ->
    ?nif_stub().

-type poly_points() :: [{X::coord(),Y::coord()}] | [coord()].

-spec poly_create(Points::poly_points()) -> epx_poly().
poly_create(Points) ->
    Poly = poly_create(),
    poly_set(Poly, Points, absolute),
    Poly.

-spec poly_create(Points::poly_points(), Flag::absolue|relative) ->
	  epx_poly().
poly_create(Points, Flag) ->
    Poly = poly_create(),
    poly_set(Poly, Points, Flag),
    Poly.

-spec poly_set(Poly::epx_poly(), 
	       Points::[{X::coord(),Y::coord()}] | [coord()]) ->
	  ok.
poly_set(_Poly, _Points) ->
    ?nif_stub().

-spec poly_set(Poly::epx_poly(), 
	       Points::[{X::coord(),Y::coord()}] | [coord()],
	       Flag::absolute|relative) -> ok.
poly_set(_Poly, _Points, _Flag) ->
    ?nif_stub().

-spec poly_draw(Poly::epx_poly(),
		Pixmap::epx_pixmap(),
		Gc::epx_gc(),
		_Xoffs,_Yoffs) -> ok.

poly_draw(_Poly,_Pixmap,_Gc,_Xoffs,_Yoffs) ->
    ?nif_stub().

-spec poly_info(Poly::epx_poly(), Key::atom()) -> term().
poly_info(_Poly, _Key) ->
    ?nif_stub().

-spec poly_info(Poly::epx_poly(), Key::atom(), Index::atom()) -> term().
poly_info(_Poly, _Key, _Index) ->
    ?nif_stub().

%%
%% Dictionary context
%%
dict_create() ->
    ?nif_stub().

dict_copy(_Dict) ->
    ?nif_stub().

dict_set(_Dict, _Key, _Value) ->
    ?nif_stub().

dict_get(_Dict, _Key) ->
    ?nif_stub().

dict_is_key(_Dict, _Key) ->
    ?nif_stub().

dict_first(_Dict) ->
    ?nif_stub().

dict_next(_Dict, _Key) ->
    ?nif_stub().

dict_get_boolean(_Dict, _Key) ->
    ?nif_stub().

dict_get_integer(_Dict, _Key) ->
    ?nif_stub().

dict_get_float(_Dict, _Key) ->
    ?nif_stub().

dict_get_string(_Dict, _Key) ->
    ?nif_stub().

dict_get_binary(_Dict, _Key) ->
    ?nif_stub().

dict_from_list(List) ->
    Dict = dict_create(),
    dict_from_list(Dict, List).

dict_from_list(Dict, [{Key,Value}|List]) ->
    dict_set(Dict, Key, Value),
    dict_from_list(Dict, List);
dict_from_list(Dict, []) ->
    Dict.

dict_info(Dict, Keys) when is_list(Keys) ->
    [{K,dict_info_(Dict,K)} || K <- Keys];
dict_info(Dict, K) when is_atom(K) ->
    dict_info_(Dict, K).

dict_info_(_Dict, _Info) ->
    ?nif_stub().

dict_info_keys() ->
    ['size', 'sorted'].

dict_info(Dict) ->
    [{K,dict_info(Dict,K)} || K <- dict_info_keys()].



-type epx_gc_info_key() ::
	'fill_style' |
	'fill_color' |
	'fill_texture' |
	'line_style' |
	'line_join_style' |
	'line_cap_style' |
	'line_width' |
	'line_texture' |
	'border_style' |
	'border_join_style' |
	'border_cap_style' |
	'border_color' |
	'border_texture' |
	'foreground_color' |
	'background_color' |
	'fader_value' |
	'font' |
	'glyph_delta_x' |
	'glyph_delta_y' |
	'glyph_fixed_width' |
	'glyph_dot_kern'.

-type epx_gc_info() ::
	{ 'fill_style',      fill_style()} |
	{ 'fill_color',      epx_color()} |
	{ 'fill_texture',    epx_pixmap() } |
	{ 'line_style',      line_style() } |
	{ 'line_join_style', join_style() } |
	{ 'line_cap_style',  cap_style() } |
	{ 'line_width',      unsigned() } |
	{ 'line_texture',    epx_pixmap() } |
	{ 'border_style',    border_style() } |
	{ 'border_join_style', join_style() } |
	{ 'border_cap_style', cap_style() } |
	{ 'border_color',     epx_color() } |
	{ 'border_texture',   epx_pixmap() } |
	{ 'foreground_color', epx_color() } |
	{ 'background_color', epx_color() } |
	{ 'fader_value', byte() } |
	{ 'font', epx_font() } |
	{ 'glyph_delta_x', integer() } |
	{ 'glyph_delta_y', integer() } |
	{ 'glyph_fixed_width', unsigned() } |
	{ 'glyph_dot_kern', unsigned() }.



%% @doc
%%  Create a new graphic context
%% @end
-spec gc_create() -> epx_gc().

gc_create() ->
    ?nif_stub().

%% @doc
%%  Get the default graphic context
%% @end

-spec gc_default() -> epx_gc().

gc_default() ->
    ?nif_stub().

%% @doc
%%  Copy a graphic context
%% @end
-spec gc_copy(Gc::epx_gc()) -> epx_gc().

gc_copy(_Gc) ->
    ?nif_stub().

%% @doc
%%  Set graphic context item
%% @end

-spec gc_set(Gc::epx_gc(), Item::epx_gc_info_key(), Value::term()) -> void().

gc_set(_Gc, _Item, _Value) ->
    ?nif_stub().

%% @doc
%%  Get graphic context item
%% @end

-spec gc_get(Gc::epx_gc(), Item::epx_gc_info_key()) -> term().

gc_get(_Gc, _Item) ->
    ?nif_stub().

%% @doc
%%   Get list of all available gc atributes
%% @end
-spec gc_info_keys() -> [epx_gc_info_key()].

gc_info_keys() ->
    [fill_style,
     fill_color,
     fill_texture,
     line_style,
     line_join_style,
     line_cap_style,
     line_width,
     line_texture,
     border_style,
     border_join_style,
     border_cap_style,
     border_color,
     border_texture,
     foreground_color,
     background_color,
     fader_value,
     font,
     glyph_delta_x,
     glyph_delta_y,
     glyph_fixed_width,
     glyph_dot_kern].

%% @doc
%%   Return information about gc
%% @end

-spec gc_info(Gc::epx_gc()) -> [epx_gc_info()].

gc_info(Gc) ->
    gc_info(Gc, gc_info_keys()).

-spec gc_info(Gc::epx_gc(), Item::epx_gc_info_key()) -> term().

gc_info(Gc, Keys) when is_list(Keys) ->
    [{K,gc_info(Gc,K)} || K <- Keys];
gc_info(Gc, Key) when is_atom(Key) ->
    gc_get(Gc, Key).

%% Font
font_open(_Filename) ->
    ?nif_stub().

font_load(_Font) ->
    ?nif_stub().
font_unload(_Font) ->
    ?nif_stub().

font_map(_Font) ->
    ?nif_stub().

font_unmap(_Font) ->
    ?nif_stub().

font_info_keys() ->
    [file_name, file_size, foundry_name, family_name,
     weight, slant, width, style, spacing, pixel_format,
     pixel_size, point_size, resolution_x, resolution_y,
     descent, ascent ].

font_info(Font) ->
    font_info(Font, font_info_keys()).

font_info(Font, Keys) when is_list(Keys) ->
    [{K,font_info_(Font,K)} || K <- Keys];
font_info(Font, K) when is_atom(K) ->
    font_info_(Font, K).


font_info_(_Font, _Item) ->
    ?nif_stub().

glyph_info_keys() ->
    [name, width, height, x, y, dx, dy].

glyph_info(Font, Encoding) ->
    glyph_info(Font, Encoding, glyph_info_keys()).

glyph_info(Font, Encoding, Keys) when is_list(Keys) ->
    [{K,glyph_info_(Font,Encoding,K)} || K <- Keys];
glyph_info(Font, Encoding, K) when is_atom(K) ->
    glyph_info_(Font, Encoding, K).

glyph_info_(_Font, _Encoding, _Item) ->
    ?nif_stub().

font_draw_glyph(_Pixmap,_Gc,_X, _Y, _C) ->
    ?nif_stub().

font_draw_string(_Pixmap, _Gc, _X, _Y, _String) ->
    ?nif_stub().

font_draw_utf8(_Pixmap,_Gc, _X, _Y, _IOList) ->
    ?nif_stub().

%% Backend
backend_list() ->
    ?nif_stub().

backend_open(Name, Params) when is_list(Params) ->
    backend_open_(Name, dict_from_list(Params));
backend_open(Name, Params) when is_map(Params) ->
    backend_open_(Name, dict_from_list(maps:map_to_list(Params)));
backend_open(Name, Params) when is_record(Params, epx_dict) ->
    backend_open_(Name, Params).

backend_open_(_Name, _Dict) ->
    ?nif_stub().

backend_info_(_Backend, _Item) ->
    ?nif_stub().

backend_info(Backend) ->
    [{K,backend_info(Backend,K)} || K <- backend_info_keys()].

backend_info(Backend, Keys) when is_list(Keys) ->
    [{K,backend_info_(Backend,K)} || K <- Keys];
backend_info(Backend, Key) when is_atom(Key) ->
    backend_info_(Backend,Key).

backend_info_keys() ->
    [name, pending, opengl, use_opengl, width, height,
     windows, pixmaps, pixel_formats, epx_pixel_formats].

backend_adjust(Backend, Params) when is_list(Params) ->
    backend_adjust_(Backend, dict_from_list(Params));
backend_adjust(Backend, Params) when is_map(Params) ->
    backend_adjust_(Backend, dict_from_list(maps:map_to_list(Params)));
backend_adjust(Backend, Params) when is_record(Params,epx_dict) ->
    backend_adjust_(Backend, Params).

backend_adjust_(_Backend, _Dict) ->
    ?nif_stub().

%% Window
-spec window_create(X::coord(), Y::coord(),
		    Width::dim(),Height::dim()) ->
			   epx_window().

window_create(_X,_Y,_Width,_Height) ->
    ?nif_stub().

-spec window_create(X::coord(), Y::coord(),
		    Width::dim(),Height::dim(),
		    Flags::epx_window_event_flags()) ->
			   epx_window().

window_create(_X,_Y,_Width,_Height,_Flags) ->
    ?nif_stub().

%%
%% window_info(Window::epx_window(), Item)
%%   Item =
%%        x        integer()
%%      | y        integer()
%%      | width    unsigned()
%%      | height   unsigned()
%%      | backend  epx_backend()
%%      | event_mask [event_flag]
%%
-type epx_window_info_key() ::
	'x' | 'y' | 'width' | 'height' | 'backend' | 'event_mask'.

-type epx_window_info() ::
	{ 'x', integer() } |
	{ 'y', integer() } |
	{ 'width', unsigned() } |
	{ 'height', unsigned() } |
	{ 'backend', epx_backend() } |
	{ 'event_mask', epx_window_event_flags() }.

-spec window_info_keys() -> [epx_window_info_key()].

window_info_keys() ->
    [x, y, width, height, backend, event_mask].

-spec window_info(Window::epx_window()) ->
			 [epx_window_info()].

window_info(Window) ->
    window_info(Window, window_info_keys()).

window_info(Window, Keys) when is_list(Keys) ->
    [{K,window_info_(Window,K)} || K <- Keys];
window_info(Window, K) when is_atom(K) ->
    window_info_(Window, K).

-spec window_info_(Window::epx_window(),Item::epx_window_info_key()) ->
			 term().

window_info_(_Window, _Item) ->
    ?nif_stub().

window_adjust(Window, Params) when is_list(Params) ->
    window_adjust_(Window, dict_from_list(Params));
window_adjust(Window, Params) when is_map(Params) ->
    window_adjust_(Window, dict_from_list(maps:map_to_list(Params)));
window_adjust(Window, Params) when is_record(Params,epx_dict) ->
    window_adjust_(Window, Params).

window_adjust_(_Window, _Dict) ->
    ?nif_stub().

-spec window_set_event_mask(Window::epx_window(),
			    Events::epx_window_event_flags()) ->
				   void().

window_set_event_mask(_Window, _Events) ->
    ?nif_stub().

-spec window_enable_events(Window::epx_window(),
			   Events::epx_window_xevent_flags()) ->
	  void().

window_enable_events(_Window, _Events) ->
    ?nif_stub().

-spec window_disable_events(Window::epx_window(),
			    Events::epx_window_xevent_flags()) ->
	  void().

window_disable_events(_Window, _Events) ->
    ?nif_stub().

window_attach(Window) ->
    window_attach(Window, epx_backend:default()).

window_attach(_Window, _Backend) ->
    ?nif_stub().

window_detach(_Window) ->
    ?nif_stub().

%% UTILS
draw_point(Pixmap,{X,Y}) ->
    draw_point(Pixmap,X,Y).

draw_point(Pixmap,Gc,{X,Y}) ->
    pixmap_draw_point(Pixmap,Gc,X,Y);
draw_point(Pixmap,X,Y) ->
    pixmap_draw_point(Pixmap,epx_gc:current(),X,Y).

draw_line(Pixmap, {X1, Y1}, {X2, Y2}) ->
    draw_line(Pixmap, X1, Y1, X2, Y2).

draw_line(Pixmap, X1, Y1, X2, Y2) ->
    pixmap_draw_line(Pixmap,epx_gc:current(),X1,Y1,X2,Y2).

draw_triangle(Pixmap, {X0,Y0}, {X1,Y1}, {X2,Y2}) ->
    pixmap_draw_triangle(Pixmap, epx_gc:current(), X0,Y0,X1,Y1,X2,Y2).

draw_triangle(Pixmap, [{X0,Y0},{X1,Y1},{X2,Y2}|_]) ->
    pixmap_draw_triangle(Pixmap, epx_gc:current(), X0,Y0,X1,Y1,X2,Y2).

draw_triangles(Pixmap, Triangles) ->
    pixmap_draw_triangles(Pixmap, epx_gc:current(), Triangles).

draw_fan(Pixmap, Points) ->
    draw_fan(Pixmap, Points, false).

draw_fan(Pixmap, Points, Closed) ->
    pixmap_draw_fan(Pixmap, epx_gc:current(), Points, Closed).

draw_strip(Pixmap, Points) ->
    pixmap_draw_strip(Pixmap, epx_gc:current(), Points).

draw_poly(Pixmap, Points) ->
    pixmap_draw_poly(Pixmap, epx_gc:current(), Points).

draw_rectangle(Pixmap, Gc, {X, Y, Width, Height}) ->
    pixmap_draw_rectangle(Pixmap, Gc, X, Y, Width, Height).

draw_rectangle(Pixmap, {X,Y,Width,Height}) ->
    pixmap_draw_rectangle(Pixmap, epx_gc:current(), X, Y, Width, Height).

draw_rectangle(Pixmap, X, Y, Width, Height) ->
    pixmap_draw_rectangle(Pixmap, epx_gc:current(), X, Y, Width, Height).

draw_rectangle(Pixmap, GC, X, Y, Width, Height) ->
    pixmap_draw_rectangle(Pixmap, GC, X, Y, Width, Height).

draw_roundrect(Pixmap, Gc, {X, Y, Width, Height}, Rw, Rh) ->
    pixmap_draw_roundrect(Pixmap, Gc, X, Y, Width, Height, Rw, Rh).

draw_roundrect(Pixmap, {X,Y,Width,Height}, Rw, Rh) ->
    pixmap_draw_roundrect(Pixmap,epx_gc:current(), X, Y, Width, Height, Rw, Rh).

draw_roundrect(Pixmap, X, Y, Width, Height, Rw, Rh) ->
    pixmap_draw_roundrect(Pixmap,epx_gc:current(), X, Y, Width, Height, Rw, Rh).

draw_ellipse(Pixmap, {X, Y, Width, Height}) ->
    pixmap_draw_ellipse(Pixmap, epx_gc:current(), X, Y, Width, Height).

draw_ellipse(Pixmap, GC, {X, Y, Width, Height}) ->
    pixmap_draw_ellipse(Pixmap, GC, X, Y, Width, Height).

draw_ellipse(Pixmap, X, Y, Width, Height) ->
    draw_ellipse(Pixmap, epx_gc:current(), X, Y, Width, Height).

draw_ellipse(Pixmap, GC, X, Y, Width, Height) ->
    pixmap_draw_ellipse(Pixmap, GC, X, Y, Width, Height).

draw_char(Pixmap, X, Y, C) ->
    font_draw_glyph(Pixmap,epx_gc:current(), X, Y, C).

draw_string(Pixmap, X, Y, String) ->
    font_draw_string(Pixmap,epx_gc:current(),X, Y, String).

draw_utf8(Pixmap, X, Y, IOList) ->
    font_draw_utf8(Pixmap,epx_gc:current(), X, Y, IOList).
