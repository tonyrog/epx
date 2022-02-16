%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    epx window content utils
%%% @end
%%% Created :  2 Jul 2019 by Tony Rogvall <tony@rogvall.se>

-ifndef(__EPX_WINDOW_CONTENT_HRL__).
-define(__EPX_WINDOW_CONTENT_HRL__, true).

%%
%% window_info cover items that are mainly used to determine
%% size information before drawing etc.
%%
-record(window_info,
	{
	 font_name,
	 font_size,
	 font,
	 gc,
	 glyph_width,
	 glyph_height,
	 glyph_ascent,
	 glyph_descent,
	 top_bar           = 0,
	 left_bar          = 0,
	 right_bar         = 0,
	 bottom_bar        = 0,
	 top_offset        = 5,
	 left_offset       = 5,
	 right_offset      = 5,
	 bottom_offset     = 5,
	 scroll_bar_size   = 16,
	 scroll_hndl_size  = 10,
	 scroll_xstep      = 4,
	 scroll_ystep      = 4,
	 scroll_horizontal = bottom  :: none | top | bottom,
	 scroll_vertical   = right   :: none | left | right
	}).

%%
%% window_profile contains color information
%%
-record(window_profile,
	{
	 scheme :: atom(),
	 font_name               = "Arial",
	 font_size               = 14,
	 font_color              = grey5,
	 background_color        = grey10,
	 scroll_bar_color        = grey12,   %% 94%
	 scroll_hndl_color       = grey15,   %% 50%
	 top_bar_color           = red,
	 left_bar_color          = green,
	 right_bar_color         = blue,
	 bottom_bar_color        = grey5,
	 scroll_horizontal       = bottom :: none | top | bottom,
	 scroll_vertical         = right  :: none | left | right,
	 status_font_color       = grey5
	}).

-record(window_content,
	{
	 profile :: #window_profile {},
	 view_xpos  = 0,  %% scroll x
	 view_ypos  = 0,  %% scroll y
	 view_left  = 0   :: integer(),
	 view_right = 0   :: integer(),
	 view_top   = 0   :: integer(),
	 view_bottom = 0  :: integer(),
	 hscroll          :: undefined | epx:epx_rect(),
	 hhndl            :: undefined | epx:epx_rect(),
	 vscroll          :: undefined | epx:epx_rect(),
	 vhndl            :: undefined | epx:epx_rect(),
	 motion           :: undefined |
			     {vhndl,epx:epx_point()} |
			     {hhndl,epx:epx_point()}	 
	 }).

-endif.
