%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%     epx menu utils
%%% @end
%%% Created :  3 Apr 2019 by Tony Rogvall <tony@rogvall.se>

-ifndef(__EPX_MENU_HRL__).
-define(__EPX_MENU_HRL__, true).

-record(menu_info,
	{
	 font_name,
	 font_size,
	 font,
	 gc,
	 glyph_width,
	 glyph_height,
	 glyph_ascent,
	 glyph_descent,
	 top_offset    = 5,
	 left_offset   = 5,
	 accel_offset  = 16,
	 right_offset  = 5,
	 bottom_offset = 5
	}).

-record(menu_profile,
	{
	 scheme :: atom(),
	 font_name               = "Arial",
	 font_size               = 14,
	 font_color              = grey5,
	 background_color        = greay10,
	 border_color            = green
	}).

-record(menu_state, 
	{
	 profile :: #menu_profile {},
	 info :: #menu_info {},
	 geometry :: {[{Left::integer(),Right::integer()}],
		      Width::integer,Height::integer()},
	 items :: [string()],
	 row  :: integer()    %% current row <0 = none
	}).

-record(keymod,
	{
	 shift = false,   %% add to selection
	 ctrl  = false,   %% add vertex
	 alt   = false    %% add edge
	}).

-endif.



