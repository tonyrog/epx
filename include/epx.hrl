%%
%% EPX structures
%%
-ifndef(__EPX_HRL__).
-define(__EPX_HRL__, true).

-record(exp_point, { x, y }).
-record(epx_rect,  {  x, y, w, h }).

-record(epx_font_info,
	{
	  file_name, %% string (key)
	  handle,    %% handle used by epx_font.erl
	  file_size,
	  foundry,   %% string
	  family,    %% string
	  weight,
	  slant,
	  width,
	  style,
	  spacing,
	  pixel_format,
	  pixel_size,
	  point_size,
	  resolution_x,
	  resolution_y,
	  descent,
	  ascent
	 }).
-endif.
