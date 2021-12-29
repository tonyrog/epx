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
%%
%% EPX structures
%%
-ifndef(__EPX_HRL__).
-define(__EPX_HRL__, true).

-record(epx_event, {  win, data }).

-record(epx_backend, { id, data }).
-record(epx_window,  { id, data }).
-record(epx_bitmap, { id, data }).
-record(epx_pixmap, { id, data }).
-record(epx_font, { id, data }).
-record(epx_gc, { id, data }).
-record(epx_dict, { id, data }).
-record(epx_animation, { id, data }).
-record(epx_canvas, { id, data }).
-record(epx_poly, { id, data }).

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

%% FIXME: lager ?
-define(epx_info(F,A), error_logger:info_msg(io_lib:format((F),(A)))).
-define(epx_warning(F,A), error_logger:warning_msg(io_lib:format((F),(A)))).
-define(epx_error(F,A), error_logger:error_msg(io_lib:format((F),(A)))).
-define(epx_debug(F,A),
	case get(debug) of
	    true ->
		error_logger:info_msg(io_lib:format((F),(A)));
	    _ -> 
		ok
	end).

-type epx_px_format() ::
	rgb4 | rgb5 | rgb8 | rgb10 | rgb12 | rgb16 | rgb332 | rgb232 |
	rgb565 | yuv8 | alpha | gray | red | green | blue | calpha.
%% note! calpha is used in efnt2 format

-record(epx_pixel_format,
	{
	  name         :: string()|atom(), %% optional name
	  format       :: epx_px_format(), %% various channel size
	  bgr          :: boolean(),       %% bgr or rgb channel order
	  alpha_first  :: boolean(),       %% alpha first or last channel
	  alpha        :: boolean(),       %% alpha used or not
	  little       :: boolean(),       %% overall pixel endian
	  bits_per_pixel :: 1..64          %% overall number of bits 
	}).

-endif.
