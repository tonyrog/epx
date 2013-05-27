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
%% epx image processing
%%
-ifndef(__EPX_IMAGE_HRL__).
-define(__EPX_IMAGE_HRL__, true).

-define(IMAGE_JPEG,      epx_image_jpeg).
-define(IMAGE_TIFF,      epx_image_tiff).
-define(IMAGE_GIF,       epx_image_gif).
-define(IMAGE_PNG,       epx_image_png).
-define(IMAGE_BMP,       epx_image_bmp).
-define(IMAGE_XPM,       epx_image_xpm).
-define(IMAGE_UNDEF,     epx_image_undef).
-define(VIDEO_MPEG,      epx_image_mpeg).

-define(IMAGE_TYPES, 
	[?IMAGE_JPEG,
	 ?IMAGE_TIFF,
	 ?IMAGE_GIF,
	 ?IMAGE_PNG,
	 ?IMAGE_BMP,
	 ?IMAGE_XPM
	]).

-record(epx_image,
	{
	  type,         %% module name of image handler
	  name,         %% Image name (no path)
	  filename,     %% Full filename
	  size,         %% File size
	  extension,    %% extension used
	  mtime,        %% file creation date {{YYYY,MM,DD},{HH,MM,SS}}
          itime,        %% image creation date {{YYYY,MM,DD},{HH,MM,SS}}
          comment = "", %% image comment (if present)
	  format,       %% pixel format:
	                %%  gray1, gray4, gray8,
	                %%  palett1, palette4, palette8
	                %%  b8g8r8 r8g8b8 r8g8b8a8 ...
	  width,        %% Image width
	  height,       %% Image height
	  depth,        %% Image depth
	  bytes_pp = 3, %% bytes per pixel
	  alignment = 1,
          attributes = [], %% list of attributes [{atom(Key),term(Value)}]
	  order,        %% sample order left_to_right or right_to_left
	  palette,      %% list [{R,G,B}]
	  pixmaps = []  %% [epx_pixmap]
	 }).

-endif.

