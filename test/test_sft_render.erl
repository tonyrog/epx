%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    Render some glyphs
%%% @end
%%% Created : 16 Jun 2023 by Tony Rogvall <tony@rogvall.se>

-module(test_sft_render).

-export([render/1, render/3]).
-export([render_to/1, render_to/3]).

-include("../include/epx_t2d.hrl").

render(Char) ->
    render(Char,12, 0).
render(Char, Size, Angle) ->
    Sft = epx:sft_open("times", Size),
    Gid = epx:sft_glyph(Sft, Char),
    Form = epx_t2d:identity(),
    Form1 = epx_t2d:rotate(Form, Angle),
    Pixmap = epx:sft_glyph_render(Sft, Gid, Form1),
    ascii_pixels(Pixmap).


render_to(Char) ->
    render_to(Char,12, 0).
render_to(Char, Size, Angle) ->
    Sft = epx:sft_open("times", Size),
    Gid = epx:sft_glyph(Sft, Char),
    Form = epx_t2d:identity(),
    Form1 = epx_t2d:rotate(Form, Angle),
    epx_gc:set_foreground_color({0,0,255,0}),
    %% epx_gc:set_item(fader_value, 0.5),
    Pixmap = epx:pixmap_create(32, 32, argb),
    epx:pixmap_fill(Pixmap, {255,255,255,255}),
    ok = epx:sft_glyph_render_to(Sft, epx_gc:current(), 
				 Gid, Form1, 2, 2, Pixmap),
    ascii_pixels(Pixmap).

ascii_pixels(Pixmap) ->
    [{width,W}, {height,H}] = epx:pixmap_info(Pixmap, [width,height]),
    lists:foreach(
      fun(Y) ->
	      lists:foreach(
		fun(X) ->
			case epx:pixmap_get_pixels(Pixmap, X, Y, 1, 1) of
			    <<0>> ->
				io:put_chars(" ");
			    <<_>> ->
				io:put_chars("X");
			    <<255,255,255,255>> ->
				io:put_chars("-");
			    <<_G,L,255,L>> ->
				io:put_chars("X");
			    _Pixel ->
				io:format("~w", [_Pixel])
				%% io:put_chars(" ")
			end
		end, lists:seq(0, W-1)),
	      io:put_chars("\n")
      end, lists:seq(0, H-1)).
	      
    
