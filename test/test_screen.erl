%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Draw 640x480 window with markers and colors
%%% @end
%%% Created : 17 Mar 2017 by Tony Rogvall <tony@rogvall.se>

-module(test_screen).
-compile(export_all).

start() ->
    epx:start(),
    W = 640,
    H = 480,
    Win = epx:window_create(50,50,W,H,[all]),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(W,H,argb),
    epx:pixmap_fill(Pix, white),
    epx:pixmap_attach(Pix),
    epx:pixmap_draw(Pix,Win,0,0,0,0,W,H),

    draw(Win,Pix),
    draw_image(Win,Pix),

    (fun Draw() ->
	    receive
		{epx_event,Win, close} ->
		    epx:window_detach(Win),
		    epx:pixmap_detach(Pix),
		    ok;
		Event ->
		    io:format("Event: ~p\n", [Event]),
		    %% epx:pixmap_draw(Pix,Win,0,0,0,0,W,H),
		    Draw()
	    end
     end)().



draw_2x_hor_line(Pix,X, Y, Len) ->
    epx:draw_line(Pix, X, Y, X+Len-1, Y),
    epx:draw_line(Pix, X, Y+1, X+Len-1, Y+1).

draw_2x_ver_line(Pix,X, Y, Len) ->
    epx:draw_line(Pix, X, Y, X, Y+Len-1),
    epx:draw_line(Pix, X+1, Y, X+1, Y+Len-1).
    

draw(Win,Pix) ->
    W = epx:window_info(Win, width),
    H = epx:window_info(Win, height),
    K = 50,
    %% Upper left corner (green)
    epx_gc:draw(fun() ->
			X = 0,Y = 0,
			epx_gc:set_foreground_color(black),
			draw_2x_hor_line(Pix, X+4, Y, K),
			draw_2x_ver_line(Pix, X, Y+4, K),
			epx_gc:set_fill_style(solid),
			epx_gc:set_fill_color(red),
			epx:draw_rectangle(Pix, X+4, Y+4, K, K)
		end),

    %% Upper right corner (red)
    epx_gc:draw(fun() ->
			X = W-2, Y = 0,
			epx_gc:set_foreground_color(black),

			draw_2x_hor_line(Pix, X-K-2, Y, K),
			draw_2x_ver_line(Pix, X, Y+4, K),
			epx_gc:set_fill_style(solid),
			epx_gc:set_fill_color(green),
			epx:draw_rectangle(Pix, X-K-2, Y+4, K, K)
		end),

    %% Lower left corner (blue)
    epx_gc:draw(fun() ->
			X = 0, Y = H-2,
			epx_gc:set_foreground_color(black),
			draw_2x_hor_line(Pix, X+4, Y, K),
			draw_2x_ver_line(Pix, X, Y-K-2, K),
			epx_gc:set_fill_style(solid),
			epx_gc:set_fill_color(blue),
			epx:draw_rectangle(Pix, X+4, Y-K-2, K, K)
		end),

    %% Lower right corner (yellow)
    epx_gc:draw(fun() ->
			X = W-2, Y = H-2,
			epx_gc:set_foreground_color(black),
			draw_2x_hor_line(Pix, X-K-2, Y, K),
			draw_2x_ver_line(Pix, X, Y-K-2, K),
			epx_gc:set_fill_style(solid),
			epx_gc:set_fill_color(yellow),
			epx:draw_rectangle(Pix, X-K-2, Y-K-2, K, K)
		end),

    epx:pixmap_draw(Pix,Win,0,0,0,0,W,H),
    ok.


image() ->
    [0,0,0,0,0,0,0,0,0,0,0,0,
     0,1,1,1,1,1,1,1,1,0,0,0,
     0,1,1,1,1,1,1,1,1,0,0,0,
     0,1,1,1,1,0,0,0,0,0,0,0,
     0,1,1,0,1,1,0,0,0,0,0,0,
     0,1,1,0,0,1,1,0,0,0,0,0,
     0,1,1,0,0,0,1,1,0,0,0,0,
     0,1,1,0,0,0,0,1,1,0,0,0,
     0,1,1,0,0,0,0,0,1,1,0,0,
     0,0,0,0,0,0,0,0,0,1,1,0,
     0,0,0,0,0,0,0,0,0,1,1,0,
     0,0,0,0,0,0,0,0,0,0,0,0 ].

draw_image(Win,Pix) ->
    W = epx:window_info(Win, width) - 100,
    H = epx:window_info(Win, height) - 100,
    A = epx:pixmap_create(12, 12, argb),
    epx:pixmap_put_pixels(A, 0, 0, 12, 12, l8,
			  [I*255 || I <- image()]),
    epx:pixmap_scale_area(A, Pix, 50, 50, W, H),

    epx:pixmap_draw(Pix,Win,50,50,50,50,W,H),
    ok.
