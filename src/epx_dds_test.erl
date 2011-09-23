%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%    Minor dds test functions
%%% @end
%%% Created : 27 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_dds_test).

-export([test/1]).

-record(state,
	{
	  backend,
	  window,
	  foreground_pixels,
	  background_pixels,
	  width,
	  height,
	  background = {255,0,0}
	}).
%%
%% Simple test
%%
test(File) ->
    case epx:backend_list() of
	[] ->
	    ok;
	[B1|_] ->
	    epx:start(B1),
	    Backend = epx_backend:default(),
	    Anim = epx:animation_open(File),
	    Width = epx:animation_info(Anim, width)+20,
	    Height = epx:animation_info(Anim, height)+20,
	    Format = epx:animation_info(Anim, pixel_format),
	    Window = epx:window_create(40, 40, Width, Height),
	    ForegroundPx = epx:pixmap_create(Width, Height, Format),
	    BackgroundPx = epx:pixmap_create(Width, Height, Format),
	    epx:window_attach(Window, Backend),
	    epx:pixmap_attach(BackgroundPx, Backend),
	    S0 = #state { backend = Backend,
			  window = Window,
			  foreground_pixels = ForegroundPx,
			  background_pixels = BackgroundPx,
			  width = Width,
			  height = Height },
	    loop(S0, Anim, 0, 0)
    end.

loop(S, Anim, Index, Timeout) ->
    receive
	{epx_event, Win, close} when Win =:= S#state.window ->
	    io:format("Got window1 close\n", []),
	    epx:pixmap_detach(S#state.background_pixels),
	    epx:window_detach(S#state.window),
	    ok;
	{epx_event, Win, Event} when Win =:= S#state.window ->
	    io:format("Got window event ~p\n", [Event]),
	    loop(S, Anim, Index, Timeout);
	{epx_event, _Win, Event} ->
	    io:format("Got other window ~w event ~p\n", [_Win,Event]),
	    loop(S, Anim, Index, Timeout)
    after Timeout ->
	    Count = epx:animation_info(Anim, count),
	    epx:pixmap_fill(S#state.background_pixels, S#state.background),
	    epx:animation_draw(Anim,Index,
			       S#state.background_pixels,epx_gc:current(),
			       10, 10),
	    epx:pixmap_draw(S#state.background_pixels, S#state.window,
			    0, 0, 0, 0, 
			    S#state.width, S#state.height),
	    Index1 = (Index + 1) rem Count,
	    loop(S, Anim, Index1, erlang:max(42,1000 div Count))
    end.

