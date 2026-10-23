%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Test the wayland backend: open backend, create a window,
%%%    draw a pixmap, animate a box and print events for a while,
%%%    then detach everything (twice) and check that the VM survives.
%%%
%%%    erl -pa ebin -noshell -s test_wayland run -s init stop
%%%    or test_wayland:start(Seconds)
%%% @end

-module(test_wayland).

-export([run/0, start/0, start/1, assumed/0]).

run() ->
    Res = start(5),
    io:format("test_wayland: result ~p\n", [Res]),
    Res.

start() ->
    start(10).

%% print the backend epx would select by default
assumed() ->
    io:format("assumed backend: ~p\n", [epx_backend:assumed_backend()]).

start(Secs) ->
    application:load(epx),
    %% make the wayland backend the default backend
    application:set_env(epx, backend, "wayland"),
    ok = epx:start(),
    Backends = epx:backend_list(),
    io:format("backends: ~p\n", [Backends]),
    true = lists:member("wayland", Backends),
    Be = epx_backend:default(),
    io:format("backend: ~p\n", [epx:backend_info(Be)]),
    "wayland" = epx:backend_info(Be, name),
    ok = session(Be, Secs, "epx wayland test 1"),
    %% once more to check that detach/attach is clean, also
    %% test resize and hide/show
    ok = session(Be, 1, "epx wayland test 2",
		 fun(Win) ->
			 io:format("resize to 500x350\n"),
			 epx:window_adjust(Win, [{width,500},{height,350}]),
			 timer:sleep(300),
			 io:format("hide\n"),
			 epx:window_adjust(Win, [{show,false}]),
			 timer:sleep(300),
			 io:format("show\n"),
			 epx:window_adjust(Win, [{show,true}])
		 end),
    ok.

session(Be, Secs, Title) ->
    session(Be, Secs, Title, fun(_) -> ok end).

session(Be, Secs, Title, Fun) ->
    W = 400, H = 300,
    Win = epx:window_create(40, 40, W, H, [all]),
    epx:window_attach(Win, Be),
    epx:window_adjust(Win, [{name, Title}]),
    Pix = epx:pixmap_create(W, H, argb),
    epx:pixmap_attach(Pix, Be),
    draw_background(Pix, W, H),
    epx:pixmap_draw(Pix, Win, 0, 0, 0, 0, W, H),
    epx:window_sync(Win),
    io:format("window info: ~p\n", [epx:window_info(Win)]),
    Fun(Win),
    T0 = erlang:monotonic_time(millisecond),
    Count = loop(Win, Pix, W, H, T0, T0 + Secs*1000, 0, #{}),
    io:format("event counts: ~p\n", [Count]),
    epx:window_detach(Win),
    epx:pixmap_detach(Pix),
    %% let the backend thread process the detach
    timer:sleep(200),
    ok.

loop(Win, Pix, W, H, T0, TEnd, Frame, Count) ->
    Now = erlang:monotonic_time(millisecond),
    if Now >= TEnd ->
	    Count;
       true ->
	    Timeout = min(50, TEnd - Now),
	    receive
		{epx_event, Win, close} ->
		    io:format("event: close\n"),
		    maps:update_with(close, fun(N) -> N+1 end, 1, Count);
		{epx_event, Win, Event} ->
		    print_event(Event),
		    {W1,H1} = case Event of
				  {configure, {_X,_Y,CW,CH}} -> {CW,CH};
				  _ -> {W,H}
			      end,
		    Key = event_key(Event),
		    loop(Win, Pix, W1, H1, T0, TEnd, Frame,
			 maps:update_with(Key, fun(N) -> N+1 end, 1, Count))
	    after Timeout ->
		    animate(Win, Pix, W, H, Frame),
		    loop(Win, Pix, W, H, T0, TEnd, Frame+1, Count)
	    end
    end.

event_key(Event) when is_tuple(Event) -> element(1, Event);
event_key(Event) -> Event.

print_event({motion, _, _}) -> ok;   %% too many
print_event(Event) ->
    io:format("event: ~p\n", [Event]).

draw_background(Pix, W, H) ->
    epx:pixmap_fill(Pix, {255,255,255,255}),
    epx_gc:draw(
      fun() ->
	      epx_gc:set_fill_style(solid),
	      epx_gc:set_fill_color({255,255,0,0}),
	      epx:draw_rectangle(Pix, 10, 10, 50, 50),
	      epx_gc:set_fill_color({255,0,255,0}),
	      epx:draw_rectangle(Pix, W-60, 10, 50, 50),
	      epx_gc:set_fill_color({255,0,0,255}),
	      epx:draw_rectangle(Pix, 10, H-60, 50, 50),
	      epx_gc:set_fill_color({255,0,0,0}),
	      epx:draw_rectangle(Pix, W-60, H-60, 50, 50)
      end).

%% move a box over the window, only redraw the changed area
animate(Win, Pix, W, H, Frame) ->
    PW = epx:pixmap_info(Pix, width),
    PH = epx:pixmap_info(Pix, height),
    Bw = 40,
    Range = max(1, min(W, PW) - Bw),
    X = (Frame*7) rem Range,
    Y = min(H, PH) div 2 - Bw div 2,
    C = (Frame*13) rem 256,
    epx_gc:draw(
      fun() ->
	      epx_gc:set_fill_style(solid),
	      epx_gc:set_fill_color({255,255,255,255}),
	      epx:draw_rectangle(Pix, 0, Y, min(W, PW), Bw),
	      epx_gc:set_fill_color({255,C,0,255-C}),
	      epx:draw_rectangle(Pix, X, Y, Bw, Bw)
      end),
    epx:pixmap_draw(Pix, Win, 0, Y, 0, Y, min(W, PW), Bw).
