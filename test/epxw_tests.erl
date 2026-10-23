%%% @doc Tests for the epxw window behaviour
%%%
%%% Run: erl -pa ebin -pa test -noshell -eval 'eunit:test(epxw_tests, [verbose]), halt().'
%%%
%%% A real window is opened on the default backend (or the none backend
%%% when there is no display). Events are simulated by sending
%%% epx_event messages to the epxw process.
-module(epxw_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/epx.hrl").

-define(W, 200).
-define(H, 100).

%% start a window that reports draw calls to the test process
start() ->
    Self = self(),
    {ok, Win} =
	epxw:start(#{ init => fun(_) -> {ok, #{ tester => Self }} end,
		      draw => fun(Pixels, Area, S=#{ tester := T }) ->
				      epx_gc:set_fill_style(solid),
				      epx_gc:set_fill_color(maps:get(color,S,red)),
				      epx:draw_rectangle(Pixels, {0,0,1000,1000}),
				      T ! {draw, Area},
				      S
			      end,
		      key_press => fun(_Event, S) ->
					   epxw:invalidate({10,10,5,5}),
					   S
				   end,
		      handle_info => fun({ping, From}, S) ->
					     From ! pong,
					     {noreply, S}
				     end
		    }, [], [{width,?W},{height,?H},{bottom_bar,0},
			    {scroll_bar_size,0}]),
    flush_draws(),
    Win.

stop(Win) ->
    unlink(Win),
    exit(Win, kill).

flush_draws() ->
    receive {draw, _} -> flush_draws()
    after 200 -> ok
    end.

draws() ->
    receive {draw, Area} -> [Area | draws()]
    after 300 -> []
    end.

%% many events give one redraw
coalesce_test() ->
    Win = start(),
    try
	EpxWin = epxw:call(Win, fun(S) -> {epxw:window(), S} end),
	[Win ! #epx_event{win=EpxWin, data={key_press,$a,[],0}} ||
	    _ <- lists:seq(1,50)],
	Ds = draws(),
	?assert(length(Ds) >= 1),
	?assert(length(Ds) < 10),
	[{10,10,5,5}|_] = Ds
    after
	stop(Win)
    end.

%% client api runs inside the window process
client_api_test() ->
    Win = start(),
    try
	{?W,?H} = epxw:call(Win, fun(S) -> {{epxw:width(),epxw:height()}, S} end),
	%% invalidate from call is drawn before the reply is sent
	ok = epxw:call(Win, fun(S) -> epxw:invalidate({1,2,3,4}), {ok,S} end),
	receive {draw, A1} -> {1,2,3,4} = A1 after 0 -> error(no_draw) end,
	%% cast is drawn later
	ok = epxw:cast(Win, fun(S) -> epxw:invalidate({5,6,7,8}), S end),
	[{5,6,7,8}] = draws(),
	%% the callback api is not usable outside the window process
	?assertError(not_in_epxw_callback, epxw:width()),
	?assertError(not_in_epxw_callback, epxw:invalidate()),
	%% user handle_info still get other messages
	Win ! {ping, self()},
	receive pong -> ok after 1000 -> error(no_pong) end
    after
	stop(Win)
    end.

%% only the dirty part of the content is drawn and sent to the window
partial_update_test() ->
    Win = start(),
    try
	ok = epxw:call(Win, fun(S) -> epxw:invalidate({50,20,10,10}),
				      {ok,S#{ color => blue }} end),
	[{50,20,10,10}] = draws(),
	{In,Out} = epxw:call(Win,
			     fun(S) ->
				     Screen = epxw:screen(),
				     {{epx:pixmap_get_pixel(Screen,55,25),
				       epx:pixmap_get_pixel(Screen,5,5)}, S}
			     end),
	%% blue inside the dirty area, the old red outside
	{_,0,0,255} = In,
	{_,255,0,0} = Out
    after
	stop(Win)
    end.

%% view_to_window_y used the x position of the view
view_transform_test() ->
    Win = start(),
    try
	{Y0,Y1,X1} =
	    epxw:call(Win,
		      fun(S) ->
			      epxw:set_view_rect({0,0,1000,1000}),
			      epxw:set_view_pos(30, 70),
			      {{epxw:view_to_window_y(70),
				epxw:view_to_window_y(80),
				epxw:view_to_window_x(40)}, S}
		      end),
	{0,10,10} = {round(Y0),round(Y1),round(X1)}
    after
	stop(Win)
    end.

%% named widgets in an epxw application
widgets_test() ->
    Self = self(),
    {ok, Win} =
	epxw:start(#{ init => fun(_) -> {ok, #{}} end,
		      draw => fun(_Pixels, _Area, S) -> S end,
		      widget => fun(Name, Env, S) ->
					Self ! {widget, Name, maps:get(event,Env)},
					S
				end,
		      button_press => fun(Ev, S) -> Self ! {press, Ev}, S end
		    }, [],
		   [{width,?W},{height,?H},{bottom_bar,0},{scroll_bar_size,0},
		    {widgets, [{"ok", [{type,button},{x,10},{y,10},
				       {width,40},{height,20},{text,"OK"}]}]}]),
    try
	EpxWin = epxw:call(Win, fun(S) -> {epxw:window(), S} end),
	Ev = fun(E) -> Win ! #epx_event{win=EpxWin, data=E} end,
	%% hit the button: widget signals, no button_press callback
	Ev({button_press,[left],{20,15,0}}),
	Ev({button_release,[left],{20,15,0}}),
	receive {widget,"ok",button_press} -> ok after 500 -> error(no_press) end,
	receive {widget,"ok",button_release} -> ok after 500 -> error(no_release) end,
	receive {press,_} -> error(press_not_consumed) after 100 -> ok end,
	%% miss the button: normal button_press callback
	Ev({button_press,[left],{150,80,0}}),
	receive {press,{button_press,[left],_}} -> ok after 500 -> error(no_user_press) end,
	Ev({button_release,[left],{150,80,0}}),
	%% create, set and read widgets from a client
	ok = epxw:call(Win, fun(S) ->
				    {epxw:widget_new("v", [{type,value},{x,60},{y,10},
							   {width,40},{height,20}]), S}
			    end),
	ok = epxw:call(Win, fun(S) -> {epxw:widget_set("v", [{value,42}]), S} end),
	[{value,42}] = epxw:call(Win, fun(S) -> {epxw:widget_get("v", [value]), S} end),
	["ok","v"] = epxw:call(Win, fun(S) -> {epxw:widgets(), S} end),
	{error,ealready} = epxw:call(Win, fun(S) -> {epxw:widget_new("v", []), S} end),
	ok = epxw:call(Win, fun(S) -> {epxw:widget_delete("v"), S} end),
	{error,enoent} = epxw:call(Win, fun(S) -> {epxw:widget_set("v", []), S} end)
    after
	stop(Win)
    end.
