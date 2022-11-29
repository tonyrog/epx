%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Askpass
%%% @end
%%% Created : 25 May 2022 by Tony Rogvall <tony@rogvall.se>

-module(epx_askpass).

-export([start/0, start/1]).

-define(FONT_COLOR, {0,255,255,255}).
-define(SCREEN_COLOR, {0,0,255}).
-define(WINDOW_WIDTH, 320).
-define(WINDOW_HEIGHT, 180).
-define(MARGIN, 32).
-define(NUM_DOTS, 8).
-define(DOT_RAD,  8).
-define(DOT_COLOR, {255,255,255}).

-record(state,
	{
	 window,
	 screen,
	 screen_color = ?SCREEN_COLOR,
	 font,
	 ascent,
	 prompt,
	 font_color = ?FONT_COLOR,
	 font_ascent,
	 entry = []
	}).


start() ->
    start(init:get_plain_arguments()).

start([Prompt|_]) ->
    start_(Prompt);
start(_) ->
    start_("Enter admin password").

start_(Prompt) ->
    epx:start(),
    {ok,Font} = epx_font:match([{name,"Arial"},{weight,bold},{size,18}]),
    Ascent = epx:font_info(Font, ascent),
    EventMask = [key_press],
    B = epx_backend:default(),
    ScreenWidth = epx:backend_info(B, width),
    ScreenHeight = epx:backend_info(B, height),
    X = (ScreenWidth-?WINDOW_WIDTH) div 2,
    Y = (ScreenHeight-?WINDOW_HEIGHT) div 2,
    Win = epx:window_create(X, Y, ?WINDOW_WIDTH, ?WINDOW_HEIGHT, EventMask),
    epx:window_attach(Win),
    epx:window_adjust(Win, [{name, "* * *"},
			    {max_width,?WINDOW_WIDTH},
			    {min_width,?WINDOW_WIDTH},
			    {max_height,?WINDOW_HEIGHT},
			    {min_height,?WINDOW_HEIGHT}]),
    epx:window_adjust(Win, [{x,X},{y,Y}]),
    Screen = epx:pixmap_create(?WINDOW_WIDTH, ?WINDOW_HEIGHT, argb),
    epx:pixmap_fill(Screen, ?SCREEN_COLOR),
    epx:pixmap_attach(Screen),
    S = #state { window = Win,
		 screen = Screen,
		 screen_color  = ?SCREEN_COLOR,
		 font   = Font,
		 font_ascent = Ascent,
		 font_color = ?FONT_COLOR,
		 prompt = Prompt,
		 entry = []
	       },
    draw(S),
    loop(S).

loop(S) ->
    receive
	{epx_event, Win, Event} when Win =:= S#state.window ->
	    event(Event, S);
	Other ->
	    erlang:display({event,Other}),
	    loop(S)
    end.

event(close, S) ->
    epx:window_detach(S#state.window),
    erlang:halt(0);
event({key_press,Sym,_Mod,_Code}, S) ->
    if Sym =:= $\r ->
	    epx:window_detach(S#state.window),
	    io:put_chars(lists:reverse(S#state.entry)),
	    erlang:halt(0);
       Sym =:= $\b ->
	    case S#state.entry of
		[_|Entry1] ->
		    S1 = S#state{entry=Entry1},
		    draw(S1),
		    loop(S1);
		[] ->
		    loop(S)
	    end;
       Sym >= $\s, Sym =< $~ ->
	    S1 = S#state{entry=[Sym|S#state.entry]},
	    draw(S1),
	    loop(S1);
       true ->
	    loop(S)
    end;
event(_Event, S) ->
    %% io:format("Got event ~p\n", [_Event]),
    loop(S).


draw(S) ->
    Screen = S#state.screen,
    epx_gc:set_font(S#state.font),
    epx_gc:set_foreground_color(S#state.font_color),
    {W,H} = epx_font:dimension(S#state.font, S#state.prompt),
    [{_,Width},{_,Height}] = epx:pixmap_info(Screen,[width,height]),
    X1 = max(0, (Width - W) div 2),
    Y1 = 2*H,
    epx:pixmap_fill(Screen, S#state.screen_color),
    epx:draw_string(Screen,X1,Y1+S#state.font_ascent,S#state.prompt),
    NDots = length(S#state.entry),
    if NDots > 0 ->
	    Pal = epx_palette:color_to_rgb(logo),
	    XStep = (Width - 2*?MARGIN) div NDots,
	    X0 = ?MARGIN + (XStep div 2),
	    Y0 = 4*H,
	    epx_gc:set_fill_color(?DOT_COLOR),
	    epx_gc:set_fill_style(solid),
	    draw_dot(lists:reverse(S#state.entry), 0, Pal, 
		     Screen, X0, Y0, XStep);
       true ->
	    ok
    end,
    epx:pixmap_draw(Screen, S#state.window, 0, 0, 0, 0, Width, Height).


draw_dot([C|Cs], I, Pal, Screen, Xc, Yt, XStep) ->
    epx_gc:set_fill_color(maps:get(I, Pal)),
    if 
	C >= $A, C =< $Z ->
	    X = Xc - ?DOT_RAD,
	    Y = Yt,
	    epx:draw_ellipse(Screen,X,Y,?DOT_RAD*2,?DOT_RAD*2);
	C >= $a, C =< $z ->
	    X = Xc - (?DOT_RAD div 2),
	    Y = Yt + (?DOT_RAD div 2),
	    epx:draw_ellipse(Screen,X,Y,?DOT_RAD,?DOT_RAD);
	C >= $0, C =< $9 ->
	    X = Xc - ?DOT_RAD,
	    Y = Yt,
	    epx:draw_rectangle(Screen,X,Y,?DOT_RAD*2,?DOT_RAD*2);
	true ->
	    X = Xc - (?DOT_RAD div 2),
	    Y = Yt + (?DOT_RAD div 2),
	    epx:draw_rectangle(Screen,X,Y,?DOT_RAD,?DOT_RAD)
    end,
    draw_dot(Cs, I+1, Pal, Screen, Xc+XStep, Yt, XStep);
draw_dot([], _I, _Pal, _Screen, _X, _Y, _XStep) ->
    ok.
