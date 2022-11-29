%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Present an installer dialog with command view
%%% @end
%%% Created : 27 May 2022 by Tony Rogvall <tony@rogvall.se>

-module(epx_installer).

-export([start/0]).

-define(FONT_COLOR, {0,255,255,255}).
-define(SCREEN_COLOR, {0,0,255}).
-define(BUTTON_COLOR, ?SCREEN_COLOR). %% {0,255,255}).
-define(WINDOW_WIDTH, 320).
-define(WINDOW_HEIGHT, 180).
-define(MARGIN, 32).

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
	 yes_rect,
	 yes_dim,
	 no_rect,
	 no_dim
	}).

start() ->
    start(#{}).
start(Options) when is_list(Options) ->
    start(maps:from_list(Options));
start(Options) ->
    epx:start(),
    {ok,Font} = epx_font:match([{name,"Arial"},{weight,bold},{size,18}]),
    Ascent = epx:font_info(Font, ascent),
    EventMask = [button_press, key_press, configure],
    B = epx_backend:default(),
    ScreenWidth = epx:backend_info(B, width),
    ScreenHeight = epx:backend_info(B, height),
    X = (ScreenWidth-?WINDOW_WIDTH) div 2,
    Y = (ScreenHeight-?WINDOW_HEIGHT) div 2,
    Win = epx:window_create(X, Y, ?WINDOW_WIDTH, ?WINDOW_HEIGHT, EventMask),
    epx:window_attach(Win),
    epx:window_adjust(Win, [{name, "Install"},
			    {max_width,?WINDOW_WIDTH},
			    {min_width,?WINDOW_WIDTH},
			    {max_height,?WINDOW_HEIGHT},
			    {min_height,?WINDOW_HEIGHT}]),
    epx:window_adjust(Win, [{x,X},{y,Y}]),
    Screen = epx:pixmap_create(?WINDOW_WIDTH, ?WINDOW_HEIGHT, argb),
    epx:pixmap_fill(Screen, ?SCREEN_COLOR),
    epx:pixmap_attach(Screen),
    Prompt = maps:get(prompt, Options, "Install Dekstop files?"),
    {Wno,Hno} = epx_font:dimension(Font, "No"),
    {Wyes,Hyes} = epx_font:dimension(Font, "Yes"),
    Bw = max(Wno, Wyes)+16,
    Bh = max(Hno, Hyes)+8,
    Xno = ?WINDOW_WIDTH - Bw - 8,
    By = ?WINDOW_HEIGHT-Bh - 8,
    No_Rect = {Xno, By, Bw, Bh},
    Xyes = ?WINDOW_WIDTH - Bw*2 - 16,
    Yes_Rect = {Xyes, By, Bw, Bh},

    S = #state { window = Win,
		 screen = Screen,
		 screen_color  = ?SCREEN_COLOR,
		 font   = Font,
		 font_ascent = Ascent,
		 font_color = ?FONT_COLOR,
		 prompt = Prompt,
		 yes_rect = Yes_Rect,
		 yes_dim = {true,Wyes,Hyes},
		 no_rect = No_Rect,
		 no_dim = {false,Wno,Hno}
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
    reply(S, no);
event({key_press,Sym,_Mod,_Code}, S) ->
    if Sym =:= $\r ->
	    case S#state.yes_dim of
		{true,_,_} -> reply(S, yes);
		_ ->
		    case S#state.no_dim of
			{true,_,_} -> reply(S, no);
			_ -> reply(S, undefined)
		    end
	    end;
       Sym =:= $y; Sym =:= $Y -> reply(S, yes);
       Sym =:= $n; Sym =:= $N -> reply(S, no);
       Sym =:= $\t ->
	    {F1,W1,H1} = S#state.yes_dim,
	    {F2,W2,H2} = S#state.no_dim,
	    S1 = S#state { yes_dim = {not F1,W1,H1},
			   no_dim = {not F2,W2,H2}},
	    draw(S1),
	    loop(S1);
       true -> loop(S)
    end;
event({button_press,_,Pt}, S) ->
    case epx_rect:contains(S#state.yes_rect, Pt) of
	true -> reply(S, yes);
	false ->
	    case epx_rect:contains(S#state.no_rect, Pt) of
		true -> reply(S, no);
		false -> loop(S)
	    end
    end;
event(_Event, S) ->
    %% io:format("Got event ~p\n", [_Event]),
    draw(S),
    loop(S).

reply(S, Answer) ->
    epx:window_detach(S#state.window),
    Answer.

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

    draw_button(S, S#state.no_rect, S#state.no_dim, "No"),
    draw_button(S, S#state.yes_rect, S#state.yes_dim, "Yes"),

    epx:pixmap_draw(Screen, S#state.window, 0, 0, 0, 0, Width, Height).

draw_button(S, Rect={X,Y,W,H}, {Focus,Bw,Bh}, Name) ->
    epx_gc:set_fill_color(?BUTTON_COLOR),
    epx_gc:set_border_width(0),
    epx_gc:set_fill_style(solid),
    epx:draw_rectangle(S#state.screen, Rect),
    epx_gc:set_fill_style(none),
    epx_gc:set_foreground_color({0,0,0}),
    if Focus ->
	    epx_gc:set_border_width(3);
       true ->
	    epx_gc:set_border_width(1)
    end,
    epx:draw_rectangle(S#state.screen, Rect),
    epx_gc:set_foreground_color(S#state.font_color),
    epx:draw_string(S#state.screen,X + (W - Bw) div 2,
		    S#state.font_ascent + Y + (H - Bh) div 2,
		    Name).
