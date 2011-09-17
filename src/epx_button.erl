%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%   Button widget
%%% @end
%%% Created :  5 Dec 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_button).

%% Process implementing a button

start(Bound) ->
    spawn(fun() -> init(Bound) end).

init(Bound) ->
    loop(Bound).

loop(Bound) ->
    receive
	{epx_event, _Button, {button_press,[left],{X,Y,_}}} ->
	    draw(press, Bound),
	    loop(Bound);
	{epx_event, _Button, {button_release,[left],{X,Y,_}}} ->
	    draw(release, Bound),
	    loop(Bound)
    end.

draw(State, Bound) ->
    ok.


