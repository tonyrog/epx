%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%   Button widget
%%% @end
%%% Created :  5 Dec 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_button).


%% Process implementing a button

loop() ->
    receive
	{epx_event, Button, {button_press,[left],{X,Y,_}}} ->
	    %% handle button press
	{epx_event, Button, {button_release,[left],{X,Y,_}}} ->
	    ok;
    end.
	    
