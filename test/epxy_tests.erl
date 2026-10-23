%%% @doc Tests for epxy (widgets in epxw windows)
%%%
%%% Run: erl -pa ebin -pa test -noshell -eval 'eunit:test(epxy_tests, [verbose]), halt().'
%%%
%%% Events are injected with epxy:inject/2, the signals are collected
%%% with epxy:add_callback/3.
-module(epxy_tests).
-include_lib("eunit/include/eunit.hrl").

epxy_test_() ->
    {setup,
     fun() -> {ok,_} = epxy:start([{width,300},{height,200}]) end,
     fun(_) -> epxy:stop() end,
     [fun button/0,
      fun switch/0,
      fun slider/0,
      fun text/0,
      fun menu/0,
      fun panel/0,
      fun window/0]}.

subscribe(ID) ->
    Self = self(),
    {ok,_} = epxy:add_callback(ID, ID, fun(_Sig, I, Env) -> Self ! {sig,I,Env} end),
    ok.

press(Win, X, Y) ->
    epxy:inject(Win, {button_press,[left],{X,Y,0}}).
release(Win, X, Y) ->
    epxy:inject(Win, {button_release,[left],{X,Y,0}}).
motion(Win, X, Y) ->
    epxy:inject(Win, {motion,[left],{X,Y,0}}).
key(Win, Sym) ->
    epxy:inject(Win, {key_press,Sym,[],0}).

signals() ->
    receive {sig,I,#{ event := E } = Env} ->
	    [{I,E,maps:get(value,Env,undefined)} | signals()]
    after 300 -> []
    end.

button() ->
    ok = epxy:new("b1", [{type,button},{x,10},{y,10},{width,40},{height,20},
			 {text,"B"}]),
    subscribe("b1"),
    press("screen", 20, 15), release("screen", 20, 15),
    [{"b1",button_press,1},{"b1",button_release,0}] = signals(),
    [{state,normal},{value,0}] = epxy:get("b1", [state,value]),
    ok = epxy:delete("b1").

switch() ->
    ok = epxy:new("sw", [{type,switch},{x,10},{y,10},{width,40},{height,20}]),
    subscribe("sw"),
    press("screen", 20, 15), release("screen", 20, 15),
    press("screen", 20, 15), release("screen", 20, 15),
    [{"sw",changed,1},{"sw",changed,0}] = signals(),
    ok = epxy:delete("sw").

slider() ->
    ok = epxy:new("sl", [{type,slider},{x,10},{y,10},{width,102},{height,20},
			 {min,0},{max,100}]),
    subscribe("sl"),
    press("screen", 11, 15),
    motion("screen", 61, 15),
    release("screen", 61, 15),
    [{"sl",changed,0},{"sl",motion,50}] = signals(),
    [{value,50}] = epxy:get("sl", [value]),
    ok = epxy:delete("sl").

text() ->
    ok = epxy:new("tx", [{type,text},{x,10},{y,10},{width,80},{height,20},
			 {edit,true},{text,"ab"},{max_length,4}]),
    subscribe("tx"),
    press("screen", 20, 15), release("screen", 20, 15),
    [key("screen", C) || C <- [$c,$d,$e]],
    key("screen", $\b),
    key("screen", $\r),
    Sigs = signals(),
    [{text,"abc"},{focus,false}] = epxy:get("tx", [text,focus]),
    true = lists:member({"tx",focus_in,undefined}, Sigs),
    true = lists:member({"tx",changed,"abc"}, Sigs),
    true = lists:member({"tx",focus_out,undefined}, Sigs),
    ok = epxy:delete("tx").

menu() ->
    ok = epxy:new("m", [{type,menu},{x,10},{y,10},{items,["a","bb","ccc"]}]),
    subscribe("m"),
    [{height,H1}] = epxy:get("m", [height]),
    press("screen", 15, 15),
    [{state,active},{height,H3}] = epxy:get("m", [state,height]),
    H3 = 3*H1,
    motion("screen", 15, 10+H1+H1 div 2),   %% second item
    release("screen", 15, 10+H1+H1 div 2),
    [{"m",changed,2}] = signals(),
    [{state,normal},{height,H1}] = epxy:get("m", [state,height]),
    ok = epxy:delete("m").

panel() ->
    ok = epxy:new("p", [{type,panel},{x,0},{y,0},{width,300},{height,200},
			{tabs,["one","two"]},{value,1}]),
    ok = epxy:new("p.one", [{type,rectangle},{x,0},{y,50},{width,300},{height,150}]),
    ok = epxy:new("p.one.b", [{type,button},{x,10},{y,10},{width,40},{height,20}]),
    ok = epxy:new("p.two", [{type,rectangle},{x,0},{y,50},{width,300},{height,150}]),
    ok = epxy:new("p.two.b", [{type,button},{x,10},{y,10},{width,40},{height,20}]),
    subscribe("p"), subscribe("p.one.b"), subscribe("p.two.b"),
    %% button in tab one at (0+0+10, 0+50+10)
    press("screen", 15, 65), release("screen", 15, 65),
    [{"p.one.b",button_press,1},{"p.one.b",button_release,0}] = signals(),
    ok = epxy:set("p", [{value,2}]),
    press("screen", 15, 65), release("screen", 15, 65),
    [{"p.two.b",button_press,1},{"p.two.b",button_release,0}] = signals(),
    ok = epxy:delete("p"),
    [] = epxy:get("p.one.b", [x]).

window() ->
    ok = epxy:new("w", [{type,window},{x,400},{y,50},{width,100},{height,80}]),
    ok = epxy:new("w.b", [{type,button},{x,10},{y,10},{width,40},{height,20}]),
    subscribe("w.b"), subscribe("w"),
    press("w", 15, 15), release("w", 15, 15),
    [{"w.b",button_press,1},{"w.b",button_release,0}] = signals(),
    [{width,100}] = epxy:get("w", [width]),
    ok = epxy:set("w", [{color, red}]),
    ok = epxy:delete("w"),
    [] = epxy:get("w", [width]),
    [] = epxy:get("w.b", [x]).
