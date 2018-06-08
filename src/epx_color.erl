%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Color conversion
%%% @end
%%% Created :  5 Dec 2011 by Tony Rogvall <tony@rogvall.se>

-module(epx_color).

-export([from_name/1]).
-export([hsv_to_rgb/1, hsv_to_rgb01/3]).
-export([rgb_to_hsv/1, rgb01_to_hsv/3]).
-export([hsl_to_rgb/1, hsl_to_rgb01/3]).
-export([hsl_to_rgbw/1,hsl_to_rgbw01/3]).
-export([rgb_to_hsl/1, rgb01_to_hsl/3]).
-export([hsv_to_cc/1]).

-define(PI, 3.141592653589793).

-define(rgb(R,G,B), {R,G,B}).

from_name(Name) when is_atom(Name) ->
    from_name(atom_to_list(Name));
from_name(Name) when is_list(Name) ->
    case string:to_lower(Name) of
	"aliceblue"    -> ?rgb(16#F0,16#F8,16#FF);
	"antiquewhite" -> ?rgb(16#FA,16#EB,16#D7);
	"aqua"         -> ?rgb(16#00,16#FF,16#FF);
	"aquamarine"   -> ?rgb(16#7F,16#FF,16#D4);
	"azure"        -> ?rgb(16#F0,16#FF,16#FF);
	"beige"        -> ?rgb(16#F5,16#F5,16#DC);
	"bisque"       -> ?rgb(16#FF,16#E4,16#C4);
	"black"        -> ?rgb(16#00,16#00,16#00);
	"blanchedalmond" ->	?rgb(16#FF,16#EB,16#CD);
	"blue" ->	  ?rgb(16#00,16#00,16#FF);
	"blueviolet" ->	?rgb(16#8A,16#2B,16#E2);
	"brown" ->	        ?rgb(16#A5,16#2A,16#2A);
	"burlywood" ->	?rgb(16#DE,16#B8,16#87);
	"cadetblue" ->	?rgb(16#5F,16#9E,16#A0);
	"chartreuse" ->	?rgb(16#7F,16#FF,16#00);
	"chocolate" ->	?rgb(16#D2,16#69,16#1E);
	"coral" ->	?rgb(16#FF,16#7F,16#50);
	"cornflowerblue" ->	?rgb(16#64,16#95,16#ED);
	"cornsilk" ->	?rgb(16#FF,16#F8,16#DC);
	"crimson" ->	?rgb(16#DC,16#14,16#3C);
	"cyan" ->	?rgb(16#00,16#FF,16#FF);
	"darkblue" ->	?rgb(16#00,16#00,16#8B);
	"darkcyan" ->	?rgb(16#00,16#8B,16#8B);
	"darkgoldenrod" ->	?rgb(16#B8,16#86,16#0B);
	"darkgray" ->	?rgb(16#A9,16#A9,16#A9);
	"darkgrey" ->	?rgb(16#A9,16#A9,16#A9);
	"darkgreen" ->	?rgb(16#00,16#64,16#00);
	"darkkhaki" ->	?rgb(16#BD,16#B7,16#6B);
	"darkmagenta" ->	?rgb(16#8B,16#00,16#8B);
	"darkolivegreen" ->	?rgb(16#55,16#6B,16#2F);
	"darkorange" ->	?rgb(16#FF,16#8C,16#00);
	"darkorchid" ->	?rgb(16#99,16#32,16#CC);
	"darkred" ->	?rgb(16#8B,16#00,16#00);
	"darksalmon" ->	?rgb(16#E9,16#96,16#7A);
	"darkseagreen" ->	?rgb(16#8F,16#BC,16#8F);
	"darkslateblue" ->	?rgb(16#48,16#3D,16#8B);
	"darkslategray" ->	?rgb(16#2F,16#4F,16#4F);
	"darkslategrey" ->	?rgb(16#2F,16#4F,16#4F);
	"darkturquoise" ->	?rgb(16#00,16#CE,16#D1);
	"darkviolet" ->	?rgb(16#94,16#00,16#D3);
	"deeppink" ->	?rgb(16#FF,16#14,16#93);
	"deepskyblue" ->	?rgb(16#00,16#BF,16#FF);
	"dimgray" ->	?rgb(16#69,16#69,16#69);
	"dimgrey" ->	?rgb(16#69,16#69,16#69);
	"dodgerblue" ->	?rgb(16#1E,16#90,16#FF);
	"firebrick" ->	?rgb(16#B2,16#22,16#22);
	"floralwhite" ->	?rgb(16#FF,16#FA,16#F0);
	"forestgreen" ->	?rgb(16#22,16#8B,16#22);
	"fuchsia" ->	?rgb(16#FF,16#00,16#FF);
	"gainsboro" ->	?rgb(16#DC,16#DC,16#DC);
	"ghostwhite" ->	?rgb(16#F8,16#F8,16#FF);
	"gold" ->	?rgb(16#FF,16#D7,16#00);
	"goldenrod" ->	?rgb(16#DA,16#A5,16#20);
	"gray" ->	?rgb(16#80,16#80,16#80);
	"grey" ->	?rgb(16#80,16#80,16#80);
	"green" ->	?rgb(16#00,16#80,16#00);
	"greenyellow" ->	?rgb(16#AD,16#FF,16#2F);
	"honeydew" ->	?rgb(16#F0,16#FF,16#F0);
	"hotpink" ->	?rgb(16#FF,16#69,16#B4);
	"indianred" -> 	?rgb(16#CD,16#5C,16#5C);
	"indigo" -> 	?rgb(16#4B,16#00,16#82);
	"ivory" ->	?rgb(16#FF,16#FF,16#F0);
	"khaki" ->	?rgb(16#F0,16#E6,16#8C);
	"lavender" ->	?rgb(16#E6,16#E6,16#FA);
	"lavenderblush" ->	?rgb(16#FF,16#F0,16#F5);
	"lawngreen" ->	?rgb(16#7C,16#FC,16#00);
	"lemonchiffon" ->	?rgb(16#FF,16#FA,16#CD);
	"lightblue" ->	?rgb(16#AD,16#D8,16#E6);
	"lightcoral" ->	?rgb(16#F0,16#80,16#80);
	"lightcyan" ->	?rgb(16#E0,16#FF,16#FF);
	"lightgoldenrodyellow" ->	?rgb(16#FA,16#FA,16#D2);
	"lightgray" ->	?rgb(16#D3,16#D3,16#D3);
	"lightgrey" ->	?rgb(16#D3,16#D3,16#D3);
	"lightgreen" ->	?rgb(16#90,16#EE,16#90);
	"lightpink" ->	?rgb(16#FF,16#B6,16#C1);
	"lightsalmon" ->	?rgb(16#FF,16#A0,16#7A);
	"lightseagreen" ->	?rgb(16#20,16#B2,16#AA);
	"lightskyblue" ->	?rgb(16#87,16#CE,16#FA);
	"lightslategray" ->	?rgb(16#77,16#88,16#99);
	"lightslategrey" ->	?rgb(16#77,16#88,16#99);
	"lightsteelblue" ->	?rgb(16#B0,16#C4,16#DE);
	"lightyellow" ->	?rgb(16#FF,16#FF,16#E0);
	"lime" ->	?rgb(16#00,16#FF,16#00);
	"limegreen" ->	?rgb(16#32,16#CD,16#32);
	"linen" ->	?rgb(16#FA,16#F0,16#E6);
	"magenta" ->	?rgb(16#FF,16#00,16#FF);
	"maroon" ->	?rgb(16#80,16#00,16#00);
	"mediumaquamarine" ->	?rgb(16#66,16#CD,16#AA);
	"mediumblue" ->	?rgb(16#00,16#00,16#CD);
	"mediumorchid" ->	?rgb(16#BA,16#55,16#D3);
	"mediumpurple" ->	?rgb(16#93,16#70,16#D8);
	"mediumseagreen" ->	?rgb(16#3C,16#B3,16#71);
	"mediumslateblue" ->	?rgb(16#7B,16#68,16#EE);
	"mediumspringgreen" ->	?rgb(16#00,16#FA,16#9A);
	"mediumturquoise" ->	?rgb(16#48,16#D1,16#CC);
	"mediumvioletred" ->	?rgb(16#C7,16#15,16#85);
	"midnightblue" ->	?rgb(16#19,16#19,16#70);
	"mintcream" ->	?rgb(16#F5,16#FF,16#FA);
	"mistyrose" ->	?rgb(16#FF,16#E4,16#E1);
	"moccasin" ->	?rgb(16#FF,16#E4,16#B5);
	"navajowhite" ->	?rgb(16#FF,16#DE,16#AD);
	"navy" ->	?rgb(16#00,16#00,16#80);
	"oldlace" ->	?rgb(16#FD,16#F5,16#E6);
	"olive" ->	?rgb(16#80,16#80,16#00);
	"olivedrab" ->	?rgb(16#6B,16#8E,16#23);
	"orange" ->	?rgb(16#FF,16#A5,16#00);
	"orangered" ->	?rgb(16#FF,16#45,16#00);
	"orchid" ->	?rgb(16#DA,16#70,16#D6);
	"palegoldenrod" ->	?rgb(16#EE,16#E8,16#AA);
	"palegreen" ->	?rgb(16#98,16#FB,16#98);
	"paleturquoise" ->	?rgb(16#AF,16#EE,16#EE);
	"palevioletred" ->	?rgb(16#D8,16#70,16#93);
	"papayawhip" ->  ?rgb(16#FF,16#EF,16#D5);
	"peachpuff" ->	  ?rgb(16#FF,16#DA,16#B9);
	"peru" ->	  ?rgb(16#CD,16#85,16#3F);
	"pink" ->	  ?rgb(16#FF,16#C0,16#CB);
	"plum" ->	  ?rgb(16#DD,16#A0,16#DD);
	"powderblue" ->  ?rgb(16#B0,16#E0,16#E6);
	"purple" ->	  ?rgb(16#80,16#00,16#80);
	"red" ->	  ?rgb(16#FF,16#00,16#00);
	"rosybrown" ->	  ?rgb(16#BC,16#8F,16#8F);
	"royalblue" ->	  ?rgb(16#41,16#69,16#E1);
	"saddlebrown" -> ?rgb(16#8B,16#45,16#13);
	"salmon" ->	  ?rgb(16#FA,16#80,16#72);
	"sandybrown" ->  ?rgb(16#F4,16#A4,16#60);
	"seagreen" ->	  ?rgb(16#2E,16#8B,16#57);
	"seashell" ->	  ?rgb(16#FF,16#F5,16#EE);
	"sienna" ->	  ?rgb(16#A0,16#52,16#2D);
	"silver" ->	  ?rgb(16#C0,16#C0,16#C0);
	"skyblue" ->	  ?rgb(16#87,16#CE,16#EB);
	"slateblue" ->	  ?rgb(16#6A,16#5A,16#CD);
	"slategray" ->	  ?rgb(16#70,16#80,16#90);
	"slategrey" ->	  ?rgb(16#70,16#80,16#90);
	"snow" ->	          ?rgb(16#FF,16#FA,16#FA);
	"springgreen" ->       ?rgb(16#00,16#FF,16#7F);
	"steelblue" ->	  ?rgb(16#46,16#82,16#B4);
	"tan" ->	          ?rgb(16#D2,16#B4,16#8C);
	"teal" ->	          ?rgb(16#00,16#80,16#80);
	"thistle" ->	          ?rgb(16#D8,16#BF,16#D8);
	"tomato" ->	          ?rgb(16#FF,16#63,16#47);
	"turquoise" ->	  ?rgb(16#40,16#E0,16#D0);
	"violet" ->	          ?rgb(16#EE,16#82,16#EE);
	"wheat" ->	          ?rgb(16#F5,16#DE,16#B3);
	"white" ->	          ?rgb(16#FF,16#FF,16#FF);
	"whitesmoke" ->        ?rgb(16#F5,16#F5,16#F5);
	"yellow" ->	          ?rgb(16#FF,16#FF,16#00);
	"yellowgreen" ->       ?rgb(16#9A,16#CD,16#32);
	_ -> false
    end.

hsl_to_rgb({H,S,L}) ->
    {R,G,B} = hsl_to_rgb01(H,S,L),
    {trunc(R*255), trunc(G*255), trunc(B*255)}.

hsl_to_rgb01(H,S,L) when 
      is_number(H), H >= 0, H < 360,
      is_number(S), S >= 0, S =< 1.0,
      is_number(L), L >= 0, L =< 1.0 ->
    C = (1 - abs(2*L -1))*S,
    X = C*(1-abs(fmod(H/60,2)-1)),
    M = L - C/2,
    if H < 60  -> {C+M,X+M,0+M};
       H < 120 -> {X+M,C+M,0+M};
       H < 180 -> {0+M,C+M,X+M};
       H < 240 -> {0+M,X+M,C+M};
       H < 300 -> {X+M,0+M,C+M};
       H < 360 -> {C+M,0+M,X+M}
    end.

hsl_to_rgbw({H,S,L}) ->
    {R,G,B,W} = hsl_to_rgbw01(H,S,L),
    {trunc(R*255), trunc(G*255), trunc(B*255), trunc(W*255)}.

hsl_to_rgbw01(H,S,L) when 
      is_number(H), H >= 0, H < 360,
      is_number(S), S >= 0, S =< 1.0,
      is_number(L), L >= 0, L =< 1.0 ->
    C = (1 - abs(2*L -1))*S,
    X = C*(1-abs(fmod(H/60,2)-1)),
    M = L - C/2,
    W = (1-S)*L,
    if H < 60  -> {C+M,X+M,0+M,W};
       H < 120 -> {X+M,C+M,0+M,W};
       H < 180 -> {0+M,C+M,X+M,W};
       H < 240 -> {0+M,X+M,C+M,W};
       H < 300 -> {X+M,0+M,C+M,W};
       H < 360 -> {C+M,0+M,X+M,W}
    end.
       
rgb_to_hsl(Color) when is_atom(Color); is_list(Color) ->
    rgb_to_hsl(from_name(Color));       
rgb_to_hsl({R,G,B}) ->
    rgb01_to_hsl(R/255,G/255,B/255).

rgb01_to_hsl(R,G,B) ->
    Cmax = erlang:max(erlang:max(R,G),B),
    Cmin = erlang:min(erlang:min(R,G),B),
    D0 = Cmax - Cmin,
    D = if D0 < 0.0001 -> 0;
	   D0 > 0.9999 -> 1;
	   true -> D0
	end,
    H0 = if D =:= 0 -> 0.0;
	   Cmax =:= R -> 60*fmod((G-B)/D, 6);
	   Cmax =:= G -> 60*((B-R)/D + 2);
	   Cmax =:= B -> 60*((R-G)/D + 4)
	end,
    H = if H0 < 0 -> H0 + 360;
	   true -> H0
	end,
    L = (Cmax + Cmin)/2,
    S0 = if D =:= 0 -> 0.0;
	   true -> D/(1-abs(2*L-1))
	end,
    S = erlang:min(S0,1.0),
    {H,S,L}.


hsv_to_rgb01(_H,S,V) when S =:= 0 ->
    {V, V, V};
hsv_to_rgb01(H,S,V) ->
    H60 = H / 60,
    I = trunc(H60),
    F = H60 - I,
    P = V * ( 1 - S ),
    Q = V * ( 1 - S * F),
    T = V * ( 1 - S * (1 - F)),
    case I of
	0 -> { V, T, P };
	1 -> { Q, V, P };
	2 -> { P, V, T };
	3 -> { P, Q, V };
	4 -> { T, P, V };
	5 -> { V, P, Q }
    end.

hsv_to_rgb({H,S,V}) ->
    {R,G,B} = hsv_to_rgb01(H,S,V),
    {trunc(R*255),trunc(G*255),trunc(B*255)}.

rgb01_to_hsv(R,G,B) ->
    RgbMax = max(max(R,G),B),
    RgbMin = min(min(R,G),B),
    Delta = RgbMax - RgbMin,
    V = RgbMax,
    if Delta == 0 -> %% invalid (gray only)
	    {-1.0, 0.0, V };
       true ->
	    S = Delta / RgbMax,
	    H0 = if R == RgbMax ->
			 ( G - B ) / Delta;
		    G == RgbMax ->
			 2 + (( B - R) / Delta);
		    true ->
			 4 + (( R - G) / Delta)
		 end,
	    H1 = H0*60.0,
	    if H1 < 0 ->
		    {H1 + 360.0, S, V };
	       true  ->
		    {H1, S, V}
	    end
    end.

rgb_to_hsv(Color) when is_atom(Color); is_list(Color) ->
    rgb_to_hsv(from_name(Color));
rgb_to_hsv({R,G,B}) ->
    rgb01_to_hsv(R/255.0,G/255.0,B/255.0).

%% HSV to "color code"
hsv_to_cc({H,S,_V}) ->
    A = H*(2*?PI/360),
    { S * math:sin(A), S * math:cos(A) }.

fmod(A, B) when is_number(A), is_number(B), B =/= 0 ->
    AB = abs(A / B),
    C = (AB - trunc(AB))*abs(B),
    if A < 0 -> -C;
       true -> C
    end.
