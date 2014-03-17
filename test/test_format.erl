%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Test pixel formats
%%% @end
%%% Created : 15 Mar 2014 by Tony Rogvall <tony@rogvall.se>

-module(test_format).

-compile(export_all).

test() ->
    Formats = [
	       "argb", "a8r8g8b8", "bgra/little", "b8g8r8a8/little",
	       "xrgb", "x8r8g8b8", "bgrx/little", "b8g8r8x8/little",
	       "abgr", "a8b8g8r8", "rgba/little", "r8g8b8a8/little", 
	       "xbgr", "x8b8g8r8", "rgbx/little", "r8g8b8x8/little", 
	       "rgba", "r8g8b8a8", "abgr/little", "a8b8g8r8/little", 
	       "rgbx", "r8g8b8x8", "xbgr/little", "x8b8g8r8/little", 
	       "bgra", "b8g8r8a8", "argb/little", "a8r8g8b8/little", 
	       "bgrx", "b8g8r8x8", "xrgb/little", "x8r8g8b8/little", 

	       "rgb", "r8g8b8",
	       "bgr", "b8g8r8",

	       "[565]", "r5g6b5",
	       "[565]/little", "r5g6b5/little",
	       "[555]", "r5g5b5x1",
	       "[555]/little", "r5g5b5x1/little"
	      ],
    lists:all(fun(F) ->test(F) end, Formats).

-define(A, 8).
-define(R, 16).
-define(G, 24).
-define(B, 32).

test(F) ->
    P = epx:pixmap_create(32, 32, F),
    epx:pixmap_put_pixel(P, 0, 0, {?A,?R,?G,?B}),
    Pixel    = epx:pixmap_get_pixel(P, 0, 0),
    RawPixel = epx:pixmap_get_pixels(P, 0, 0, 1, 1),
    io:format("test: ~s ", [F]),
    case validate(F, Pixel, RawPixel) of
	false ->
	    io:format("validation failed: ~s ~w =  ~w\n", [F,Pixel,RawPixel]),
	    false;
	true -> 
	    io:format("ok\n"),
	    true
    end.

validate("argb", {?A,?R,?G,?B}, <<?A,?R,?G,?B>>) -> true;
validate("a8r8g8b8", {?A,?R,?G,?B}, <<?A,?R,?G,?B>>) -> true;
validate("bgra/little", {?A,?R,?G,?B}, <<?A,?R,?G,?B>>) -> true;
validate("b8g8r8a8/little", {?A,?R,?G,?B}, <<?A,?R,?G,?B>>) -> true;
validate("xrgb", {?A,?R,?G,?B}, <<?A,?R,?G,?B>>) -> true;
validate("x8r8g8b8", {?A,?R,?G,?B}, <<?A,?R,?G,?B>>) -> true;
validate("bgrx/little", {?A,?R,?G,?B}, <<?A,?R,?G,?B>>) -> true;
validate("b8g8r8x8/little", {?A,?R,?G,?B}, <<?A,?R,?G,?B>>) -> true;

validate("abgr", {?A,?R,?G,?B}, <<?A,?B,?G,?R>>) -> true;
validate("a8b8g8r8", {?A,?R,?G,?B}, <<?A,?B,?G,?R>>) -> true;
validate("rgba/little", {?A,?R,?G,?B}, <<?A,?B,?G,?R>>) -> true;
validate("r8g8b8a8/little", {?A,?R,?G,?B}, <<?A,?B,?G,?R>>) -> true;
validate("xbgr", {?A,?R,?G,?B}, <<?A,?B,?G,?R>>) -> true;
validate("x8b8g8r8", {?A,?R,?G,?B}, <<?A,?B,?G,?R>>) -> true;
validate("rgbx/little", {?A,?R,?G,?B}, <<?A,?B,?G,?R>>) -> true;
validate("r8g8b8x8/little", {?A,?R,?G,?B}, <<?A,?B,?G,?R>>) -> true;

validate("rgba", {?A,?R,?G,?B}, <<?R,?G,?B,?A>>) -> true;
validate("r8g8b8a8", {?A,?R,?G,?B}, <<?R,?G,?B,?A>>) -> true;
validate("abgr/little", {?A,?R,?G,?B}, <<?R,?G,?B,?A>>) -> true;
validate("a8b8g8r8/little", {?A,?R,?G,?B}, <<?R,?G,?B,?A>>) -> true;

validate("rgbx", {?A,?R,?G,?B}, <<?R,?G,?B,?A>>) -> true;
validate("r8g8b8x8", {?A,?R,?G,?B}, <<?R,?G,?B,?A>>) -> true;
validate("xbgr/little", {?A,?R,?G,?B}, <<?R,?G,?B,?A>>) -> true;
validate("x8b8g8r8/little", {?A,?R,?G,?B}, <<?R,?G,?B,?A>>) -> true;

validate("bgra", {?A,?R,?G,?B}, <<?B,?G,?R,?A>>) -> true;
validate("b8g8r8a8", {?A,?R,?G,?B}, <<?B,?G,?R,?A>>) -> true;
validate("argb/little", {?A,?R,?G,?B},  <<?B,?G,?R,?A>>) -> true;
validate("a8r8g8b8/little", {?A,?R,?G,?B},  <<?B,?G,?R,?A>>) -> true;

validate("bgrx", {?A,?R,?G,?B}, <<?B,?G,?R,?A>>) -> true;
validate("b8g8r8x8", {?A,?R,?G,?B}, <<?B,?G,?R,?A>>) -> true;
validate("xrgb/little", {?A,?R,?G,?B},  <<?B,?G,?R,?A>>) -> true;
validate("x8r8g8b8/little", {?A,?R,?G,?B},  <<?B,?G,?R,?A>>) -> true;

validate("rgb",  {255,?R,?G,?B}, <<?R,?G,?B>>) -> true;
validate("bgr",  {255,?R,?G,?B}, <<?B,?G,?R>>) -> true;
validate("r8g8b8",  {255,?R,?G,?B}, <<?R,?G,?B>>) -> true;
validate("b8g8r8",  {255,?R,?G,?B}, <<?B,?G,?R>>) -> true;

validate("[565]", {255,?R,?G,?B},
	 <<(?R bsr 3):5,(?G bsr 2):6,(?B bsr 3):5>>) -> true;
validate("[565]/little", {255,?R,?G,?B}, <<B0,B1>>) ->
    case <<B1,B0>> of
	<<(?R bsr 3):5,(?G bsr 2):6,(?B bsr 3):5>> -> true;
	_ -> false
    end;
validate("r5g6b5", {255,?R,?G,?B},
	 <<(?R bsr 3):5,(?G bsr 2):6,(?B bsr 3):5>>) -> true;
validate("r5g6b5/little", {255,?R,?G,?B}, <<B0,B1>>) ->
    case <<B1,B0>> of
	<<(?R bsr 3):5,(?G bsr 2):6,(?B bsr 3):5>> -> true;
	_ -> false
    end;
validate("[555]", {255,?R,?G,?B},
	 <<(?R bsr 3):5,(?G bsr 3):5,(?B bsr 3):5,_:1>>) -> true;
validate("[555]/little", {255,?R,?G,?B}, <<B0,B1>>) ->
    case <<B1,B0>> of
	<<(?R bsr 3):5,(?G bsr 3):5,(?B bsr 3):5,_:1>> -> true;
	_ -> false
    end;
validate("r5g5b5x1", {255,?R,?G,?B},
	 <<(?R bsr 3):5,(?G bsr 3):5,(?B bsr 3):5,_:1>>) -> true;

validate("r5g5b5x1/little", {255,?R,?G,?B}, <<B0,B1>>) ->
    case <<B1,B0>> of
	<<(?R bsr 3):5,(?G bsr 3):5,(?B bsr 3):5,_:1>> -> true;
	_ -> false
    end;
validate(_F, _Pixel, _RawPixel) ->
    false.
