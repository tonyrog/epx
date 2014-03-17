%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%     Epx pixel format parser
%%% @end
%%% Created : 14 Mar 2014 by Tony Rogvall <tony@rogvall.se>

-module(epx_format).

-export([parse/1, format/1]).
-export([scan/1]).
-include("../include/epx.hrl").

-define(is_digit(C), (((C)>=$0) andalso ((C) =< $9))).

parse(Format) ->
    set0(scan(Format)).

set0([big|Fs]) ->  set1(Fs, false);
set0([little|Fs]) -> set1(Fs, true);
set0([native|Fs]) -> set1(Fs, erlang:system_info(endian)=:=little);
set0(Fs) -> set1(Fs, false).

set1([{alpha,A}|Fs], Little) -> set2(Fs, A, true, true, Little);
set1([{x,X}|Fs], Little)     -> set2(Fs, X, false, true, Little);
set1(Fs, Little) -> set2(Fs, undefined, false, false, Little).

set2([{red,R},{green,G},{blue,B}|Fs], A, Alpha, AlphaFirst, Little) ->
    set3(Fs,R,G,B,A,false,Alpha,AlphaFirst,Little);
set2([{blue,B},{green,G},{red,R}|Fs], A, Alpha, AlphaFirst, Little) ->
    set3(Fs,R,G,B,A,true,Alpha,AlphaFirst,Little);
set2([{red,R}|Fs],A,Alpha, AlphaFirst, Little) ->
    set3(Fs,R,0,0,A,false,Alpha,AlphaFirst,Little);
set2([{green,G}|Fs],A,Alpha, AlphaFirst, Little) ->
    set3(Fs,0,G,0,A,false,Alpha,AlphaFirst,Little);
set2([{blue,B}|Fs],A,Alpha, AlphaFirst, Little) ->
    set3(Fs,0,0,B,A,false,Alpha,AlphaFirst,Little);
set2([{gray,G}|Fs],A,Alpha, AlphaFirst, Little) ->
    set4(Fs,gray,G,A,false,Alpha,AlphaFirst,Little);
set2(Fs, A,Alpha, AlphaFirst, Little) ->
    set3(Fs,0,0,0,A,false,Alpha,AlphaFirst,Little).

set3(Fs,0,0,0,A,BGR,_Alpha,_AlphaFirst,Little) ->
    set4(Fs,alpha,A,0,BGR,true,false,Little);
set3(Fs,N,N,N,A,BGR,Alpha,AlphaFirst,Little) ->
    case N of
	4 -> set4(Fs,rgb4,12,A,BGR,Alpha,AlphaFirst,Little);
	5 -> set4(Fs,rgb5,15,A,BGR,Alpha,AlphaFirst,Little);
	8 -> set4(Fs,rgb8,24,A,BGR,Alpha,AlphaFirst,Little);
	10 -> set4(Fs,rgb10,30,A,BGR,Alpha,AlphaFirst,Little);
	12 -> set4(Fs,rgb12,36,A,BGR,Alpha,AlphaFirst,Little);
	16 -> set4(Fs,rgb16,38,A,BGR,Alpha,AlphaFirst,Little)
    end;
set3(Fs,2,3,2,A,BGR,Alpha,AlphaFirst,Little) ->
    set4(Fs,rgb232,7,A,BGR,Alpha,AlphaFirst,Little);
set3(Fs,3,3,2,A,BGR,Alpha,AlphaFirst,Little) ->
    set4(Fs,rgb332,8,A,BGR,Alpha,AlphaFirst,Little);
set3(Fs,5,6,5,A,BGR,Alpha,AlphaFirst,Little) ->
    set4(Fs,rgb565,16,A,BGR,Alpha,AlphaFirst,Little);
set3(Fs,R,0,0,A,BGR,Alpha,AlphaFirst,Little) ->
    set4(Fs,red,R,A,BGR,Alpha,AlphaFirst,Little);
set3(Fs,0,G,0,A,BGR,Alpha,AlphaFirst,Little) ->
    set4(Fs,green,G,A,BGR,Alpha,AlphaFirst,Little);
set3(Fs,0,0,B,A,BGR,Alpha,AlphaFirst,Little) ->
    set4(Fs,blue,B,A,BGR,Alpha,AlphaFirst,Little).

set4([{alpha,A}],Format,Size,undefined,BGR,_Alpha,_AlphaFirst,Little) ->
    setf(Format,Size+A,BGR,true,false,Little);
set4([{calpha,A}],_Format,_Size,_A,BGR,_Alpha,_AlphaFirst,Little) ->
    setf(calpha,A,BGR,true,false,Little);
set4([{x,X}],Format,Size,undefined,BGR,_Alpha,_AlphaFirst,Little) ->
    setf(Format,Size+X,BGR,false,false,Little);
set4([],Format,Size,undefined,BGR,Alpha,AlphaFirst,Little) ->
    setf(Format,Size,BGR,Alpha,AlphaFirst,Little);
set4([],Format,Size,A,BGR,Alpha,AlphaFirst,Little) ->
    setf(Format,Size+A,BGR,Alpha,AlphaFirst,Little).

setf(Format,Bpp0,BGR,Alpha,AlphaFirst,Little) ->
    Bpp = case Bpp0 of
	      7  -> 8;
	      15 -> 16;
	      12 -> 16;
	      30 -> 32;
	      _ -> Bpp0
	  end,
    #epx_pixel_format { format = Format,
			bgr = BGR,
			alpha_first = AlphaFirst,
			alpha = Alpha,
			little = Little,
			bits_per_pixel = Bpp }.

scan(String) ->
    scan(String, []).

scan([$r|Cs], Fs) -> scan_(Cs, red, Fs);
scan([$g|Cs], Fs) -> scan_(Cs, green, Fs);
scan([$b|Cs], Fs) -> scan_(Cs, blue, Fs);
scan([$a|Cs], Fs) -> scan_(Cs, alpha, Fs);
scan([$l|Cs], Fs) -> scan_(Cs, gray, Fs);
scan([$x|Cs], Fs) -> scan_(Cs, x, Fs);
scan([$_|Cs], Fs) -> scan_(Cs, x, Fs);
scan("[565]"++Cs, Fs) ->
    scan(Cs, [{blue,5},{green,6},{red,5}|Fs]);
scan("[1555]"++Cs, Fs) ->
    scan(Cs, [{blue,5},{green,5},{red,5},{alpha,1}|Fs]);
scan("[5551]"++Cs, Fs) -> 
    scan(Cs, [{alpha,1},{blue,5},{green,5},{red,5}|Fs]);
scan("[555]"++Cs, Fs) ->
    scan(Cs, [{x,1},{blue,5},{green,5},{red,5}|Fs]);
scan("[efnt2]"++Cs, Fs) ->
    scan(Cs, [{calpha,8}|Fs]);
scan([], Fs)      -> [big|lists:reverse(Fs)];
scan("/big", Fs) ->  [big|lists:reverse(Fs)];
scan("/little", Fs) -> [little|lists:reverse(Fs)];
scan("/native", Fs) -> [native|lists:reverse(Fs)].

scan_([D1,D0|Cs],Component, Fs) when ?is_digit(D1), ?is_digit(D0) ->
    scan(Cs,[{Component,(D1-$0)*10+(D0-$0)}|Fs]);
scan_([D0|Cs],Component, Fs) when ?is_digit(D0) ->
    scan(Cs,[{Component,(D0-$0)}|Fs]);
scan_("/big", Component, Fs) ->
    [big|lists:reverse([{Component,8}|Fs])];
scan_("/little" ,Component, Fs) ->
    [little|lists:reverse([{Component,8}|Fs])];
scan_("/native" ,Component, Fs) ->
    [native|lists:reverse([{Component,8}|Fs])];
scan_([], Component, Fs) ->
    [big|lists:reverse([{Component,8}|Fs])];
scan_(Cs,Component, Fs) ->
    scan(Cs,[{Component,8}|Fs]).

%%
%% Format a epx_pixel_format as a format string
%%
put_fmt(_, 0) -> "";
put_fmt(F, 8) -> F;
put_fmt(F, N) -> [F,integer_to_list(N)].
     
format(#epx_pixel_format { format = Fmt,
			   bgr = Bgr,
			   alpha_first = AFirst,
			   alpha = Alpha,
			   little = Little,
			   bits_per_pixel = Bpp }) ->
    An = case Fmt of
	     rgb4 -> Bpp - 12;
	     rgb5 -> Bpp - 15;
	     rgb8 -> Bpp - 24;
	     rgb10 -> Bpp - 30;
	     gray  when Bpp>8 -> 8;
	     red   when Bpp>8 -> 8;
	     green when Bpp>8 -> 8;
	     blue  when Bpp>8 -> 8;
	     alpha -> Bpp;
	     calpha -> Bpp;
	     _ -> 0
	 end,
    {Rn,Gn,Bn,Ln} =
	case Fmt of
	    rgb4 -> {4,4,4,0};
	    rgb5 -> {5,5,5,0};
	    rgb8 -> {8,8,8,0};
	    rgb10 -> {10,10,10,0};
	    rgb232 -> {2,3,2,0};
	    rgb332 -> {3,3,2,0};
	    rgb565 -> {5,6,5,0};
	    gray   -> {0,0,0,(Bpp-An)};
	    red    -> {(Bpp-An),0,0,0};
	    green  -> {0,(Bpp-An),0,0};
	    blue   -> {0,0,(Bpp-An),0};
	    _ -> {0,0,0,0}
	end,
    lists:flatten(
      [
       if An> 0, AFirst ->
	       if Alpha -> put_fmt("a", An);
		  true -> put_fmt("x", An)
	       end;
	  true ->
	       ""
       end,
       put_fmt("l", Ln),
       if Bgr -> 
	       [put_fmt("b",Bn),put_fmt("g",Gn),put_fmt("r",Rn)];
	  true ->
	       [put_fmt("r",Rn),put_fmt("g",Gn),put_fmt("b",Bn)]
     end,
     if An> 0, not AFirst ->
	     if Alpha, Fmt =:= calpha, An =:= 8 ->
		     "[efnt2]";
		Alpha -> put_fmt("a",An);
		true -> put_fmt("x",An)
	     end;
	true ->
	     ""
     end,
     if Little -> "/little"; true -> "" end
      ]).
	    
	     

	    
	    

    
    
				       
			     
    
