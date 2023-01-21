%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Test bitmap operations
%%% @end
%%% Created : 28 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(test_bitmap).

-compile(export_all).

test() ->
    ok = test_image(),
    ok = test_pattern(),
    ok = test_fill_rect(),
    ok = test_fill_circle(),
    ok = test_draw_circle(),
    ok = test_put_get_bits(),
    ok.

test_put_get_bits() ->
    Bitmap = epx:bitmap_create(32, 32, 0),
    epx:bitmap_put_bits(Bitmap, 16, 16, 8, 4,
			<<2#11001100,
			  2#00110011,
			  2#11001100,
			  2#00110011 >>),
    Data = epx:bitmap_get_bits(Bitmap, 16, 16, 8, 4),
    epx:bitmap_put_bits(Bitmap, 24, 24, 8, 4, Data),
    plot_bitmap(Bitmap),
    ok.

			  
    
test_image() ->
    BitLists = ascii_bits_to_bits(bits0()),
    Bitmap = make_bitmap(BitLists),
    plot_bitmap(Bitmap),
    BitLists = get_bitlist(Bitmap),
    verify_bitmap(Bitmap, BitLists).

test_fill_rect() ->
    BitLists = ascii_bits_to_bits(bits1()),
    W = length(hd(BitLists)),
    H = length(BitLists),
    Bitmap = epx:bitmap_create(W, H),
    epx:bitmap_fill(Bitmap, 2#00000000),
    epx:bitmap_fill_rectangle(Bitmap, 3, 3, 6, 3, 2#11111111),
    epx:bitmap_fill_rectangle(Bitmap, 3, 7, 5, 3, 2#11111111),
    plot_bitmap(Bitmap),
    verify_bitmap(Bitmap, BitLists).

test_fill_circle() ->
    %% BitLists = ascii_bits_to_bits(bits1()),
    %% W = length(hd(BitLists)),
    %% H = length(BitLists),
    W = 32,
    H = 32,
    Bitmap = epx:bitmap_create(W, H),
    epx:bitmap_fill(Bitmap, 2#00000000),
    epx:bitmap_fill_ellipse(Bitmap, 2, 2, W-4, H-4, 2#10101010),
    plot_bitmap(Bitmap).
    %% verify_bitmap(Bitmap, BitLists).

test_draw_circle() ->
    %% BitLists = ascii_bits_to_bits(bits1()),
    %% W = length(hd(BitLists)),
    %% H = length(BitLists),
    W = 32,
    H = 32,
    Bitmap = epx:bitmap_create(W, H),
    epx:bitmap_fill(Bitmap, 0),
    epx:bitmap_draw_ellipse(Bitmap, 2, 2, W-4, H-4, 2#10101010),
    plot_bitmap(Bitmap).
    %% verify_bitmap(Bitmap, BitLists).

test_pattern() ->
    Bitmap = epx:bitmap_create(33, 64),
    33 = epx:bitmap_info(Bitmap, width),
    64 = epx:bitmap_info(Bitmap, height),
    ok = epx:bitmap_fill(Bitmap, 2#10110010),
    plot_bitmap(Bitmap),
    BitLists = get_bitlist(Bitmap),
    BitRow = [(C-$0) || 
		 C <- integer_to_list(2#101100101011001010110010101100101, 2)],
    lists:foreach(
      fun(Row) ->
	      true = (Row =:= BitRow)
      end, BitLists),
    ok.

get_bitlist(Bitmap) ->
    W = epx:bitmap_info(Bitmap, width),
    H = epx:bitmap_info(Bitmap, height),
    [ [ epx:bitmap_get_bit(Bitmap, X, Y) || X <- lists:seq(0,W-1)] ||
	Y <- lists:seq(0,H-1)].

plot_bitmap(Bitmap) ->
    W = epx:bitmap_info(Bitmap, width),
    H = epx:bitmap_info(Bitmap, height),
    lists:foreach(
      fun(Y) ->
	      io:put_chars("|"),
	      lists:foreach(
		fun(X) ->
			case epx:bitmap_get_bit(Bitmap, X, Y) of
			    1 -> io:put_chars("#");
			    0 -> io:put_chars(" ")
			end
		end, lists:seq(0, W-1)),
	      io:put_chars("|\n")
      end, lists:seq(0, H-1)).

			
make_bitmap(BitLists) ->
    W = length(hd(BitLists)),
    H = length(BitLists),
    Bitmap = epx:bitmap_create(W, H),
    each(fun(Y,Row) ->
		 each(fun(X, Bit) ->
			      epx:bitmap_put_bit(Bitmap, X, Y, Bit)
		      end, Row)
	 end, BitLists),
    Bitmap.


verify_bitmap(Bitmap, BitLists) ->
    each(fun(Y,Row) ->
		 each(fun(X, Bit) ->
			      Bit = epx:bitmap_get_bit(Bitmap, X, Y)
		      end, Row)
	 end, BitLists).

%% iterator with element index
each(Fun, List) ->
    each_(Fun, 0, List).

each_(_Fun, _I, []) -> 
    ok;
each_(Fun, I, [H|T]) ->
    Fun(I,H),
    each_(Fun,I+1,T).

%% convert rows of strings to integer 0|1
ascii_bits_to_bits(AsciiBits) ->
    [ [ case C of
	    $0  -> 0;
	    $\s -> 0;
	    $\t -> 0;
	    _ -> 1
	end || C <- Row] || Row <- AsciiBits].

bits0() ->
    ["            ",
     " ########   ",
     " ########   ",
     " ####       ",
     " ## ##      ",
     " ##  ##     ",
     " ##   ##    ",
     " ##    ##   ",
     " ##     ##  ",
     "         ## ",
     "         ## ",
     "            " ].

bits1() ->
    ["            ",
     "            ",
     "            ",
     "   ######   ",
     "   ######   ",
     "   ######   ",
     "            ",
     "   #####    ",
     "   #####    ",
     "   #####    ",
     "            ",
     "            " ].
