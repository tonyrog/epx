%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Small font tool
%%% @end
%%% Created : 17 Apr 2017 by Tony Rogvall <tony@rogvall.se>

-module(epx_font_util).

-export([read_font_file/1]).
-export([read_info/1]).
-compile(export_all).

-define(EFNT, <<"EFNT">>).

-define(EPX_FONT_WEIGHT_NONE, 0).
-define(EPX_FONT_WEIGHT_MEDIUM, 1).
-define(EPX_FONT_WEIGHT_BOLD, 2).
-define(EPX_FONT_WEIGHT_DEMIBOLD, 3).

-define(EPX_FONT_SLANT_NONE,            0).
-define(EPX_FONT_SLANT_ROMAN,           1).
-define(EPX_FONT_SLANT_ITALIC,          2).
-define(EPX_FONT_SLANT_OBLIQUE,         3).
-define(EPX_FONT_SLANT_REVERSE_ITALIC, 4).
-define(EPX_FONT_SLANT_REVERSE_OBLIQUE, 5).
-define(EPX_FONT_SLANT_OTHER,          6).

-define(EPX_FONT_WIDTH_NONE,           0).
-define(EPX_FONT_WIDTH_NORMAL,         1).
-define(EPX_FONT_WIDTH_CONDENSED,      2).
-define(EPX_FONT_WIDTH_NARROW,         3).
-define(EPX_FONT_WIDTH_DOUBLE_WIDE,    4).

-define(EPX_FONT_STYLE_NONE,           0).
-define(EPX_FONT_STYLE_SERIF,          1).
-define(EPX_FONT_STYLE_SANS_SERIF,     2).
-define(EPX_FONT_STYLE_INFORMAL,       3).
-define(EPX_FONT_STYLE_DECORATED,      4).

-define(EPX_FONT_SPACING_NONE,         0).
-define(EPX_FONT_SPACING_PROPORTIONAL, 1).
-define(EPX_FONT_SPACING_MONOSPACED,   2).
-define(EPX_FONT_SPACING_CHAR_CELL,    3).

-type char4_t() :: <<_:4>>.
-type uint16_t() :: 0..16#ffff.
-type uint32_t() :: 0..16#ffffffff.
-type int16_t()  :: -16#8000..16#7fff.

-define(WEIGHT_TYPE,
	{enum,uint32_t,
	 [{?EPX_FONT_WEIGHT_NONE, none},
	  {?EPX_FONT_WEIGHT_MEDIUM, medium},
	  {?EPX_FONT_WEIGHT_BOLD, bold},
	  {?EPX_FONT_WEIGHT_DEMIBOLD, demibold}]}).

-define(SLANT_TYPE,
	{enum,uint32_t,
	 [{?EPX_FONT_SLANT_NONE, none},
	  {?EPX_FONT_SLANT_ROMAN, roman},
	  {?EPX_FONT_SLANT_ITALIC,italic},
	  {?EPX_FONT_SLANT_OBLIQUE, oblique},
	  {?EPX_FONT_SLANT_REVERSE_ITALIC, reverse_italic},
	  {?EPX_FONT_SLANT_REVERSE_OBLIQUE, reverse_oblique},
	  {?EPX_FONT_SLANT_OTHER, other}]}).

-define(WIDTH_TYPE,
	{enum,uint32_t,
	 [{?EPX_FONT_WIDTH_NONE, none},
	  {?EPX_FONT_WIDTH_NORMAL, normal},
	  {?EPX_FONT_WIDTH_CONDENSED, condensed},
	  {?EPX_FONT_WIDTH_NARROW, narrow},
	  {?EPX_FONT_WIDTH_DOUBLE_WIDE, double_width}]}).

-define(STYLE_TYPE,
	{enum,uint32_t,
	 [{?EPX_FONT_STYLE_NONE, none},
	  {?EPX_FONT_STYLE_SERIF, serif},
	  {?EPX_FONT_STYLE_SANS_SERIF, sans_serif},
	  {?EPX_FONT_STYLE_INFORMAL, informal},
	  {?EPX_FONT_STYLE_DECORATED, decorated}]}).

-define(SPACING_TYPE,
	{enum,uint32_t,
	 [{?EPX_FONT_SPACING_NONE, none},
	  {?EPX_FONT_SPACING_PROPORTIONAL, proportional},
	  {?EPX_FONT_SPACING_MONOSPACED, monospaced},
	  {?EPX_FONT_SPACING_CHAR_CELL, char_cell}]}).

-record(epx_font_info,
	{
	  weight :: atom() | uint32_t(),
	  slant  :: atom() | uint32_t(),
	  width   :: atom() | uint32_t(),
	  style  :: atom() | uint32_t(),
	  spacing  :: atom() | uint32_t(),
	  pixel_format  :: uint32_t(),
	  pixel_size  :: uint32_t(),
	  point_size  :: uint32_t(),
	  resolution_x  :: uint32_t(),
	  resolution_y  :: uint32_t(),
	  descent  :: uint32_t(),
	  ascent  :: uint32_t()
	  %% NOTE! add pad to 16 byte alignment if needed
	}).

%% ! Font File format
-record(epx_font_file, 
	{
	  magic   = <<"EFNT">> :: char4_t(),
	  foundry_offset :: uint32_t(),
	  family_offset  :: uint32_t(),
	  font_info      :: #epx_font_info{},
	  encoding_start :: uint32_t(),
	  encoding_stop  :: uint32_t(),
	  encoding_default :: uint32_t(),
	  string_table_start :: uint32_t(),
	  string_table_length :: uint32_t(),
	  offset_table_start :: uint32_t(),
	  offset_table_length :: uint32_t(),
	  glyph_table_start :: uint32_t(),
	  glyph_table_length :: uint32_t()
	  %%  NOTE! add pad to 16 byte alignment if needed
	}).

-record(epx_glyph, 
	{
	  name_offset :: uint32_t(),
	  width :: uint16_t(),
	  height :: uint16_t(),
	  xoffs :: int16_t(),
	  yoffs :: int16_t(),
	  dwx :: int16_t(),
	  dwy :: int16_t()
	  %% data start here 
	}).

epx_font_info_spec() ->
    {epx_font_info,
     [
      {weight,?WEIGHT_TYPE},
      {slant,?SLANT_TYPE},
      {width, ?WIDTH_TYPE},
      {style, ?STYLE_TYPE},
      {spacing, ?SPACING_TYPE},
      {pixel_format,uint32_t},
      {pixel_size, uint32_t},
      {point_size, uint32_t},
      {resolution_x, uint32_t},
      {resolution_y, uint32_t},
      {descent, uint32_t},
      {ascent, uint32_t}
     ]}.

epx_font_file_spec() ->
    {epx_font_file,
     [
      {magic, char4_t},
      {foundry_offset,uint32_t},
      {family_offset, uint32_t},
      {font_info    , epx_font_info_spec()},
      {encoding_start, uint32_t},
      {encoding_stop  , uint32_t},
      {encoding_default , uint32_t},
      {string_table_start , uint32_t},
      {string_table_length , uint32_t},
      {offset_table_start , uint32_t},
      {offset_table_length , uint32_t},
      {glyph_table_start , uint32_t},
      {glyph_table_length , uint32_t}
     ]}.
    
read_font_file(File) ->
    case file:open(File, [read,binary]) of
	{ok,Fd} ->
	    Result = read_type(Fd, epx_font_file_spec()),
	    file:close(Fd),
	    case Result of
		{ok, FileInfo}  when FileInfo#epx_font_file.magic =:=
				     <<"EFNT">> ->
		    {ok,FileInfo};
		{ok, _} ->
		    {error, bad_magic};
		{error,_} = Error -> Error
	    end;
	Error ->
	    Error
    end.

write_font_file(File, FileInfo) ->
    case file:open(File, [read,write,binary]) of
	{ok,Fd} ->
	    Result = write_type(Fd, epx_font_file_spec(), FileInfo),
	    file:close(Fd),
	    Result;
	Error ->
	    Error
    end.

set_weight(FileInfo, Weight) ->
    Info = set_weight_info(FileInfo#epx_font_file.font_info, Weight),
    FileInfo#epx_font_file { font_info = Info }.

set_weight_info(Info, Weight) ->
    Info#epx_font_info { weight = Weight }.

fix_font(File) ->
    {ok,FI} = read_font_file(File),
    FIB = set_weight(FI, bold),
    write_font_file(File, FIB).
    
read_info(Fd) ->
    read_type(Fd, epx_font_info_spec()).

write_type(Fd, Type, Data) ->
    IOList = encode_type(Type, Data),
    file:write(Fd, IOList).

read_type(Fd, Type) ->
    Size = type_size(Type),
    case file:read(Fd, Size) of
	{ok, Data} ->
	    {X,_Data1} = decode_type(Type, Data),
	    {ok, X};
	Error ->
	    Error
    end.

%% Decode type spec
decode_type(uint32_t, <<X:32/little,R/binary>>) -> {X,R};
decode_type(int32_t,  <<X:32/signed-little,R/binary>>) -> {X,R};
decode_type(uint16_t, <<X:16/little,R/binary>>) -> {X,R};
decode_type(int16_t,  <<X:16/signed-little,R/binary>>) -> {X,R};
decode_type(uint8_t, <<X:8/little,R/binary>>) -> {X,R};
decode_type(int8_t,  <<X:8/signed-little,R/binary>>) -> {X,R};
decode_type(char4_t, <<X:4/binary,R/binary>>) -> {X,R};
decode_type({enum,T,Es},Data) ->
    {X,Data1} = decode_type(T,Data),
    case lists:keyfind(X,1,Es) of
	false -> {X,Data1};
	{_,A} -> {A,Data1}
    end;
decode_type({Record,Fields}, Data) ->
    {Fs,Data1} = decode_type_list(Fields, Data),
    {list_to_tuple([Record|Fs]), Data1}.

decode_type_list(Ts, Data) ->
    decode_type_list(Ts, Data, []).

decode_type_list([{_Name,T}|Ts], Data, Acc) ->
    {X,Data1} = decode_type(T,Data),
    decode_type_list(Ts, Data1, [X|Acc]);
decode_type_list([], Data, Acc) ->
    {lists:reverse(Acc), Data}.

encode_type(uint32_t, X) -> <<X:32/little>>;
encode_type(int32_t, X) ->  <<X:32/signed-little>>;
encode_type(uint16_t, X) -> <<X:16/little>>;
encode_type(int16_t, X) ->  <<X:16/signed-little>>;
encode_type(uint8_t, X) -> <<X:8/little>>;
encode_type(int8_t, X) ->  <<X:8/signed-little>>;
encode_type(char4_t, X) ->  <<X:4/binary>>;
encode_type({enum,T,Es}, X) ->
    if is_atom(X) ->
	    case lists:keyfind(X,2,Es) of
		{Y,X} -> encode_type(T,Y)
	    end;
       true ->
	    encode_type(T,X)
    end;
encode_type({Record,Fields},X) when is_tuple(X) ->
    case tuple_to_list(X) of
	[Record|Xs] ->
	    encode_type_list(Fields,Xs)
    end.

encode_type_list(Ts, Xs) ->
    encode_type_list(Ts, Xs, []).

encode_type_list([{_Name,T}|Ts], [X|Xs], Acc) ->
    Data = encode_type(T, X),
    encode_type_list(Ts, Xs, [Data|Acc]);
encode_type_list([], [], Acc) ->
    lists:reverse(Acc).
    

%% Size of type spec    
type_size(uint32_t) -> 4;
type_size(int32_t) -> 4;
type_size(uint16_t) -> 2;
type_size(int16_t) -> 2;
type_size(uint8_t) -> 1;
type_size(int8_t) -> 1;
type_size(char_t) -> 1;
type_size(char4_t) -> 4;
type_size({enum,T,_Es}) -> type_size(T);
type_size({_Name,Fields}) -> type_list_size(Fields).

type_list_size(Ts) ->
    type_list_size(Ts,0).
type_list_size([{_Name,T}|Ts],Size) ->
    type_list_size(Ts, type_size(T)+Size);
type_list_size([],Size) -> Size.


    
    

	    
