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
%%%    EPX font server / font draw api
%%%
%%%    The server keeps a table of efnt fonts found in the font path.
%%%    TrueType (.ttf) files in the path are scalable fonts: when a
%%%    size is asked for that has no efnt font, an efnt font is
%%%    generated from the ttf file (see epx_efnt) and kept in the font
%%%    cache directory. Application environment (epx):
%%%
%%%      font_path        extra directories with .efnt/.ttf files
%%%      font_cache_dir   directory for generated fonts
%%%                       (default: user cache dir "epx/fonts")
%%%      font_cache_size  max size of the cache in bytes (64M)
%%% @end
%%% Created : 15 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_font).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).
-export([sync/0]).
-export([add_path/1, refresh/0, match/1]).
-export([i/0, i/1]).
-export([scalable/0, cache_dir/0, cache_clear/0]).

-export([dimension/2]).
-export([info/1, info/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/epx.hrl").
-include_lib("kernel/include/file.hrl").

-define(SERVER, epx_font_srv).
-define(TABLE,  epx_font_table).
-define(TTF_TABLE,  epx_font_ttf_table).

-define(DEFAULT_CACHE_SIZE, (64*1024*1024)).
-define(GENERATE_TIMEOUT, 30000).
-define(CACHE_VERSION, 2).  %% bump when generated fonts change

%% scalable font
-record(ttf,
	{
	  file_name,  %% string (key)
	  family,     %% string
	  style,      %% string, "Regular", "Bold Italic" ...
	  weight,     %% medium | bold
	  slant,      %% roman | italic
	  hash        %% hash of file name, size, mtime and cache version
	}).

-record(state,
	{
	  paths=[],
	  ftab,        %% table of #epx_font_info{}
	  cache_dir,
	  cache_size,
	  pending = #{}  %% CacheFile => [From]
	}).

%%====================================================================
%% API
%%====================================================================

add_path(Path) ->
    gen_server:call(?SERVER, {add_path,Path}).

sync() ->
    gen_server:call(?SERVER, sync).

%% Calculate the size of the text in pixels given either
%% Gc or Font handle
dimension(Gc,String) when is_record(Gc, epx_gc) ->
    Font = epx:gc_get(Gc, font),
    {W,_Y} = epx:font_draw_string(undefined,Gc,0,0,String),
    H = epx:font_info(Font, descent) + epx:font_info(Font, ascent),
    {W,H};
dimension(Font,String) when is_record(Font, epx_font) ->
    Gc = epx:gc_copy(epx:gc_default()),
    epx:gc_set(Gc, font, Font),
    {W,_Y} = epx:font_draw_string(undefined,Gc,0,0,String),
    H = epx:font_info(Font, descent) + epx:font_info(Font, ascent),
    {W,H};
dimension(Spec,String) when is_list(Spec) ->
    {ok,Font} = match(Spec),
    dimension(Font,String).


info(Font, Item) ->
    %% io:format("item: ~p\n", [Item]),
    epx:font_info(Font, Item).

%% load epx_font_info structure
info(Font) when is_record(Font, epx_font) ->
    #epx_font_info {
      file_name = info(Font, file_name),
      handle = Font,
      file_size = info(Font, file_size),
      foundry   = info(Font, foundry_name),
      family    = info(Font, family_name),
      weight    = info(Font, weight),
      slant     = info(Font, slant),
      width     = info(Font, width),
      style     = info(Font, style),
      spacing   = info(Font, spacing),
      pixel_format = info(Font, pixel_format),
      pixel_size = info(Font, pixel_size),
      point_size = info(Font, point_size),
      resolution_x = info(Font, resolution_x),
      resolution_y = info(Font, resolution_y),
      descent = info(Font, descent),
      ascent = info(Font, ascent)
     }.

-record(match_arg,
	{
	  resolution=75,  %% default resolution
	  name,
	  size=0,
	  weight=none,
	  slant=none,
	  width=none,
	  style=none,
	  spacing=none
	}).

%% Find a font: [{name,Family},{size,Points},{weight,W},{slant,S},
%%               {resolution,Dpi}]
%% An efnt font in the exact size is used when there is one, otherwise
%% a font is generated from a matching ttf file (when size is given).
match(Spec) ->
    MA = match_arg(Spec, #match_arg{}),
    case match_i(MA) of
	false when MA#match_arg.size > 0 ->
	    match_ttf(MA);
	Result ->
	    Result
    end.

match_arg([{name,Name}|L], A) ->
    match_arg(L, A#match_arg { name=Name});
match_arg([{resolution,Res}|L], A) ->
    match_arg(L, A#match_arg { resolution=Res});
match_arg([{weight,W}|L], A) ->
    match_arg(L,A#match_arg{weight=W});
match_arg([{slant,S}|L], A) ->
    match_arg(L,A#match_arg{slant=S});
match_arg([{size,Size}|L], A) ->
    match_arg(L,A#match_arg{size=Size});
match_arg([_|L],A) ->
    match_arg(L,A);
match_arg([],A) ->
    A.

match_i(MA) ->
    match_i(ets:first(?TABLE), MA).

match_i(Key, MA) ->
    case ets:lookup(?TABLE, Key) of
	[] ->
	    false;
	[FI] ->
	    case match_fi(FI, MA) of
		false ->
		    match_i(ets:next(?TABLE,Key), MA);
		true ->
		    Handle = FI#epx_font_info.handle,
		    epx:font_map(Handle),
		    {ok,Handle}
	    end
    end.

match_fi(FI, MA) ->
    Scale = FI#epx_font_info.resolution_y/MA#match_arg.resolution,
    FSize = round(Scale*FI#epx_font_info.point_size / 10.0),
    if is_list(MA#match_arg.name),
       MA#match_arg.name =/= FI#epx_font_info.family ->
	    false;
       MA#match_arg.size > 0, MA#match_arg.size =/= FSize ->
	    false;
       MA#match_arg.weight =/= none,
       MA#match_arg.weight =/= FI#epx_font_info.weight ->
	    false;
       MA#match_arg.slant =/= none,
       MA#match_arg.slant =/= FI#epx_font_info.slant ->
	    false;
       true ->
	    true
    end.

%% Find a ttf file, prefer regular weight and slant when not given
match_ttf(MA) ->
    Ts = [T || T <- ets:tab2list(?TTF_TABLE), match_ttf(T, MA)],
    Pref = fun(#ttf{weight=W,slant=S}) -> {W =/= medium, S =/= roman} end,
    case lists:sort(fun(A,B) -> Pref(A) =< Pref(B) end, Ts) of
	[] ->
	    false;
	[T|_] ->
	    Size = round(MA#match_arg.size),
	    Res = MA#match_arg.resolution,
	    try gen_server:call(?SERVER, {scalable, T, Size, Res},
				?GENERATE_TIMEOUT) of
		{ok, Font} -> {ok, Font};
		_Error -> false
	    catch
		exit:_ -> false
	    end
    end.

match_ttf(T, MA) ->
    Name = MA#match_arg.name,
    (not is_list(Name) orelse
     string:equal(Name, T#ttf.family, true)) andalso
	(MA#match_arg.weight =:= none orelse
	 MA#match_arg.weight =:= T#ttf.weight) andalso
	(MA#match_arg.slant =:= none orelse
	 MA#match_arg.slant =:= T#ttf.slant).

%% list fonts
i() ->
    i([]).

i(Spec) ->
    io:format("~-20s ~5s ~8s ~15s ~4s ~s\n",
	      ["Name", "Size", "Weight", "Slant", "Res", "File"]),
    MA = match_arg(Spec, #match_arg{}),
    Fs = [FI || FI <- ets:tab2list(?TABLE), match_fi(FI, MA)],
    Key = fun(FI) -> {FI#epx_font_info.family, FI#epx_font_info.weight,
		      FI#epx_font_info.slant, FI#epx_font_info.point_size,
		      FI#epx_font_info.file_name} end,
    lists:foreach(
      fun(FI) ->
	      io:format("~-20s ~3w.~w ~8s ~15s ~4w ~s\n",
			[FI#epx_font_info.family,
			 FI#epx_font_info.point_size div 10,
			 FI#epx_font_info.point_size rem 10,
			 FI#epx_font_info.weight,
			 FI#epx_font_info.slant,
			 FI#epx_font_info.resolution_y,
			 filename:basename(FI#epx_font_info.file_name)])
      end, lists:sort(fun(A,B) -> Key(A) =< Key(B) end, Fs)),
    MA1 = MA#match_arg { size = 0 },
    lists:foreach(
      fun(T) ->
	      case match_ttf(T, MA1) of
		  true ->
		      io:format("~-20s ~5s ~8s ~15s ~4s ~s\n",
				[T#ttf.family, "*", T#ttf.weight, T#ttf.slant,
				 "*", filename:basename(T#ttf.file_name)]);
		  false ->
		      ok
	      end
      end, lists:keysort(#ttf.family, ets:tab2list(?TTF_TABLE))).

%% scalable (ttf) fonts [{Family, Style, File}]
scalable() ->
    lists:sort([{T#ttf.family, T#ttf.style, T#ttf.file_name} ||
		   T <- ets:tab2list(?TTF_TABLE)]).

cache_dir() ->
    gen_server:call(?SERVER, cache_dir).

%% remove generated fonts that are not loaded
cache_clear() ->
    gen_server:call(?SERVER, cache_clear).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

refresh() ->
    gen_server:cast(?SERVER, refresh).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    FTab = ets:new(?TABLE, [named_table,set,public,
			    {keypos, #epx_font_info.file_name}]),
    ets:new(?TTF_TABLE, [named_table,set,public,
			 {keypos, #ttf.file_name}]),
    Priv = code:priv_dir(epx),
    Paths = [filename:join(Priv, "fonts"), filename:join(Priv, "truetype")]
	++ application:get_env(epx, font_path, []),
    CacheDir = case application:get_env(epx, font_cache_dir) of
		   {ok, Dir} -> Dir;
		   undefined ->
		       filename:join(filename:basedir(user_cache, "epx"),
				     "fonts")
	       end,
    CacheSize = application:get_env(epx, font_cache_size,
				    ?DEFAULT_CACHE_SIZE),
    %% We may not block nor do recursive calls here
    self() ! refresh,
    {ok, #state{ paths=Paths, ftab=FTab,
		 cache_dir=CacheDir, cache_size=CacheSize }}.

handle_call({add_path,Path}, _From, State) ->
    case lists:member(Path, State#state.paths) of
	true ->
	    {reply, ok, State};
	false ->
	    case load_path(Path, State#state.ftab) of
		ok ->
		    Ps = State#state.paths,
		    {reply, ok, State#state { paths=Ps++[Path]}};
		Error ->
		    {reply, Error, State}
	    end
    end;
handle_call({scalable, T, Size, Res}, From, State) ->
    File = cache_file(T, Size, Res, State),
    case ets:lookup(?TABLE, File) of
	[FI] -> %% generated by an earlier call
	    {reply, {ok, FI#epx_font_info.handle}, State};
	[] ->
	    case maps:find(File, State#state.pending) of
		{ok, Waiting} ->
		    Pending = (State#state.pending)#{ File => [From|Waiting] },
		    {noreply, State#state { pending = Pending }};
		error ->
		    case filelib:is_regular(File) of
			true -> %% in the disk cache
			    touch(File),
			    {reply, open_font(File), State};
			false ->
			    generate(T, Size, Res, File),
			    Pending = (State#state.pending)#{ File => [From] },
			    {noreply, State#state { pending = Pending }}
		    end
	    end
    end;
handle_call(cache_dir, _From, State) ->
    {reply, State#state.cache_dir, State};
handle_call(cache_clear, _From, State) ->
    evict(State#state.cache_dir, 0),
    {reply, ok, State};
handle_call(sync, _From, State) ->
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, bad_call}, State}.

handle_cast(refresh, State) ->
    {noreply, refresh_(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({generated, File, Result}, State) ->
    {Waiting, Pending} = case maps:take(File, State#state.pending) of
			     error -> {[], State#state.pending};
			     Found -> Found
			 end,
    Reply = case Result of
		ok ->
		    %% open first, fonts in use are not evicted
		    Res = open_font(File),
		    evict(State#state.cache_dir, State#state.cache_size),
		    Res;
		{error, _} = Error ->
		    Error
	    end,
    [gen_server:reply(From, Reply) || From <- Waiting],
    {noreply, State#state { pending = Pending }};
handle_info(refresh, State) ->
    {noreply, refresh_(State)};
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% FIXME: clean away fonts not found any more (and not break code)
refresh_(State) ->
    lists:foreach(
      fun(Path) ->
	      load_path(Path, State#state.ftab)
      end, State#state.paths),
    State.

load_path(Path, FTab) ->
    case file:list_dir(Path) of
	{ok, Files} ->
	    lists:foreach(
	      fun(FName) ->
		      FileName = filename:join(Path, FName),
		      case filename:extension(FName) of
			  ".efnt" -> load_efnt_file(FileName, FTab);
			  ".efnt2" -> load_efnt_file(FileName, FTab);
			  ".ttf" -> load_ttf_file(FileName);
			  ".TTF" -> load_ttf_file(FileName);
			  _ -> ignore
		      end
	      end, Files);
	Error ->
	    Error
    end.

load_efnt_file(FileName, FTab) ->
    %% io:format("Try load: ~s\n", [FileName]),
    try epx:font_open(FileName) of
	Font ->
	    %% io:format("Open: ~p\n", [Font]),
	    FInfo = info(Font),
	    %% io:format("Info: ~p\n", [FInfo]),
	    ets:insert(FTab, FInfo)
    catch
	error:Error ->
	    {error, Error}
    end.

load_ttf_file(FileName) ->
    try epx_efnt:ttf_info(FileName) of
	#{ family := Family, style := Style, weight := W, slant := S } ->
	    {ok, FI} = file:read_file_info(FileName, [{time,posix}]),
	    Hash = erlang:phash2({filename:basename(FileName),
				  FI#file_info.size,
				  FI#file_info.mtime,
				  ?CACHE_VERSION}),
	    ets:insert(?TTF_TABLE, #ttf{ file_name = FileName,
					 family = Family, style = Style,
					 weight = W, slant = S, hash = Hash })
    catch
	error:Error ->
	    {error, Error}
    end.

%% name of the generated font in the cache
cache_file(T, Size, Res, State) ->
    Name = lists:flatten(
	     io_lib:format("~s-~s-~w-~w-~.16b.efnt",
			   [safe(T#ttf.family), safe(T#ttf.style),
			    Size, Res, T#ttf.hash])),
    filename:join(State#state.cache_dir, Name).

safe(Name) ->
    [if C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9 -> C;
	true -> $_
     end || C <- Name].

%% generate a font in a separate process, the result is sent back
generate(T, Size, Res, File) ->
    Server = self(),
    spawn(fun() ->
		  Result =
		      try
			  ok = filelib:ensure_dir(File),
			  Data = epx_efnt:from_ttf(T#ttf.file_name, Size,
						   [{resolution, Res}]),
			  Tmp = File ++ ".tmp" ++ os:getpid(),
			  ok = file:write_file(Tmp, Data),
			  ok = file:rename(Tmp, File)
		      catch
			  error:Reason ->
			      {error, Reason}
		      end,
		  Server ! {generated, File, Result}
	  end).

open_font(File) ->
    case load_efnt_file(File, ?TABLE) of
	true ->
	    [FI] = ets:lookup(?TABLE, File),
	    Handle = FI#epx_font_info.handle,
	    epx:font_map(Handle),
	    {ok, Handle};
	Error ->
	    Error
    end.

touch(File) ->
    file:change_time(File, calendar:local_time()).

%% Remove the oldest generated fonts that are not loaded until the
%% cache is at most MaxSize bytes
evict(Dir, MaxSize) ->
    Files = [{mtime(F), filelib:file_size(F), F} ||
		F <- filelib:wildcard(filename:join(Dir, "*.efnt"))],
    Total = lists:sum([S || {_,S,_} <- Files]),
    evict_(lists:sort(Files), Total, MaxSize).

evict_(_Files, Total, MaxSize) when Total =< MaxSize ->
    ok;
evict_([{_,Size,File}|Files], Total, MaxSize) ->
    case ets:member(?TABLE, File) of
	true -> %% in use
	    evict_(Files, Total, MaxSize);
	false ->
	    file:delete(File),
	    evict_(Files, Total - Size, MaxSize)
    end;
evict_([], _Total, _MaxSize) ->
    ok.

mtime(File) ->
    case file:read_file_info(File, [{time,posix}]) of
	{ok, FI} -> FI#file_info.mtime;
	_ -> 0
    end.
