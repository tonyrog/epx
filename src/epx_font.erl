%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%    EPX font server / font draw api
%%% @end
%%% Created : 15 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_font).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0]).
-export([add_path/1, match/1, i/0]).

-export([dimension/2]).
-export([info/1, info/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/epx.hrl").

-define(SERVER, epx_font_srv).
-define(TABLE,  epx_font_table).

-record(state,
	{
	  paths=[],
	  ftab        %% table of #epx_font_info{}
	 }).

%%====================================================================
%% API
%%====================================================================

add_path(Path) ->
    gen_server:call(?SERVER, {add_path,Path}).

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
	  
match(Spec) ->
    MA = match_arg(Spec, #match_arg{}),
    match_i(MA).

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
    if is_list(MA#match_arg.name), MA#match_arg.name =/= FI#epx_font_info.family ->
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
    

i() ->
    io:format("~15s ~5s ~8s ~15s ~4s ~s\n",
	      ["Name", "Size", "Weight", "Slant", "Res", "File"]),
    i(ets:first(?TABLE)).

i(Key) ->
    case ets:lookup(?TABLE, Key) of
	[] ->
	    ok;
	[FInfo] ->
	    io:format("~15s ~4w.~w ~8s ~15s ~4w\n",
		      [FInfo#epx_font_info.family,
		       FInfo#epx_font_info.point_size div 10,
		       FInfo#epx_font_info.point_size rem 10,
		       FInfo#epx_font_info.weight,
		       FInfo#epx_font_info.slant,
		       FInfo#epx_font_info.resolution_y
		      ]),
	    i(ets:next(?TABLE, Key))
    end.

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    FTab = ets:new(?TABLE, [named_table,set,public,
			    {keypos, #epx_font_info.file_name}]),
    Path0 = filename:join([code:priv_dir(epx), "fonts"]),
    %% We may not block nor do recursive calls here
    self() ! refresh,
    {ok, #state{ paths=[Path0], ftab=FTab }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({add_path,Path}, _From, State) ->
    case lists:member(Path, State#state.paths) of
	true -> 
	    {reply, ok, State};
	false ->
	    case load_efnt_path(Path, State#state.ftab) of
		ok ->
		    Ps = State#state.paths,
		    {reply, ok, State#state { paths=[Path|Ps]}};
		Error ->
		    {reply, Error, State}
	    end
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(refresh, State) ->
    %% FIXME: clean away fonts not found any more
    lists:foreach(
      fun(Path) ->
	      load_efnt_path(Path, State#state.ftab)
      end, State#state.paths),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

load_efnt_path(Path, FTab) ->
    case file:list_dir(Path) of
	{ok, Files} ->
	    lists:foreach(
	      fun(FName) ->
		      case lists:suffix(".efnt", FName) orelse
			  lists:suffix(".efnt2", FName)  of
			  false ->
			      ignore;
			  true ->
			      FileName = filename:join(Path, FName),
			      load_efnt_file(FileName, FTab)
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
