%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2026, Rogvall Invest AB, <tony@rogvall.se>
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
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    A tree of named widgets (see epxy_widget) in one window.
%%%
%%%    Names are dot separated paths, "panel.button" is a child of
%%%    "panel". Children are drawn and hit tested in name order, where
%%%    numeric name components come before other names. A child of a
%%%    panel with tabs is named "panel.Tab", only the child for the
%%%    selected tab is shown.
%%%
%%%    The tree is a plain value, the owner (epxw) keeps it.
%%% @end
%%%-------------------------------------------------------------------

-module(epxy_tree).

-export([new/0, add/3, set/3, get/3, delete/2, find/2, names/1]).
-export([draw/2, event/2, bounds/2, animating_bounds/1]).

-define(ROOT, "").

-type name() :: string().
-type rect() :: {number(),number(),number(),number()}.
-type signal() :: {name(), Env::map()}.
-type tree() :: #{ widgets := #{ name() => epxy_widget:widget() },
		   children := #{ name() => [name()] },
		   active := [{name(), {number(),number()}}],
		   focus := [{name(), {number(),number()}}] }.
-export_type([tree/0]).

-spec new() -> tree().
new() ->
    #{ widgets => #{}, children => #{}, active => [], focus => [] }.

%% Add widget Name, the id of the widget is Name unless given in Props
-spec add(Name::name(), Props::list(), T::tree()) ->
	  {ok, tree(), Dirty::all|[rect()]} | {error, term()}.
add(Name, Props, T = #{ widgets := Ws }) when is_list(Name), Name =/= "" ->
    case maps:is_key(Name, Ws) of
	true ->
	    {error, ealready};
	false ->
	    try epxy_widget:new([{id,Name}|Props]) of
		W ->
		    T1 = add_child(parent(Name), Name,
				   T#{ widgets => Ws#{ Name => W }}),
		    {ok, T1, dirty_rects(Name, [W], T1)}
	    catch
		error:Reason ->
		    {error, Reason}
	    end
    end;
add(_Name, _Props, _T) ->
    {error, badarg}.

-spec set(Name::name(), Props::list(), T::tree()) ->
	  {ok, tree(), [signal()], Dirty::all|[rect()]} | {error, term()}.
set(Name, Props, T) ->
    case find(Name, T) of
	error ->
	    {error, enoent};
	{ok, W} ->
	    try epxy_widget:set(W, Props) of
		W1 ->
		    #{ x := X, y := Y } = W,
		    XY = {X,Y},
		    {T1,Sigs1} = update_active(Name, W, W1, XY, T, []),
		    {T2,Sigs2} = update_key_focus(Name, W, W1, XY, T1, Sigs1),
		    T3 = store(Name, W1, T2),
		    Dirty = merge_dirty(dirty_rects(Name, [W], T),
					dirty_rects(Name, [W1], T3)),
		    {ok, T3, lists:reverse(Sigs2), Dirty}
	    catch
		error:Reason ->
		    {error, Reason}
	    end
    end.

-spec get(Name::name(), Keys::[atom()], T::tree()) -> [{atom(),term()}].
get(Name, Keys, T) ->
    case find(Name, T) of
	error -> [];
	{ok, W} -> epxy_widget:get(W, Keys)
    end.

%% delete a widget and all widgets below it
-spec delete(Name::name(), T::tree()) ->
	  {ok, tree(), Dirty::all|[rect()]} | {error, enoent}.
delete(Name, T) ->
    case find(Name, T) of
	error ->
	    {error, enoent};
	{ok, W} ->
	    Dirty = dirty_rects(Name, [W], T),
	    T1 = delete_tree(Name, T),
	    #{ children := Cs } = T1,
	    P = parent(Name),
	    Cs1 = case maps:get(P, Cs, []) of
		      [] -> Cs;
		      Siblings -> Cs#{ P => lists:delete(Name, Siblings) }
		  end,
	    {ok, T1#{ children => Cs1 }, Dirty}
    end.

delete_tree(Name, T = #{ widgets := Ws, children := Cs,
			 active := A, focus := F }) ->
    T1 = lists:foldl(fun delete_tree/2, T, maps:get(Name, Cs, [])),
    #{ widgets := Ws1, children := Cs1 } = T1,
    _ = Ws, _ = Cs,
    T1#{ widgets => maps:remove(Name, Ws1),
	 children => maps:remove(Name, Cs1),
	 active => lists:keydelete(Name, 1, A),
	 focus => lists:keydelete(Name, 1, F) }.

-spec find(Name::name(), T::tree()) -> {ok, epxy_widget:widget()} | error.
find(Name, #{ widgets := Ws }) ->
    maps:find(Name, Ws).

names(#{ widgets := Ws }) ->
    lists:sort(maps:keys(Ws)).

store(Name, W, T = #{ widgets := Ws }) ->
    T#{ widgets => Ws#{ Name => W }}.

%%%===================================================================
%%% Names
%%%===================================================================

parent(Name) ->
    case string:split(Name, ".", trailing) of
	[_] -> ?ROOT;
	[Parent, _] -> Parent
    end.

%% sort key of the last name component, numbers before names
sort_key(Name) ->
    Last = lists:last(string:split(Name, ".", all)),
    try list_to_integer(Last) of
	I -> {0, I}
    catch
	error:_ -> {1, Last}
    end.

add_child(Parent, Name, T = #{ children := Cs }) ->
    Siblings = maps:get(Parent, Cs, []),
    Key = sort_key(Name),
    T#{ children => Cs#{ Parent => insert_sorted(Name, Key, Siblings) }}.

insert_sorted(Name, Key, [N|Ns]) ->
    case sort_key(N) of
	K when K < Key -> [N | insert_sorted(Name, Key, Ns)];
	_ -> [Name, N | Ns]
    end;
insert_sorted(Name, _Key, []) ->
    [Name].

children(Name, #{ children := Cs }) ->
    maps:get(Name, Cs, []).

%% absolute position of a widget
abs_pos(?ROOT, _T) ->
    {0,0};
abs_pos(Name, T) ->
    case find(Name, T) of
	error -> {0,0};
	{ok,W} -> epxy_widget:pos(W, abs_pos(parent(Name), T))
    end.

%% window area covered by the widget
-spec bounds(Name::name(), T::tree()) -> rect() | undefined.
bounds(Name, T) ->
    case find(Name, T) of
	error -> undefined;
	{ok,W} -> epxy_widget:bounds(W, abs_pos(Name, T))
    end.

%% Area to redraw when the widget Name changes from/to the widgets Ws.
%% A widget with children may move or hide them, redraw all.
dirty_rects(Name, Ws, T) ->
    case children(Name, T) of
	[] ->
	    XY = abs_pos(parent(Name), T),
	    [epxy_widget:bounds(W, epxy_widget:pos(W, XY)) || W <- Ws];
	_ ->
	    all
    end.

merge_dirty(all, _) -> all;
merge_dirty(_, all) -> all;
merge_dirty(A, B) -> A ++ B.

%% bounds of widgets that are animating
animating_bounds(T = #{ widgets := Ws }) ->
    [bounds(Name, T) || {Name,W} <- maps:to_list(Ws),
			epxy_widget:is_animating(W)].

%%%===================================================================
%%% Drawing
%%%===================================================================

%% Draw all widgets, return the tree (with animations stepped) and true
%% when some widget is animating
-spec draw(T::tree(), Pixmap::epx:epx_pixmap()) -> {tree(), boolean()}.
draw(T, Pixmap) ->
    {T1, Last} = draw_tree(?ROOT, Pixmap, {0,0}, T, []),
    T2 = lists:foldl(fun({Name,W,XY}, Ti) ->
			     draw_widget(Name, W, Pixmap, XY, Ti)
		     end, T1, Last),
    {T2, lists:any(fun epxy_widget:is_animating/1,
		   maps:values(maps:get(widgets, T2)))}.

draw_tree(Parent, Pixmap, XY, T, Last) ->
    lists:foldl(
      fun(Name, {Ti,Li}) ->
	      case find(Name, Ti) of
		  error -> {Ti,Li};
		  {ok,W} ->
		      draw_one(Name, W, Pixmap, XY, maps:get(children_first,W),
			       Ti, Li)
	      end
      end, {T, Last}, children(Parent, T)).

draw_one(Name, W, Pixmap, XY, ChildrenFirst, T, Last) ->
    XY1 = epxy_widget:pos(W, XY),
    IsLast = maps:get(last, W),
    if ChildrenFirst ->
	    {T1,Last1} = draw_children(Name, W, Pixmap, XY1, T, Last),
	    if IsLast -> {T1,[{Name,W,XY1}|Last1]};
	       true -> {draw_widget(Name, W, Pixmap, XY1, T1), Last1}
	    end;
       IsLast ->
	    draw_children(Name, W, Pixmap, XY1, T, [{Name,W,XY1}|Last]);
       true ->
	    T1 = draw_widget(Name, W, Pixmap, XY1, T),
	    draw_children(Name, W, Pixmap, XY1, T1, Last)
    end.

draw_children(_Name, #{ hidden := all }, _Pixmap, _XY, T, Last) ->
    {T, Last};
draw_children(Name, W = #{ type := panel, disabled := D },
	      Pixmap, XY, T, Last) when D =:= false; D =:= rest ->
    case tab_child(Name, W, T) of
	false -> {T, Last};
	{TabName, TW} -> draw_one(TabName, TW, Pixmap, XY, false, T, Last)
    end;
draw_children(Name, _W, Pixmap, XY, T, Last) ->
    draw_tree(Name, Pixmap, XY, T, Last).

draw_widget(Name, W, Pixmap, XY, T) ->
    case epxy_widget:draw(W, Pixmap, XY) of
	W -> T;
	W1 -> store(Name, W1, T)
    end.

%% the child of the selected panel tab
tab_child(Name, #{ value := V, tabs := Tabs }, T) ->
    if is_integer(V), V >= 1, V =< length(Tabs) ->
	    TabName = Name ++ "." ++ tab_name(lists:nth(V, Tabs)),
	    case find(TabName, T) of
		{ok,TW} -> {TabName, TW};
		error -> false
	    end;
       true ->
	    false
    end.

tab_name(Tab) when is_atom(Tab) -> atom_to_list(Tab);
tab_name(Tab) when is_binary(Tab) -> binary_to_list(Tab);
tab_name(Tab) when is_list(Tab) -> Tab.

%%%===================================================================
%%% Hit test
%%%===================================================================

%% Find all widgets hit by the position Pos
at_location(Pos, T) ->
    Hits = select_tree(?ROOT, Pos, {0,0}, [], T),
    unique(select_rest(Hits, [])).

%% reverse list but stop if widget disables rest
select_rest([H={_Name,#{ disabled := rest },_XY}|_], _Acc) -> [H];
select_rest([H|Hs], Acc) -> select_rest(Hs, [H|Acc]);
select_rest([], Acc) -> Acc.

unique(Hs) -> unique(Hs, []).
unique([H={Name,_,_}|Hs], Seen) ->
    case lists:member(Name, Seen) of
	true -> unique(Hs, Seen);
	false -> [H | unique(Hs, [Name|Seen])]
    end;
unique([], _) -> [].

select_tree(Parent, Pos, XY, Acc, T) ->
    lists:foldl(
      fun(Name, Acci) ->
	      case find(Name, T) of
		  error -> Acci;
		  {ok,W} -> select_one(Name, W, Pos, XY, Acci, true, T)
	      end
      end, Acc, children(Parent, T)).

select_one(Name, W, Pos, XY, Acc, ChildrenFirst, T) ->
    XY1 = epxy_widget:pos(W, XY),
    if ChildrenFirst ->
	    Acc1 = select_children(Name, W, Pos, XY1, Acc, T),
	    select_widget(Name, W, Pos, XY1, Acc1);
       true ->
	    Acc1 = select_widget(Name, W, Pos, XY1, Acc),
	    select_children(Name, W, Pos, XY1, Acc1, T)
    end.

select_children(_Name, #{ disabled := all }, _Pos, _XY, Acc, _T) ->
    Acc;
select_children(Name, W = #{ type := panel, disabled := D }, Pos, XY, Acc, T)
  when D =:= false; D =:= rest ->
    case epxy_widget:tab_at_location(W, Pos, XY) of
	0 -> %% check in current tab
	    case tab_child(Name, W, T) of
		false -> Acc;
		{TabName, TW} -> select_one(TabName, TW, Pos, XY, Acc, false, T)
	    end;
	_Tab ->
	    [{Name,W,XY}|Acc]
    end;
select_children(Name, _W, Pos, XY, Acc, T) ->
    select_tree(Name, Pos, XY, Acc, T).

select_widget(Name, W, Pos, XY, Acc) ->
    case epxy_widget:hit(W, Pos, XY) of
	{true, W1} -> [{Name,W1,XY}|Acc];
	false -> Acc
    end.

%%%===================================================================
%%% Events
%%%===================================================================

%% Dispatch a window event to the widgets.
%% Positions are {X,Y} or {X,Y,Z} in window/view coordinates.
%% Return the new tree, the signals, the effects (motion_on/motion_off),
%% the area to redraw and if a widget used the event.
-spec event(Event::term(), T::tree()) ->
	  {tree(), [signal()], [motion_on|motion_off], all|[rect()], boolean()}.
event(Event, T) ->
    event_(normalize(Event), T).

normalize({Tag, Button, {X,Y,_Z}}) -> normalize({Tag, Button, {X,Y}});
normalize({Tag, Button, {X,Y}}) when Tag =:= button_press;
				      Tag =:= button_release;
				      Tag =:= motion ->
    {Tag, Button, {floor(X), floor(Y)}};
normalize(Event) -> Event.

event_(Event={key_press,_,_,_}, T) -> focus_event(Event, T);
event_(Event={key_release,_,_,_}, T) -> focus_event(Event, T);
event_(Event={button_press,Button,Pos}, T) ->
    case lists:member(left, Button) of
	true ->
	    case at_location(Pos, T) of
		[] -> none(T);
		Ws -> widgets_event(Ws, Event, T)
	    end;
	false ->
	    none(T)
    end;
event_(Event={button_release,Button,Pos}, T = #{ active := Active }) ->
    case lists:member(left, Button) of
	true ->
	    Ws0 = at_location(Pos, T),
	    %% also add active widgets not already on the list
	    Ws1 = lists:foldl(
		    fun({Name,XY},Ws) ->
			    case lists:keymember(Name, 1, Ws) of
				false ->
				    case find(Name, T) of
					{ok,W} -> [{Name,W,XY}|Ws];
					error -> Ws
				    end;
				true -> Ws
			    end
		    end, Ws0, Active),
	    {T1,Sigs,Effs,Dirty,_} =
		lists:foldl(
		  fun({Name,W,XY}, {Ti,Si,Ei,Di,_}) ->
			  {W1,Es} = epxy_widget:event(Event, W, XY),
			  {Ti1,Di1} = changed(Name, W, W1, Ti, Di),
			  {Ti1, signals(Name,Es,Si), effects(Es,Ei), Di1, true}
		  end, {T,[],[],[],false}, Ws1),
	    {T1#{ active => [] }, lists:reverse(Sigs), lists:reverse(Effs),
	     Dirty, Ws1 =/= []};
	false ->
	    none(T)
    end;
event_(Event={motion,Button,Pos}, T = #{ active := Active }) ->
    case lists:member(left, Button) of
	true ->
	    Ws = case Active of
		     [] -> at_location(Pos, T);
		     _ -> [{Name,W,XY} || {Name,XY} <- Active,
					  {ok,W} <- [find(Name, T)]]
		 end,
	    case Ws of
		[] -> none(T);
		_ ->
		    {T1,Sigs,Effs,Dirty} =
			lists:foldl(
			  fun({Name,W,XY}, {Ti,Si,Ei,Di}) ->
				  {W1,Es} = epxy_widget:event(Event, W, XY),
				  {Ti1,Di1} = changed(Name, W, W1, Ti, Di),
				  {Ti1, signals(Name,Es,Si), effects(Es,Ei), Di1}
			  end, {T,[],[],[]}, Ws),
		    {T1, lists:reverse(Sigs), lists:reverse(Effs), Dirty, true}
	    end;
	false ->
	    none(T)
    end;
event_(_Event, T) ->
    none(T).

none(T) -> {T, [], [], [], false}.

focus_event(Event, T = #{ focus := Focus }) ->
    case [{Name,W,XY} || {Name,XY} <- Focus, {ok,W} <- [find(Name, T)]] of
	[] -> none(T);
	Ws -> widgets_event(Ws, Event, T)
    end.

widgets_event(Ws, Event, T) ->
    {T1,Sigs,Effs,Dirty} =
	lists:foldl(
	  fun({Name,W,XY}, {Ti,Si,Ei,Di}) ->
		  {W1,Es} = epxy_widget:event(Event, W, XY),
		  Si1 = signals(Name, Es, Si),
		  Ei1 = effects(Es, Ei),
		  if W1 =:= W ->
			  {Ti,Si1,Ei1,Di};
		     true ->
			  {Ti1,Si2} = update_key_focus(Name, W, W1, XY, Ti, Si1),
			  {Ti2,Si3} = update_active(Name, W, W1, XY, Ti1, Si2),
			  {Ti3,Di1} = changed(Name, W, W1, Ti2, Di),
			  %% focus moved, the old focus widget is redrawn
			  Di2 = case maps:get(focus,Ti) =:= maps:get(focus,Ti3) of
				    true -> Di1;
				    false -> all
				end,
			  {Ti3,Si3,Ei1,Di2}
		  end
	  end, {T,[],[],[]}, Ws),
    {T1, lists:reverse(Sigs), lists:reverse(Effs), Dirty, true}.

%% store a changed widget and add the area to redraw
changed(_Name, W, W, T, Dirty) ->
    {T, Dirty};
changed(Name, W0, W1, T, Dirty) ->
    T1 = store(Name, W1, T),
    {T1, merge_dirty(Dirty, dirty_rects(Name, [W0,W1], T1))}.

signals(Name, Es, Sigs) ->
    lists:foldl(fun({signal,Env}, Acc) -> [{Name,Env}|Acc];
		   (_, Acc) -> Acc
		end, Sigs, Es).

effects(Es, Effs) ->
    lists:foldl(fun({signal,_}, Acc) -> Acc;
		   (E, Acc) -> [E|Acc]
		end, Effs, Es).

update_active(Name, #{ state := S0, focus := F0 }, #{ state := S1, focus := F1 },
	      XY, T = #{ active := Active }, Sigs) ->
    if (S0 =:= normal andalso S1 =:= active) orelse
       ((not F0) andalso F1) ->
	    case lists:keymember(Name, 1, Active) of
		false -> {T#{ active => [{Name,XY}|Active] }, Sigs};
		true -> {T, Sigs}
	    end;
       true ->
	    {T, Sigs}
    end.

%% W0 and W1 is before and after an update, check focus
update_key_focus(Name, #{ focus := F0 }, #{ focus := F1 }, XY,
		 T = #{ focus := Focus }, Sigs) ->
    if (not F0) andalso F1 -> %% got focus
	    case Focus of
		[{Old,_}|_] when Old =/= Name ->
		    T1 = case find(Old, T) of
			     {ok,Wf} -> store(Old, Wf#{ focus => false }, T);
			     error -> T
			 end,
		    {T1#{ focus => [{Name,XY}] },
		     [{Name,#{event=>focus_in,xy=>XY}},
		      {Old,#{event=>focus_out,xy=>XY}} | Sigs]};
		_ ->
		    {T#{ focus => [{Name,XY}] },
		     [{Name,#{event=>focus_in,xy=>XY}} | Sigs]}
	    end;
       F0 andalso (not F1) -> %% lost focus
	    {T#{ focus => lists:keydelete(Name, 1, Focus) },
	     [{Name,#{event=>focus_out,xy=>XY}} | Sigs]};
       true ->
	    {T, Sigs}
    end.
