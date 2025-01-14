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
%%
%% EPX 2D Transform
%%
-ifndef(__EPX_T2D_HRL__).
-define(__EPX_T2D_HRL__, true).

-record(t2d,
	{
	 sx=1.0, ry=0.0, tx=0.0,
	 rx=0.0, sy=1.0, ty=0.0
	 %% 0.0     0.0     1.0
	}).

-define(is_t2d(T), is_record(td2, T)).

-endif.
