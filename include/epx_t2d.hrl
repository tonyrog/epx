%%
%% EPX 2D Transform
%%
-ifndef(__EPX_T2D_HRL__).
-define(__EPX_T2D_HRL__, true).

-record(t2d,
	{
	  sx=1.0, ry=0.0,
	  rx=0.0, sy=1.0,
	  tx=0.0, ty=0.0
	 }).

-define(is_t2d(T), is_record(td2, T)).

-endif.
