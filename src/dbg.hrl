-ifndef(EPX_DEBG_HRL).
-define(EPD_DEBG_HRL, true).

-ifdef(debug).
-define(dbg(Fmt,Args), io:format((Fmt),(Args))).
-else.
-define(dbg(Fmt,Args), ok).
-endif.

-endif.
