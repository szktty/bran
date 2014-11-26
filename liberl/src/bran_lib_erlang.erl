-module(bran_lib_erlang).

-export([eval/1]).

eval(S)->
    {ok, Toks, _} = erl_scan:string(S),
    {ok, [Exp]} = erl_parse:parse_exprs(Toks),
    {value, Ret, _} = erl_eval:expr(Exp, erl_eval:bindings(erl_eval:new_bindings())),
    hd(io_lib:fwrite("~p", [Ret])).
