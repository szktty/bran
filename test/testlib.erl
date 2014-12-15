-module(testlib).

-export([eval/1, simple_test/1]).

eval(S)->
    {ok, Toks, _} = erl_scan:string(S),
    {ok, [Exp]} = erl_parse:parse_exprs(Toks),
    {value, Ret, _} = erl_eval:expr(Exp, erl_eval:bindings(erl_eval:new_bindings())),
    Ret.

simple_test(Mod) ->
    Res = Mod:test(ok) =:= eval(Mod:expected_value(ok)),
    io:format("~p", [Res]).
