-module(testlib).

-export([eval/1, type/1, is_char/1, simple_test/1]).

eval(S)->
    {ok, Toks, _} = erl_scan:string(S),
    {ok, [Exp]} = erl_parse:parse_exprs(Toks),
    {value, Ret, _} = erl_eval:expr(Exp, erl_eval:bindings(erl_eval:new_bindings())),
    Ret.

is_char(C) when is_integer(C) -> 0 =< C andalso C =< 255;
is_char(_) -> false.

type(T) when is_boolean(T) -> boolean;
type(T) when is_atom(T) -> atom;
type(T) when is_binary(T) -> binary;
type(T) when is_bitstring(T) -> bitstring;
type(T) when is_float(T) -> float;
type(T) when is_integer(T) ->
    case is_char(T) of
        true -> char;
        false -> integer
    end;
type(T) when is_map(T) -> map;
type(T) when is_pid(T) -> pid;
type(T) when is_port(T) -> port;
type(T) when is_tuple(T) -> tuple;
type(T) when is_list(T) ->
    case lists:all(fun is_char/1, T) of
      true -> string;
      false -> list
    end;
type(_) -> error.

simple_test(Mod) ->
    Res = type(Mod:test(ok)) =:= Mod:expected_type(ok) andalso
        Mod:test(ok) =:= eval(Mod:expected_value(ok)),
    io:format("~p", [Res]).
