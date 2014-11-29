-module(bran_lib_pervasives).

-export([print_string/1]).

print_string(S) -> io:fwrite(S).
