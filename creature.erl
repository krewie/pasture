-module(creature).
-extends(object).
-export([print_name/0]).

print_name() ->
    io:format("~p~n", ?MODULE).
