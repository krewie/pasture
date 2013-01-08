-module(fence).
-extends(object).
-export([print_name/0]).

print_name() ->
    io:format("MODULE : ~p~n", [?MODULE]).