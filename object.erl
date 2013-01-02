-module(object).

-exports([get_coordinate/0, print_name/0]).




print_name() ->
    io:format("~p~n", ?MODULE).

