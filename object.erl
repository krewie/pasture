-module(object).
-export([print_name/0, loop/1]).
-define(CELL, "black").

print_name() ->
    io:format("MODULE : ~p~n", [?MODULE]).

loop(State) ->
    [Type, {X, Y}] = State,
    receive
	_ ->
	    io:format("I am a ~p at X: ~p Y: ~p with PID: ~p~n", [Type, X, Y, self()]),
	    frame ! {change_cell, X, Y, ?CELL},
	    loop(State)
    end.
		      


