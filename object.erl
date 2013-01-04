-module(object).
-export([print_name/0, loop/1]).

print_name() ->
    io:format("~p~n", [?MODULE]).

loop(State) ->
    [Type, {X, Y}] = State,
    receive
	_ ->
	    io:format("I am a ~p at X: ~p Y: ~p with PID: ~p~n", [Type, self(), X, Y]),
	    frame ! {change_cell, X, Y, "red"},
	    loop(State)
    end.
		      


