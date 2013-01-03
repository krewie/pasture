-module(object).
-export([print_name/0, loop/0]).

print_name() ->
    io:format("~p~n", [?MODULE]).

loop() ->
    receive
	_ ->
	    io:format("I am a ~p with PID: ~p~n", [?MODULE, self()]),
	    loop()
    end.
		      


