-module(fence).
-extends(object).
-export([loop/0]).

loop() ->
	receive 
		_ ->
			io:format("I am a fence with PID : ~p~n", [self()]),
			loop()
		end.