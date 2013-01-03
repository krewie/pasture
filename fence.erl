-module(fence).
-extends(object).
-export([loop/0]).

loop() ->
	receive 
		{PID, _} ->
			PID ! {self(), ?MODULE, coordinate},
			loop()
		end.