-module(fence).
-extends(object).
-export([init/1]).
-define(CELL, "black").


init(Coordinate) ->
	loop(Coordinate).

tick(Coordinate) ->
	{X, Y} = Coordinate,
	io:format("I'm a ~p, updating the coordinate ~p with color ~p~n", [?MODULE, Coordinate, ?CELL]),
	frame ! {change_cell, X, Y, ?CELL}.

loop(Coordinate) ->
	receive
		{tick} -> tick(Coordinate),
		loop(Coordinate);
		_ -> tick(Coordinate)
	end.