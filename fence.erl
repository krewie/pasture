-module(fence).
-extend(object).
-export([init/1]).
-define(CELL, "black").


init(Coordinate) ->
	loop(Coordinate).

tick(Coordinate) ->
	{X, Y} = Coordinate,
	frame ! {change_cell, X, Y, ?CELL}.

loop(Coordinate) ->
	receive
		{tick} -> tick(Coordinate),
		loop(Coordinate);
		_ -> loop(Coordinate)
	end.