-module(rock).
-extends(object).
-export([init/1]).
-define(CELL, "grey").


init(Coordinate) ->
    loop(Coordinate).

tick(Coordinate) ->
    {X, Y} = Coordinate,
    frame ! {change_cell, X, Y, ?CELL}.

loop(Coordinate) ->
    receive
        {tick} -> tick(Coordinate),
                  loop(Coordinate);
        _ -> tick(Coordinate)
    end.
