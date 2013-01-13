-module(fox).
-extends(animal).
-define(CELL, "red").
-define(REPRO_RATE, 6).
-define(HUNGER, 18).
-define(REPRO_AGE, 3).
-define(SPEED, 2).
-export([init/1]).

init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    loop({X, Y}, 0, 0, 0, ?REPRO_RATE).


loop({X, Y}, _, ?HUNGER, _, _) ->
    simulator ! {kill, {X, Y}};
loop(_Coordinate, _Speed, _Hunger, _Age, 0) when _Age >= ?REPRO_AGE ->
    ok;
loop(Coordinate, ?SPEED, Hunger, Age, Rate) ->
    receive
        {tick} ->
            loop(fox:move(Coordinate, ?MODULE, ?CELL),
                 0, Hunger+1, Age+1, Rate)
    end;
loop(Coordinate, Speed, Hunger, Age, Rate) ->
    receive
        {tick} ->
            loop(Coordinate, Speed+1, Hunger+1, Age, Rate)
    end.
