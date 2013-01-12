-module(rabbit).
-extends(animal).
-define(CELL, "yellow").
-define(DEF, "white").
-define(REPRO_RATE, 6).
-define(HUNGER, 3).
-define(REPRO_AGE, 3).
-define(SPEED, 3).
-export([init/1]).

init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    % The idea is that a Move-Speed, Hunger, Reproduce-rate ticks down every
    % tick and that age increase every time we move.
    % Coordinate, Move-Speed, Hunger, Age, Reproduce-rate
    loop({X, Y}, 0, 0, 0, ?REPRO_RATE).


move(Coordinate) ->
    {OldX, OldY} = Coordinate,
    Neighbors = rabbit:get_neighbors(Coordinate),
    Empty = rabbit:get_all_empty(Neighbors),
    Coor = rabbit:get_random(Empty),
    case Coor of
        none -> Coordinate;
        {X, Y} ->
            simulator ! {move, self(), Coor},
            receive
                {move_ok} ->
                    frame ! {change_cell, OldX, OldY, ?DEF},
                    frame ! {change_cell, X, Y, ?CELL},
                    Coor;
                {move_error} -> move(Coordinate)
            end
    end.


loop(_, _, ?HUNGER, _, _) -> dead;
loop(_Coordinate, _Speed, _Hunger, _Age, 0) when _Age >= ?REPRO_AGE ->
    %can eat? then reproduce and/or move/flee else move/flee
    ok;
loop(Coordinate, ?SPEED, Hunger, Age, Rate) ->
    receive
        {tick} ->
            loop(move(Coordinate), 0, Hunger, Age+1, Rate)
    end;
loop(Coordinate, Speed, Hunger, Age, Rate) ->
    receive
        {tick} ->
            loop(Coordinate, Speed+1, Hunger, Age, Rate)
    end.

                                               

    
