-module(creature).
-extends(object).
-export([move/3, reproduce/2]).

move(Coordinate, Object, Color) ->
    Neighbors = creature:get_neighbors(Coordinate),
    Empty = creature:get_all_empty(Neighbors),
    Coor = creature:get_random(Empty),
    case Coor of
        none ->
            Coordinate;
        {X, Y} ->
            simulator ! {move, self(), Object, Coordinate, Coor},
            receive
                {move_ok} ->
                    frame ! {change_cell, X, Y, Color},
                    Coor;
                {move_error} ->
                    move(Coordinate, Object, Color)
            end
    end.


reproduce(Coordinate, Object) ->
    Neighbors = creature:get_neighbors(Coordinate),
    Empty = creature:get_all_empty(Neighbors),
    Coor = creature:get_random(Empty),
    case Coor of
        none -> error;
        {X, Y} ->
            simulator ! {reproduce, self(), Object, Coor},
            receive
                {reproduction_ok} -> ok;
                {reproduction_error} -> reproduce(Coordinate, Object)
            end
    end.
