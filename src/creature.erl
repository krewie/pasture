-module(creature).
-extends(object).
-export([move/3, reproduce/2, eat/2]).


% tries to move. returns new coordinate upon success.
move(Coordinate, _Object, 0) -> Coordinate;
move(Coordinate, Object, Speed) ->
    NewCoordinate = move(Coordinate, Object),
    move(NewCoordinate, Object, Speed-1).
move(Coordinate, Object) ->
    Neighbors = creature:get_neighbors(Coordinate),
    Empty = creature:get_all_empty(Neighbors),
    Coor = creature:get_random(Empty),
    case Coor of
        none ->
            Coordinate;
        {_X, _Y} ->
            simulator ! {move, self(), Object, Coordinate, Coor},
            receive
                {move_ok} ->
                    Coor;
                {move_error} ->
                    move(Coordinate, Object)
            end
    end.


eat(Coordinate, PID) ->
    PID ! {get_eaten, self(), Coordinate},
    receive
        {eat_ok} ->
            Coordinate;
        {eat_error} ->
            eat_error
    after 500 ->
        timeout
    end.

reproduce(Coordinate, Object) ->
    Neighbors = creature:get_neighbors(Coordinate),
    Empty = creature:get_all_empty(Neighbors),
    Coor = creature:get_random(Empty),
    case Coor of
        none -> error;
        {_X, _Y} ->
            simulator ! {reproduce, self(), Object, Coor},
            receive
                {reproduction_ok} -> ok;
                {reproduction_error} -> reproduce(Coordinate, Object)
            end
    end.
