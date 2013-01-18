-module(grass).
-extends(object).
-behaviour(plant).
-export([loop/1, init/1]).
-define(REPRO_RATE, 10).
-define(LOOKUP(X, Y), ets:lookup(grid, {X, Y})).
-define(CELL, "green").


init(Coordinate) ->
    {X, Y} = Coordinate,
    frame ! {change_cell, X, Y, ?CELL},
    loop({Coordinate, 0}).


% Changes state. returns new state.
tick({Coordinate, Reproduction}) when Reproduction > ?REPRO_RATE ->
    Neighbors = grass:get_neighbours(Coordinate, 1),
    Empty = grass:get_all_empty(Neighbors),
    %Empty_Random = grass:randomize_list(Empty),
    Repro_Result = reproduce(Empty),
    case Repro_Result of
        ok -> {Coordinate, 0};
        error -> {Coordinate, Reproduction+1}
    end;
tick({Coordinate, Reproduction}) ->
     {Coordinate, Reproduction+1}.
    


%% Tries to reproduce by asking simulator to spawn
%% object at currently empty coordinate.
%% Not sure if we att the current case (grass) need
%% to know the result of the reproduction.
reproduce([]) -> error;
reproduce([Coordinate|T]) ->
    simulator ! {reproduce, self(), ?MODULE, Coordinate},
    receive
        {reproduction_ok} ->
            ok;
        {reproduction_error} ->
            reproduce(T)
    end.

loop(State) ->
    {Coordinate, _Reproduction} = State,
    receive 
        {tick} -> 
            NewState = tick(State),
            loop(NewState);
        {get_eaten, PID, Coordinate} ->
            PID ! {eat_ok}
    end.
