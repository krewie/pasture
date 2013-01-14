-module(grass).
-extends(object).
-behaviour(plant).
-export([loop/1, init/1]).
-define(REPRO_RATE, 5).
-define(LOOKUP(X, Y), ets:lookup(grid, {X, Y})).
-define(CELL, "green").


init(Coordinate) ->
    {X, Y} = Coordinate,
    frame ! {change_cell, X, Y, ?CELL},
    loop({Coordinate, ?REPRO_RATE}).


% Changes state. returns new state.
tick({{X, Y}, 0}) ->
    Repro_Result = reproduce({X, Y}),
    case Repro_Result of
        ok -> {{X, Y}, ?REPRO_RATE};
        error -> {{X, Y}, 0}
    end;
tick({{X, Y}, Reproduction}) ->
     {{X, Y}, Reproduction-1}.
    


%% Tries to reproduce by asking simulator to spawn
%% object at currently empty coordinate.
%% Not sure if we att the current case (grass) need
%% to know the result of the reproduction.
reproduce(Coordinate) ->
    Neighbors = grass:get_neighbors(Coordinate),
    Empty = grass:get_all_empty(Neighbors),
    Rep_Coor = grass:get_random(Empty),
    case Rep_Coor of 
        none -> 
            error;
        {X, Y} -> 
            simulator ! {reproduce, self(), ?MODULE, {X, Y}}
    end,
    receive
        {reproduction_ok} ->
            ok;
        {reproduction_error} ->
            reproduce(Coordinate)
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
