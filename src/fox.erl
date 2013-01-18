-module(fox).
-extends(animal).
-define(CELL, "orange").
-define(STARVE, 80).
-define(REPRO_RATE, 10).
-define(HUNGER, 20).
-define(REPRO_AGE, 30).
-define(SPEED, 2).
-define(FOOD, [rabbit]).
-define(ENEMIES, []).
-define(SIGHT, 4).
-export([init/1]).

init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    State = {{X, Y}, 0, 0, 0, ?REPRO_RATE},
    Constants = {?STARVE, ?HUNGER, ?SIGHT, ?SPEED, ?REPRO_AGE, ?REPRO_RATE, ?FOOD, ?ENEMIES, ?MODULE, ?CELL},
    loop(State, Constants).


loop(State, Constants) ->
    {Coordinate, _Speed, _Hunger, _Age, _Repro} = State,
    receive
        {tick} ->
            NewState = animal:tick(State, Constants),
            loop(NewState, Constants);
        {get_eaten, PID, Coordinate} ->
            PID ! {eat_ok};
        {get_eaten, PID, {_X, _Y}} ->
            PID ! {eat_error},
            loop(State, Constants)
    end.