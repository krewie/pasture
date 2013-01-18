-module(rabbit).
-extends(animal).
-define(CELL, "pink").
-define(STARVE, 80).
-define(REPRO_RATE, 15).
-define(HUNGER, 5).
-define(REPRO_AGE, 10).
-define(SPEED, 3).
-define(FOOD, [grass]).
-define(ENEMIES, [fox]).
-define(SIGHT, 4).
-export([init/1]).

% A thought is that if we initiate them with ?REPRO_RATE then as soon
% as they are in ?REPRO_AGE they are able to breed and then we reset it to 0.
% That way the ?REPRO_RATE will remain intact, becaue now when it reaches
% ?REPRO_AGE then we have that the ?REPRO_RATE is equally large as ?REPRO_AGE
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
