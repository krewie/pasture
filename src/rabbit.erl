-module(rabbit).
-extends(animal).
-define(CELL, "pink").
-define(REPRO_RATE, 6).
-define(HUNGER, 1800).
-define(REPRO_AGE, 3).
-define(SPEED, 2).
-export([init/1]).

init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    % The idea is that a Move-Speed, Hunger, Reproduce-rate ticks down every
    % tick and that age increase every time we move.
    % Coordinate, Move-Speed, Hunger, Age, Reproduce-rate
    loop({X, Y}, 0, 0, 0, 0).


loop({X, Y}, _, ?HUNGER, _, _) ->
    simulator ! {kill, {X, Y}};
% can eat?, then reproduce and/or move/flee else move/flee
loop(Coordinate, ?SPEED, Hunger, ?REPRO_AGE, ?REPRO_RATE) ->
    rabbit:reproduce(Coordinate, ?MODULE),
    loop(rabbit:move(Coordinate, ?MODULE, ?CELL),
         0, Hunger+1, ?REPRO_AGE, 0);
%% can eat? then reproduce
loop(Coordinate, Speed, Hunger, ?REPRO_AGE, ?REPRO_RATE) ->
    rabbit:reproduce(Coordinate, ?MODULE),
    loop(Coordinate, Speed+1, Hunger+1, ?REPRO_AGE, 0);
%% in age but can't reproduce, just move
loop(Coordinate, ?SPEED, Hunger, ?REPRO_AGE, Rate) ->
    receive
        {tick} ->
            loop(rabbit:move(Coordinate, ?MODULE, ?CELL),
                 0, Hunger+1, ?REPRO_AGE, Rate+1)
    end;
%% in age, but can't do any actions
loop(Coordinate, Speed, Hunger, ?REPRO_AGE, Rate) ->
    receive
        {tick} ->
            loop(Coordinate, Speed+1, Hunger+1, ?REPRO_AGE, Rate+1)
    end;
loop(Coordinate, ?SPEED, Hunger, Age, Rate) ->
    receive
        {tick} ->
            loop(rabbit:move(Coordinate, ?MODULE, ?CELL),
                 0, Hunger+1, Age+1, Rate)
    end;
loop(Coordinate, Speed, Hunger, Age, Rate) ->
    receive
        {tick} ->
            loop(Coordinate, Speed+1, Hunger+1, Age, Rate)
    end.
