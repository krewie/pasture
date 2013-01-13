-module(rabbit).
-extends(animal).
-define(CELL, "pink").
-define(REPRO_RATE, 6).
-define(HUNGER, 1800).
-define(REPRO_AGE, 3).
-define(SPEED, 2).
-define(FOOD, [grass]).
-define(ENEMIES, [fox]).
-define(REMOVE_SELF, ets:delete(grid, {X, Y})).
-export([init/1]).

init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    % The idea is that a Move-Speed, Hunger, Reproduce-rate ticks down every
    % tick and that age increase every time we move.
    % Coordinate, Move-Speed, Hunger, Age, Reproduce-rate
    loop({X, Y}, 0, 0, 0, 0).

tick({{X, Y}, _Speed, 0, _Age, _Repro}) ->
    frame ! {change_cell, X, Y, ?DEF},
    ?REMOVE_SELF(X, Y),
    exit(normal);
tick(State) ->
    {Coordinate, Speed, Hunger, Age, Repro} = State,

    %% ifsatsen är naiv.... Kristian skriver på en Choicefunktion... :)
    if
        Hunger < ?HUNGER ->
            Neigbours = get_neighbours(Coordinate),
            {{X, Y}, [_, _, PID]} = get_of_types(Neighbours, ?FOOD),
            Eat_Result = eat({X, Y}, PID),
            case Eat_Result of
                eat_error -> tick(State);
                timeout ->
                {X, Y} ->
                    {{X, Y}, Speed, Hunger, Age, Repro}
            end;
            %try to eat;
        Repro < ?REPRO ->
            Repro_Result = reproduce(Coordinate, )
            %try to reproduce
        true ->
            NewCoordinate = move(Coordinate, ?MODULE, Speed),
            %try to move.
    end



loop(State) ->
    {Coordinate, Speed, Hunger, Age, Repro} = State,
    receive
        {tick} ->
            NewState = tick(State),
            loop(NewState);
        {get_eaten, PID, Coordinate} ->
            PID ! {eat_ok},
        {get_eaten, PID, {_X, _Y}} ->
            PID ! {eat_error},
            loop(State)
    end.
