-module(rabbit).
-extends(animal).
-define(CELL, "pink").
-define(DEF, "white").
-define(REPRO_RATE, 6).
-define(HUNGER, 10).
-define(REPRO_AGE, 3).
-define(SPEED, 2).
-define(FOOD, [grass]).
-define(ENEMIES, [fox]).
-define(REMOVE_SELF(X, Y), ets:delete(grid, {X, Y})).
-export([init/1]).

init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    % The idea is that a Move-Speed, Hunger, Reproduce-rate ticks down every
    % tick and that age increase every time we move.
    % Coordinate, Move-Speed, Hunger, Age, Reproduce-rate
    loop({{X, Y}, ?SPEED, ?HUNGER+20, 0, 0}).

tick({{X, Y}, _Speed, 0, _Age, _Repro}) ->
    frame ! {change_cell, X, Y, ?DEF},
    ?REMOVE_SELF(X, Y),
    exit(normal);
tick(State) ->
    {Coordinate, Speed, Hunger, Age, Repro} = State,
    %% ifsatsen är naiv.... Kristian skriver på en Choicefunktion... :)
    if
        Hunger < ?HUNGER ->
            Neighbours = rabbit:get_neighbors(Coordinate),
            Eatables = rabbit:get_of_types(Neighbours, ?FOOD),
            io:format("Eatables: ~p ~n", [Eatables]),
            {{X,Y},[{{X,Y}, _, PID}]} = rabbit:get_random(Eatables),
            Eat_Result = rabbit:eat({X, Y}, PID),
            case Eat_Result of
                eat_error -> 
                    {NewX, NewY} = rabbit:move(Coordinate, ?MODULE, Speed),
                    frame ! {change_cell, NewX, NewY, ?CELL},
                    {{NewX, NewY}, Speed, Hunger-1, Age+1, Repro+1};
                timeout ->
                    {NewX, NewY} = rabbit:move(Coordinate, ?MODULE, Speed),
                    frame ! {change_cell, NewX, NewY, ?CELL},
                    {{NewX, NewY}, Speed, Hunger-1, Age+1, Repro+1};
                {X, Y} ->
                    frame ! {change_cell, X, Y, ?CELL},
                    {{X, Y}, Speed, ?HUNGER+5, Age+1, Repro+1}
            end;
            %try to eat;
        Repro > ?REPRO_RATE ->
            Repro_Result = rabbit:reproduce(Coordinate, ?MODULE),
            case Repro_Result of
                error ->
                    {Coordinate, Speed, Hunger-1, Age+1, Repro+1};
                ok ->
                    {Coordinate, Speed, Hunger-1, Age+1, 0}
            end;           
            %try to reproduce
        true ->
            {NewX, NewY} = rabbit:move(Coordinate, ?MODULE, Speed),
            frame ! {change_cell, NewX, NewY, ?CELL},
            {{NewX, NewY}, Speed, Hunger-1, Age+1, Repro+1}
            %try to move.
    end.



loop(State) ->
    {Coordinate, Speed, Hunger, Age, Repro} = State,
    receive
        {tick} ->
            NewState = tick(State),
            loop(NewState);
        {get_eaten, PID, Coordinate} ->
            PID ! {eat_ok};
        {get_eaten, PID, {_X, _Y}} ->
            PID ! {eat_error},
            loop(State)
    end.
