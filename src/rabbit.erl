-module(rabbit).
-extends(animal).
%-behavior(animal).
-define(CELL, "pink").
-define(DEF, "white").
-define(REPRO_RATE, 6).
-define(HUNGER, 10).
-define(REPRO_AGE, 3).
-define(SPEED, 2).
-define(FOOD, [grass]).
-define(ENEMIES, [fox]).
-define(REMOVE_SELF(X, Y), ets:delete(grid, {X, Y})).
-define(UPDATE_FRAME(X, Y), frame ! {change_cell, X, Y, ?CELL}).
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
tick({Coordinate, Speed, Hunger, Age, Repro}) when Hunger < ?HUNGER ->
    %try to eat
    Neighbours = rabbit:get_neighbours(Coordinate, 1),
    Food = rabbit:get_of_types(Neighbours, ?FOOD),
    Eat_Result = rabbit:eat(Coordinate, Food, ?MODULE, ?CELL),
    case Eat_Result of
        fail -> {Coordinate, Speed-1, Hunger-1, Age+1, Repro+1};
        NewCoordinate -> {NewCoordinate, Speed, ?HUNGER+5, Age+1, Repro+1}
    end;
tick(State) ->
    % har nu bara att jag går random....
    {Coordinate, Speed, Hunger, Age, Repro} = State,
    Neighbors = rabbit:get_neighbours(Coordinate, 1),
    Empty = creature:get_all_empty(Neighbors),
    Empty_Random = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Empty])],
    Move_Result = rabbit:move(Coordinate, Empty_Random, ?MODULE, ?CELL),
    {Move_Result, Speed, Hunger-1, Age+1, Repro+1}.



loop(State) ->
    {Coordinate, _Speed, _Hunger, _Age, _Repro} = State,
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
