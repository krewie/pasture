-module(rabbit).
-extends(animal).
-define(CELL, "pink").
-define(REPRO_RATE, 6).
-define(HUNGER, 10).
-define(REPRO_AGE, 3).
-define(SPEED, 2).
-define(FOOD, [grass]).
-define(ENEMIES, [fox]).
%-define(SIGHT, ??).
%-define(AGE, ??).
-define(UPDATE_FRAME(X, Y), frame ! {change_cell, X, Y, ?CELL}).
-export([init/1]).

init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    loop({{X, Y}, ?SPEED, ?HUNGER+20, 0, 0}).


% THERE ARE SEVERAL STATES A ANIMAL CAN BE IN, THESE ARE:
% 1. dead by starvation
% 2. move/flee, eat, reproduce
% 3. eat, reproduce
% 4. move/flee, eat
% 5. move/flee
% 6. eat
% 7. no actions
% THESE STATES SHOULD BE CHECKED IN THE ABOVE ORDER I BELIEVE
tick({{X, Y}, Speed, 0, Age, Repro}) ->
    simulator ! {kill, self(), {X, Y}},
    {{X,Y}, Speed, 0, Age, Repro};
tick({Coordinate, Speed, Hunger, Age, Repro}) when Hunger < ?HUNGER ->
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
    Neighbours = rabbit:get_neighbours(Coordinate, 1),
    Empty = creature:get_all_empty(Neighbours),
    Empty_Random = [X || {_,X} <- lists:sort(
                                    [ {random:uniform(), N} || N <- Empty])],
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
