-module(rabbit).
-extends(animal).
-define(CELL, "pink").
<<<<<<< HEAD
=======
-define(DEF, "white").
-define(STARVE, 100).
>>>>>>> Lite allt möjligt.
-define(REPRO_RATE, 6).
-define(HUNGER, 5).
-define(REPRO_AGE, 3).
-define(SPEED, 1).
-define(FOOD, [grass]).
-define(ENEMIES, [fox]).
-define(SIGHT, 1).
-define(UPDATE_FRAME(X, Y), frame ! {change_cell, X, Y, ?CELL}).
-export([init/1]).

init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    loop({{X, Y}, 0, 0, 0, 0}).

% THERE ARE SEVERAL STATES A ANIMAL CAN BE IN, THESE ARE:
% 1. dead by starvation
% 2. move/flee, eat, reproduce
% 3. eat, reproduce
% 4. move/flee, eat
% 5. move/flee
% 6. eat
% 7. no actions
% THESE STATES SHOULD BE CHECKED IN THE ABOVE ORDER I BELIEVE
tick({{X, Y}, _Speed, ?STARVE, _Age, _Repro}) ->
    % Unnecessary because is in macro in Simulator and ?DEF is good if only
    % defined at one location so that it doesn't need to be changed on
    % several places in case you do some background switch
%    frame ! {change_cell, X, Y, ?DEF},
    simulator ! {kill, {X, Y}},
    exit(normal);
tick({Coordinate, Speed, Hunger, Age, Repro}) when Hunger > ?HUNGER ->
    %try to eat
    io:format("~p: hungry, trying to eat... ~n", [Coordinate]),
    Neighbours = rabbit:get_neighbours(Coordinate, ?SIGHT),
    Food = rabbit:get_of_types(Neighbours, ?FOOD),
    Eat_Result = rabbit:eat(Coordinate, Food, ?MODULE, ?CELL),
    case Eat_Result of
        fail -> {Coordinate, Speed+1, Hunger+1, Age+1, Repro+1};
        NewCoordinate -> 
            case Repro > ?REPRO_RATE andalso Age > ?REPRO_AGE of
                true ->
                    simulator ! {reproduce_eat, self(), ?MODULE, NewCoordinate},
                    {Coordinate, Speed+1, 0, Age+1, 0};
                _ -> 
                    case Speed > ?SPEED of
                        true -> 
                            simulator ! {move_eat, self(), ?MODULE, Coordinate,
                                         NewCoordinate, ?CELL},
                            {NewCoordinate, 0, 0, Age+1, Repro+1};
                        _ ->
                            simulator ! {kill, NewCoordinate},
                            {Coordinate, Speed+1, 0, Age+1, Repro+1}
                    end
            end
    end;
tick(State) ->
    % har nu bara att jag går random....
    {Coordinate, Speed, Hunger, Age, Repro} = State,
    case Speed > ?SPEED of
        true ->
            Neighbors = rabbit:get_neighbours(Coordinate, 1),
            Empty = creature:get_all_empty(Neighbors),
            Empty_Random = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Empty])],
            Move_Result = rabbit:move(Coordinate, Empty_Random, ?MODULE, ?CELL),
            {Move_Result, 0, Hunger+1, Age+1, Repro+1};
        _ ->
            {Coordinate, Speed+1, Hunger+1, Age+1, Repro+1}
    end.


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
