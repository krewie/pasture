-module(fox).
-extends(animal).
-define(CELL, "red").
-define(REPRO_RATE, 6).
-define(HUNGER, 18).
-define(STARVE, 70).
-define(REPRO_AGE, 3).
-define(SPEED, 2).
-define(FOOD, [rabbit]).
-define(ENEMIES, []).
-define(SIGHT, 1).
-define(UPDATE_FRAME(X, Y), frame ! {change_cell, X, Y, ?CELL}).
-export([init/1]).

init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    loop({{X, Y}, 0, 0, 0, ?REPRO_RATE}).


% THERE ARE SEVERAL STATES A ANIMAL CAN BE IN, THESE ARE:
% 1. dead by starvation (DONE)
% 2. move/flee, eat, reproduce?
%    2.1 When in age and ready to reproduce and can eat
%    2.2 When in age and not ready to reproduce, but can eat
% 3. move/flee, reproduce?
%    3.1 When in age and ready to reproduce, but do want to eat
%    3.2 When in age but not ready to reproduce
% 4. eat, reproduce? (DONE)
%    4.1 When in age and ready to reproduce and can eat
%    4.2 When in age and not ready to reproduce, but can eat
% 5. move/flee, eat
% 6. move/flee (DONE)
% 7. eat (DONE)
% 8. no actions (DONE)
% THESE STATES SHOULD BE CHECKED IN THE ABOVE ORDER I BELIEVE
tick({Coordinate, _Speed, ?STARVE, _Age, _Repro}) ->
    simulator ! {kill, Coordinate},
    exit(normal);
tick({Coordinate, Speed, Hunger, Age, Repro}) when Hunger > ?HUNGER ->
    io:format("~p: hungry, trying to eat... ~n", [Coordinate]),
    Neighbours = fox:get_neighbours(Coordinate, ?SIGHT),
    Food = fox:get_of_types(Neighbours, ?FOOD),
    Eat_Result = fox:eat(Coordinate, Food, ?MODULE, ?CELL),
    case Eat_Result of
        fail ->
            case Speed > ?SPEED of
                true ->
                    Empty = fox:get_all_empty(Neighbours),
                    Empty_Random = fox:randomize_list(Empty),
                    % Could handle if should flee or not?
                    Move_Result = fox:move(Coordinate, Empty_Random,
                                           ?MODULE, ?CELL),
                    {Move_Result, 0, Hunger+1, Age+1, Repro+1};
                _ ->
                    {Coordinate, Speed+1, Hunger+1, Age+1, Repro+1}
            end;
        NewCoordinate ->
            case Repro > ?REPRO_RATE andalso Age > ?REPRO_AGE of
                true ->
                    simulator ! {reproduce_eat, self(), ?MODULE, NewCoordinate},
                    case Speed > ?SPEED of
                        true ->
                            TempNeigh = fox:get_neighbours(Coordinate, ?SIGHT),
                            Empty = fox:get_all_empty(TempNeigh),
                            Empty_Random = fox:randomize_list(Empty),
                            Move_Result = fox:move(Coordinate, Empty_Random,
                                                   ?MODULE, ?CELL),
                            {Move_Result, 0, 0, Age+1, 0};
                        _ -> {Coordinate, Speed+1, 0, Age+1, 0}
                    end;
                _ ->
                    case Speed > ?SPEED of
                        true ->
                            simulator ! {move_eat, self(), ?MODULE, Coordinate,
                                         NewCoordinate, ?CELL},
                            {NewCoordinate, 0, 0, Age+1, Repro+1};
                        _ ->
                            simulator ! {eat, self(), ?MODULE, Coordinate,
                                         NewCoordinate, ?CELL},
                            {NewCoordinate, Speed+1, 0, Age+1, Repro+1}
                    end
            end
    end;
tick({Coordinate, Speed, Hunger, Age, Repro}) ->
    case Speed > ?SPEED of
        true ->
            Neighbours = fox:get_neighbours(Coordinate, ?SIGHT),
            Empty = fox:get_all_empty(Neighbours),
            Empty_Random = fox:randomize_list(Empty),
            Move_Result = fox:move(Coordinate, Empty_Random, ?MODULE, ?CELL),
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
