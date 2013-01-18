-module(rabbit).
-extends(animal).
-define(CELL, "grey").
-define(STARVE, 100).
-define(REPRO_RATE, 6).
-define(HUNGER, 5).
-define(REPRO_AGE, 3).
-define(SPEED, 3).
-define(FOOD, [grass]).
-define(ENEMIES, [fox]).
-define(SIGHT, 2).
-export([init/1]).

% A thought is that if we initiate them with ?REPRO_RATE then as soon
% as they are in ?REPRO_AGE they are able to breed and then we reset it to 0.
% That way the ?REPRO_RATE will remain intact, becaue now when it reaches
% ?REPRO_AGE then we have that the ?REPRO_RATE is equally large as ?REPRO_AGE
init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    loop({{X, Y}, 0, 0, 0, ?REPRO_RATE}).

% THERE ARE SEVERAL STATES A ANIMAL CAN BE IN, THESE ARE:
% 1. dead by starvation ***DONE
% 2. move/flee, eat, reproduce
% 3. eat, reproduce **DONE
% 4. move/flee, eat
% 5. move/flee
% 6. eat
% 7. no actions
% THESE STATES SHOULD BE CHECKED IN THE ABOVE ORDER I BELIEVE
tick({{X, Y}, _Speed, ?STARVE, _Age, _Repro}) ->
    simulator ! {kill, {X, Y}},
    exit(normal);
tick({Coordinate, Speed, Hunger, Age, Repro}) when Hunger > ?HUNGER ->
    %try to eat
    Neighbours = rabbit:get_neighbours(Coordinate),
    Food = rabbit:get_of_types(Neighbours, ?FOOD),
    Eat_Result = rabbit:eat(Coordinate, Food, ?MODULE, ?CELL),
    case Eat_Result of
        fail -> 
            %io:format("Could not eat ~n"),
            case Speed > ?SPEED of
                true ->
                    Move_List = rabbit:choice({Coordinate, ?SIGHT,
                                               Speed, Hunger, Age, Repro},
                                              ?FOOD, ?ENEMIES,?STARVE - Hunger),
                    %io:format("trying to move to: ~p ~n", [Move_List]),
                    NewCoordinate = rabbit:move(
                                      Coordinate, Move_List, ?MODULE, ?CELL),
                    {NewCoordinate, 0, Hunger+1, Age+1, Repro+1};
                _ ->
                    {Coordinate, Speed+1, Hunger+1, Age+1, Repro+1}
            end;
        NewCoordinate -> 
            case Repro > ?REPRO_RATE andalso Age > ?REPRO_AGE of
                true ->
                    simulator ! {reproduce_eat, self(), ?MODULE, NewCoordinate},
                    case Speed > ?SPEED of
                        true ->
                            Move_List = rabbit:choice({Coordinate, ?SIGHT,
                                                       Speed, 0, Age, Repro},
                                                      ?FOOD,?ENEMIES,?STARVE - Hunger),
                            MoveCoordinate = rabbit:move(Coordinate, Move_List,
                                                         ?MODULE, ?CELL),
                            {MoveCoordinate, 0, 0, Age+1, 0};
                        _ -> {Coordinate, Speed+1, 0, Age+1, 0}
                    end;
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
            Empty = rabbit:get_all_empty(Neighbors),
            Empty_Random = rabbit:randomize_list(Empty),
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
