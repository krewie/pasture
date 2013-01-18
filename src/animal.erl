-module(animal).
-extends(creature).
-export([tick/2]).

% THERE ARE SEVERAL STATES A ANIMAL CAN BE IN, THESE ARE:
% 1. dead by starvation ***DONE
% 2. move/flee, eat, reproduce
% 3. eat, reproduce **DONE
% 4. move/flee, eat
% 5. move/flee
% 6. eat
% 7. no actions
% THESE STATES SHOULD BE CHECKED IN THE ABOVE ORDER I BELIEVE
tick({Coordinate, _, Hunger, _, _},{Starve_C, _, _, _, _, _, _, _, _, _}) 
	when Hunger == Starve_C ->
    simulator ! {kill, Coordinate},
    exit(normal);
tick({Coordinate, Speed, Hunger, Age, Repro},
	{Starve_C, Hunger_C, Sight_C, Speed_C, ReproAge_C, ReproRate_C, Food_C, Enemies_C, Module_C, Color_C}) 
	when Hunger > Hunger_C ->
    
    Neighbours = animal:get_neighbours(Coordinate),
    Food = animal:get_of_types(Neighbours, Food_C),
    Eat_Result = animal:eat(Coordinate, Food, Module_C, Color_C),
    case Eat_Result of
        fail -> 
            %io:format("Could not eat ~n"),
            case Speed > Speed_C of
                true ->
                    Move_List = animal:choice({Coordinate, Sight_C,
                                               Speed, Hunger, Age, Repro},
                                              Food_C, Enemies_C,Starve_C - Hunger),
                    %io:format("trying to move to: ~p ~n", [Move_List]),
                    NewCoordinate = animal:move(
                                      Coordinate, Move_List, Module_C, Color_C),
                    {NewCoordinate, 0, Hunger+1, Age+1, Repro+1};
                _ ->
                    {Coordinate, Speed+1, Hunger+1, Age+1, Repro+1}
            end;
        NewCoordinate -> 
            case Repro > ReproRate_C andalso Age > ReproAge_C of
                true ->
                    simulator ! {reproduce_eat, self(), Module_C, NewCoordinate},
                    case Speed > Speed_C of
                        true ->
                            Move_List = animal:choice({Coordinate, Sight_C,
                                                       Speed, 0, Age, Repro},
                                                      Food_C,Enemies_C,Starve_C - Hunger),
                            MoveCoordinate = animal:move(Coordinate, Move_List,
                                                         Module_C, Color_C),
                            {MoveCoordinate, 0, 0, Age+1, 0};
                        _ -> {Coordinate, Speed+1, 0, Age+1, 0}
                    end;
                _ -> 
                    case Speed > Speed_C of
                        true -> 
                            simulator ! {move_eat, self(), Module_C, Coordinate,
                                         NewCoordinate, Color_C},
                            {NewCoordinate, 0, 0, Age+1, Repro+1};
                        _ ->
                            simulator ! {kill, NewCoordinate},
                            {Coordinate, Speed+1, 0, Age+1, Repro+1}
                    end
            end
    end;
tick(State, Constants) ->
    {Coordinate, Speed, Hunger, Age, Repro} = State,
    {_, _, Sight_C, Speed_C, _, _, _, Enemies_C, Module_C, Color_C} = Constants,
    %Not hungry, will flee
    case Speed > Speed_C of
        true ->
    		Neighbors = creature:get_neighbours(Coordinate),
    		View = creature:get_neighbours(Coordinate, Sight_C),
    		Empty = creature:get_all_empty(Neighbors),
            Enemy_present = creature:get_of_types(View, Enemies_C),
            case Enemy_present of 
            	[] -> 
            		MoveList = animal:randomize_list(Empty);
            	_ -> 
            EnemyDistanceList = creature:calc_distance(Empty, Enemy_present, []),
        	MoveList = animal:qsort(EnemyDistanceList)
        	end,
            Move_Result = animal:move(Coordinate, MoveList, Module_C, Color_C),
            {Move_Result, 0, Hunger+1, Age+1, Repro+1};
        _ ->
            {Coordinate, Speed+1, Hunger+1, Age+1, Repro+1}
    end.