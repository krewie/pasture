-module(creature).
-extends(object).
-define(HUNGRY, 4).
-export([move/3, reproduce/2, eat/2, find_way/3, calc_distance/3, qsort/1]).

qsort([]) -> [];
qsort([ {Coor,D} | T]) -> 
   qsort([ {Coor,D1} || {Coor,D1} <- T, D1 > D ]) ++ [{Coor, D}] ++ qsort([ {Coor,D1} || {Coor,D1} <- T, D1 =< D ]).

calc_distance(_Paths, [], Result) -> Result;
calc_distance(Paths, [EH|ET], Result) ->
    calc_distance(Paths, ET, find_way(Paths,EH,Result)).

find_way([],_Enemy,_Res) -> [];
find_way([PH|PT],Enemy,[]) ->
    {{X1,Y1}, _} = PH,
    {{X2,Y2}, _} = Enemy,
    Distance = erlang:round(math:sqrt(math:pow((X2 - X1),2) + math:pow((Y2 - Y1),2))),
    [{{X1,Y1},Distance} | find_way(PT, Enemy, [])];
find_way([PH|PT],Enemy,[HR|TR]) ->
    {{X1,Y1}, _} = PH,
    {{X2,Y2}, _} = Enemy,
    Distance = erlang:round(math:sqrt(math:pow((X2 - X1),2) + math:pow((Y2 - Y1),2))),
    {{_X,_Y}, Old_Dis} = HR,
    case Old_Dis > Distance of
        true -> [{{X1,Y1},Distance} | find_way(PT, Enemy, TR)];
        false -> [{{X1,Y1},Old_Dis} | find_way(PT, Enemy, TR)]  
    end. 

%choice(State,Module, Food, Enemies) ->
%{Coordinate, Sight, Speed, Hunger, Age, Repro} = State,
%Neighbors = creature:get_neighbours(Coordinate, Sight),
%Empty = creature:get_all_empty(Neighbors),
%Enemy_present = creature:get_of_types(Neighbors, Enemies),
%Food_present = creature:get_of_types(Neighbors, Food).


move(Coordinate, _Object, 0) -> Coordinate;
move(Coordinate, Object, Speed) ->
    NewCoordinate = move(Coordinate, Object),
    move(NewCoordinate, Object, Speed-1).
move(Coordinate, Object) ->
    Neighbors = creature:get_neighbors(Coordinate),
    Empty = creature:get_all_empty(Neighbors),
    Coor = creature:get_random(Empty),
    case Coor of
        none ->
            Coordinate;
        {_X, _Y} ->
            simulator ! {move, self(), Object, Coordinate, Coor},
            receive
                {move_ok} ->
                    Coor;
                {move_error} ->
                    move(Coordinate, Object)
            end
    end.


eat(Coordinate, PID) ->
    PID ! {get_eaten, self(), Coordinate},
    receive
        {eat_ok} ->
            Coordinate;
        {eat_error} ->
            eat_error
    after 500 ->
        timeout
    end.

reproduce(Coordinate, Object) ->
    Neighbors = creature:get_neighbors(Coordinate),
    Empty = creature:get_all_empty(Neighbors),
    Coor = creature:get_random(Empty),
    case Coor of
        none -> error;
        {_X, _Y} ->
            simulator ! {reproduce, self(), Object, Coor},
            receive
                {reproduction_ok} -> ok;
                {reproduction_error} -> reproduce(Coordinate, Object)
            end
    end.
