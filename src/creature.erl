-module(creature).
-extends(object).
-define(HUNGRY, 4).
-export([move/4, reproduce/2, eat/4, find_way/3, calc_distance/3, qsort/1]).

%Sorterar en lista med element av följande struktur :
% {{X,Y}, Distance} i fallande ordning.

qsort([]) -> [];
qsort([ {Coor,D} | T]) -> 
   qsort([ {_Coor,D1} || {_Coor,D1} <- T, D1 > D ]) ++
         [{Coor, D}] ++
         qsort([ {_Coor,D1} || {_Coor,D1} <- T, D1 =< D ]).

% Returnerar en lista med koordinater till tomma platser 
% och distansen från den tomma platsen till fienderna
% i närheten. Givet en lista med tomma platser,en lista med fiender och 
% en tom initieringslista, detta är listan som kommer
% innehålla resultaten som används när find_way påkallas
% med ny fiende från fiende listan.

%Paths och fiende listan är strukturerad på följande sätt: 
%[ {{X,Y}, [{{X,Y}, Module, PID}]} ... ]

calc_distance(_Paths, [], Result) -> Result;
calc_distance(Paths, [EH|ET], Result) ->
    calc_distance(Paths, ET, find_way(Paths,EH,Result)).

% Hjälpfunktion till calc_distance.

find_way([],_Enemy,_Res) -> [];
find_way([PH|PT],Enemy,[]) ->
    {{X1,Y1}, _} = PH,
    {{X2,Y2}, _} = Enemy,
    Distance = erlang:round(math:sqrt(math:pow((X2 - X1),2)
                                      + math:pow((Y2 - Y1),2))),
    [{{X1,Y1},Distance} | find_way(PT, Enemy, [])];
find_way([PH|PT],Enemy,[HR|TR]) ->
    {{X1,Y1}, _} = PH,
    {{X2,Y2}, _} = Enemy,
    Distance = erlang:round(math:sqrt(math:pow((X2 - X1),2)
                                      + math:pow((Y2 - Y1),2))),
    {{_X,_Y}, Old_Dis} = HR,
    case Old_Dis > Distance of
        true -> [{{X1,Y1},Distance} | find_way(PT, Enemy, TR)];
        false -> [{{X1,Y1},Old_Dis} | find_way(PT, Enemy, TR)]  
    end.

% Låter modulen / djuret göra nödvändiga drag beroende på
% situation.

%choice(State,Module,Cell,Food,Enemies) ->
%    {Coordinate, Sight, Speed, Hunger, Age, Repro} = State,
%    Neighbors = creature:get_neighbours(Coordinate, Sight),
%    Empty = creature:get_all_empty(Neighbors),
%    Enemy_present = creature:get_of_types(Neighbors, Enemies),
%    Food_present = creature:get_of_types(Neighbors, Food),
%    case Hunger =< ?HUNGRY of 
%        true -> 
%            Random_food = object:get_random(Food_present),
%            case Random_food /= [] of
%                true ->
%                    {{X,Y},[{{_X,_Y}, _Module, PID}]} = Random_food,
%                    eat({X,Y}, PID)
%            end
%    end,
%    case lists:length(Enemy_present) /= [] of
%        true -> 
%            Distance = creature:calc_distance(Empty, Enemy_present, []),
%            case Distance /= [] of                                 
%                true ->
%                    [{Coor, _Distance}|T] = creature:qsort(Distance)
%            end
%    end.

    % What to do when there are no food / not time to eat OR
    % when there are no enemies to evade ?.

% tries to move. returns new coordinate upon success.

move(Coordinate, [], _Module, _Color) -> Coordinate;
move(Coordinate, [NewCoordinate|T], Module, Color) ->
    simulator ! {move, self(), Module, Coordinate, NewCoordinate, Color},
    receive
        {move_ok} ->
            NewCoordinate;
        {move_error} ->
            move(Coordinate, T, Module, Color)
    end.

eat(_Coordinate, [], _Module, _Color) -> fail;
eat(Coordinate, [{NewCoordinate, [{NewCoordinate, _, PID}]}|T], Module, Color) ->
    PID ! {get_eaten, self(), NewCoordinate},
    receive
        {eat_ok} -> NewCoordinate;
        {eat_error} ->
            eat(Coordinate, T, Module, Color)
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
