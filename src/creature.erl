-module(creature).
-extends(object).
-define(THRESHOLD, 10).
-export([move/4, reproduce/2, eat/4, find_way/3, calc_distance/3, qsort/1, calc_fitness/3, choice/4]).

%Sorterar en lista med element av följande struktur :
% {{X,Y}, Distance} i fallande ordning.

qsort([]) -> [];
qsort([ {Coor,D} | T]) -> 
   qsort([ {_Coor,D1} || {_Coor,D1} <- T, D1 > D ]) ++
         [Coor] ++
         qsort([ {_Coor,D1} || {_Coor,D1} <- T, D1 =< D ]).

% Returnerar en lista med koordinater till tomma platser 
% och distansen från den tomma platsen till fienderna
% i närheten. Givet en lista med tomma platser,en lista med fiender och 
% en tom initieringslista, detta är listan som kommer
% innehålla resultaten som används när find_way påkallas
% med ny fiende från fiende listan.

%Paths och fiende listan är strukturerad på följande sätt: 
%[ {{X,Y}, [{{X,Y}, Module, PID}]} ... ]

calc_distance(Paths, [], []) -> [{{X, Y}, random:uniform(20)} || {X, Y} <- Paths];
calc_distance(_Paths, [], Result) -> Result;
calc_distance(Paths, [EH|ET], Result) ->
    calc_distance(Paths, ET, find_way(Paths,EH,Result)).

% Hjälpfunktion till calc_distance.

find_way([],_Enemy,_Res) -> [];
find_way([PH|PT],Enemy,[]) ->
    {X1,Y1} = PH,
    {{X2,Y2}, _} = Enemy,
    %Xres = abs(X1-X2),
    %Yres = abs(Y1-Y2),
    %Distance = max(Xres, Yres),
    Distance = erlang:round(math:sqrt(math:pow((X2 - X1),2) + math:pow((Y2 - Y1),2))),
    [{{X1,Y1},Distance} | find_way(PT, Enemy, [])];
find_way([PH|PT],Enemy,[HR|TR]) ->
    {X1,Y1} = PH,
    {{X2,Y2}, _} = Enemy,
    %Xres = abs(X1-X2),
    %Yres = abs(Y1-Y2),
    %Distance = max(Xres, Yres),
    Distance = erlang:round(math:sqrt(math:pow((X2 - X1),2) + math:pow((Y2 - Y1),2))),
    {{_X,_Y}, Old_Dis} = HR,
    case Old_Dis > Distance of
        true -> [{{X1,Y1},Distance} | find_way(PT, Enemy, TR)];
        false -> [{{X1,Y1},Old_Dis} | find_way(PT, Enemy, TR)]  
    end.

% Låter modulen / djuret göra nödvändiga drag beroende på
% situation.

calc_fitness([],[],_Starv) -> [];
calc_fitness([FH|FT],[EH|ET],Starv) ->
    {{X1,Y1}, D1} = FH,
    {{_X2,_Y2}, D2} = EH,
    [{{X1,Y1}, D1 * Starv - D2 * Starv} | calc_fitness(FT, ET, Starv) ].

choice(State,Food,Enemies,Starv) ->
    {Coordinate, Sight, _Speed, Hunger, _Age, _Repro} = State,
    Neighbors = creature:get_neighbours(Coordinate),
    View = creature:get_neighbours(Coordinate, Sight),
    Empty = creature:get_all_empty(Neighbors),

   % If 'Empty' = [] then we only need to check for food that is as far away
   % from an enemy as possible, but should there be empty areas we need to
   % decide wether eating is of higher concern that fleeing, this might occur
   % when 'Hunger' is close to ?STARVE (maybe should be passed as parameter).
   % 1. If we decide to eat, then find food that is as close as possible
   % 2. If we decide to flee, then move to the empty space that is as far away
   %    from an enemy as possible
   % My idea is a function that take two lists: *Enemies and *Cells
   % Were we try to find the/those cells in *Cells that is as far away from
   % all the enemies in *Enemies
   % Maybe that is what calc_distance does, but I'm not sure

    Enemy_present = creature:get_of_types(View, Enemies),
    Food_present = creature:get_of_types(View, Food),

    FoodDistanceList = creature:calc_distance(Empty, Food_present, []),
    EnemyDistanceList = creature:calc_distance(Empty, Enemy_present, []),
    
    creature:qsort(creature:calc_fitness(FoodDistanceList, EnemyDistanceList, Starv)).
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
    Neighbors = creature:get_neighbours(Coordinate),
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
