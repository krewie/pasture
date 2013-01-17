-module(fox).
-extends(animal).
-define(CELL, "red").
-define(REPRO_RATE, 6).
-define(HUNGER, 18).
-define(REPRO_AGE, 3).
-define(SPEED, 2).
-define(FOOD, [rabbit]).
-define(ENEMIES, []).
%-define(SIGHT, ??).
%-define(AGE, ??).
-define(UPDATE_FRAME(X, Y), frame ! {change_cell, X, Y, ?CELL}).
-export([init/1]).

init({X, Y}) ->
    frame ! {change_cell, X, Y, ?CELL},
    loop({X, Y}, ?SPEED, ?HUNGER+20, 0, 0).


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
% REALIZED NOW THAT THIS SHOULD BE HANDELD IN CHOICE()
tick({Coordinate, Speed, 0, Age, Repro}) ->
    % 1. dead by starvation
    simulator ! {kill, self(), Coordinate},
    {Coordinate, Speed, 0, Age, Repro};
tick({Coordinate, 0, Hunger, ?REPRO_AGE, ?REPRO_RATE})
  when Hunger < ?HUNGER ->
    % 2.1 move/flee, eat, reproduce
    {Coordinate, ?SPEED, ?HUNGER+5, ?REPRO_AGE, 0};
tick({Coordinate, 0, Hunger, ?REPRO_AGE, Repro})
  when Hunger < ?HUNGER ->
    % 2.2 move/flee, eat, reproduce
    {Coordinate, ?SPEED, ?HUNGER+5, ?REPRO_AGE, Repro+1};
tick({Coordinate, 0, Hunger, ?REPRO_AGE, ?REPRO_RATE}) ->
    % 3.1. move/flee, reproduce
    {Coordinate, ?SPEED, Hunger-1, ?REPRO_AGE, ?REPRO_RATE};
tick({Coordinate, 0, Hunger, ?REPRO_AGE, Repro}) ->
    % 3.2. move/flee, reproduce
    {Coordinate, ?SPEED, Hunger-1, ?REPRO_AGE, Repro+1};
tick({Coordinate, Speed, Hunger, ?REPRO_AGE, ?REPRO_RATE})
  when Hunger < ?HUNGER ->
    % 4.1 eat, reproduce (positvie)
    Neighbours = fox:get_neighbours(Coordinate, 1),
    Food = fox:get_of_types(Neighbours, ?FOOD),
    Eat_Result = fox:eat(Coordinate, Food, ?MODULE, ?CELL),
    case Eat_Result of
        fail -> {Coordinate, Speed-1, Hunger-1, ?REPRO_AGE, ?REPRO_RATE};
        NewCoordinate ->
            Repro_Result = fox:reproduce(NewCoordinate, ?MODULE),
            case Repro_Result of
                error ->
                    {NewCoordinate, Speed-1, ?HUNGER+5, ?REPRO_AGE, ?REPRO_RATE};
                ok -> {NewCoordinate, Speed-1, ?HUNGER+5, ?REPRO_AGE, 0}
            end
    end;
tick({Coordinate, Speed, Hunger, ?REPRO_AGE, Repro})
  when Hunger < ?HUNGER ->
    % 4.2 eat, reproduce (negative)
    Neighbours = fox:get_neighbours(Coordinate, 1),
    Food = fox:get_of_types(Neighbours, ?FOOD),
    Eat_Result = fox:eat(Coordinate, Food, ?MODULE, ?CELL),
    case Eat_Result of
        fail -> {Coordinate, Speed-1, Hunger-1, ?REPRO_AGE, Repro+1};
        NewCoordinate -> {NewCoordinate, Speed, ?HUNGER+5, ?REPRO_AGE, Repro+1}
    end;
tick({{X, Y}, 0, Hunger, Age, Repro})
  when Hunger < ?HUNGER ->
    % 5. move/flee, eat
    {{X, Y}, ?SPEED, ?HUNGER+5, Age, Repro};
tick({Coordinate, 0, Hunger, Age, Repro}) ->
    % 6. move/flee
    Neighbours = fox:get_neighbours(Coordinate, 1),
    Empty = fox:get_all_empty(Neighbours),
    Empty_Random = [X || {_, X} <- list:sort(
                                     [ {random:uniform(), N} || N <- Empty ])],
    Move_Result = fox:move(Coordinate, Empty_Random, ?MODULE, ?CELL),
    {Move_Result, Speed, Hunger-1, Age+1, Repro};
tick({Coordinate, Speed, Hunger, Age, Repro})
  when Hunger < ?HUNGER ->
    % 7. eat
    Neighbours = fox:get_neighbours(Coordinate, 1),
    Food = fox:get_of_types(Neighbours, ?FOOD),
    Eat_Result = fox:eat(Coordinate, Food, ?MODULE, ?CELL),
    case Eat_Result of
        fail -> {Coordinate, Speed-1, Hunger-1, Age+1, Repro};
        NewCoordinate -> {NewCoordinate, Speed, ?HUNGER+5, Age+1, Repro}
    end;
tick({Coordinate, Speed, Hunger, Age, Repro})->
    % 8. no actions
    {Coordinate, Speed-1, Hunger-1, Age, Repro}.

%tick({Coordinate, Speed, Hunger, Age, Repro}) when Hunger < ?HUNGER ->
%    Neighbours = fox:get_neighbours(Coordinate, 1),
%    Food = fox:get_of_types(Neighbours, ?FOOD),
%    Eat_Result = fox:eat(Coordinate, Food, ?MODULE, ?CELL),
%    case Eat_Result of
%        fail -> {Coordinate, Speed-1, Hunger-1, Age+1, Repro+1};
%        NewCoordinate -> {NewCoordinate, Speed, ?HUNGER+5, Age+1, Repro+1}
%    end;
%tick(State) ->
%    % har nu bara att jag går random....
%    {Coordinate, Speed, Hunger, Age, Repro} = State,
%    Neighbours = fox:get_neighbours(Coordinate, 1),
%    Empty = fox:get_all_empty(Neighbours),
%    Empty_Random = [X || {_, X} <- list:sort(
%                                     [ {random:uniform(), N} || N <- Empty ])],
%    Move_Result = fox:move(Coordinate, Empty_Random, ?MODULE, ?CELL),
%    {Move_Result, Speed, Hunger-1, Age+1, Repro+1}.



%loop({X, Y}, _, ?HUNGER, _, _) ->
%    simulator ! {kill, {X, Y}};
%loop(_Coordinate, _Speed, _Hunger, _Age, 0) when _Age >= ?REPRO_AGE ->
%    ok;
%loop(Coordinate, ?SPEED, Hunger, Age, Rate) ->
%    receive
%        {tick} ->
            %{{X,Y}, ?SIGHT, ?SPEED, ?HUNGER, ?AGE, ?REPRO_AGE} = State,
            %loop(choice(State,?MODULE,?CELL,[grass],[??]))
%            loop(fox:move(Coordinate, ?MODULE, ?CELL),
%                 0, Hunger+1, Age+1, Rate)
%    end;
loop(State) ->
    {Coordinate, _Speed, _Hunger, _Age, _Repro} = State,
    receive
        {tick} ->
            NewState = tick(State),
            loop(NewState);
        {get_eaten, PID, Coordinate} ->
            PID ! {eat_ok};
        {get_eaten, PID {_X, _Y}} ->
            PID ! {eat_error},
            loop(State)
    end.
