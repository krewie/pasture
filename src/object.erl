-module(object).
-export([get_neighbours/1, get_neighbours/2, get_types/2, get_of_types/2,
         get_occupied/1, get_first_empty/1, get_all_empty/1, get_random/1,
        randomize_list/1]).
-define(LOOKUP(X, Y), ets:lookup(grid, {X, Y})).

% Returns neighboors that are not empty, occupied by some
% module.
get_occupied(Neighbors) ->
    lists:filter(fun({{_X,_Y},Object}) -> Object /= [] end, Neighbors).

% Retrieves a list of neigbours that has module name in the list [H|T].
% returns it as a list of tuples, structured as
% {{X-coordinate, Y-coordinate}, Object}


get_of_types(Neighbors, Modlist) -> get_types(get_occupied(Neighbors), Modlist).

get_types(_Neighbors, []) -> [];
get_types(Neighbors, [H|T]) ->
    lists:append(
        lists:filter(fun({{_X,_Y},[{{_X,_Y}, Module, _PID}]}) -> Module == H end, Neighbors),
    get_types(Neighbors, T)).


% retreive the adjecent areas of X, Y.
% returns it as a list of tuples, structured as
% {{X-coordinate, Y-coordinate}, Object}
get_neighbours({X, Y}) ->
	[{{X-1, Y-1}, ?LOOKUP(X-1, Y-1)}, {{X, Y-1}, ?LOOKUP(X, Y-1)},
         {{X+1, Y-1}, ?LOOKUP(X+1, Y-1)}, {{X-1, Y},?LOOKUP(X-1, Y)},
         {{X+1, Y}, ?LOOKUP(X+1, Y)}, {{X-1, Y+1}, ?LOOKUP(X-1, Y+1)},
         {{X, Y+1}, ?LOOKUP(X, Y+1)}, {{X+1, Y+1}, ?LOOKUP(X+1, Y+1)}].


% creates an list of tuples from cell {X, Y} to {XSight, Y}
% structured as {Coordinates, Object}
% Gabriel
collect_row(X, Y, XSight) when X == XSight ->
    [{{X, Y}, ?LOOKUP(X, Y)}];
collect_row(X, Y, XSight) ->
    [{{X, Y}, ?LOOKUP(X, Y)} | collect_row(X+1, Y, XSight)].

% creates an list of tuples from cell {X, Y} to {XSight, YSight}
% collecting row by row structured as {Coordinates, Object}.
% Gabriel
collect_columns(X, Y, XSight, YSight) when Y == YSight ->
    collect_row(X, Y, XSight);
collect_columns(X, Y, XSight, YSight) ->
    lists:append(collect_row(X, Y, XSight),
                 collect_columns(X, Y+1, XSight, YSight)).

% As get_neighbors/1 but will retreive all adjacent cells at most 'Sight' away
% from the coordinate of the given cell.
% returns a list of tuples, structured as {Coordinates, Object}
% Gabriel
get_neighbours(_, 0) -> [];
get_neighbours({X, Y}, Sight) ->
    lists:delete({{X, Y}, ?LOOKUP(X, Y)},
                 collect_columns(X-Sight, Y-Sight, X+Sight, Y+Sight)).

    
% retreives the the first coordinate with no object from
% a list of Coordinate-Object-Tuples
% if all coordinates in list are occupied, return none.
get_first_empty([])-> none;
get_first_empty([{Coordinate, []}|_T]) ->
	Coordinate;
get_first_empty([{_Coordinate, _}|T]) ->
	get_first_empty(T).


get_all_empty(List) -> 
    get_all_empty(List, []).
get_all_empty([], Acc) -> Acc;
get_all_empty([{Coordinate, []}|T], Acc) ->
    get_all_empty(T, [Coordinate|Acc]);
get_all_empty([{_Coordinate, _}|T], Acc) ->
    get_all_empty(T, Acc).

get_random([]) -> none;
get_random(List) ->
    random:seed(erlang:now()),
    N = length(List),
    Random = random:uniform(N),
    lists:nth(Random, List).

randomize_list([]) -> [];
randomize_list(List) ->
    random:seed(erlang:now()),
    [X || {_, X} <- lists:sort(
                      [{random:uniform(), N} || N <- List])].
