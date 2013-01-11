-module(object).
-export([get_neighbors/1, get_first_empty/1, get_neighbours/2]).
-define(LOOKUP(X, Y), ets:lookup(grid, {X, Y})).


% retreive the adjecent areas of X, Y.
% returns it as a list of tuples, structured as
% {{X-coordinate, Y-coordinate}, Object}
get_neighbors({X, Y}) ->
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
