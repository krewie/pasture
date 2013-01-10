-module(object).
-export([get_neighbors/1, get_first_empty/1]).
-define(LOOKUP(X, Y), ets:lookup(grid, {X, Y})).


% retreive the adjecent areas of X, Y.
% returns it as a list of tuples, structured as
% {{X-coordinate, Y-coordinate}, Module, ObjectPID}
get_neighbors({X, Y}) ->
	[{{X-1, Y-1}, ?LOOKUP(X-1, Y-1)}, {{X, Y-1}, ?LOOKUP(X, Y-1)},
         {{X+1, Y-1}, ?LOOKUP(X+1, Y-1)}, {{X-1, Y},?LOOKUP(X-1, Y)},
         {{X+1, Y}, ?LOOKUP(X+1, Y)}, {{X-1, Y+1}, ?LOOKUP(X-1, Y+1)},
         {{X, Y+1}, ?LOOKUP(X, Y+1)}, {{X+1, Y+1}, ?LOOKUP(X+1, Y+1)}].


% retreives the the first coordinate with no object from
% a list of Coordinate-Object-Tuples
% if all coordinates in list are occupied, return none.
get_first_empty([])-> none;
get_first_empty([{Coordinate, []}|_T]) ->
	Coordinate;
get_first_empty([{_Coordinate, _}|T]) ->
	get_first_empty(T).
