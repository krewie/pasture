-module(object).
-export([get_neighbours/1, get_neighbours2/1, get_first_empty/1]).
-define(CELL, "black").
-define(LOOKUP(X, Y), ets:lookup(grid, {X, Y})).


% retreive the adjecent areas of X, Y.
% returns it as a list of tuples, structured as
% {{X-coordinate, Y-coordinate}, Object}
get_neighbours({X, Y}) ->
    [{{X-1, Y-1}, ?LOOKUP(X-1, Y-1)}, {{X  , Y-1}, ?LOOKUP(X  , Y-1)},
     {{X+1, Y-1}, ?LOOKUP(X+1, Y-1)}, {{X-1, Y  }, ?LOOKUP(X-1, Y  )},
     {{X+1, Y  }, ?LOOKUP(X+1, Y  )}, {{X-1, Y+1}, ?LOOKUP(X-1, Y+1)},
     {{X  , Y+1}, ?LOOKUP(X  , Y+1)}, {{X+1, Y+1}, ?LOOKUP(X+1, Y+1)}].

% Don't know if you wanted something else but this one makes it so that
% You get a list with all the neighbours in the form
% [{Coordinates, Object}, {Coordinate, Object},...]
% And if one object doesn't exist, such that it is out of the grid, ?LOOKUP
% returns an empty list which then is appended, i.e it is not included.
% Gabriel
get_neighbours2({X, Y}) ->
    lists:append([?LOOKUP(X-1, Y-1), ?LOOKUP(X  , Y-1), ?LOOKUP(X+1, Y-1),
                  ?LOOKUP(X-1, Y  ), ?LOOKUP(X+1, Y  ),
                  ?LOOKUP(X-1, Y+1), ?LOOKUP(X  , Y+1), ?LOOKUP(X+1, Y+1)
                 ]).


% retreives the the first coordinate with no object from a list of
% Coordinate-Object-Tuples
% if all coordinates in list are occupied, return none.
get_first_empty([])-> none;
get_first_empty([{Coordinate, []}|_T]) ->
    Coordinate;
get_first_empty([{_Coordinate, _}|T]) ->
    get_first_empty(T).
