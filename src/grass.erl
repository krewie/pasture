-module(grass).
-extends(object).
-behaviour(plant).
-export([loop/1, init/1, get_neighbors/1, get_first_empty/1]).
-define(REPRODUCTION, 1).
-define(LOOKUP(X, Y), ets:lookup(grid, {X, Y})).
-define(CELL, "green").

%% Object %%
% retreive the adjecent areas of X, Y.
% returns it as a list of tuples, structured as {{X-coordinate, Y-coordinate}, Object}
get_neighbors({X, Y}) ->
	[{{X-1, Y-1}, ?LOOKUP(X-1, Y-1)}, {{X, Y-1}, ?LOOKUP(X, Y-1)}, {{X+1, Y-1}, ?LOOKUP(X+1, Y-1) }, 
	 	{{X-1, Y},?LOOKUP(X-1, Y)},					 			   	  {{X+1, Y},?LOOKUP(X+1, Y)},
	 {{X-1, Y+1}, ?LOOKUP(X-1, Y+1)}, {{X, Y+1}, ?LOOKUP(X, Y+1)}, {{X+1, Y+1}, ?LOOKUP(X+1, Y+1)}].


% retreives the the first coordinate with no object from a list of Coordinate-Object-Tuples
% if all coordinates in list are occupied, return none.
get_first_empty([])-> none;
get_first_empty([{Coordinate, []}|_T]) ->
	Coordinate;
get_first_empty([{_Coordinate, _}|T]) ->
	get_first_empty(T).




init(Coordinate) ->
	{X, Y} = Coordinate,
	frame ! {change_cell, X, Y, ?CELL},
	loop({Coordinate, ?REPRODUCTION}).

tick(State) ->
	{{X, Y}, Reproduction} = State,
	frame ! {change_cell, X, Y, ?CELL},
	NewState = {{X, Y}, Reproduction-1},
	NewState.	


%% Tries to reproduce by asking simulator to spawn object at currently empty coordinate.
%% Not sure if we att the current case (grass) need to know the result of the reproduction.
reproduce(Coordinate) ->
	Neighbors = get_neighbors(Coordinate),
	Rep_Coor = get_first_empty(Neighbors),
	case Rep_Coor of 
		none -> 
			error;
		{X, Y} -> 
			simulator ! {reproduce, self(), ?MODULE, {X, Y}}
	end,
	receive
		{reproduction_ok} ->
			ok;
		{reproduction_error} ->
			error
	end.

loop({Coordinate, 0}) ->
	reproduce(Coordinate),
	loop({Coordinate, ?REPRODUCTION});
loop({Coordinate, Reproduction}) ->
	State = {Coordinate, Reproduction},
	receive 
		{tick} -> 
			NewState = tick(State),
			loop(NewState)
	end.