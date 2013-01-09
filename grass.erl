-module(grass).
-extends(object).
%-behaviour(plant).
-export([loop/1, init/1]).
-define(REPRODUCTION, 10).
-define(CELL, "green").


init(Coordinate) ->
	loop({Coordinate, ?REPRODUCTION}).

tick(State) ->
	{{X, Y}, Reproduction} = State,
	frame ! {change_cell, X, Y, ?CELL},
	NewState = {{X, Y}, Reproduction-1},
	NewState.	


%% Tries to reproduce by asking simulator to spawn object at currently empty coordinate.
%% Not sure if we att the current case (grass) need to know the result of the reproduction.
reproduce(Coordinate) ->
	Neighbors = grass:get_neighbors(Coordinate),
	Rep_Coor = grass:first_empty(Neighbors),
	case Rep_Coor of 
		none -> loop({Coordinate, 1});
		{X, Y} -> simulator ! {reproduce, self(), ?MODULE, {X, Y}}
	end,
	receive
		{reproduction_ok} ->
			ok;
		{reproduction_error} ->
			error
	end.

loop({Coordinate, 0}) ->
	_Result = reproduce(Coordinate),
	loop({Coordinate, ?REPRODUCTION});
loop({Coordinate, Reproduction}) ->
	{X, Y} = Coordinate,
	State = {Coordinate, Reproduction},
	receive 
		{tick} -> NewState = tick(State),
			loop(NewState)
	end.