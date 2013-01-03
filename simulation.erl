-module(simulation).
-export([field/1, put_fences/2]).

put_fences(Size,0) -> 
	ets:insert(grid, {{0,0}, spawn(fun fence:loop/0)}),
	ets:insert(grid, {{0,Size}, spawn(fun fence:loop/0)}),
	ets:insert(grid, {{Size,0}, spawn(fun fence:loop/0)}),
	ets:insert(grid, {{Size,Size}, spawn(fun fence:loop/0)});
put_fences(Size, Iter) ->
	ets:insert(grid, {{Iter,0}, spawn(fun fence:loop/0)}),
	ets:insert(grid, {{0,Iter}, spawn(fun fence:loop/0)}),
	ets:insert(grid, {{Iter,Size}, spawn(fun fence:loop/0)}),
	ets:insert(grid, {{Size,Iter}, spawn(fun fence:loop/0)}),
	put_fences(Size, Iter-1).

field(Size) -> 
	ets:new(grid, [set, named_table]),
	put_fences(Size-1, Size-2).
