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

create_animals(Animals) ->
    io:format("Want to create ~p animals ~n", [Animals]).

create_plants(Plants) ->
    io:format("Want to create ~p plants ~n", [Plants]).


init() ->
    %%lägga till spawnade object också %%
    field(50),
    create_animals(4),
    create_plants(6).

init([Size, Animals, Plants]) ->
    field(Size),
    create_animals(Animals),
    create_plants(Plants),
    step().


generate_message(Module, Coordinate) ->
    %% komma på nåt sätt att generera ett meddelande? %%
    {self(), ?MODULE, {Module, Coordinate}}.

step() ->
    %% traversera ets på nåt sätt? %%
    loop().


loop() ->
    receive
	{Pid, Module, Coordinate} ->
	    Message = generate_message(Module, Coordinate),
	    Pid ! Message,
	    loop();
	_ -> loop()
    after 1000 ->
	    step()	
    end.
