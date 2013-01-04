-module(simulation).
-export([field/1, put_fences/2,trav_ets/1, init/0, init/1]).

%%  -------------------  %%
%% Initieringsfunktioner %%
%%  -------------------  %%

put_fences(Size,0) -> 
	ets:insert(grid, {{0,0}, spawn(fence, loop, [[fence, {0,0}]])}),
	ets:insert(grid, {{0,Size}, spawn(fence, loop, [[fence, {0, Size}]])}),
	ets:insert(grid, {{Size,0}, spawn(fence, loop, [[fence, {Size, 0}]])}),
	ets:insert(grid, {{Size,Size}, spawn(fence, loop, [[fence, {Size, Size}]])});
put_fences(Size, Iter) ->
	ets:insert(grid, {{Iter,0}, spawn(fence, loop, [[fence, {Iter, 0}]])}),
	ets:insert(grid, {{0,Iter}, spawn(fence, loop, [[fence, {0, Iter}]])}),
	ets:insert(grid, {{Iter,Size}, spawn(fence, loop, [[fence, {Iter, Size}]])}),
	ets:insert(grid, {{Size,Iter}, spawn(fence, loop, [[fence, {Size, Iter}]])}),
	put_fences(Size, Iter-1).

field(Size) -> 
	ets:new(grid, [named_table]),
	put_fences(Size-1, Size-2).

create_animals(Animals) ->
    io:format("Want to create ~p animals ~n", [Animals]).

create_plants(Plants) ->
    io:format("Want to create ~p plants ~n", [Plants]).


init() ->
    %%lägga till spawnade object också %%
    frame ! {set_w, 20},
    frame ! {set_h, 20},
    field(20),
    create_animals(4),
    create_plants(6),
    step().

init([Size, Animals, Plants]) ->
    frame ! {set_w, Size},
    frame ! {set_h, Size},
    field(Size),
    create_animals(Animals),
    create_plants(Plants),
    step().


generate_message(Module, Coordinate) ->
    %% komma på nåt sätt att generera ett meddelande? %%
    {self(), ?MODULE, {Module, Coordinate}}.

%% traverserar och applicerar funktionen fun på alla inlägg i ets : grid %%
%% spec:en till foldl kräver att 'Accin' defineras, skall användas om tabellen är tom %%
trav_ets(Message) -> ets:foldl(fun({{X,Y}, PID}, Accin) ->
				PID ! {self(), Message},
				Accin end, notused, grid).


step() ->
    %% traversera ets på nåt sätt? %%
    trav_ets(step),
    loop().


loop() ->
    receive
	{Pid, Module, Coordinate} ->
	    Message = generate_message(Module, Coordinate),
	    Pid ! Message,
	    loop();
	_ -> loop()
    after 15000 ->
	    step()	
    end.
