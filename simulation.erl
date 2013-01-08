-module(simulation).
-export([field/2, put_fences/3,trav_ets/1, init/0, init/1]).
-define(HEIGHT, 20).
-define(WIDTH, 40). %% t
-define(PUT_OBJECT(Object, X, Y), ets:insert(grid, {{X, Y}, spawn(Object, loop, [[Object, {X, Y}]])})).
%%  -------------------  %%
%% Initieringsfunktioner %%
%%  -------------------  %%

put_fences(_H,0,0) -> ?PUT_OBJECT(fence,0,0);
put_fences(Height,0,Width) -> 
        ?PUT_OBJECT(fence, Width, 0),
        ?PUT_OBJECT(fence, Width, Height),
        put_fences(Height, 0, Width-1);
put_fences(Height,Acc,Width) ->
    ?PUT_OBJECT(fence, 0, Acc),
    ?PUT_OBJECT(fence, Width, Acc),
    put_fences(Height, Acc-1, Width).

field(Height, Width) -> 
	ets:new(grid, [named_table]),
	put_fences(Height-1, Height-1, Width-1).

create_animals(Animals) ->
    io:format("Want to create ~p animals ~n", [Animals]).

create_plants(Plants) ->
    io:format("Want to create ~p plants ~n", [Plants]).


init() ->
    %%lägga till spawnade object också %%
    frame ! {set_w, ?WIDTH},
    frame ! {set_h, ?HEIGHT},
    field(?HEIGHT, ?WIDTH),
    create_animals(4),
    create_plants(6),
    step().

init([Height, Width, Animals, Plants]) ->
    frame ! {set_w, Width},
    frame ! {set_h, Height},
    field(Height, Width),
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
