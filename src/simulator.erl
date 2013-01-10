-module(simulator).
-export([field/2, put_fences/3,trav_ets/1, init/0, setup/1]).
-define(HEIGHT, 20).
-define(WIDTH, 40).
-define(PUT_OBJECT(Object, X, Y),
        ets:insert(grid, {{X, Y}, spawn(Object, init, [{X, Y}])})).
-define(LOOKUP(X, Y), ets:lookup(grid, {X, Y})).
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
    io:format("Init simulator~n",[]),
    register(simulator, 
             spawn_link(simulator, setup, [[?HEIGHT, ?WIDTH, 4, 6]])).


setup([Height, Width, Animals, Plants]) ->
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
%% spec:en till foldl kräver att 'Accin' defineras, skall användas om tabellen
%% är tom %%
trav_ets(Message) -> ets:foldl(fun({{_X,_Y}, PID}, Accin) ->
				PID ! Message,
				Accin end, notused, grid).


step() ->
    %% traversera ets på nåt sätt? %%
    trav_ets({tick}),
    loop().


loop() ->
    receive
    {reproduce, PID, Module, {X, Y}} -> 
        case ?LOOKUP(X, Y) of
            [] -> ?PUT_OBJECT(Module, X, Y),
                PID ! {reproduction_ok};
            _ -> 
                PID ! {reproduction_error}
        end,
        loop();
	{Pid, Module, Coordinate} ->
	    Message = generate_message(Module, Coordinate),
	    Pid ! Message,
	    loop();
	_ -> loop()
    after 15000 ->
	    step()	
    end.
