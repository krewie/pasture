-module(simulator).
-export([field/2, put_fences/3, trav_ets/1, init/0, setup/1]).
-define(HEIGHT, 20).
-define(WIDTH, 40).
-define(DEF, "white").
-define(PUT_OBJECT(Object, X, Y),
        ets:insert(grid, {{X, Y}, Object, spawn(Object, init, [{X, Y}])})).
-define(MOVE_OBJECT(Object, OldX, OldY, X, Y, PID),
        ets:delete(grid, {OldX, OldY}),
        ets:insert(grid, {{X, Y}, Object, PID})).
-define(LOOKUP(X, Y), ets:lookup(grid, {X, Y})).
-define(KILL(X, Y),
        frame ! {change_cell, X, Y, ?DEF},
        ets:delete(grid, {X, Y})).
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


create_object(0, _Module, _Xbound, _Ybound) -> ok;
create_object(N, Module, Xbound, Ybound) ->
    RandX = random:uniform(Xbound-2)+1,
    RandY = random:uniform(Ybound-2)+1,
    io:format("Creating ~p at ~p, ~p ~n", [Module, RandX, RandY]),
    self() ! {reproduce, self(), Module, {RandX, RandY}},
    create_object(N-1, Module, Xbound, Ybound).

init() ->
    %%lägga till spawnade object också %%
    io:format("Init simulator~n",[]),
    register(simulator, 
             spawn_link(simulator, setup, [[?WIDTH, ?HEIGHT, 4, 6]])).


setup([Width, Height, Animals, Plants]) ->
    frame ! {set_w, Width},
    frame ! {set_h, Height},
    field(Height, Width),
    create_object(4, rabbit, Width, Height),
    create_object(4, grass, Width, Height),
    step().


%% traverserar och applicerar funktionen fun på alla inlägg i ets : grid %%
%% spec:en till foldl kräver att 'Accin' defineras, skall användas om tabellen
%% är tom %%
trav_ets(Message) -> ets:foldl(fun({{_X,_Y}, _Object, PID}, Accin) ->
                                       PID ! Message,
                                       Accin end,
                               notused, grid).


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
        {move, PID, Module, {OldX, OldY}, {NewX, NewY}, Color} ->
            case ?LOOKUP(NewX, NewY) of
                [] ->   ?MOVE_OBJECT(Module, OldX, OldY, NewX, NewY, PID),
                        frame ! {change_cell, OldX, OldY, "white"},
                        frame ! {change_cell, NewX, NewY, Color},
                        PID ! {move_ok};
                _ -> PID ! {move_error}
            end,
            loop();
        {eat, PID, Module, {OldX, OldY}, {NewX, NewY}, Color} ->
            ?MOVE_OBJECT(Module, OldX, OldY, NewX, NewY, PID),
                        frame ! {change_cell, OldX, OldY, "white"},
                        frame ! {change_cell, NewX, NewY, Color},
                        PID ! {eat_ok};
        {kill, PID, {X, Y}} ->
            ?KILL(X, Y),
            exit(PID, kill),
            loop();
	    _ -> loop()
    after 1500 ->
	    step()	
    end.
