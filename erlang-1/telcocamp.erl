%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module definition

-module(telcocamp).
-export([animal/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Logic - basic behaviour

list_fruit(Fruit) ->
    io:format("~w: I got: ~w~n", [self(), Fruit]).

give(NewFruit, Inventory) ->
    io:format("~w: I got a ~w! Yay!~n", [self(), NewFruit]),
    [NewFruit|Inventory].

eat([]) ->
    io:format("~w: I do not have any fruit.~n", [self()]),
    [];
eat(Inventory) ->
    [First|Rest] = Inventory,
    io:format("~w: I ate a ~w. Yummy!~n", [self(), First]),
    Rest.

eat_this(_, []) ->
    io:format("~w: I do not have any fruit.~n", [self()]),
    [];
eat_this(Fruit, Inventory) ->
    IsInInventory = lists:member(Fruit, Inventory),
    case IsInInventory of
        false ->
            io:format("~w: I don't have a ~w.~n", [self(), Fruit]),
            Inventory;
        true ->
            io:format("~w: I ate a ~w. Yummy!~n", [self(), Fruit]),
            lists:delete(Fruit, Inventory)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Logic - animal definition

animal() ->
    animal([]).

animal(Inventory) ->
    receive
        % List fruit
        {list_fruit} -> list_fruit(Inventory),
                        animal(Inventory);
        % Receive new fruit
        {give, Fruit} -> NewInventory = give(Fruit, Inventory),
                        animal(NewInventory);
        % Eat the last fruit
        {eat}        -> NewInventory = eat(Inventory),
                        animal(NewInventory);
        % Eat a particular fruit
        {eat, Fruit} -> NewInventory = eat_this(Fruit, Inventory),
                        animal(NewInventory);
        % Stop the process
        {exit}       -> io:format("~w: Goodbye!~n", [self()]),
                        exit(normal);
        % Fallthrough - malformed message
        X            -> io:format("~w: Huh? ~w?~n", [self(), X]),
                        animal(Inventory)
    end.
