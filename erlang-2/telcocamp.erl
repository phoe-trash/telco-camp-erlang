%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module definition

-module(telcocamp).
-export([animal/1,
         horse/0, monkey/0, human/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Logic - basic behaviour

list_fruit(Fruit) ->
    io:format("~w: I got: ~w~n", [self(), Fruit]).

give(NewFruit, Inventory) ->
    io:format("~w: I got a ~w! Yay!~n", [self(), NewFruit]),
    [NewFruit|Inventory].

eat([], _) ->
    io:format("~w: I do not have any fruit.~n", [self()]),
    [];
eat(Inventory, EdibleFruit) ->
    [First|Rest] = Inventory,
    case lists:member(First, EdibleFruit) of
        true -> io:format("~w: I ate a ~w. Yummy!~n", [self(), First]),
                Rest;
        false -> io:format("~w: I don't want a ~w.~n", [self(), First]),
                 Inventory
    end.

eat_this(_, [], _) ->
    io:format("~w: I do not have any fruit.~n", [self()]),
    [];
eat_this(Fruit, Inventory, EdibleFruit) ->
    IsInInventory = lists:member(Fruit, Inventory),
    IsEdible = lists:member(Fruit, EdibleFruit),
    if IsInInventory == false ->
            io:format("~w: I don't have a ~w.~n", [self(), Fruit]),
            Inventory;
       IsEdible == false ->
            io:format("~w: I don't want a ~w.~n", [self(), Fruit]),
            Inventory;
       true ->
            io:format("~w: I ate a ~w. Yummy!~n", [self(), Fruit]),
            lists:delete(Fruit, Inventory)
           end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Logic - animal definition

animal(Edible) ->
    animal([], Edible).

animal(Inventory, Edible) ->
    receive
        {list_fruit} -> list_fruit(Inventory),
                        animal(Inventory, Edible);
        {give, Fruit} -> Result = give(Fruit, Inventory),
                         animal(Result, Edible);
        {eat}        -> Result = eat(Inventory, Edible),
                        animal(Result, Edible);
        {eat, Fruit} -> Result = eat_this(Fruit, Inventory, Edible),
                        animal(Result, Edible);
        {exit}       -> io:format("~w: Goodbye!~n", [self()]),
                        exit(normal);
        X            -> io:format("~w: Huh? ~w?~n", [self(), X]),
                        animal(Inventory, Edible)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Animal species definitions

horse() ->
    animal([apple]).

monkey() ->
    animal([banana]).

human() ->
    animal([apple, banana, cucumber]).
