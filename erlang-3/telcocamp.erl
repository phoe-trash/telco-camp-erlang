%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module definition

-module(telcocamp).
-export([animal/1,
         horse/0, monkey/0, human/0,
         hoarder/0, devourer/0, sick/0]).

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

animal(Inventory, Edible) ->
    animal(Inventory, Edible,
           [fun give/2, fun eat/2, fun eat_this/3]).

animal(Inventory, Edible, Funs) ->
    [OnGive, OnEat, OnEatThis] = Funs,
    receive
        {list_fruit} -> list_fruit(Inventory),
                        animal(Inventory, Edible, Funs);
        {give, Fruit} -> Result = OnGive(Fruit, Inventory),
                        animal(Result, Edible, Funs);
        {eat}        -> Result = OnEat(Inventory, Edible),
                        animal(Result, Edible, Funs);
        {eat, Fruit} -> Result = OnEatThis(Fruit, Inventory, Edible),
                        animal(Result, Edible, Funs);
        {exit}       -> io:format("~w: Goodbye!~n", [self()]),
                        exit(normal);
        X            -> io:format("~w: Huh? ~w?~n", [self(), X]),
                        animal(Inventory, Edible, Funs)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Animal species definitions

horse() ->
    animal([], [apple]).

monkey() ->
    animal([], [banana]).

human() ->
    animal([], [apple, banana, cucumber]).

human(Funs) ->
    animal([], [apple, banana, cucumber], Funs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Human definitions - devourer

devourer_give (Fruit, _) ->
    Result = give(Fruit, []),
    eat(Result, Result).

devourer() ->
    human([fun devourer_give/2,
           fun eat/2,
           fun eat_this/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Human definitions - hoarder

hoarder_eat (Inventory) ->
    io:format("~w: No! Leave my precious fruit alone!~n", [self()]),
    Inventory.

hoarder() ->
    human([fun give/2,
           fun (Inventory, _) -> hoarder_eat(Inventory) end,
           fun (_, Inventory, _) -> hoarder_eat(Inventory) end]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Human definitions - sick

sick_check(Inventory, Result) ->
    if Inventory =:= Result -> Result;
       true -> timer:sleep(3000),
               io:format("~w: I should not have eaten this...~n", [self()]),
               timer:sleep(3000),
               error(horrible_death)
    end.

sick_eat(Inventory, EdibleFruit) ->
    Result = eat(Inventory, EdibleFruit),
    sick_check(Inventory, Result).

sick_eat_this(Fruit, Inventory, EdibleFruit) ->
    Result = eat_this(Fruit, Inventory, EdibleFruit),
    sick_check(Inventory, Result).

sick() ->
    human([fun give/2,
           fun sick_eat/2,
           fun sick_eat_this/3]).
