-module(menu_db).

-export([start_menu_table/0, get_all_dishes/0]).

-record(menu, {dish_name, dish_price, modifications}).

%% Starts Mnesia and creates the menu table
start_menu_table() ->
    mnesia:start(),
    mnesia:wait_for_tables([], 5000),
    case mnesia:create_table(menu, [{attributes, record_info(fields, menu)}]) of
        {atomic, ok} ->
            io:format("Menu table created successfully~n");
        {aborted, {already_exists, menu}} ->
            io:format("Menu table already exists~n");
        Error ->
            io:format("Error creating menu table: ~p~n", [Error])
    end,
    fill_menu_table().

%% Retrieves all dishes from the menu
get_all_dishes() ->
    F = fun() -> mnesia:match_object(#menu{_ = '_'}) end,
    case mnesia:transaction(F) of
        {atomic, Results} ->
            Results;
        {aborted, Reason} ->
            io:format("Failed to retrieve all dishes: ~p~n", [Reason])
    end.

%%% --- INTERNAL --- %%%
fill_menu_table() ->
    DishList =
        [{<<"Dish1">>, <<"Price1">>, <<"Mods1">>},
         {<<"Dish2">>, <<"Price2">>, <<"Mods2">>},
         {<<"Dish3">>, <<"Price1">>, <<"Mods3">>},
         {<<"Dish4">>, <<"Price3">>, <<"Mods4">>},
         {<<"Dish5">>, <<"Price2">>, <<"Mods4">>}],
    [add_dish(DishName, DishPrice, Modifications)
     || {DishName, DishPrice, Modifications} <- DishList].

%% Adds a dish to the menu
add_dish(DishName, DishPrice, Modifications) ->
    F = fun() ->
           mnesia:write(#menu{dish_name = DishName,
                              dish_price = DishPrice,
                              modifications = Modifications})
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            % io:format("Dish added successfully~n");
            ok;
        {aborted, Reason} ->
            io:format("Failed to add dish: ~p~n", [Reason])
    end.

%% Deletes a dish by name
% delete_dish(DishName) ->
%     F = fun() ->
%            case mnesia:read({menu, DishName}) of
%                [] ->
%                    {error, not_found};
%                _ ->
%                    mnesia:delete({menu, DishName})
%            end
%         end,
%     case mnesia:transaction(F) of
%         {atomic, ok} ->
%             io:format("Dish deleted successfully~n");
%         {atomic, {error, not_found}} ->
%             io:format("Dish not found~n");
%         {aborted, Reason} ->
%             io:format("Failed to delete dish: ~p~n", [Reason])
%     end.
