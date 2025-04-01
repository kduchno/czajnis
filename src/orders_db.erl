-module(orders_db).

-export([start_orders_table/0, add_order/4, delete_order/2, get_all_orders/0,
         get_orders_by_user/1]).

-record(order, {user, dish, order_date, price}).

%% Starts Mnesia and creates the orders table
start_orders_table() ->
    mnesia:start(),
    mnesia:wait_for_tables([], 5000),
    case mnesia:create_table(order, [{attributes, record_info(fields, order)}]) of
        {atomic, ok} ->
            io:format("Orders table created successfully~n");
        {aborted, {already_exists, order}} ->
            io:format("Orders table already exists~n");
        Error ->
            io:format("Error creating orders table: ~p~n", [Error])
    end.

%% Adds a new order
add_order(User, Dish, OrderDate, Price) ->
    F = fun() ->
           mnesia:write(#order{user = User,
                               dish = Dish,
                               order_date = OrderDate,
                               price = Price})
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            io:format("Order added successfully~n");
        {aborted, Reason} ->
            io:format("Failed to add order: ~p~n", [Reason])
    end.

%% Deletes a specific order by user and dish
delete_order(User, Dish) ->
    F = fun() ->
           case mnesia:match_object(#order{user = User,
                                           dish = Dish,
                                           _ = '_'})
           of
               [] ->
                   {error, not_found};
               [Order] ->
                   mnesia:delete({order, Order#order.user})
           end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            io:format("Order deleted successfully~n");
        {atomic, {error, not_found}} ->
            io:format("Order not found~n");
        {aborted, Reason} ->
            io:format("Failed to delete order: ~p~n", [Reason])
    end.

%%%% ---------------- %%%%

get_orders_by_user(User) ->
    ok.

get_all_orders() ->
    ok.
