-module(user_db).

-export([start/0, add_user/3, get_user/1, delete_user/1, stop/0]).

-record(user, {login, password, email}).

%% Starts Mnesia and initializes the schema and table
start() ->
    mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:wait_for_tables([], 5000),
    case mnesia:create_table(user, [{attributes, record_info(fields, user)}]) of
        {atomic, ok} ->
            io:format("Table created successfully~n");
        {aborted, {already_exists, user}} ->
            io:format("Table already exists~n");
        Error ->
            io:format("Error creating table: ~p~n", [Error])
    end.

%% Stops Mnesia
stop() ->
    mnesia:stop().

%% Adds a new user
add_user(Login, Password, Email) ->
    F = fun() ->
           mnesia:write(#user{login = Login,
                              password = Password,
                              email = Email})
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            io:format("User added successfully~n");
        {aborted, Reason} ->
            io:format("Failed to add user: ~p~n", [Reason])
    end.

%% Retrieves a user by login
get_user(Login) ->
    F = fun() ->
           case mnesia:read({user, Login}) of
               [] ->
                   {error, not_found};
               [Record] ->
                   {ok, Record}
           end
        end,
    case mnesia:transaction(F) of
        {atomic, {ok, Record}} ->
            io:format("User found: ~p~n", [Record]);
        {atomic, {error, not_found}} ->
            io:format("User not found~n");
        {aborted, Reason} ->
            io:format("Failed to retrieve user: ~p~n", [Reason])
    end.

%% Deletes a user by login
delete_user(Login) ->
    F = fun() ->
           case mnesia:read({user, Login}) of
               [] ->
                   {error, not_found};
               _ ->
                   mnesia:delete({user, Login})
           end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            io:format("User deleted successfully~n");
        {atomic, {error, not_found}} ->
            io:format("User not found~n");
        {aborted, Reason} ->
            io:format("Failed to delete user: ~p~n", [Reason])
    end.
