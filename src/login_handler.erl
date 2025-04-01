-module(login_handler).

-behavior(cowboy_handler).

-export([init/2]).

% Default bcrypt work factor (cost). Higher is slower/more secure. 12 is common.
-define(REDIRECT_URL, "/user_created").
-define(USER_PANEL, "/user_panel").

init(Req0, State) ->
    % io:format("Req0=~p~n~n", [Req0]),
    {ok, Data, _Req} = cowboy_req:read_body(Req0),
    % io:format("Data=~p~n~n", [Data]),
    [Login, Password, Email] = binary:split(Data, <<"&">>, [global]),
    LoginVal =
        lists:last(
            binary:split(Login, <<"=">>)),
    PasswordVal =
        lists:last(
            binary:split(Password, <<"=">>)),
    HashedPassword = crypto:hash(blake2b, PasswordVal),
    EmailVal =
        lists:last(
            binary:split(Email, <<"=">>)),

    % io:format("LoginVal=~p~n", [LoginVal]),
    % io:format("PasswordVal=~p~n", [PasswordVal]),
    % io:format("HashedPassword=~p~n", [HashedPassword]),
    % io:format("EmailVal=~p~n", [EmailVal]),
    Req = case user_db:get_user(LoginVal) of
              {ok, _UserRecord} ->
                  cowboy_req:reply(302,
                                   #{<<"location">> => ?USER_PANEL},
                                   <<"Redirecting...">>,
                                   Req0);
              _ ->
                  user_db:add_user(LoginVal, HashedPassword, EmailVal),
                  cowboy_req:reply(302,
                                   #{<<"location">> => ?REDIRECT_URL},
                                   <<"Redirecting...">>,
                                   Req0)
          end,
    {ok, Req, State}.
