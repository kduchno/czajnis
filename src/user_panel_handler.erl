-module(user_panel_handler).

-behavior(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    InputFile = code:priv_dir(basic_web) ++ "/static/user_panel.html",

    Orders = orders_db:get_orders_by_user(<<"kduchnoTest">>),
    io:format("Orders=~p~n", [Orders]),

    Content =
        case file:read_file(InputFile) of
            {ok, BinaryContent} ->
                %% Convert the binary content to a string
                binary_to_list(BinaryContent);
            {error, Reason} ->
                io:format("Failed to read the file: ~p~n", [Reason]),
                {error, Reason}
        end,

    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Content, Req),
    {ok, Req2, State}.
