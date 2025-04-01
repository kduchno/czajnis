-module(menu_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-record(menu, {dish_name, dish_price, modifications}).

init(Req, State) ->
    %% Start Mnesia
    menu_db:start_menu_table(),

    %% Fetch all dishes from the menu table
    Dishes = menu_db:get_all_dishes(),

    %% Generate HTML content
    MenuHTML = generate_html(Dishes),
    % io:format("MenuHTML=~p~n", [MenuHTML]),
    HTML = inject_menu_html(MenuHTML),

    %% Reply with the HTML
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, HTML, Req),
    {ok, Req2, State}.

% ENDED HERE
generate_html(Dishes) ->
    %% Convert dish records into table rows
    Rows =
        lists:foldl(fun(#menu{dish_name = DishName,
                              dish_price = DishPrice,
                              modifications = Modifications},
                        Acc) ->
                       Acc
                       ++ "<tr><td>"
                       ++ binary_to_list(DishName)
                       ++ "</td><td>"
                       ++ binary_to_list(DishPrice)
                       ++ "</td><td>"
                       ++ binary_to_list(Modifications)
                       ++ "</td></tr>"
                    end,
                    "",
                    Dishes),
    %% Combine rows into an HTML table
    "    <table id=\"menuTable\">"
    ++ "        <thead>"
    ++ "            <tr>"
    ++ "                <th>Dish Name</th>"
    ++ "                <th>Price</th>"
    ++ "                <th>Modifications</th>"
    ++ "            </tr>"
    ++ "        </thead>"
    ++ "        <tbody>"
    ++ [Rows]
    ++ "</tbody>"
    ++ "    </table>".

inject_menu_html(MenuHTML) ->
    %% Read the file content
    InputFile = code:priv_dir(basic_web) ++ "/static/menu.html",
    % io:format("InputFile=~p~n", [InputFile]),
    case file:read_file(InputFile) of
        {ok, BinaryContent} ->
            %% Convert the binary content to a string
            Content = binary_to_list(BinaryContent),

            %% Replace the expression "REPLACE_THIS_TEXT" with NewText
            UpdatedContent = string:replace(Content, "REPLACE_THIS_TEXT", MenuHTML, all),
            % io:format("UpdatedContent=~p~n", [UpdatedContent]),
            UpdatedContent;
        {error, Reason} ->
            io:format("Failed to read the file: ~p~n", [Reason]),
            {error, Reason}
    end.
