-module(basic_web_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    user_db:start(),
    orders_db:start_orders_table(),
    menu_db:start_menu_table(),

    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/", menu_handler, []},
                                 % {"/", cowboy_static, {priv_file, basic_web, "static/index.html"}}
                                 {"/assets/[...]",
                                  cowboy_static,
                                  {priv_dir, basic_web, "static/assets"}},
                                 {"/about",
                                  cowboy_static,
                                  {priv_file, basic_web, "static/about.html"}},
                                 {"/user_created",
                                  cowboy_static,
                                  {priv_file, basic_web, "static/user_created.html"}},
                                 {"/login",
                                  cowboy_static,
                                  {priv_file, basic_web, "static/log_reg.html"}},
                                 %  {"/menu", menu_handler, []},
                                 {"/register", login_handler, []},
                                 {"/user_panel", user_panel_handler, []}]}]),

    {ok, _} =
        cowboy:start_clear(my_http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),

    basic_web_sup:start_link().

stop(_State) ->
    user_db:stop(),
    ok.
