-module(momental_storage_app).
-behaviour(application).
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([{momental_storage_config:hostname(),
                                       [{"/r/[:filename]", r_handler, []},
                                        {"/d/[:id]", d_handler, []},
                                        {"/s/[:id]", s_handler, []},
                                        {"/ping", pong_handler, []}]}]),
    PrivDir = code:priv_dir(momental_storage),
    {ok, _} = cowboy:start_https(https,
                                 100,
                                 [{port, momental_storage_config:port()},
                                  {cacertfile, PrivDir ++ "/ssl/server.crt"},
                                  {certfile, PrivDir ++ "/ssl/server.crt"},
                                  {keyfile, PrivDir ++ "/ssl/server.key"}],
                                 [{env, [{dispatch, Dispatch}]}]),
    ok = momental_storage_session:setup(),
    momental_storage_sup:start_link().

stop(_State) ->
    ok.
