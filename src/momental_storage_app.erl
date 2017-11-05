-module(momental_storage_app).
-behaviour(application).
-export([start/2]).
-export([stop/1]).

start_protocol_scheme(<<"https">>, Dispatch) ->
    PrivDir = code:priv_dir(momental_storage),
    {ok, _} = cowboy:start_tls(https,
                               [{port, momental_storage_config:port()},
                                {cacertfile, PrivDir ++ "/ssl/server.crt"},
                                {certfile, PrivDir ++ "/ssl/server.crt"},
                                {keyfile, PrivDir ++ "/ssl/server.key"}],
                               #{env => #{dispatch => Dispatch}}),
    ok;
start_protocol_scheme(<<"http">>, Dispatch) ->
    {ok, _} = cowboy:start_clear(http,
                                 [{port, momental_storage_config:port()}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([{momental_storage_config:hostname(),
                                       [{"/r/[:filename]", r_handler, []},
                                        {"/d/[:id]", d_handler, []},
                                        {"/s/[:id]", s_handler, []},
                                        {"/ping", pong_handler, []}]}]),
    start_protocol_scheme(momental_storage_config:protocol_scheme(), Dispatch),
    ok = momental_storage_session:setup(),
    momental_storage_sup:start_link().

stop(_State) ->
    ok.
