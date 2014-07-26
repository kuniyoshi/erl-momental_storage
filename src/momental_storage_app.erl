-module(momental_storage_app).
-behaviour(application).
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([{momental_storage_config:hostname(),
                                       [{"/r/", r_handler, []}]}]),
    {ok, _} = cowboy:start_http(http,
                                100,
                                [{port, momental_storage_config:port()}],
                                [{env, [{dispatch, Dispatch}]}]),
    momental_storage_sup:start_link().

stop(_State) ->
    ok.
