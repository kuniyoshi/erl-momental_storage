-module(momental_storage_config).
-export([hostname/0,
         port/0,
         max_transfer_size/0,
         fixed_chunk_size/0,
         timeout_ms/0]).

get_from_env(Key) ->
    {ok, Val} = application:get_env(momental_storage, Key),
    Val.

hostname() ->
    get_from_env(hostname).

port() ->
    get_from_env(port).

max_transfer_size() ->
    500000000.

fixed_chunk_size() ->
    100000.

timeout_ms() ->
    180000.
