-module(momental_storage_config).
-export([hostname/0,
         port/0]).

from_application_env(Key) ->
    {ok, Val} = application:get_env(momental_storage, Key),
    Val.

hostname() ->
    from_application_env(hostname).

port() ->
    from_application_env(port).
