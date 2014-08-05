-module(momental_storage_url).
-export([schemeful/1]).

hostful(Path) ->
    Hostname = momental_storage_config:hostname(),
    Port = momental_storage_config:port(),
    [Hostname, <<":">>, integer_to_binary(Port), Path].

schemeful(Path) ->
    Hostful = hostful(Path),
    Scheme = <<"https://">>,
    [Scheme, Hostful].

