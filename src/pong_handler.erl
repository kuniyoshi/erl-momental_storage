-module(pong_handler).
-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"pong.\n">>,
                           Req0),
    {ok, Req, Opts}.
