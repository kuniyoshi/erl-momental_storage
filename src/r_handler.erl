-module(r_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    {ok, redirect_to_d_path(Method, Req0), Opts}.

redirect_to_d_path(<<"GET">>, Req) ->
    Session = momental_storage_session:new(),
    Id = momental_storage_session:get_id(Session),
    Url = momental_storage_url:schemeful([<<"/d/">>, Id]),
    momental_storage_session:write(Session),
    cowboy_req:reply(302,
                     #{<<"location">> => Url},
                     [],
                     Req);
redirect_to_d_path(_, Req) ->
    cowboy_req:reply(405, Req).
