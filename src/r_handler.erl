-module(r_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

redirect_to_d_path(<<"GET">>, Req) ->
    Session = momental_storage_session:new(),
    Id = momental_storage_session:get_id(Session),
    Url = momental_storage_url:schemeful([<<"/d/">>, Id]),
    momental_storage_session:write(Session),
    cowboy_req:reply(302,
                     [{<<"location">>, Url}],
                     [],
                     Req);
redirect_to_d_path(_, Req) ->
    cowboy_req:reply(405, Req).

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = redirect_to_d_path(Method, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
