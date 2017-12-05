-module(s_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    {ok, reject_except_put_or_post(Method, Req0), Opts}.

read_body({ok, Body, _Req}, BridgeFun, _Opts) ->
    BridgeFun(Body),
    ok;
read_body({more, Body, Req}, BridgeFun, Opts) ->
    BridgeFun(Body),
    read_body(cowboy_req:read_body(Req), BridgeFun, Opts).

bridge_to_get(Req, Session) ->
    BridgeFun = momental_storage_session:build_sender_fun(Session),
    Opts = [{content_decode, BridgeFun},
            {length, momental_storage_config:max_transfer_size()},
            {read_length, momental_storage_config:fixed_chunk_size()},
            {read_timeout, momental_storage_config:timeout_ms()}],
    ok = read_body(cowboy_req:read_body(Req), BridgeFun, Opts),
    momental_storage_session:close_receiver(Session),
    momental_storage_session:delete(Session),
    cowboy_req:reply(200,
                     #{},
                     <<"200 ok\n">>,
                     Req).

forbidden_invalid_state(true, Req, Session) ->
    bridge_to_get(Req, Session);
forbidden_invalid_state(false, Req, _Session) ->
    cowboy_req:reply(403, Req).

reject_huge_body(undefined, _Limitation, Req) ->
    cowboy_req:reply(411, Req);
reject_huge_body(Size, Limitation, Req) when Size > Limitation ->
    cowboy_req:reply(413, Req);
reject_huge_body(_Size, _Limitation, Req) ->
    Id = cowboy_req:binding(id, Req),
    Session = momental_storage_session:read(Id),
    CanSend = momental_storage_session:can_send(Session),
    forbidden_invalid_state(CanSend, Req, Session).

reject_except_put_or_post(<<"POST">>, Req) ->
    Length = cowboy_req:body_length(Req),
    Limitation = momental_storage_config:max_transfer_size(),
    reject_huge_body(Length, Limitation, Req);
reject_except_put_or_post(<<"PUT">>, Req) ->
    Length = cowboy_req:body_length(Req),
    Limitation = momental_storage_config:max_transfer_size(),
    reject_huge_body(Length, Limitation, Req);
reject_except_put_or_post(_, Req) ->
    cowboy_req:reply(405, Req).
