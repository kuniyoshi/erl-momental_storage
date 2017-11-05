-module(d_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    {ok, reject_except_get(Method, Req0), Opts}.

bridge_from_put(Req0, Session) ->
    Req = cowboy_req:stream_reply(200, Req0),
    BridgeFun = fun(fin) -> ok = cowboy_req:stream_body([], fin, Req);
        (Data) ->
            ok = cowboy_req:stream_body(Data, nofin, Req)
    end,
    momental_storage_session:bridge(Session, BridgeFun),
    {ok, Req}.

forbidden_invalid_state(true, Req, Session) ->
    bridge_from_put(Req, Session);
forbidden_invalid_state(false, Req, _Session) ->
    cowboy_req:reply(403, Req).

reject_except_get(<<"GET">>, Req) ->
    Id = cowboy_req:binding(id, Req),
    Session = momental_storage_session:read(Id),
    CanReceive = momental_storage_session:can_receive(Session),
    forbidden_invalid_state(CanReceive, Req, Session);
reject_except_get(_, Req) ->
    cowboy_req:reply(405, Req).
