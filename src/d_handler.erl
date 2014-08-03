-module(d_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

bridge_from_put(Req, Session) ->
    {ok, Req2} = cowboy_req:chunked_reply(200, Req),
    BridgeFun = fun(Data) ->
            ok = cowboy_req:chunk(Data, Req2)
    end,
    momental_storage_session:bridge(Session, BridgeFun),
    {ok, Req2}.

forbidden_invalid_state(true, Req, Session) ->
    bridge_from_put(Req, Session);
forbidden_invalid_state(false, Req, _Session) ->
    cowboy_req:reply(403, Req).

reject_except_get(<<"GET">>, Req) ->
    {Id, Req2} = cowboy_req:binding(id, Req),
    Session = momental_storage_session:read(Id),
    CanReceive = momental_storage_session:can_receive(Session),
    forbidden_invalid_state(CanReceive, Req2, Session);
reject_except_get(_, Req) ->
    cowboy_req:reply(405, Req).

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = reject_except_get(Method, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
