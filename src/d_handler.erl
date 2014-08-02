-module(d_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include_lib("eunit/include/eunit.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

bridge_from_put(Req, Session) ->
    {ok, Req2} = cowboy_req:chunked_reply(200, Req),
    BridgeFun = fun(Data) ->
            ok = cowboy_req:chunk(Data, Req2)
    end,
    momental_storage_session:bridge(Session, out, BridgeFun),
    {ok, Req2}.

forbidden_invalid_state(Req, Session, true) ->
    bridge_from_put(Req, Session);
forbidden_invalid_state(Req, _Session, false) ->
    cowboy_req:reply(403, Req).

reject_except_get(<<"GET">>, Req) ->
    {Id, Req2} = cowboy_req:binding(filename, Req),
    ?debugVal(Id),
    Session = momental_storage_session:read(Id),
    ?debugVal(Session),
    CanReceive = momental_storage_session:can_receive(Session),
    forbidden_invalid_state(Req2, Session, CanReceive);
reject_except_get(_, Req) ->
    cowboy_req:reply(405, Req).

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = reject_except_get(Method, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
