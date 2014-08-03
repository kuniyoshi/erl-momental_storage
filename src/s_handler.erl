-module(s_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include_lib("eunit/include/eunit.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

bridge_to_get(Req, Session) ->
    ?debugVal(cowboy_req:has_body(Req)),
    {Length, Req2} = cowboy_req:body_length(Req),
    ?debugVal(Length),
    BridgeFun = momental_storage_session:build_sender_fun(Session),
    {ok, _Body, Req3} = cowboy_req:body(Req2, [{content_decode, BridgeFun},
                                               {length, momental_storage_config:max_transfer_size()},
                                               {read_length, momental_storage_config:fixed_chunk_size()},
                                               {read_timeout, momental_storage_config:timeout_ms()}]),
    momental_storage_session:close_receiver(Session),
    momental_storage_session:delete(Session),
    cowboy_req:reply(200,
                     [],
                     ["200 ok\n"],
                     Req3).

forbidden_invalid_state(Req, Session, true) ->
    bridge_to_get(Req, Session);
forbidden_invalid_state(Req, _Session, false) ->
    cowboy_req:reply(403, Req).

reject_huge_body(undefined, _Limitation, Req) ->
    cowboy_req:reply(411, Req);
reject_huge_body(Size, Limitation, Req) when Size > Limitation ->
    cowboy_req:reply(413, Req);
reject_huge_body(_Size, _Limitation, Req) ->
    {Id, Req2} = cowboy_req:binding(filename, Req),
    ?debugVal(Id),
    Session = momental_storage_session:read(Id),
    ?debugVal(Session),
    CanSend = momental_storage_session:can_send(Session),
    forbidden_invalid_state(Req2, Session, CanSend).

reject_except_put(<<"PUT">>, Req) ->
    {Length, Req2} = cowboy_req:body_length(Req),
    Limitation = momental_storage_config:max_transfer_size(),
    reject_huge_body(Length, Limitation, Req2);
reject_except_put(_, Req) ->
    cowboy_req:reply(405, Req).

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = reject_except_put(Method, Req2), % TODO: How long way to bridge_to_get?  Should be shorten.
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
