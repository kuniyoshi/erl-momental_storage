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
%                                               {length, 80},
                                               {read_length, 10},
                                               {read_timeout, 60000}]),
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

reject_except_post(<<"PUT">>, Req) ->
    {Id, Req2} = cowboy_req:binding(filename, Req),
    ?debugVal(Id),
    Session = momental_storage_session:read(Id),
    ?debugVal(Session),
    CanSend = momental_storage_session:can_send(Session),
    forbidden_invalid_state(Req2, Session, CanSend);
reject_except_post(_, Req) ->
    cowboy_req:reply(405, Req).

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = reject_except_post(Method, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
