-module(momental_storage_session).
-export([setup/0]).
-export([get_id/1, get_started_at/1, get_state/1]).
-export([can_receive/1, can_send/1]).
-export([new/1, new/0, write/1, read/1, delete/1]).
-export([bridge/2, build_sender_fun/1, close_receiver/1]).

-record(session, {id = <<>> :: binary(),
                  started_at = 0 :: calendar:datetime(),
                  state = init :: init | receiver_ready | bridging | done,
                  receiver = undefined :: pid()}).

setup() ->
    {atomic, ok} = mnesia:create_table(session,
                                       [{attributes, record_info(fields, session)}]),
    ok.

get_id(#session{id = Id}) -> Id.

get_started_at(#session{started_at = StartedAt}) -> StartedAt.

get_state(#session{state = State}) -> State.

get_receiver(#session{receiver = Receiver}) -> Receiver.

can_receive(Session) -> get_state(Session) =:= init.

can_send(Session) -> get_state(Session) =:= receiver_ready.

gen_id() ->
    Sha = crypto:hash(sha512, crypto:rand_bytes(64)),
    Id = lists:map(fun(Bin) -> io_lib:format("~.16B", [Bin]) end, binary_to_list(Sha)),
    Id2 = list_to_binary(lists:flatten(Id)),
    Id2.

new(Props) ->
    Id = proplists:get_value(id, Props, gen_id()),
    StartedAt = proplists:get_value(started_at, Props, calendar:datetime_to_gregorian_seconds(erlang:localtime())),
    State = proplists:get_value(state, Props, init),
    Session = #session{id = Id, started_at = StartedAt, state = State},
    Session.

new() ->
    new([]).

write(Session) ->
    mnesia:dirty_write(Session).

read(Id) ->
    [Session] = mnesia:dirty_read(session, Id),
    Session.

delete(Session) ->
    mnesia:dirty_delete_object(Session).

bridge_loop(Fun) ->
    receive
        {data, Data} ->
            Fun(Data),
            bridge_loop(Fun);
        done ->
            ok
    end.

to_be_ready(Session, ReceiverPid) ->
    Session2 = Session#session{state = receiver_ready, receiver = ReceiverPid},
    write(Session2),
    Session2.

bridge(Session, Fun) ->
    Session2 = to_be_ready(Session, self()),
    write(Session2),
    bridge_loop(Fun).

build_sender_fun(Session) ->
    Receiver = get_receiver(Session),
    SenderFun = fun(Data) ->
            Receiver ! {data, Data},
            {ok, Data}
    end,
    SenderFun.

close_receiver(Session) ->
    Receiver = get_receiver(Session),
    Receiver ! done.
