-module(r_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include_lib("eunit/include/eunit.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = echo(Method, Req2),
    {ok, Req3, State}.

gen_path() ->
    Ref = make_ref(),
    Sha = crypto:hash(sha512, list_to_binary(erlang:ref_to_list(Ref))),
    Path = lists:map(fun(Bin) -> io_lib:format("~.16B", [Bin]) end, binary_to_list(Sha)),
    list_to_binary(lists:flatten(Path)).

echo(<<"GET">>, Req) ->
    Path = gen_path(),
    Url = momental_storage_url:schemeful([<<"/d/">>, Path]),
    cowboy_req:reply(302,
                     [{<<"location">>, Url}],
                     [],
                     Req);
echo(_, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
    ok.
