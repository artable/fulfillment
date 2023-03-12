-module(toppage_h).
-export([init/2]).

init(Req0, Opts) ->
    DIE = 1 / 0,
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
        }, "[\"henlo worlb\"]", Req0),
    {ok, Req, Opts}.