-module(otp_wrapper).
-export([store_query/2, push_query/2]).

store_query(Req0, Distributor) ->
    Req_dec = jsx:decode(Req0),
    Worker = distributor:call(Distributor),
    case gen_server:call(Worker, Req_dec) of
        fail -> Req0;
        _ -> cowboy_req:reply(200, Req0)
    end.

push_query(Req0, Distributor) ->
    Worker = distributor:call(Distributor),
    case gen_server:call(Worker,jsx:decode(Req0)) of
        fail -> Req0;
        Data -> cowboy_reply:reply(200, 
            #{<<"content-type">> => <<"text/json">>},
            Data,
            Req0)
    end.
