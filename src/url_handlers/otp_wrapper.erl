-module(otp_wrapper).
-export([store_query/2, push_query/2]).

store_query(Req0, [Distributor]) ->
    {ok,Data,_} = cowboy_req:read_body(Req0),
    [UUID,Place,_Time] = jsx:decode(Data),
    Worker = distributor:call(Distributor),
    case gen_server:call(Worker, {UUID, Place, _Time}) of
        fail -> Req0;
        _ -> cowboy_req:reply(200, Req0)
    end.

push_query(Req0, [Distributor]) ->
    Worker = distributor:call(Distributor),
    {ok,Data,_} = cowboy_req:read_body(Req0),
    [UUID,_] = jsx:decode(Data),
    case gen_server:call(Worker,{get, UUID}) of
        fail -> Req0;
        Data -> cowboy_reply:reply(200, 
            #{<<"content-type">> => <<"text/json">>},
            Data,
            Req0)
    end.
