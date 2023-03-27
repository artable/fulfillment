-module(otp_wrapper).
-export([store_query/2, store_query_veh/2, push_query/2]).

store_query(Req0, [Distributor]) ->
    {ok,Data,_} = cowboy_req:read_body(Req0),
    {Pack_UUID,Holder_UUID,Time} = jsx:decode(Data),
    Worker = distributor:call(Distributor),
    case gen_server:call(Worker, {Pack_UUID, Holder_UUID, Time}) of
        fail -> Req0;
        _ -> cowboy_req:reply(200, Req0)
    end.

store_query_veh(Req0, [Distributor]) ->
    {ok,Data,_} = cowboy_req:read_body(Req0),
    Map = jsx:decode(Data, [return_maps]),
    Location = maps:get(<<"location">>, Map),
    UUID = maps:get(<<"vehicle_uuid">>, Map),
    Worker = distributor:call(Distributor),
    case gen_server:call(Worker, {UUID, Location}) of
        fail -> Req0;
        _ -> cowboy_req:reply(200, Req0)
    end.

push_query(Req0, [Distributor]) ->
    Worker = distributor:call(Distributor),
    {ok,Data,_} = cowboy_req:read_body(Req0),
    {UUID,_} = jsx:decode(Data),
    case gen_server:call(Worker,{get, UUID}) of
        fail -> Req0;
        Data -> cowboy_reply:reply(200, 
            #{<<"content-type">> => <<"text/json">>},
            Data,
            Req0)
    end.
