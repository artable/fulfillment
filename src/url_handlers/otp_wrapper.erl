-module(otp_wrapper).
-export([store_query/2, store_query_veh/2, push_query/2]).

store_query(Req0, [Distributor]) ->
    {ok,Data,_} = cowboy_req:read_body(Req0),
    Map = jsx:decode(Data),
    Holder_UUID = maps:get(<<"holder_uuid">>, Map),
    Pack_UUID = maps:get(<<"package_uuid">>, Map),
    Time = maps:get(<<"time_stamp">>, Map),
    {ok, Worker} = distributor:call(Distributor),
    case gen_server:call(Worker, {put, {Pack_UUID, Holder_UUID, Time}}) of
        fail -> Req0;
        _ -> cowboy_req:reply(200, Req0)
    end.

store_query_veh(Req0, [Distributor]) ->
    {ok,Data,_} = cowboy_req:read_body(Req0),
    Map = jsx:decode(Data, [return_maps]),
    Location = maps:get(<<"location">>, Map),
    UUID = maps:get(<<"vehicle_uuid">>, Map),
    Time = maps:get(<<"time_stamp">>, Map),
    {ok, PID} = distributor:call(Distributor),
    case gen_server:call(PID, {get, {UUID, Location, Time}}) of
        fail -> Req0;
        _ -> cowboy_req:reply(200, Req0)
    end.

push_query(Req0, [Distributor]) ->
    {ok,Worker} = distributor:call(Distributor),
    {ok,Data,_} = cowboy_req:read_body(Req0),
    Map = jsx:decode(Data),
    case maps:is_key(<<"package_uuid">>, Map) of
        true -> UUID = maps:get(<<"package_uuid">>, Map);
        false -> case maps:is_key(<<"vehicle_uuid">>, Map) of
                    true -> UUID = maps:get(<<"vehicle_uuid">>, Map);
                    false -> case maps:is_key(<<"facility_uuid">>, Map) of
                                true -> UUID = maps:get(<<"facility_uuid">>, Map);
                                false -> UUID = fail
                            end
                end
    end,
    case gen_server:call(Worker,{get, UUID}) of
        fail -> Req0;
        Data -> cowboy_reply:reply(200, 
            #{<<"content-type">> => <<"text/json">>},
            Data,
            Req0)
    end.
