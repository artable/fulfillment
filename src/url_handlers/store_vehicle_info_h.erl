-module(store_vehicle_info_h).

-export([init/2]).

init(Req8, Distributor) ->
    Req = otp_wrapper:store_query_veh(Req8,Distributor),
	{ok, Req, Distributor}.