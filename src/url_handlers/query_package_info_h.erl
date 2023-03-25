-module(query_package_info_h).

-export([init/2]).

init(Req8, Distributor) ->
    Req = otp_wrapper:push_query(Req8,Distributor),
	{ok, Req, Distributor}.
