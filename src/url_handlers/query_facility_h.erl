-module(query_facility_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	[Name|_] = jsx:decode(Data),
    Worker_PID = distributor:get_worker(fac_dist, Name),
	City = jsx:encode(query_facility:get_city_of(Name,Worker_PID)),
	%io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, City, Req0),
	{ok, Req, Opts}.
