-module(query_facility_h).

-export([init/2]).

<<<<<<< Updated upstream
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
=======
init(Req0, State) ->
    {ok, otp_wrapper:store_query(Req0, State), State}.



    % {ok, Pid} = riakc_pb_socket:start("143.198.108.90", 8087),
    % MyBucket = <<"test">>,

    % Val1 = 1,
    % Obj1 = riakc_obj:new(MyBucket, <<"one">>, Val1),
    % riakc_pb_socket:put(Pid, Obj1),
    % {ok, Fetched1} = riakc_pb_socket:get(Pid, MyBucket, <<"one">>),
    % String = string:concat("[\"", binary_to_list(Fetched1)),
    % Req = cowboy_req:reply(200, #{
    %     <<"content-type">> => <<"text/plain">>
    % }, string:concat(String, "\"]"), Req8),
>>>>>>> Stashed changes
