-module(query_package_info_h).

-export([init/2]).

init(Req8, Opts) ->
    {ok, Pid} = riakc_pb_socket:start("143.198.108.90", 8087),
    MyBucket = <<"test">>,

    Val1 = 1,
    Obj1 = riakc_obj:new(MyBucket, <<"one">>, Val1),
    riakc_pb_socket:put(Pid, Obj1),
    {ok, Fetched1} = riakc_pb_socket:get(Pid, MyBucket, <<"one">>),
    String = string:concat("[\"", binary_to_list(Fetched1)),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, string:concat(String, "\"]"), Req8),
    {ok, Req, Opts}.