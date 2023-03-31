-module(test_fds).
-export([count/0, push_map/1]).

count() -> count(1, []).

count(N, Fds) ->
  case file:open(integer_to_list(N), [write]) of
    {ok, F} ->
      count(N+1, [F| Fds]);

    {error, Err} ->
      [ file:close(F) || F <- Fds ],
      delete(N-1),
      {Err, N}
  end.

delete(0) -> ok;
delete(N) -> 
  case file:delete(integer_to_list(N)) of
    ok         -> ok;
    {error, _} -> meh
  end,

delete(N-1).

push_map(Map) -> 
  {ok, Riak_Pid} = riakc_pb_socket:start_link("143.198.108.90", 8087),
  Request = riakc_obj:new(<<"test">>, <<"a_thing">>, Map),
  riakc_pb_socket:put(Riak_Pid, Request),
  io:format("~p~n", [riakc_pb_socket:get(Riak_Pid, <<"test">>, <<"a_thing">>)]).