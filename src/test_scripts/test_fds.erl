-module(test_fds).
-export([count/0]).

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