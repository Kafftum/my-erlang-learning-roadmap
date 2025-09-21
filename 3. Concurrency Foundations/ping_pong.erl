-module(ping_pong).
-export([start/0, ping/1, pong/0]).

start() ->
  %% Handle starting processes
  ProcessList = rec_start([], 3, 0),
  lists:map(fun ping/1, ProcessList).

rec_start(Acc, Count, Idx) when Idx < Count ->
  NewAcc = [spawn(?MODULE, pong, []) | Acc],
  rec_start(NewAcc, Count, Idx + 1);
rec_start(Acc, _, _) ->
  Acc.

ping(PID) ->
  PID ! {self(), ping},
  receive
    {From, pong} -> io:format("Pong! Coming from: [~p]~n", [From]);
    {From, {error, Error}} -> io:format("Worker: ~p~nError: ~p~n", [From, Error]);
     _ -> io:format("*cricket noises*~n")
  end.

pong() ->
  receive 
    {From, ping} -> From ! {self(), pong};
    {From, _} -> From ! {self(), {error, <<"Unregistered message">>}};
    _ -> io:format("Uh Oh! Malformed message sent to [~p]~n", [self()])
  end.