-module(supervisor_process).
-export([start/0, supervisor/2, worker/3]).

start() ->
  spawn(?MODULE, supervisor, [0, true]).

supervisor(FuncDir, true) ->
  process_flag(trap_exit, true),
  _Worker = spawn_link(?MODULE, worker, [self(), FuncDir]),
  receive
    {From, ping} -> 
      io:format("Ping from: [Worker - ~p]~n", [From]),
      supervisor(FuncDir, false);
    
    {"Exit", _, normal} -> 
      ok;   
    
    {"Exit", _, shutdown} -> 
      ok;

    {"Exit", _, _} -> 
      supervisor(FuncDir + 1, true)
  end;

supervisor(FuncDir, _StartProc) when _StartProc =:= false ->
  receive
    {From, ping} -> 
      io:format("Ping from: [Worker - ~p]~n", [From]),
      supervisor(FuncDir, false);
    
    {"Exit", _, normal} -> 
      ok;   
    
    {"Exit", _, shutdown} -> 
      ok;

    {"Exit", _, _} -> 
      supervisor(FuncDir + 1, true)
  end.

worker(SupervisorPid, FuncDir = 3, _StartProc = true) ->
  ProcessList = ping_pong:start_and_ret_proc(),
  [ping_pong:ping(Process, empty) || Process <- ProcessList],
  receive
    {_From, pong} -> 
      io:format("Pong!~n"),
      SupervisorPid ! {self(), ping},
      worker(SupervisorPid, FuncDir, false),
      exit(normal)
  end;

worker(SupervisorPid, _FuncDir, _StartProc = true) ->
  ProcessList = ping_pong:start_and_ret_proc(),
  [ping_pong:ping(Process, empty) || Process <- ProcessList],
  receive
    {_From, pong} -> 
      io:format("Pong!~n"),
      SupervisorPid ! {self(), ping},
      worker(SupervisorPid, _FuncDir, false),
      exit(kill)
  end;

worker(SupervisorPid, _FuncDir, _StartProc = false) ->
  receive
    {_From, pong} -> 
      io:format("Pong!~n"),
      SupervisorPid ! {self(), ping},
      worker(SupervisorPid, _FuncDir, false),
      exit(kill)
  end.