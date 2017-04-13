-module(caller).
-export([start/0]).

start()->
    erlang:set_cookie(node(),distribute),
    {ok,Hostname} = inet:gethostname(),
    Pid = self(),
    register(caller,Pid),
    Host = "distributor@" ++ Hostname,
    net_kernel:connect_node(list_to_atom(Host)),
    Listener = spawn(fun()->listen_for_result()end),
    register(listener,Listener).

listen_for_result()->
    receive 
        X ->
            io:format("~p~n",[X])            
    end,
    listen_for_result().

