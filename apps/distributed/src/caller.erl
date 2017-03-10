-module(caller).
-export([start/0]).

start()->
    erlang:set_cookie(node(),distribute),
    {ok,Hostname} = inet:gethostname(),
    Pid = self(),
    register(caller,Pid),
    Host = "distributor@" ++ Hostname,
    net_kernel:connect_node(list_to_atom(Host)).

