-module(worker).
-export([start/0]).

start()->
    Pid = spawn(fun()->listen_for_task()end),
    register(worker,Pid),
    Pid2 = spawn(fun()->handle_task_list([])end),
    register(tasks,Pid2),
    Pid3 = spawn(fun()->execute_tasks()end),
    register(execute,Pid3),  
    erlang:set_cookie(node(),distribute),
    connect_to_distributor().


listen_for_task()->
    receive
        X ->
            tasks ! {add,X}
    end,
    listen_for_task().

connect_to_distributor()->
    {ok,Hostname} = inet:gethostname(),
    Host = "distributor@" ++ Hostname,
    net_kernel:connect_node(list_to_atom(Host)).

handle_task_list(List)->
    New_list =  receive
                    {add,X} ->
                        Result = format_input(X),
                        case Result of
                            [Fun,Pid] ->
                                List ++ [{Fun,Pid,erlang:timestamp()}];
                            _->
                                List
                        end;
                    {size,From,Ref} ->
                        {distributor,From} ! {length(List),Ref},
                        List;
                    {pop,From,Ref} ->
                        case List of 
                            [H|T] ->
                                From ! {H, Ref},
                                T;
                            []->
                                []
                        end                        
                end,
    handle_task_list(New_list).


execute_tasks()->
    Ref = make_ref(),
    tasks ! {pop, self(), Ref},
    receive
        {X, Ref} ->
            check_timestamp(X)
    after 50 ->
            do_nothing
    end,
        execute_tasks().
        
check_timestamp(Task)->
    {Function,Pid,Timestamp} = Task,
    Time_now = erlang:timestamp(),
    case timer:now_diff(Time_now,Timestamp) of
        Result when Result =< 1000000 ->
            do_execute(Function,Pid);
        Result when Result > 1000000 ->
            send_response(Function,Pid)
    end.


do_execute(Function,Pid)->    
    try Function() of
        Result ->
            Pid ! Result
    catch
        error:Error ->
            Pid ! Error
    end.

send_response(Function,Pid)->
    Pid ! io:format("Function: ~p has been timed out ~n",[Function]).

format_input(X)->
    case X of
        {Fun,Pid} ->
            [Fun,Pid];
        [Fun,Pid] ->
            [Fun,Pid];
        _ ->
            io:format("Incorrect format: ~p~n",[X]),
            []
    end.
           
    
