-module(worker).
-export([start/0]).

start()->
    Task_listener = spawn(fun()->listen_for_task()end),
    register(worker,Task_listener),
    Task_handler = spawn(fun()->handle_task_list([])end),
    register(tasks,Task_handler),
    Task_executer = spawn(fun()->execute_tasks()end),
    register(execute,Task_executer),  
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
            execute_if_not_expired(X)
    after 50 ->
            do_nothing
    end,
        execute_tasks().
   
execute_if_not_expired({Function,Pid,Timestamp})->    
    case has_expired(Timestamp) of        
        true ->
            send_response(Function,Pid);
        false ->
            do_execute(Function,Pid)
    end.

has_expired(Timestamp)->
    Time_now = erlang:timestamp(),
    timer:now_diff(Time_now,Timestamp) > 1000000.



do_execute(Function,Pid)->    
    try Function() of
        Result ->
            Pid ! Result
    catch
        error:Error ->
            Pid ! Error
    end.





send_response(Function,Pid)->
    Pid ! io_lib:format("Function: ~p has been timed out ~n",[Function]).

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
           
    
