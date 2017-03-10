-module(distributor).
-export([start/0]).

start()->
    Pid = spawn(fun()->listen_for_function()end),
    register(distributor,Pid),
    erlang:set_cookie(node(),distribute).


listen_for_function()->
    receive
        X ->
           Worker = get_worker(),
            {worker, Worker} ! X
    end,
    listen_for_function().

get_worker()->
    List_of_nodes =  nodes(),
    Workers = lists:flatten(filter_nodes(List_of_nodes)),
    Node_and_size = get_task_number(Workers),
    Node = get_node(Node_and_size,{default,999}),
    Node.
    
get_task_number([])->
    [];
get_task_number([H|T])->
    Ref = make_ref(),
    {tasks, H} ! {size,node(),Ref},
        Size = receive 
                   {Number,Ref} ->
                       Number
               end,
    [{H,Size}|get_task_number(T)].
    
get_node([],Result)->
    {Node,_} = Result,
    Node;
get_node([H|T],Lowest_node)->
    {_,Task_number} = H,
    {_,Prev_Task_number} = Lowest_node,
    Result = case Task_number =< Prev_Task_number of
                 true ->
                     H;
                 false ->
                     Lowest_node
             end,
    get_node(T,Result).

filter_nodes(Nodes)->
    Pred = fun(N)-> string:str(atom_to_list(N),"bob") == 1 end,
    lists:filter(Pred,Nodes).
                   
