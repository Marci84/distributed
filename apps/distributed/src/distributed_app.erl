%%%-------------------------------------------------------------------
%% @doc distributed public API
%% @end
%%%-------------------------------------------------------------------

-module(distributed_app).

%%-behaviour(application).

%% Application callbacks
-export([start/1]).

%%====================================================================
%% API
%%====================================================================

%% start(_StartType, _StartArgs) ->
%%     distributed_sup:start_link().

%% %%--------------------------------------------------------------------
%% stop(_State) ->
%%     ok.

%%====================================================================
%% Internal functions
%%====================================================================


start(Function)->
    case Function of
        distributor ->
            check_start(distributor);
        worker ->
            check_start(worker);
        caller ->
            check_start(caller);
        _ ->
            io:format("Invalid function : ~p~n",[Function])
    end.

check_start(Type)->
    case [whereis(distributor),whereis(worker),whereis(caller)] of
        [undefined,undefined,undefined] ->
            set_nodename(Type),
            handle_start(Type);        
        [_,_,_] ->
            io:format("Function has been already started.~n")
    end.

handle_start(Type)->
    case Type of
        distributor ->
            distributor:start();
        worker ->
            worker:start();
        caller ->
            caller:start()        
    end.

set_nodename(Type)->
    case Type of
        worker ->
            Number = integer_to_list(rand:uniform(1000)),
            Full_name= list_to_atom("bob" ++ Number),
            net_kernel:start([Full_name, shortnames]);
        distributor ->
            net_kernel:start([distributor, shortnames]);
        caller ->
            net_kernel:start([caller,shortnames])
    end.

