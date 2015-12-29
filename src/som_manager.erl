-module(som_manager).
-beaviour(gen_server).

-export([start/2, start_link/2, start/3, start_link/3]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2, handle_call/3]).
-export([handle_info/2]).
-export([init/1]).
-export([dump_som/1]).


start(Server_name, Iteration, Max_iteration) ->
    gen_server:start(
        Server_name, 
        ?MODULE, 
        [Iteration, Max_iteration],
        []
    ). 

start_link(Server_name, Iteration, Max_iteration) ->
    gen_server:start_link(
        Server_name, 
        ?MODULE, 
        [Iteration, Max_iteration],
        []
    ).
start(Iteration, Max_iteration) ->
    gen_server:start(
        ?MODULE, 
        [Iteration, Max_iteration],
        []
    ).

start_link(Iteration, Max_iteration) ->
    gen_server:start_link(
        ?MODULE, 
        [Iteration, Max_iteration],
        []
    ).


stop() ->
    gen_server:cast({global, ?MODULE}, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).

terminate(_Reason, _Neuron_state) ->
    ok
    .


init(State) ->
    {ok, State}.


% Takes List like [{neuron_event_handler,{neuron,<0.91.0>}}, {neuron_event_handler,{neuron,<0.91.0>}},...]
% as input and outputs a list conainting neurons worker pids
filter_neurons_worker_list(Neurons_worker_list) ->
    filter_neurons_worker_list(Neurons_worker_list, []).
filter_neurons_worker_list([], Filtered_list) ->
    Filtered_list;
filter_neurons_worker_list(Neurons_worker_list, Filtered_list) -> 
    [Head|Tail] = Neurons_worker_list,
    case Head of
        {neuron_event_handler,{neuron, Pid}} ->
            filter_neurons_worker_list(Tail, [Pid|Filtered_list]);
        _Other -> filter_neurons_worker_list(Tail, Filtered_list)
    end.

dump_som(Output_file) ->
    gen_server:call({global, ?MODULE}, {dump_som, Output_file}, 60000).

handle_call({dump_som, Output_file}, _From, State) ->
    {ok, F} = file:open(Output_file, write),
    Neurons_worker_list = filter_neurons_worker_list(gen_event:which_handlers({global, neuron_event_manager})),
    lists:foreach(fun(Neuron_worker_pid) ->
        lists:foreach(fun(Line) ->
            io:format(F, "~w~n", [lists:flatten(Line)])
        end,
        binary_to_term(neurons:map_dump(Neuron_worker_pid)))
    end,
    Neurons_worker_list),

    {reply, ok, State}
.


handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Message, State) ->
    io:format("Unexpected message: ~w~n",[{Message, State}]).

