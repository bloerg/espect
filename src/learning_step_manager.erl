-module(learning_step_manager).
-beaviour(gen_server).

-export([start/0, start_link/0, start/1, start_link/1]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2, handle_call/3]).
-export([init/1]).
-export([next_learning_step/0, compare_complete/0, update_complete/0]).

-record(learning_step_manager_state, {
    neurons_worker_list = []
}).

%%Start an iteration state server

start(Server_name) ->
    gen_server:start(Server_name, ?MODULE, [], []). 

start_link(Server_name) ->
    gen_server:start_link(Server_name, ?MODULE, [], []).

start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).

terminate(_Reason, _Neuron_state) ->
    ok
    .

init(_State) ->
    {ok, #learning_step_manager_state{}}.

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

next_learning_step() ->
    gen_server:cast(?MODULE, next_learning_step).

% triggered by bmu_manager after all neurons have sent their compare results
% and the bmu_manager has found a bmu
compare_complete() ->
    gen_server:cast(?MODULE, compare_complete).

% triggered by individual neuron workers when they have updated their share of neurons
update_complete() ->
    gen_server:call(?MODULE, update_complete).

handle_cast(next_learning_step, State) ->
    %~ erlang:display({"next learning step, bmu: ", bmu_manager:get_bmu(bmu_manager)}),
    ok = spectrum_dispatcher:next_learning_step(),
    %ok = neuron_event_handler:trigger_neuron_update({update, bmu_manager:get_bmu(bmu_manager)}), 
    {noreply, State};
    
% called when all neuron workers have compared a spectrum to all the neurons
% gets the workers registered at the neuron_event_handler for learning_step_manager_state
% then initiates the update of the neurons
handle_cast(compare_complete, State) ->
    Neurons_worker_list = gen_event:which_handlers(neuron_event_manager),
    New_state = State#learning_step_manager_state{neurons_worker_list = filter_neurons_worker_list(Neurons_worker_list)},
    ok = neuron_event_handler:trigger_neuron_update({update, bmu_manager:get_bmu(bmu_manager)}),
    {noreply, New_state}.
    
handle_call(update_complete, From, State) ->
    {From_pid, _From_tag} = From,
    % remove the pid of the sending neurons worker from the list of
    % registered workers. This is to keep track of the responses of the neuron workers
    Neurons_worker_list_new = lists:takewhile(
        fun(Pid) -> Pid =/= From_pid end, 
        State#learning_step_manager_state.neurons_worker_list
    ),
    case length(Neurons_worker_list_new) of
        0 ->
            ok = spectrum_dispatcher:next_learning_step();
        Other_number -> 
            erlang:display({"updated another neurons", Other_number}),
            ok
    end,
    {reply, ok, State#learning_step_manager_state{ neurons_worker_list = Neurons_worker_list_new}}.
