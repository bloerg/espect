-module(learning_step_manager).
-beaviour(gen_server).

-export([start/0, start_link/0, start/1, start_link/1]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
-export([set_iteration/2]).
-export([init/1]).
-export([next_learning_step/0, compare_complete/3, update_complete/1]).

-record(learning_step_manager_state, {
    neurons_worker_list = [],
    learning_step = 1,
    iteration = 1,
    max_iteration = 200,
    learning_step_start_timestamp = 0, % for statistics
    learning_step_runtime_sum = 0 % for statistics
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
    gen_server:cast({global, ?MODULE}, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).

terminate(_Reason, _Neuron_state) ->
    ok
    .

init(_State) ->
    %~ gen_event:add_handler({global, iteration_event_manager}, {iteration_event_handler, self()}, [{pid, self()}, {module, ?MODULE}]),
    {ok, #learning_step_manager_state{learning_step_start_timestamp = os:timestamp()}}.

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

remove_pid_from_list([], _Pid) ->
    [];
remove_pid_from_list(Pid_list, Pid) ->
    remove_pid_from_list(Pid_list, [], Pid).
    
remove_pid_from_list([], Filtered_pid_list, _Pid) ->
    Filtered_pid_list;
remove_pid_from_list(Pid_list, Filtered_pid_list, Pid) ->
    [Head|Tail] = Pid_list,
    case Head of
        Pid -> remove_pid_from_list(Tail, Filtered_pid_list, Pid);
        _Other -> remove_pid_from_list(Tail, [Head|Filtered_pid_list], Pid)
    end.


set_iteration(Server_name, New_iteration) ->
    gen_server:cast(Server_name, {set_iteration, New_iteration}).

next_learning_step() ->
    gen_server:cast({global, ?MODULE}, next_learning_step).

% triggered by bmu_manager after all neurons have sent their compare results
% and the bmu_manager has found a bmu
compare_complete(BMU_neurons_worker_pid, BMU_coordinates, BMU_spectrum_metadata) ->
    gen_server:cast({global, ?MODULE}, {compare_complete, BMU_neurons_worker_pid, BMU_coordinates, BMU_spectrum_metadata}).

% triggered by individual neuron workers when they have updated their share of neurons
update_complete(From) ->
    gen_server:cast({global, ?MODULE}, {update_complete, From}).

handle_cast(next_learning_step, State) ->
    New_timestamp = os:timestamp(),
    Time_diff = timer:now_diff(New_timestamp, State#learning_step_manager_state.learning_step_start_timestamp)/1000000,
    %~ io:format("Starting learning step ~w. Previous learning step took ~w seconds. Mean LS time is ~w~n", 
        %~ [
         %~ State#learning_step_manager_state.learning_step +1, 
         %~ Time_diff,
         %~ (State#learning_step_manager_state.learning_step_runtime_sum + Time_diff) / ((State#learning_step_manager_state.iteration * State#learning_step_manager_state.learning_step) +1)
        %~ ]
    %~ ),
    case spectrum_dispatcher:next_learning_step() of
        nospectraleft -> iteration_state_server:next_iteration();
        ok -> 
            ok = bmu_manager:next_learning_step(),
            ok = neuron_event_handler:trigger_neuron_compare(
                {compare, spectrum_dispatcher:get_spectrum_id()}
            )
    end,
    {noreply, 
        State#learning_step_manager_state{
            learning_step = State#learning_step_manager_state.learning_step + 1,
            learning_step_start_timestamp = New_timestamp,
            learning_step_runtime_sum = State#learning_step_manager_state.learning_step_runtime_sum + Time_diff
        }
    };
    
% called when all neuron workers have compared a spectrum to all the neurons
% sets the bmu in the neurons worker containing the BMU-neuron
% gets the workers registered at the neuron_event_handler for learning_step_manager_state
% then initiates the update of the neurons
handle_cast({compare_complete, BMU_neurons_worker_pid, BMU_coordinates, BMU_spectrum_metadata}, State) ->
    %~ io:format("called handle_cast ~w~n", [{compare_complete, BMU_neurons_worker_pid, BMU_coordinates, BMU_spectrum_metadata}]),
    ok = neurons:set_bmu(
        BMU_neurons_worker_pid, BMU_coordinates, BMU_spectrum_metadata
    ),
    Neurons_worker_list = gen_event:which_handlers({global, neuron_event_manager}),
    New_state = State#learning_step_manager_state{neurons_worker_list = filter_neurons_worker_list(Neurons_worker_list)},
    ok = neuron_event_handler:trigger_neuron_update({update, bmu_manager:get_bmu({global, bmu_manager})}),
    {noreply, New_state};
    
handle_cast({update_complete, From_pid}, State) ->
    % remove the pid of the sending neurons worker from the list of
    % registered workers. This is to keep track of the responses of the neuron workers
    
    Neurons_worker_list_new = remove_pid_from_list(
        State#learning_step_manager_state.neurons_worker_list,
        From_pid
    ),
    
    case length(Neurons_worker_list_new) of
        0 ->
            spectrum_dispatcher:next_learning_step(),
            ok = bmu_manager:next_learning_step(),
            ok = next_learning_step();
        _Other_number -> 
            ok
    end,
    {noreply, State#learning_step_manager_state{ neurons_worker_list = Neurons_worker_list_new}};
    
handle_cast({set_iteration, New_iteration}, State) ->
    {noreply, State#learning_step_manager_state{
        iteration = New_iteration,
        learning_step = 1} 
    }.
