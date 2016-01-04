-module(cluster_supervisor).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([distribute_neuron_indeces/0]).

start_link(Cluster_cookie) ->
    supervisor:start_link(
        {global, ?MODULE}, 
        ?MODULE, 
        Cluster_cookie
    ).



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

% @doc distributes the indeces over all neuron workers after initialisation
% this has to happen before computations starts and after at least the cluster supervisor was started
% even better, if the initial set of cluster nodes is started, too
distribute_neuron_indeces() ->
    Neurons_worker_list = filter_neurons_worker_list(gen_event:which_handlers({global, neuron_event_manager})),
    Number_of_workers = length(Neurons_worker_list),
    [SOM_x_edge_length, SOM_y_edge_length] = spectrum_dispatcher:get_minimum_som_dimensions({global, spectrum_dispatcher}),
    Number_of_neurons = SOM_x_edge_length * SOM_y_edge_length,
    Number_of_neurons_per_worker = Number_of_neurons div Number_of_workers +1,
    Neurons_indeces = [
        lists:seq(Lower_limit, min(Lower_limit + Number_of_neurons_per_worker-1, Number_of_neurons - 1))
        || Lower_limit <- lists:seq(0, Number_of_neurons, Number_of_neurons_per_worker) 
    ],

    lists:foreach(fun({Neuron_worker_pid, Neuron_indeces}) ->
        neurons:set_neuron_indeces(Neuron_worker_pid, Neuron_indeces)
    end,
    lists:zip(Neurons_worker_list, Neurons_indeces))

.
    

init(Cluster_cookie) ->
    erlang:set_cookie(node(), Cluster_cookie),
    Iteration = application:get_env(iteration),
    Max_iteration = application:get_env(max_iteration),
    
    Iteration_state_server_spec = {
        iteration_state_server, 
        {iteration_state_server, start_link, [{global, iteration_state_server}, Iteration, Max_iteration]},
        temporary,
        10000,
        worker,
        [iteration_state_server]
    },
    
    Learning_step_manager_spec = {
        learning_step_manager, 
        {learning_step_manager, start_link, [{global, learning_step_manager}]},
        temporary,
        10000,
        worker,
        [learning_step_manager]
    },

    Iteration_event_manager_spec = {
        iteration_event_manager, 
        {iteration_event_handler, start_link, [{global, iteration_event_manager}]},
        temporary,
        10000,
        worker,
        [iteration_event_manager]
    },
    
    Spectrum_dispatcher_spec = {
        spectrum_dispatcher, 
        {spectrum_dispatcher, start_link, [{global, spectrum_dispatcher}, {filesystem, application:get_env(spectra_directory), binary, 1}, Iteration, Max_iteration]},
        temporary,
        10000,
        worker,
        [spectrum_dispatcher]
    },
    
    Neuron_event_manager_spec = {
        neuron_event_manager, 
        {neuron_event_handler, start_link, [{global, neuron_event_manager}]},
        temporary,
        10000,
        worker,
        [neuron_event_handler]
    },    

    BMU_manager_spec = {
        bmu_manager, 
        {bmu_manager, start_link, [{global, bmu_manager}, Iteration, Max_iteration]},
        temporary,
        10000,
        worker,
        [neuron_event_handler]
    },
    
    SOM_manager_spec = {
        som_manager, 
        {som_manager, start_link, [{global, som_manager}, Iteration, Max_iteration]},
        temporary,
        10000,
        worker,
        [neuron_event_handler]
    },

    
    Supervisor_specification = {
        one_for_one, 
        10,
        10
    },
    Child_specification_list = 
        [ 
            Iteration_state_server_spec,
            Learning_step_manager_spec,
            Iteration_event_manager_spec,
            Spectrum_dispatcher_spec,
            Neuron_event_manager_spec,
            BMU_manager_spec,
            SOM_manager_spec
        ],
    {ok, 
        {
        Supervisor_specification, 
        Child_specification_list
        }
    }.



