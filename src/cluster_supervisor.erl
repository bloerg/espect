-module(cluster_supervisor).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Cluster_cookie) ->
    supervisor:start_link(
        {global, ?MODULE}, 
        ?MODULE, 
        Cluster_cookie
    ).

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
        {spectrum_dispatcher, start_link, [{global, spectrum_dispatcher}, {filesystem, "/var/tmp/sine/binary/", binary, 1}, Iteration, Max_iteration]},
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



