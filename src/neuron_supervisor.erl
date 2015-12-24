-module(neuron_supervisor).
-behaviour(supervisor).
-export([start_link/2, start_link/3, start/2, start/3]).
-export([init/1]).
-export([force_stop/1]).
-export([get_x_y_from_sequence/2]).


% @doc Example start command: neuron_supervisor:start_link({local, testsup}, 10, {child_specs, 0, 200, {neuron_coordinates, 0}}).

start_link(Server_name, Number_of_workers_per_supervisor, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, Next_free_neuron}
        }
    ) ->
    supervisor:start_link(
        Server_name, 
        ?MODULE, 
        [Number_of_workers_per_supervisor, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, Next_free_neuron}
            }
        ]
    ).

start_link(Number_of_workers_per_supervisor, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, Next_free_neuron}
        }
    ) ->
    supervisor:start_link(
        ?MODULE, 
        [Number_of_workers_per_supervisor, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, Next_free_neuron}
            }
        ]
    ).

start(Server_name, Number_of_workers_per_supervisor, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, Next_free_neuron}
        }
    ) ->
    supervisor:start(
        Server_name, 
        ?MODULE, 
        [Number_of_workers_per_supervisor, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, Next_free_neuron}
            }
        ]
    ).

start(Number_of_workers_per_supervisor, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, Next_free_neuron}
        }
    ) ->
    supervisor:start(
        ?MODULE, 
        [Number_of_workers_per_supervisor, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, Next_free_neuron}
            }
        ]
    ).
    

%% TODO: Check whether this really gives the right numbers
%% X is supposed to start at 0, as well as the index, so (0,0) corresponds to 0
get_x_y_from_sequence(X_max, Sequence_number) ->
    [Sequence_number rem (X_max +1), Sequence_number div (X_max + 1)].


init([Number_of_workers_per_supervisor, 
        {child_specs, Iteration, Max_iteration, 
            {neuron_coordinates, Next_free_neuron}
        }
    ]) ->
    
    %start iteration state server
    iteration_state_server:start({global, iteration_state_server}, Iteration, Max_iteration),
    
    %start learning step manager
    learning_step_manager:start({global, learning_step_manager}),

    %start iteration event manager
    iteration_event_handler:start_link({local, iteration_event_manager}),
    
    %start spectrum dispatcher and determine SOM size from number of spectra files
    spectrum_dispatcher:start_link({local, spectrum_dispatcher}, {filesystem, "/var/tmp/sine/", binary, 1}, Iteration, Max_iteration),
    [X_max, Y_max] = spectrum_dispatcher:get_minimum_som_dimensions(spectrum_dispatcher),
    
    %Start neuron event manager
    neuron_event_handler:start_link({local, neuron_event_manager}),
    
    %Start BMU manager
    bmu_manager:start_link({local, bmu_manager}, Iteration, Max_iteration),
    
    Supervisor_specification = {
        one_for_one, 
        10,
        10
    },
    % make a list of {x,y} coordinate tuples for neurons
    %Children_coordinates = [get_x_y_from_sequence(X_max, Neuron_index) || Neuron_index <- lists:seq(Next_free_neuron, Next_free_neuron + Number_of_workers_per_supervisor)],
    Number_of_neurons = X_max * Y_max,
    Number_of_neurons_per_worker = (Number_of_neurons - Next_free_neuron) div Number_of_workers_per_supervisor + (Number_of_neurons - Next_free_neuron) rem Number_of_workers_per_supervisor,
    Children_neuron_coordinate_ranges = [
            [Lower_limit, min(Lower_limit + Number_of_neurons_per_worker-1, Number_of_neurons)] 
            || Lower_limit <- lists:seq(Next_free_neuron, Number_of_neurons, Number_of_neurons_per_worker) 
    ],
    Child_specification_list = 
        [ 
            {Neuron_coordinate_range, 
                {neurons, start_link, [spectrum_dispatcher, Neuron_coordinate_range, [X_max, Y_max], Iteration, Max_iteration]},
                permanent,
                10000,
                worker,
                [neuron]
            }
        || Neuron_coordinate_range <- Children_neuron_coordinate_ranges
        ],
    {ok, 
        {
        Supervisor_specification, 
        Child_specification_list
        }
    }.

% @doc for shell testing
force_stop(PID) ->
    exit(PID, shutdown).

