-module(neuron_supervisor).
-behaviour(supervisor).
-export([start_link/1, start_link/2, start/1, start/2]).
-export([init/1]).
-export([force_stop/1]).
-export([get_x_y_from_sequence/2]).


% @doc Example start command: neuron_supervisor:start_link({local, testsup}, 10, {child_specs, 0, 200, {neuron_coordinates, 0}}).

start_link(Server_name,
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, Next_free_neuron}
        }
    ) ->
    Number_of_workers_per_supervisor = erlang:system_info(schedulers_online) * 4,
    supervisor:start_link(
        Server_name, 
        ?MODULE, 
        [Number_of_workers_per_supervisor, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, Next_free_neuron}
            }
        ]
    ).

start_link( 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, Next_free_neuron}
        }
    ) ->
    Number_of_workers_per_supervisor = erlang:system_info(schedulers_online) * 4,
    supervisor:start_link(
        ?MODULE, 
        [Number_of_workers_per_supervisor, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, Next_free_neuron}
            }
        ]
    ).

start(Server_name, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, Next_free_neuron}
        }
    ) ->
    Number_of_workers_per_supervisor = erlang:system_info(schedulers_online) * 4,
    supervisor:start(
        Server_name, 
        ?MODULE, 
        [Number_of_workers_per_supervisor, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, Next_free_neuron}
            }
        ]
    ).

start(
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, Next_free_neuron}
        }
    ) ->
    Number_of_workers_per_supervisor = erlang:system_info(schedulers_online) * 4,
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
    

    [SOM_x_edge_length, SOM_y_edge_length] = spectrum_dispatcher:get_minimum_som_dimensions({global, spectrum_dispatcher}),
    [X_max, Y_max] = [SOM_x_edge_length - 1, SOM_y_edge_length -1],

    
    Supervisor_specification = {
        one_for_one, 
        10,
        10
    },

    %~ Number_of_neurons = SOM_x_edge_length * SOM_y_edge_length,
    %~ Number_of_neurons_per_worker = (Number_of_neurons - Next_free_neuron) div Number_of_workers_per_supervisor +1,
    %~ io:format("Number of Neurons per worker: ~w~n", [Number_of_neurons_per_worker]),

    %~ Children_neuron_indeces = [
            %~ lists:seq(Lower_limit, min(Lower_limit + Number_of_neurons_per_worker-1, Number_of_neurons - 1))
            %~ || Lower_limit <- lists:seq(Next_free_neuron, Number_of_neurons, Number_of_neurons_per_worker) 
    %~ ],
    %~ io:format("Children_neuron_coordinate_indeces: ~w~n", [Children_neuron_indeces]),
    %~ Child_specification_list = 
        %~ [ 
            %~ {hd(Neuron_indeces), 
                %~ {neurons, start_link, [{global, spectrum_dispatcher}, Neuron_indeces, [X_max, Y_max], Iteration, Max_iteration]},
                %~ permanent,
                %~ 10000,
                %~ worker,
                %~ [neuron]
            %~ }
        %~ || Neuron_indeces <- Children_neuron_indeces
        %~ ],
        
    Child_specification_list = 
        [
            {Id,
                {neurons, start_link, [{global, spectrum_dispatcher}, [], [X_max, Y_max],Iteration, Max_iteration]},
                permanent,
                10000,
                worker,
                [neuron]
            }
            || Id <- lists:seq(1, Number_of_workers_per_supervisor)
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

