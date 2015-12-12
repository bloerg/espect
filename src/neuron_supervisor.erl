-module(neuron_supervisor).
-behaviour(supervisor).
-export([start_link/2, start_link/3, start/2, start/3]).
-export([init/1]).
-export([force_stop/1]).
-export([get_x_y_from_sequence/2]).


% @doc Example start command: neuron_supervisor:start_link({local, testsup}, 10, {child_specs, 0, 200, {neuron_coordinates, 500, 500, 0}}).

start_link(Server_name, Number_of_workers, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, X_max, Y_max, Next_free_neuron}
        }
    ) ->
    supervisor:start_link(
        Server_name, 
        ?MODULE, 
        [Number_of_workers, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, X_max, Y_max, Next_free_neuron}
            }
        ]
    ).

start_link(Number_of_workers, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, X_max, Y_max, Next_free_neuron}
        }
    ) ->
    supervisor:start_link(
        ?MODULE, 
        [Number_of_workers, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, X_max, Y_max, Next_free_neuron}
            }
        ]
    ).

start(Server_name, Number_of_workers, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, X_max, Y_max, Next_free_neuron}
        }
    ) ->
    supervisor:start(
        Server_name, 
        ?MODULE, 
        [Number_of_workers, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, X_max, Y_max, Next_free_neuron}
            }
        ]
    ).

start(Number_of_workers, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinates, X_max, Y_max, Next_free_neuron}
        }
    ) ->
    supervisor:start(
        ?MODULE, 
        [Number_of_workers, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinates, X_max, Y_max, Next_free_neuron}
            }
        ]
    ).
    

%% TODO: Check whether this really gives the right numbers
%% X is supposed to start at 0, as well as the index, so (0,0) corresponds to 0
get_x_y_from_sequence(X_max, Sequence_number) ->
    {Sequence_number rem (X_max +1), Sequence_number div (X_max + 1)}.


init([Number_of_workers, 
        {child_specs, Iteration, Max_iteration, 
            {neuron_coordinates, X_max, Y_max, Next_free_neuron}
        }
    ]) ->
    %Start neuron event manager
    gen_event:start_link({local, neuron_events}),
    Supervisor_specification = {
        one_for_one, 
        10,
        10
    },
    % make a list of {x,y} coordinate tuples for neurons
    Children_coordinates = [get_x_y_from_sequence(X_max, Neuron_index) || Neuron_index <- lists:seq(Next_free_neuron, Next_free_neuron + Number_of_workers)],
    Child_specification_list = 
        [ 
            {Neuron_coordinates, 
                {neuron, start_link, [Neuron_coordinates, spectrum_dispatcher:get_spectrum(spectrum_dispatcher), [], Iteration, Max_iteration]},
                permanent,
                10000,
                worker,
                [neuron]
            }
        || Neuron_coordinates <- Children_coordinates
        ],
    case Next_free_neuron + Number_of_workers < X_max * Y_max of
        true -> 
            Sub_supervisor_specification = [
                {Next_free_neuron + Number_of_workers,
                    {neuron_supervisor, start_link, 
                        [Number_of_workers, 
                            {child_specs, Iteration, Max_iteration, 
                              {neuron_coordinates, X_max, Y_max, min(Next_free_neuron + Number_of_workers, X_max * Y_max)}
                            }
                        ]
                    },
                permanent,
                10000,
                supervisor,
                [neuron_supervisor]
                }
            ];
        false -> 
            Sub_supervisor_specification = []
    end,

    {ok, 
        {
        Supervisor_specification, 
        Child_specification_list ++ Sub_supervisor_specification
        }
    }.

% @doc for shell testing
force_stop(PID) ->
    exit(PID, shutdown).

