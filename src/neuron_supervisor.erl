-module(neuron_supervisor).
-behaviour(supervisor).
-export([start_link/2, start_link/3, start/2, start/3]).
-export([init/1]).
-export([force_stop/1]).


start_link(Server_name, Number_of_workers, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinate_range, X_start, X_end, Y_start, Y_end}
        }
    ) ->
    supervisor:start_link(
        Server_name, 
        ?MODULE, 
        [Number_of_workers, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinate_range, X_start, X_end, Y_start, Y_end}
            }
        ]
    ).

start_link(Number_of_workers, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinate_range, X_start, X_end, Y_start, Y_end}
        }
    ) ->
    supervisor:start_link(
        ?MODULE, 
        [Number_of_workers, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinate_range, X_start, X_end, Y_start, Y_end}
            }
        ]
    ).

start(Server_name, Number_of_workers,
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinate_range, X_start, X_end, Y_start, Y_end}
        }
    ) ->
    supervisor:start(
        Server_name, 
        ?MODULE, 
        [Number_of_workers, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinate_range, X_start, X_end, Y_start, Y_end}
            }
        ]
    ).

start(Number_of_workers, 
        {child_specs, Iteration, Max_iteration, 
          {neuron_coordinate_range, X_start, X_end, Y_start, Y_end}
        }
    ) ->
    supervisor:start(
        ?MODULE, 
        [Number_of_workers, 
            {child_specs, Iteration, Max_iteration, 
                {neuron_coordinate_range, X_start, X_end, Y_start, Y_end}
            }
        ]
    ).

init([Number_of_workers, 
        {child_specs, Iteration, Max_iteration, 
            {neuron_coordinate_range, X_start, X_end, Y_start, Y_end}
        }
    ]) ->
    Supervisor_specification = {
        one_for_one, 
        10,
        10
    },
    % make a list of {x,y} coordinate tuples for neurons
    All_children_coordinates = [{Neuron_x, Neuron_y} || Neuron_x <- lists:seq(X_start, X_end), Neuron_y <- lists:seq(Y_start, Y_end)],
    % if the length of the coordinate tuple list exceeds the maximum worker number for one supervisor,
    % split the list and use one part for this supervisor and the other to start another supervisor (TODO!)
    case length(All_children_coordinates) > Number_of_workers of
        true -> 
            {This_children_coordinates, Rest_children_coordinates} = lists:split(Number_of_workers, All_children_coordinates);
        false -> 
            {This_children_coordinates, Rest_children_coordinates} = {All_children_coordinates, []}
    end,
    Child_specification_list = 
        [ 
            {Neuron_coordinates, 
                {neuron, start_link, [Neuron_coordinates, spectrum_dispatcher:get_spectrum(), [], Iteration, Max_iteration]},
                permanent,
                10000,
                worker,
                [neuron]
            }
        || Neuron_coordinates <- This_children_coordinates
        ],
    if 
        length(Rest_children_coordinates) == 0 ->
            Sub_supervisor_specification = [];
        length(Rest_children_coordinates) > 0 ->
            Sub_supervisor_specification = [] %%FIXME!
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

