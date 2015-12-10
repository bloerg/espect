-module(neuron_supervisor).
-behaviour(supervisor).
-export([start_link/2, start_link/3, start/2, start/3]).
-export([init/1]).
-export([force_stop/1]).


start_link(Server_name, Number_of_workers, {child_specs, Iteration, Max_iteration}) ->
    supervisor:start_link(Server_name, ?MODULE, [Number_of_workers, {child_specs, Iteration, Max_iteration}]).

start_link(Number_of_workers, {child_specs, Iteration, Max_iteration}) ->
    supervisor:start_link(?MODULE, [Number_of_workers, {child_specs, Iteration, Max_iteration}]).

start(Server_name, Number_of_workers, {child_specs, Iteration, Max_iteration}) ->
    supervisor:start_link(Server_name, ?MODULE, [Number_of_workers, {child_specs, Iteration, Max_iteration}]).

start(Number_of_workers, {child_specs, Iteration, Max_iteration}) ->
    supervisor:start_link(?MODULE, [Number_of_workers, {child_specs, Iteration, Max_iteration}]).

init([Number_of_workers, {child_specs, Iteration, Max_iteration}]) ->
    Supervisor_specification = {
        one_for_one, 
        10,
        10
    },
    Neuron_coordinates = [1,2],
    Neuron_vector = [1,2,3],
    Child_specification_list = 
        [ 
            {Worker_id, 
                {neuron, start_link, [Neuron_coordinates, Neuron_vector, [], Iteration, Max_iteration]},
                permanent,
                10000,
                worker,
                [neuron]
            }
        || Worker_id <- lists:seq(1, Number_of_workers)
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

