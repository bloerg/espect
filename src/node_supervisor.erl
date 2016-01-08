-module(node_supervisor).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-export([join_espect_cluster/2, start_espect_cluster/1]).


start_link() ->
    supervisor:start_link(
        {local, ?MODULE}, 
        ?MODULE, 
        state_undefined
    ).

init(_Args) ->
    application:start(mnesia),
    Supervisor_specification = {
        one_for_one,
        10,
        10
    },
    Child_specification_list = [],
    {ok, {Supervisor_specification, Child_specification_list}}.

join_espect_cluster(Primary_node, Cluster_cookie) ->
    erlang:set_cookie(node(), Cluster_cookie),
    case net_adm:ping(Primary_node) of
        pang -> io:format("Could not connect to cluster.", []);
        pong ->
            mnesia:change_config(extra_db_nodes, [Primary_node]),
            supervisor:start_child(node_supervisor,
                {
                    neuron_supervisor,
                    {neuron_supervisor, start_link, [{local, neuron_supervisor}, {child_specs, 0, 200, {neuron_coordinates, 0}}]},
                    temporary,
                    10000,
                    supervisor,
                    [neuron_supervisor]
                }
            )
    end.

start_espect_cluster(Cluster_cookie) ->

    %start the cluster_supervisor on this node
    supervisor:start_child(node_supervisor,
        {
            cluster_supervisor,
            {cluster_supervisor, start_link, [Cluster_cookie]},
            temporary,
            10000,
            supervisor,
            [cluster_supervisor]
        }
    ),
    
    %then start a neuron_supervisor and join with this cluster
    join_espect_cluster(node(), Cluster_cookie)
.



