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
    Supervisor_specification = {
        one_for_one,
        10,
        10
    },
    % A neuron supervisor is needed on every node
    Child_specification_list = [
        %~ {
            %~ neuron_supervisor, 
            %~ {neuron_supervisor, start_link, [{local, testsup}, {child_specs, 0, 200, {neuron_coordinates, 0}}]},
            %~ temporary,
            %~ 10000,
            %~ supervisor,
            %~ [neuron_supervisor]
        %~ }
    ],
    {ok, {Supervisor_specification, Child_specification_list}}.

join_espect_cluster(Primary_node, Cluster_cookie) ->
    erlang:set_cookie(node(), Cluster_cookie),
    case net_adm:ping(Primary_node) of
        pang -> io:format("Could not connect to cluster.", []);
        pong ->
            cluster_supervisor:register_node(node())
    end.

start_espect_cluster(Cluster_cookie) ->
    supervisor:start_child(node_supervisor,
        {
            cluster_supervisor,
            {cluster_supervisor, start_link, [Cluster_cookie]},
            temporary,
            10000,
            supervisor,
            [cluster_supervisor]
        }
    )
.



