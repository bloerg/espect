-module(neuron).
-beaviour(gen_server).

-export([start/4, start_link/4, start/5, start_link/5]).
-export([init/1]).


start(Server_name, Neuron_coordinates, Neuron_vector, BMU, Iteration) ->
    gen_server:start(Server_name, ?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration], []).

start_link(Server_name, Neuron_coordinates, Neuron_vector, BMU, Iteration) ->
    gen_server:start(Server_name, ?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration], []).

start(Neuron_coordinates, Neuron_vector, BMU, Iteration) ->
    gen_server:start(?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration], []).

start_link(Neuron_coordinates, Neuron_vector, BMU, Iteration) ->
    gen_server:start(?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration], []).


stop() ->
    gen_server:cast(?MODULE, stop).
handle_cast(stop, Neuron_state) ->
    {stop, normal, Neuron_state}.
%~ terminate(_Reason, _Neuron_state) ->
    %~ %Do cleanup here
    %~ .


init(Init_data) ->
    {ok, Init_data}.
