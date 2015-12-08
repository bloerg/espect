-module(neuron).
-beaviour(gen_server).

-export([start/4, start_link/4, start/5, start_link/5]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([init/1]).
-export([compare_neuron_with_spectrum/3]).


%%Start a neuron server
start(Server_name, Neuron_coordinates, Neuron_vector, BMU, Iteration) ->
    gen_server:start(Server_name, ?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration], []).

start_link(Server_name, Neuron_coordinates, Neuron_vector, BMU, Iteration) ->
    gen_server:start(Server_name, ?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration], []).

start(Neuron_coordinates, Neuron_vector, BMU, Iteration) ->
    gen_server:start(?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration], []).

start_link(Neuron_coordinates, Neuron_vector, BMU, Iteration) ->
    gen_server:start(?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration], []).

%%Stop a neuron server
stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).
handle_cast(stop, Neuron_state) ->
    {stop, normal, Neuron_state}.
terminate(_Reason, _Neuron_state) ->
    noop
    .


init(Init_data) ->
    {ok, Init_data}.


%% @doc computes the vector distance between neuron and spectrum and sends the result to the caller
compare_neuron_with_spectrum(Server_name, Spectrum, Spectrum_metadata) ->
    gen_server:call(Server_name, {Spectrum, Spectrum_metadata}).

handle_call({Spectrum, Spectrum_metadata}, _From, [Neuron_coordinates, Neuron_vector, BMU, Iteration]) ->
    Reply = [Neuron_coordinates, 
             Spectrum_metadata, 
             vector_operations:vector_distance(Neuron_vector, Spectrum)
            ],
    {reply, Reply, [Neuron_coordinates, Neuron_vector, BMU, Iteration]}
.

