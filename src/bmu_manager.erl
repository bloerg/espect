-module(bmu_manager).
-beaviour(gen_server).

-export([start/2, start_link/2, start/3, start_link/3]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([init/1]).
-export([neuron_spectrum_distance/2, get_bmu/1, get_state/1, set_iteration/2]).


%%Start a neuron server
%returns [Iteration, Max_iteration, Shortest_distance, BMU_coordinates]
start(Server_name, Iteration, Max_iteration) ->
    gen_server:start(Server_name, ?MODULE, [Iteration, Max_iteration, 1000000, [], []], []). 

start_link(Server_name, Iteration, Max_iteration) ->
    gen_server:start_link(Server_name, ?MODULE, [Iteration, Max_iteration, 1000000, [], []], []).

start(Iteration, Max_iteration) ->
    gen_server:start(?MODULE, [Iteration, Max_iteration, 1000000, [], []], []).

start_link(Iteration, Max_iteration) ->
    gen_server:start(?MODULE, [Iteration, Max_iteration, 1000000 , [], []], []).

%%Stop a neuron server
stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).

terminate(_Reason, _Neuron_state) ->
    noop
    .


init(State) ->
    gen_event:add_handler(iteration_event_manager, {?MODULE, self()}, [{pid, self()}, {module, ?MODULE}]),
    {ok, State}.
    
neuron_spectrum_distance(Result_receiver_name, Data
    %~ % i. e.
    %~ [Neuron_coordinates, 
         %~ Spectrum_metadata, 
         %~ Spectrum_neuron_distance,
         %~ Neuron_vector
    %~ ]
    ) ->
        gen_server:cast(Result_receiver_name, Data).

get_bmu(Result_receiver_name) ->
    gen_server:call(Result_receiver_name, get_bmu).
    

get_state(Result_receiver_name) ->
    gen_server:call(Result_receiver_name, get_state).

set_iteration(Server_name, New_iteration) ->
    gen_server:cast(Server_name, {set_iteration, New_iteration}).


handle_cast([Neuron_coordinates, _Spectrum_metadata, Spectrum_neuron_distance, Neuron_vector], [Iteration, Max_iteration, Shortest_distance, BMU_coordinates, BMU_neuron_vector]) ->
    case Spectrum_neuron_distance < Shortest_distance of
        true -> {noreply, [Iteration, Max_iteration, Spectrum_neuron_distance, Neuron_coordinates, Neuron_vector]};
        false -> {noreply, [Iteration, Max_iteration, Shortest_distance, BMU_coordinates, BMU_neuron_vector]}
    end;
handle_cast({set_iteration, New_iteration}, [_Old_iteration, Max_iteration, Shortest_distance, BMU_coordinates, BMU_neuron_vector]) ->
    {noreply, [New_iteration, Max_iteration, Shortest_distance, BMU_coordinates, BMU_neuron_vector]};
    
handle_cast(stop, Neuron_state) ->
    {stop, normal, Neuron_state}.
    
handle_call(get_bmu, _From, State) ->
    [_Iteration, _Max_iteration, _Shortest_distance, BMU_coordinates, _BMU_neuron_vector] = State,
    {reply, BMU_coordinates, State};
handle_call(get_state, _From, State) ->
    {reply, State, State}.
