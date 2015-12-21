-module(bmu_manager).
-beaviour(gen_server).

-export([start/2, start_link/2, start/3, start_link/3]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([init/1]).
-export([neuron_spectrum_distance/2, get_bmu/1, get_state/1, set_iteration/2]).
-export([set_neurons_worker_list/2]).

-record(bmu_manager_state, {
    bmu_coordinates = [], % The som_x, som_y coordinates of the best matching unit
    bmu_spectrum_metadata = [], % The identifier of the spectrum associated with the BMU, e.g. mjd, plateid, fiberid or objid
    shortest_distance = 576460752303423487, %A large Number, every distance should be less than this number, taken from http://www.erlang.org/doc/efficiency_guide/advanced.html
    iteration = 0, %iteration step
    max_iteration = 200, %maximum number of iterations
    neurons_worker_list = []
}).

start(Server_name, Iteration, Max_iteration) ->
    gen_server:start(
        Server_name, 
        ?MODULE, 
        #bmu_manager_state{
            iteration = Iteration,
            max_iteration = Max_iteration
        },
        []
    ). 

start_link(Server_name, Iteration, Max_iteration) ->
    gen_server:start_link(
        Server_name, 
        ?MODULE, 
        #bmu_manager_state{
            iteration = Iteration,
            max_iteration = Max_iteration
        },
        []
    ).
start(Iteration, Max_iteration) ->
    gen_server:start(
        ?MODULE, 
        #bmu_manager_state{
            iteration = Iteration,
            max_iteration = Max_iteration
        },
        []
    ).

start_link(Iteration, Max_iteration) ->
    gen_server:start_link(
        ?MODULE, 
        #bmu_manager_state{
            iteration = Iteration,
            max_iteration = Max_iteration
        },
        []
    ).


stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).

terminate(_Reason, _Neuron_state) ->
    ok
    .


init(State) ->
    gen_event:add_handler(iteration_event_manager, {iteration_event_handler, self()}, [{pid, self()}, {module, ?MODULE}]),
    {ok, State}.


% Takes List like [{neuron_event_handler,{neuron,<0.91.0>}}, {neuron_event_handler,{neuron,<0.91.0>}},...]
% as input and outputs a list conainting neurons worker pids
filter_neurons_worker_list(Neurons_worker_list) ->
    filter_neurons_worker_list(Neurons_worker_list, []).
filter_neurons_worker_list([], Filtered_list) ->
    Filtered_list;
filter_neurons_worker_list(Neurons_worker_list, Filtered_list) -> 
    [Head|Tail] = Neurons_worker_list,
    case Head of
        {neuron_event_handler,{neuron, Pid}} ->
            filter_neurons_worker_list(Tail, [Pid|Filtered_list]);
        _Other -> filter_neurons_worker_list(Tail, Filtered_list)
    end.


neuron_spectrum_distance(Result_receiver_name, Data
    %~ % i. e.
    %~ [Neuron_coordinates, 
         %~ Spectrum_metadata, 
         %~ Spectrum_neuron_distance,
         %~ Neuron_worker_pid
    %~ ]
    ) ->
        gen_server:call(Result_receiver_name, {intermediate, Data}).

get_bmu(Result_receiver_name) ->
    gen_server:call(Result_receiver_name, get_bmu).
    

get_state(Result_receiver_name) ->
    gen_server:call(Result_receiver_name, get_state).

set_iteration(Server_name, New_iteration) ->
    gen_server:cast(Server_name, {set_iteration, New_iteration}).

set_neurons_worker_list(Server_name, Neurons_worker_list) ->
    gen_server:call(Server_name, {set_neurons_worker_list, Neurons_worker_list}).

handle_cast({set_iteration, New_iteration}, BMU_manager_state) ->
    {noreply, BMU_manager_state#bmu_manager_state{iteration = New_iteration} };
    

    
handle_cast(stop, Neuron_state) ->
    {stop, normal, Neuron_state}.
    
handle_call({set_neurons_worker_list, Neurons_worker_list}, _From, BMU_manager_state) ->
    {reply, ok, BMU_manager_state#bmu_manager_state{neurons_worker_list = filter_neurons_worker_list(Neurons_worker_list)}};
handle_call(get_bmu, _From, BMU_manager_state) ->
    {reply, BMU_manager_state#bmu_manager_state.bmu_coordinates, BMU_manager_state};
handle_call(get_state, _From, BMU_manager_state) ->
    {reply, BMU_manager_state, BMU_manager_state};
    
handle_call({intermediate, [Neuron_coordinates, Spectrum_metadata, Spectrum_neuron_distance]}, From,
    BMU_manager_state) ->
        {From_pid, _From_tag} = From,
        % remove the pid of the sending neurons worker from the list of
        % registered workers. This is to keep track of the responses of the neuron workers
        Neurons_worker_list_new = lists:takewhile(
            fun(Pid) -> Pid =/= From_pid end, 
            BMU_manager_state#bmu_manager_state.neurons_worker_list
        ),
        case Spectrum_neuron_distance < BMU_manager_state#bmu_manager_state.shortest_distance of
            true -> 
                {   reply, 
                    case length(Neurons_worker_list_new) of 
                        0 -> learning_step_manager:next_learning_step();
                        _Other -> ok
                    end, 
                    BMU_manager_state#bmu_manager_state{
                        shortest_distance = Spectrum_neuron_distance,
                        bmu_coordinates = Neuron_coordinates,
                        bmu_spectrum_metadata = Spectrum_metadata,
                        neurons_worker_list = Neurons_worker_list_new

                    }
                };
            false -> 
                {   reply, 
                    case length(Neurons_worker_list_new) of 
                        0 -> learning_step_manager:next_learning_step();
                        _Other -> ok
                    end, 
                    BMU_manager_state#bmu_manager_state{
                        neurons_worker_list = Neurons_worker_list_new
                    }
                }
        end.

