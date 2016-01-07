-module(neuron_initialization_manager).
-beaviour(gen_server).

-export([start/0, start_link/0, start/1, start_link/1]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_call/3]).
-export([initialize_neurons/0, neuron_initialized/0]).
-export([init/1]).

-record(neuron_init_manager_state, {
    neurons_worker_list = [],
    start_time_stamp = none,
    iteration = 1,
    max_iteration = 200
}).

%%Start an iteration state server

start(Server_name) ->
    gen_server:start(Server_name, ?MODULE, [], []). 

start_link(Server_name) ->
    gen_server:start_link(Server_name, ?MODULE, [], []).

start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


stop() ->
    gen_server:cast({global, ?MODULE}, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).

terminate(_Reason, _Neuron_state) ->
    ok
    .

init(_State) ->
    %~ gen_event:add_handler({global, iteration_event_manager}, {iteration_event_handler, self()}, [{pid, self()}, {module, ?MODULE}]),
    {ok, #neuron_init_manager_state{}}.

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

remove_pid_from_list([], _Pid) ->
    [];
remove_pid_from_list(Pid_list, Pid) ->
    remove_pid_from_list(Pid_list, [], Pid).
    
remove_pid_from_list([], Filtered_pid_list, _Pid) ->
    Filtered_pid_list;
remove_pid_from_list(Pid_list, Filtered_pid_list, Pid) ->
    [Head|Tail] = Pid_list,
    case Head of
        Pid -> remove_pid_from_list(Tail, Filtered_pid_list, Pid);
        _Other -> remove_pid_from_list(Tail, [Head|Filtered_pid_list], Pid)
    end.


initialize_neurons() ->
    gen_server:call({global, ?MODULE}, initialize_neurons).

neuron_initialized() ->
    gen_server:call({global, ?MODULE}, neuron_initialized).
    

handle_call(initialize_neurons, _From, State) ->
    Neurons_worker_list = gen_event:which_handlers({global, neuron_event_manager}),
    New_state = State#neuron_init_manager_state{
        start_time_stamp = os:timestamp(),
        neurons_worker_list = filter_neurons_worker_list(Neurons_worker_list)
    },
    ok = neuron_event_handler:trigger_load_spectra_to_neurons_workers(),
    {reply, ok, New_state};
    
handle_call(neuron_initialized, From, State) ->
    % remove the pid of the sending neurons worker from the list of
    % registered workers. This is to keep track of the responses of the neuron workers
    {_, From_pid} = From, 
    Neurons_worker_list_new = remove_pid_from_list(
        State#neuron_init_manager_state.neurons_worker_list,
        From_pid
    ),
    
    case length(Neurons_worker_list_new) of
        0 ->
            io:format("Loading spectra to Neurons took ~w seconds.~n", 
                [timer:now_diff(State#neuron_init_manager_state.start_time_stamp, os:timestamp())/1000000]),
            ok;
        _Other_number -> 
            ok
    end,
    {reply, ok, State#neuron_init_manager_state{ neurons_worker_list = Neurons_worker_list_new}}.
