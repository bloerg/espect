-module(learning_step_manager).
-beaviour(gen_server).

-export([start/0, start_link/0, start/1, start_link/1]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_call/3]).
-export([init/1]).
-export([next_learning_step/1]).


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
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).

terminate(_Reason, _Neuron_state) ->
    ok
    .

init(State) ->
    {ok, State}.

next_learning_step() ->
    gen_server:call(learning_step_manager, next_learning_step).
    

handle_call(next_learning_step, _From, State) ->
    %~ erlang:display({"next learning step, bmu: ", bmu_manager:get_bmu(bmu_manager)}),
    neuron_event_handler:trigger_neuron_update({update, bmu_manager:get_bmu(bmu_manager)}), 
    neuron_dispatcher:next_learning_step(),
    {reply, 
     ok,
     State}.
