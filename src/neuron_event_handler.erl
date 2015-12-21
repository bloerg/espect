-module(neuron_event_handler).
-behaviour(gen_event).
-export([start/1, start_link/1, init/1]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).
-export([stop/1, terminate/2]).
-export([trigger_neuron_compare/1, trigger_neuron_update/1]).


start_link(Event_manager_name) ->
    gen_event:start_link(Event_manager_name).
start(Event_manager_name) ->
    gen_event:start_link(Event_manager_name).
    
stop(Event_manager_name) ->
    gen_event:stop(Event_manager_name).

init([{pid, Pid}]) ->
    {ok, [{pid, Pid}]}.

handle_event({compare, Spectrum, Spectrum_metadata}, [{pid, Pid}]) ->
    neurons:get_neuron_spectrum_distance(Pid, Spectrum, Spectrum_metadata),
    {ok, [{pid, Pid}]};
    
handle_event({compare_async, Spectrum, Spectrum_metadata}, [{pid, Pid}]) ->
    neurons:get_neuron_spectrum_distance({async, bmu_manager}, Pid, Spectrum, Spectrum_metadata),
    {ok, [{pid, Pid}]};
    
handle_event({update_async, BMU_spectrum, BMU_coordinates}, [{pid, Pid}]) ->
    neurons:update_neuron(async, Pid, BMU_spectrum, BMU_coordinates),
    {ok, [{pid, Pid}]}
.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


trigger_neuron_compare({compare, Spectrum, Spectrum_metadata}) ->
    %gen_event:sync_notify(neuron_event_manager, {compare, Spectrum, Spectrum_metadata}).
    %erlang:write(gen_event:which_handlers(neuron_event_manager)),
    bmu_manager:set_neurons_worker_list(bmu_manager, gen_event:which_handlers(neuron_event_manager)),
    gen_event:sync_notify(neuron_event_manager, {compare_async, Spectrum, Spectrum_metadata}).

trigger_neuron_update({update_async, BMU_spectrum, BMU_coordinates}) ->
    gen_event:sync_notify(neuron_event_manager, {update_async, BMU_spectrum, BMU_coordinates}).
