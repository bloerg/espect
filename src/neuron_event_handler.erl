-module(neuron_event_handler).
-behaviour(gen_event).
-export([init/1]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).
-export([trigger_neuron_compare/1]).

init([{pid, Pid}]) ->
    {ok, [{pid, Pid}]}.

handle_event({compare, Spectrum, Spectrum_metadata}, [{pid, Pid}]) ->
    neuron:get_neuron_spectrum_distance(Pid, Spectrum, Spectrum_metadata),
    {ok, [{pid, Pid}]};
handle_event({compare_async, Spectrum, Spectrum_metadata}, [{pid, Pid}]) ->
    neuron:get_neuron_spectrum_distance({async, bmu_manager}, Pid, Spectrum, Spectrum_metadata),
    {ok, [{pid, Pid}]}.

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
    gen_event:sync_notify(neuron_event_manager, {compare_async, Spectrum, Spectrum_metadata}).
