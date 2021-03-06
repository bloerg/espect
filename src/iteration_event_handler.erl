-module(iteration_event_handler).
-behaviour(gen_event).
-export([start/1, start_link/1, init/1]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).
-export([stop/1, terminate/2]).
-export([trigger_iteration_update/1]).

start_link(Event_manager_name) ->
    gen_event:start_link(Event_manager_name).
start(Event_manager_name) ->
    gen_event:start_link(Event_manager_name).
stop(Event_manager_name) ->
    gen_event:stop(Event_manager_name).

init([{pid, Pid}, {module, Module}]) ->
    {ok, [{pid, Pid}, {module, Module}]}.

handle_event({set_iteration, New_iteration}, [{pid, Pid}, {module, Module}]) ->
    %code here to make sure, all spectrum dispatchers on other nodes also have delivered all spectra
    Module:set_iteration(Pid, New_iteration),
    {ok, [{pid, Pid}, {module, Module}]}
.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


trigger_iteration_update(New_iteration) ->
    gen_event:sync_notify({global, iteration_event_manager}, {set_iteration, New_iteration}).



