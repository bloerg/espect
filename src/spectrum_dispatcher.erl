-module(spectrum_dispatcher).
-beaviour(gen_server).

-export([start/3, start_link/3, start/4, start_link/4]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([init/1]).
-export([get_spectrum/1]).
%-export([update_iteration/1)].


% @doc Example start: spectrum_dispatcher:start({local, spectrum_dispatcher},{random_sine, 256}, 0, 200).


%%Start a spectrum_dispatcher server
start(Server_name, Spectra_source, Iteration, Max_iteration ) ->
    gen_server:start(Server_name, ?MODULE, [Spectra_source, Iteration, Max_iteration], []).

start_link(Server_name, Spectra_source, Iteration, Max_iteration ) ->
    gen_server:start(Server_name, ?MODULE, [Spectra_source, Iteration, Max_iteration], []).

start(Spectra_source, Iteration, Max_iteration ) ->
    gen_server:start(?MODULE, [Spectra_source, Iteration, Max_iteration], []).

start_link(Spectra_source, Iteration, Max_iteration ) ->
    gen_server:start(?MODULE, [Spectra_source, Iteration, Max_iteration], []).

%%Stop a spectrum_dispatcher server
stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).
handle_cast(stop, Spectrum_dispatcher_state) ->
    {stop, normal, Spectrum_dispatcher_state}.
terminate(_Reason, _Spectrum_dispatcher_state) ->
    noop
    .


init([{random_sine, Spectrum_length}, Iteration, Max_iteration]) ->
    {ok, [{random_sine, Spectrum_length}, Iteration, Max_iteration]}.



get_spectrum(Server_name) ->
    gen_server:call(Server_name, get_spectrum).

%~ update_iteration(Server_name, New_BMU) ->
    %~ gen_server:call(Server_name, {set_bmu, New_BMU}).


handle_call(
    get_spectrum, _From, [{random_sine, Spectrum_length}, Iteration, Max_iteration]) ->
        Reply = spectrum_handling:read_spectrum_from(random_sine, Spectrum_length),
        {reply, Reply, [{random_sine, Spectrum_length}, Iteration, Max_iteration]}
.


