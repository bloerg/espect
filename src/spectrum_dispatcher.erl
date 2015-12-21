-module(spectrum_dispatcher).
-beaviour(gen_server).

-export([start/3, start_link/3, start/4, start_link/4]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([init/1]).
-export([get_spectrum/1, get_spectrum_with_id/1, set_iteration/2, get_minimum_som_dimensions/1, get_number_of_spectra/1]).
%-export([update_iteration/1)].


% @doc Example start: spectrum_dispatcher:start({local, spectrum_dispatcher},{random_sine, 256}, 0, 200).
%                     spectrum_dispatcher:start({local, spectrum_dispatcher}, {filesystem, "/var/tmp/sine/", binary, 1}, 0, 200).


%%Start a spectrum_dispatcher server
start(Server_name, Spectra_source, Iteration, Max_iteration ) ->
    gen_server:start(Server_name, ?MODULE, [Spectra_source, Iteration, Max_iteration], []).

start_link(Server_name, Spectra_source, Iteration, Max_iteration ) ->
    gen_server:start_link(Server_name, ?MODULE, [Spectra_source, Iteration, Max_iteration], []).

start(Spectra_source, Iteration, Max_iteration ) ->
    gen_server:start(?MODULE, [Spectra_source, Iteration, Max_iteration], []).

start_link(Spectra_source, Iteration, Max_iteration ) ->
    gen_server:start_link(?MODULE, [Spectra_source, Iteration, Max_iteration], []).

%%Stop a spectrum_dispatcher server
stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).
terminate(_Reason, _Spectrum_dispatcher_state) ->
    ok
    .


init([{random_sine, Spectrum_length}, Iteration, Max_iteration]) ->
    {ok, [{random_sine, Spectrum_length}, Iteration, Max_iteration]};

init([{filesystem, Directory, File_format, Start_index}, Iteration, Max_iteration])
    when Start_index > 0 ->
    gen_event:add_handler(iteration_event_manager, {?MODULE, self()}, [{pid, self()}, {module, ?MODULE}]),
    {ok, [
        { filesystem, Directory,
          File_format,
          spectrum_handling:provide_spectra_list(filesystem, Directory),
          Start_index
        }, 
        Iteration, Max_iteration]
    }
.

get_minimum_som_dimensions_helper(Number_of_spectra) ->
    get_minimum_som_dimensions_helper(1, Number_of_spectra).
get_minimum_som_dimensions_helper(Edge_length, Number_of_spectra) ->
    case Edge_length * Edge_length > Number_of_spectra of
        true -> [Edge_length, Edge_length];
        false -> get_minimum_som_dimensions_helper(Edge_length + 1, Number_of_spectra)
    end.

get_spectrum(Server_name) ->
    gen_server:call(Server_name, get_spectrum).

get_spectrum_with_id(Server_name) ->
    gen_server:call(Server_name, get_spectrum_with_id).
    
get_minimum_som_dimensions(Server_name) ->
    gen_server:call(Server_name, get_minimum_som_dimensions).

get_number_of_spectra(Server_name) ->
    gen_server:call(Server_name, get_number_of_spectra).

set_iteration(Server_name, New_iteration) ->
    gen_server:call(Server_name, {set_iteration, New_iteration}).
    
handle_cast(stop, Spectrum_dispatcher_state) ->
    {stop, normal, Spectrum_dispatcher_state}.


handle_call(
    get_spectrum, _From, [{random_sine, Spectrum_length}, Iteration, Max_iteration]) ->
        Reply = spectrum_handling:read_spectrum_from(random_sine, Spectrum_length),
        {reply, Reply, [{random_sine, Spectrum_length}, Iteration, Max_iteration]};

%returns list of lists: [Spectrum_id, Spectrum]
handle_call(
    get_spectrum_with_id, _From, [{filesystem, Directory, File_format, Spectra_file_list, Spec_list_index}, Iteration, Max_iteration]) ->
        Reply = spectrum_handling:read_spectrum_from(
                    filesystem, 
                    string:concat(Directory, lists:nth(Spec_list_index, Spectra_file_list)), 
                    File_format
        ),
        {reply, Reply, [{filesystem, Directory, File_format, Spectra_file_list, 
                            case Spec_list_index == length(Spectra_file_list) of
                                true -> _Next_iteration = iteration_state_server:next_iteration(iteration_state_server),
                                        1;
                                false -> Spec_list_index +1 
                            end
                        }, Iteration, Max_iteration
                        ]
        };
        
% returns a spectrum from the filesystem as long as spectra are left
% returns [] if all spectra where delivered in the current Iterations step
handle_call(
    get_spectrum, _From, [{filesystem, Directory, File_format, Spectra_file_list, Spec_list_index}, Iteration, Max_iteration]) ->
        case Spec_list_index > length(Spectra_file_list) of 
            false ->
                [_Spectrum_id, Spectrum] = spectrum_handling:read_spectrum_from(
                    filesystem, 
                    string:concat(Directory, lists:nth(Spec_list_index, Spectra_file_list)), 
                    File_format
                );
            true ->
                Spectrum = []
        end,
        {reply, Spectrum, 
            [{filesystem, Directory, File_format, Spectra_file_list, Spec_list_index +1}, 
              Iteration, Max_iteration
            ]
        };

handle_call(
    get_minimum_som_dimensions, _From, [{filesystem, Directory, File_format, Spectra_file_list, Spec_list_index}, Iteration, Max_iteration]) ->
        {reply, 
         get_minimum_som_dimensions_helper(length(Spectra_file_list)), 
         [{filesystem, Directory, File_format, Spectra_file_list, Spec_list_index}, Iteration, Max_iteration]
        };
handle_call(
    get_number_of_spectra, _From, [{filesystem, Directory, File_format, Spectra_file_list, Spec_list_index}, Iteration, Max_iteration]) ->
        {reply,
         length(Spectra_file_list), 
         [{filesystem, Directory, File_format, Spectra_file_list, Spec_list_index}, Iteration, Max_iteration]
        };
handle_call({set_iteration, New_iteration}, _From, [{filesystem, Directory, File_format, Spectra_file_list, _Spec_list_index}, _Iteration, Max_iteration]) ->
    {reply, ok, [{filesystem, Directory, File_format, Spectra_file_list, 1}, New_iteration, Max_iteration]}.


