-module(spectrum_dispatcher).
-beaviour(gen_server).

-export([start/3, start_link/3, start/4, start_link/4]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([init/1]).
-export([get_spectrum/1, get_spectrum_id/0, get_spectrum_with_id/1, get_spectrum_for_neuron_initialization/1]).
-export([get_spectrum_id_for_neuron_initialization/0]).
-export([set_iteration/2, get_minimum_som_dimensions/1, get_number_of_spectra/1]).
-export([get_state/1]).
-export([reset_speclist_index/0, next_learning_step/0]).
%-export([update_iteration/1)].

-record(spectrum_dispatcher_state, {
    spectra_source = {}, % The spectra source, would be set to filesystem in production
    iteration = 0, %iteration step
    max_iteration = 200, %maximum number of iterations
    spectra_list_unused = [], % Contains spectra filenames, when delivering a spectrum to a neuron the filename is removed and put into spectra_list_used
    spectra_list_used = [] % This list contains the spectra file names already delivered to neurons in an iteration step. Its content will be copied back to spectra_list_unused upon iteration step update
}).

-record(spectra, {
    spectrum_id = term_to_binary([-1,-1,-1]), %the neuron is BMU to spectrum with id
    spectrum = term_to_binary([]) %the vector representing the spectrum occupied by the neuron
}).

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


%% @doc Helper function, reads spectra from binary files and writes
%% {Spec_id, term_to_binary(Spectrum)} to mnesia-table
binary_spectra_files_to_mnesia(Directory, File_format) ->
    binary_spectra_files_to_mnesia(spectrum_handling:provide_spectra_list(filesystem, Directory), Directory, File_format).
binary_spectra_files_to_mnesia([], _Directory, _File_format) ->
    ok;
binary_spectra_files_to_mnesia(Spectra_list, Directory, File_format) ->
    [Spectrum_id, Spectrum] = spectrum_handling:read_spectrum_from(
        filesystem, 
        string:concat(Directory, hd(Spectra_list)), 
        binary
    ),
    case mnesia:transaction(fun() -> mnesia:write(spectra, #spectra{
        spectrum_id = Spectrum_id, 
        spectrum = term_to_binary(Spectrum)
    }, write) end)
    of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            io:format("Write to mnesia spectra table failed. Error: ~w~n", [{aborted, Reason}])
    end,
    %~ io:format("mnesia: ~w~n",[{Return_status, Result}]),
    binary_spectra_files_to_mnesia(
        tl(Spectra_list),
        Directory,
        File_format
    ).

init([{filesystem, Directory, File_format, Start_index}, Iteration, Max_iteration])
    when Start_index > 0 ->
    mnesia:create_table(spectra, [{ram_copies, [node()|nodes()]}, {attributes, record_info(fields, spectra)}]),
    gen_event:add_handler({global, iteration_event_manager}, {iteration_event_handler, self()}, [{pid, self()}, {module, ?MODULE}]),
    case File_format of
        binary_spectra_in_ram ->
            ok = binary_spectra_files_to_mnesia(Directory, File_format),
            {atomic, Key_list} = mnesia:transaction(fun() -> mnesia:all_keys(spectra) end),
            State = #spectrum_dispatcher_state{
                spectra_source = { 
                  filesystem, 
                  Directory,
                  File_format,
                  Start_index
                },
                iteration = Iteration,
                max_iteration = Max_iteration,
                spectra_list_unused = Key_list
            };
        _Other ->
            State = #spectrum_dispatcher_state{
                spectra_source = { 
                  filesystem, 
                  Directory,
                  File_format,
                  Start_index
                },
                spectra_list_unused = spectrum_handling:provide_spectra_list(filesystem, Directory),
                iteration = Iteration,
                max_iteration = Max_iteration
            }
    end,
    
    
    {ok, State}.


get_minimum_som_dimensions_helper(Number_of_spectra) ->
    get_minimum_som_dimensions_helper(1, Number_of_spectra).
get_minimum_som_dimensions_helper(Edge_length, Number_of_spectra) ->
    case Edge_length * Edge_length > Number_of_spectra of
        true -> [Edge_length, Edge_length];
        false -> get_minimum_som_dimensions_helper(Edge_length + 1, Number_of_spectra)
    end.

get_spectrum(Server_name) ->
    gen_server:call(Server_name, get_spectrum).

get_spectrum_for_neuron_initialization(Server_name) ->
    gen_server:call(Server_name, get_spectrum_for_neuron_initialization).

get_spectrum_id_for_neuron_initialization() ->
    gen_server:call({global, ?MODULE}, get_spectrum_id_for_neuron_initialization).

get_spectrum_with_id(Server_name) ->
    gen_server:call(Server_name, get_spectrum_with_id).

get_spectrum_id() ->
    gen_server:call({global, ?MODULE}, get_spectrum_id).

get_minimum_som_dimensions(Server_name) ->
    gen_server:call(Server_name, get_minimum_som_dimensions).

get_number_of_spectra(Server_name) ->
    gen_server:call(Server_name, get_number_of_spectra).

get_state(Server_name) ->
    gen_server:call(Server_name, get_state).

set_iteration(Server_name, New_iteration) ->
    gen_server:call(Server_name, {set_iteration, New_iteration}).
    
next_learning_step() ->
    gen_server:call({global, ?MODULE}, next_learning_step).
    
reset_speclist_index() ->
    gen_server:call({global, ?MODULE}, reset_speclist_index).
    
handle_cast(stop, Spectrum_dispatcher_state) ->
    {stop, normal, Spectrum_dispatcher_state}.


%returns list of lists: [Spectrum_id, Spectrum]
handle_call(
    get_spectrum_with_id, _From, Spectrum_dispatcher_state) ->
        {filesystem, Directory, File_format, _Spec_list_index} = Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_source,
        case File_format of 
            binary_spectra_in_ram ->
                Key = hd(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused),
                {atomic, [Spectrum]} = mnesia:transaction(fun() -> mnesia:read(spectra, Key) end),
                Reply = [Key, Spectrum#spectra.spectrum];
            binary ->
                Reply = spectrum_handling:read_spectrum_from(
                            filesystem, 
                            string:concat(Directory, hd(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused)), 
                            File_format
                )
        end,
        {   reply, 
            Reply, 
            Spectrum_dispatcher_state#spectrum_dispatcher_state{
                spectra_list_unused = tl(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused),
                spectra_list_used = [
                    hd(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused) |
                    Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_used
                ]
            }
        
        };

%returns a spectrum_id for neuron_spectrum_compare
handle_call(get_spectrum_id, _From, Spectrum_dispatcher_state) ->
        {   reply, 
            hd(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused), 
            Spectrum_dispatcher_state#spectrum_dispatcher_state{
                spectra_list_unused = tl(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused),
                spectra_list_used = [
                    hd(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused) |
                    Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_used
                ]
            }
        
        };

% returns a spectrum from the filesystem as long as spectra are left
% returns [] if all spectra where delivered in the current Iterations step
handle_call(
    get_spectrum, _From, Spectrum_dispatcher_state) ->
        {filesystem, Directory, File_format, _Spec_list_index} = Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_source,
        case length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused) of 
            0 -> Spectrum = [];
            _Else ->
                [_Spectrum_id, Spectrum] = spectrum_handling:read_spectrum_from(
                    filesystem, 
                    string:concat(Directory, hd(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused)), 
                    File_format
                )
        end,
        {reply, Spectrum, 
            Spectrum_dispatcher_state#spectrum_dispatcher_state{
                spectra_list_unused = tl(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused),
                spectra_list_used = hd(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused)
            }
        };
        
% returns a spectrum from the filesystem as long as spectra are left
% returns zero vector with length of spectra if all spectra are delivered
handle_call(
    get_spectrum_for_neuron_initialization, _From, Spectrum_dispatcher_state) ->
    {filesystem, Directory, File_format, Spec_list_index} = Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_source,
    case Spec_list_index > length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused) of 
        false ->
            case File_format of
                binary_spectra_in_ram ->
                    Spectrum_id = lists:nth(Spec_list_index, Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused),
                    {atomic, [Spectrum_record]} = mnesia:transaction(fun() -> mnesia:read(spectra, Spectrum_id) end),
                    Spectrum = Spectrum_record#spectra.spectrum;
                binary ->
                    [_Spectrum_id, Plain_spectrum] = spectrum_handling:read_spectrum_from(
                        filesystem, 
                        string:concat(Directory, lists:nth(Spec_list_index, Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused)), 
                        File_format
                    ),
                    Spectrum = term_to_binary(Plain_spectrum)
            end;
        %% if all spectra are dispatched but neurons keep asking,
        %% return random spectras from the list of same spectra, but reversed
        true ->
            case File_format of
                binary_spectra_in_ram ->
                    Spectrum_id = lists:nth(random:uniform(length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused)),Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused),
                    {atomic, [Spectrum]} = mnesia:transaction(fun() -> mnesia:read(spectra, Spectrum_id) end),
                    Spectrum = 
                        term_to_binary(
                            lists:reverse(
                                binary_to_term(
                                    Spectrum#spectra.spectrum
                                )
                            )
                        );
                binary ->
                    [_Spectrum_id, Plain_spectrum] = spectrum_handling:read_spectrum_from(
                        filesystem, 
                        string:concat(Directory, lists:nth(random:uniform(length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused)), Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused)), 
                        File_format
                    ),
                    Spectrum = term_to_binary(lists:reverse(Plain_spectrum))
            end
    end,
    {reply, Spectrum, 
        Spectrum_dispatcher_state#spectrum_dispatcher_state{
            spectra_source = {filesystem, Directory, File_format, Spec_list_index +1}
        }
    };

% returns {ok, spectrum_id} from ets table of spectra
% returns {reverse, spectrum_id} if all spectra are delivered
handle_call(
    get_spectrum_id_for_neuron_initialization, _From, Spectrum_dispatcher_state) ->
    {filesystem, Directory, File_format, Spec_list_index} = Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_source,
    case Spec_list_index > length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused) of 
        false ->
            Key = lists:nth(Spec_list_index, Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused),
            Reply = {forward, Key};
        %% if all spectra are dispatched but neurons keep asking,
        %% return random spectras from the list of same spectra, but reversed
        true ->
            Key = lists:nth(random:uniform(length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused)),Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused),
            Reply = {reverse, Key}
    end,
    {reply, Reply, 
        Spectrum_dispatcher_state#spectrum_dispatcher_state{
            spectra_source = {filesystem, Directory, File_format, Spec_list_index +1}
        }
    };


handle_call(
    get_minimum_som_dimensions, _From, Spectrum_dispatcher_state) ->
        {reply, 
         get_minimum_som_dimensions_helper(
            length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused) + 
            length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_used)
        ), 
         Spectrum_dispatcher_state
        };
handle_call(
    get_number_of_spectra, _From, Spectrum_dispatcher_state) ->
        {reply,
        length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused) + 
        length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_used), 
        Spectrum_dispatcher_state
        };

handle_call({set_iteration, New_iteration}, _From, Spectrum_dispatcher_state) ->
    {filesystem, Directory, File_format, _Spec_list_index} = Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_source, 
    {reply, ok, 
        Spectrum_dispatcher_state#spectrum_dispatcher_state{
            spectra_source = {filesystem, Directory, File_format, 1},
            spectra_list_unused = Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_used,
            spectra_list_used = [],
            iteration = New_iteration
        }
    };
handle_call(reset_speclist_index, _From, Spectrum_dispatcher_state) ->
    {filesystem, Directory, File_format, _Spec_list_index} = Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_source, 
    {reply, ok, 
        Spectrum_dispatcher_state#spectrum_dispatcher_state{
            spectra_source = {filesystem, Directory, File_format, 1}
        }
    };


handle_call(next_learning_step, _From, Spectrum_dispatcher_state) ->
    {reply, 
    case length(Spectrum_dispatcher_state#spectrum_dispatcher_state.spectra_list_unused) of 
        0 -> nospectraleft;
        _Other -> ok
    end,
    Spectrum_dispatcher_state};
    
handle_call(get_state, _From, State) ->
    {reply, State, State}.



