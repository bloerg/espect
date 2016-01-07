-module(neurons).
-beaviour(gen_server).

-export([start/5, start_link/5, start/6, start_link/6]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([init/1, set_neuron_indeces/2, load_spectra_to_neurons_worker/1]).
-export([add_neurons/2, remove_neurons/2]).
-export([get_neuron_spectrum_distance/2, get_neuron_spectrum_distance/3]).
-export([set_bmu/3, set_iteration/2]).
-export([update_neuron/2, update_neuron/3]).
-export([map_dump/1]).

%for testing, remove later
-export([alpha/4, sigma/4, neighbourhood_function1/4, neighbourhood_function2/4, neighbourhood_function3/3, neighbourhood_function4/3]).

-record(neuron, {
    neuron_vector = term_to_binary([]), %the vector representing the spectrum occupied by the neuron
    neuron_coordinates = [], %the coordinates of the neuron in the som
    bmu_to_spectrum_id = term_to_binary([-1,-1,-1]), %the neuron is BMU to spectrum with id
    last_spectrum_neuron_vector_difference = [] %the vector difference computed in the compare process, used later for the update computation
}).

-record(neuron_worker_state, {
    spectrum_dispatcher = {global,spectrum_dispatcher},  %Name of the spectrum dispatcher server
    neuron_indeces = [], %List of neuron index, i. e. the serial index starting at 0 and ending at X_max*Y_max
    som_dimensions = [], % maximum x and maximum y coordinate of the self organizing map
    iteration = 0, %iteration step
    max_iteration = 200, %maximum number of iterations
    neuron_table_id = none, % an id of an ets table containing all the neurons
    coordinate_table_id = none % contains the coordinates of the neurons for cross querying
}).

%%Start a neuron server
start(Server_name, Spectrum_dispatcher, Neuron_indeces, [Max_x, Max_y], Iteration, Max_iteration) ->
    gen_server:start(Server_name, ?MODULE, 
            #neuron_worker_state {
                spectrum_dispatcher = Spectrum_dispatcher,
                neuron_indeces = Neuron_indeces,
                som_dimensions = [Max_x, Max_y],
                iteration = Iteration,
                max_iteration = Max_iteration
            }
        ,[]
    ).

start_link(Server_name, Spectrum_dispatcher, Neuron_indeces, [Max_x, Max_y], Iteration, Max_iteration) ->
    gen_server:start_link(Server_name, ?MODULE, 
            #neuron_worker_state {
                spectrum_dispatcher = Spectrum_dispatcher,
                neuron_indeces = Neuron_indeces,
                som_dimensions = [Max_x, Max_y],
                iteration = Iteration,
                max_iteration = Max_iteration
            }
        ,[]
    ).

start(Spectrum_dispatcher, Neuron_indeces, [Max_x, Max_y], Iteration, Max_iteration) ->
    gen_server:start(?MODULE, 
            #neuron_worker_state {
                spectrum_dispatcher = Spectrum_dispatcher,
                neuron_indeces = Neuron_indeces,
                som_dimensions = [Max_x, Max_y],
                iteration = Iteration,
                max_iteration = Max_iteration
            }
        ,[]
    ).

start_link(Spectrum_dispatcher, Neuron_indeces, [Max_x, Max_y], Iteration, Max_iteration) ->
    gen_server:start_link(?MODULE, 
            #neuron_worker_state {
                spectrum_dispatcher = Spectrum_dispatcher,
                neuron_indeces = Neuron_indeces,
                som_dimensions = [Max_x, Max_y],
                iteration = Iteration,
                max_iteration = Max_iteration
            }
        ,[]
    ).

%%Stop a neuron server
stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).
terminate(_Reason, _Neuron_state) ->
    gen_event:delete_handler({global, neuron_event_manager}, {neuron_event_handler, {neuron, self()}}, [{pid, self()}]),
    gen_event:delete_handler({global, iteration_event_manager}, {iteration_event_handler, self()}, [{pid, self()}, {module, ?MODULE}]),
    ok.



init(State) ->
    gen_event:add_handler({global, neuron_event_manager}, {neuron_event_handler, {neuron, self()}}, [{pid, self()}]),
    gen_event:add_handler({global, iteration_event_manager}, {iteration_event_handler, self()}, [{pid, self()}, {module, ?MODULE}]),
    {ok, [
        State#neuron_worker_state{
            neuron_table_id = ets:new(neurons, [{keypos, #neuron.neuron_coordinates}]),
            coordinate_table_id = ets:new(coordinates, [])
        }]
    }.


load_spectrum_from_filesystem({Direction, Path}) ->
    case Direction of 
        forward ->
            [_Spectrum_id, Spectrum] = spectrum_handling:read_spectrum_from(
                filesystem, 
                Path,
                binary
            );
        %% if all spectra are dispatched but neurons keep asking,
        %% return random spectras from the list of same spectra, but reversed
        reverse ->
            [_Spectrum_id, Temp_spectrum] = spectrum_handling:read_spectrum_from(
                filesystem, 
                Path,
                binary
            ),
            Spectrum = lists:reverse(Temp_spectrum)
    end,
    Spectrum.

%% @doc init neuron_workers with spectra
load_spectra_to_neurons_worker(Server_name) ->
    gen_server:call(Server_name, load_spectra_to_neurons_worker, 3600*1000).

%% @doc add neurons to the neurons list
%List_of_neurons must be list of #neuron record
add_neurons(Server_name, List_of_neurons) ->
    gen_server:call(Server_name, {add_neurons, List_of_neurons}).

%% @doc deletes neurons from the neurons list and returns the deleted neurons
remove_neurons(Server_name, Number) ->
    gen_server:call(Server_name, {remove_neurons, Number}).

%% @doc computes the vector distance between neuron and spectrum and sends the result to the caller
get_neuron_spectrum_distance(Server_name, Spectrum_with_id) ->
    gen_server:call(Server_name, {compare, Spectrum_with_id}).
    
%% @doc async vector distance computing
get_neuron_spectrum_distance({async, BMU_manager}, Server_name, Spectrum_with_id) ->
    gen_server:cast(Server_name, {{async, BMU_manager}, {compare, Spectrum_with_id}}).

%% @doc set neuron indeces
set_neuron_indeces(Neurons_worker_name, Indeces) ->
    gen_server:call(Neurons_worker_name, {set_neuron_indeces, Indeces}).

set_bmu(Neurons_worker_name, Neuron_coordinates, Spectrum_id) ->
    gen_server:call(Neurons_worker_name, {set_bmu, Neuron_coordinates, Spectrum_id}).

%% @doc calls the function to compute the new neuron vector
update_neuron(Server_name, BMU_neuron_coordinates) ->
    gen_server:call(Server_name, {update_neuron, BMU_neuron_coordinates}).

update_neuron(async, Server_name, BMU_neuron_coordinates) ->
    gen_server:cast(Server_name, {update_neuron, BMU_neuron_coordinates}).

set_iteration(Server_name, New_iteration) ->
    gen_server:call(Server_name, {set_iteration, New_iteration}).

map_dump(Server_name) ->
    gen_server:call(Server_name, map_dump).



%% @doc recursive function; run from spectra - neuron compare function
%% compares a spectrum with all neurons and returns
%% minimum spec-neuron distance and corresponding neuron coordinates
compare_helper(
    Neurons_table, 
    Coordinate_table, 
    Coordinates, 
    Spectrum, 
    [Min_spectrum_neuron_distance, Min_spectrum_neuron_distance_coordinates]
) ->
    case Coordinates of 
        '$end_of_table' ->
            [Min_spectrum_neuron_distance, Min_spectrum_neuron_distance_coordinates];
        _Other ->
            [Neuron] = ets:lookup(Neurons_table, Coordinates),
            Neuron_vector_difference = vector_operations:vector_difference(binary_to_term(Neuron#neuron.neuron_vector), Spectrum),
            ets:insert(Neurons_table, Neuron#neuron{
                last_spectrum_neuron_vector_difference = Neuron_vector_difference
            }),
            compare_helper(
                Neurons_table,
                Coordinate_table,
                ets:next(Coordinate_table, Coordinates),
                Spectrum,
                case binary_to_term(Neuron#neuron.bmu_to_spectrum_id) of
                    [-1, -1, -1] -> 
                        Spectrum_vector_distance = vector_operations:vector_length(Neuron_vector_difference),
                        case Spectrum_vector_distance < Min_spectrum_neuron_distance of
                            true -> 
                                [Spectrum_vector_distance, Coordinates];
                            false -> [Min_spectrum_neuron_distance, Min_spectrum_neuron_distance_coordinates]
                        end;
                    _Else -> [Min_spectrum_neuron_distance, Min_spectrum_neuron_distance_coordinates]
                end
            )
    end.


%% @doc recursive function; run from spectra - neuron compare function
%% compares a spectrum with all neurons and returns
%% minimum spec-neuron distance and corresponding neuron coordinates
update_helper(
    Neurons_table, 
    Coordinate_table, 
    Coordinates,
    Neuron_worker_state,
    BMU_neuron_coordinates
) ->
    case Coordinates of 
        '$end_of_table' ->
            ok;
        _Other ->
            [Neuron] = ets:lookup(Neurons_table, Coordinates),
            ets:insert(Neurons_table, 
                Neuron#neuron{ neuron_vector = 
                    term_to_binary(
                        vector_operations:vector_sum(
                            binary_to_term(Neuron#neuron.neuron_vector),
                            vector_operations:scalar_multiplication(
                                neighbourhood_function1(
                                    Neuron_worker_state#neuron_worker_state.iteration, 
                                    Neuron_worker_state#neuron_worker_state.max_iteration, 
                                    Neuron#neuron.neuron_coordinates, 
                                    BMU_neuron_coordinates
                                ),
                                %vector_operations:vector_difference(BMU_spectrum, Neuron_vector)
                                Neuron#neuron.last_spectrum_neuron_vector_difference
                            )
                        )
                    )
                }
            ),
            update_helper(
                Neurons_table,
                Coordinate_table,
                ets:next(Coordinate_table, Coordinates),
                Neuron_worker_state,
                BMU_neuron_coordinates
            )
    end.

handle_cast(
    {{async, BMU_manager}, {compare, Spectrum_with_id}}, 
    [Neuron_worker_state]) ->
        [Spectrum_metadata, Spectrum] = Spectrum_with_id,
        [Min_spectrum_neuron_distance, Min_spectrum_neuron_distance_coordinates] =
            compare_helper(
                Neuron_worker_state#neuron_worker_state.neuron_table_id, 
                Neuron_worker_state#neuron_worker_state.coordinate_table_id, 
                ets:first(Neuron_worker_state#neuron_worker_state.coordinate_table_id), 
                Spectrum, 
                [576460752303423487, []]
            ),
        bmu_manager:neuron_spectrum_distance(BMU_manager,
            [Min_spectrum_neuron_distance_coordinates, 
                 Spectrum_metadata, 
                 Min_spectrum_neuron_distance
            ]
        ),
        {noreply, [Neuron_worker_state]}; %%FIXME [#neuron{}] depricated

handle_cast({update_neuron, BMU_neuron_coordinates}, [Neuron_worker_state]) ->
        Neurons_table = Neuron_worker_state#neuron_worker_state.neuron_table_id,
        Coordinate_table = Neuron_worker_state#neuron_worker_state.coordinate_table_id,
        update_helper(
            Neurons_table, 
            Coordinate_table, 
            ets:first(Neuron_worker_state#neuron_worker_state.coordinate_table_id),
            Neuron_worker_state,
            BMU_neuron_coordinates
        ),
    learning_step_manager:update_complete(self()),
    {noreply, [Neuron_worker_state]};

handle_cast(stop, State) ->
    {stop, normal, State}.


handle_call({set_neuron_indeces, Indeces}, _From, [Neuron_worker_state]) ->
    {reply, ok, 
        [
            Neuron_worker_state#neuron_worker_state{neuron_indeces = Indeces}
        ]
    };

handle_call(load_spectra_to_neurons_worker, _From, [Neuron_worker_state]) ->
    %~ [First_neuron, Last_neuron] = Neuron_worker_state#neuron_worker_state.neuron_coordinate_range,
    [X_max, _Y_max] = Neuron_worker_state#neuron_worker_state.som_dimensions,
    %~ Spectra_table = spectrum_dispatcher:get_spectra_table_id(),
    Neurons_table = Neuron_worker_state#neuron_worker_state.neuron_table_id,
    Coordinate_table = Neuron_worker_state#neuron_worker_state.coordinate_table_id,
    lists:foreach(fun(Sequence_number)  ->
            %~ Spectrum_id = spectrum_dispatcher:get_spectrum_id_for_neuron_initialization(),
            Neuron_coordinates = neuron_supervisor:get_x_y_from_sequence(X_max, Sequence_number),
            %~ case Spectrum_id of 
                %~ {ok, Key} ->
                    %~ [{_Key, Spectrum}] = ets:lookup(Spectra_table, Key);
                %~ {reverse, Key} ->
                    %~ [{_Key, Forward_spectrum}] = ets:lookup(Spectra_table, Key),
                    %~ Spectrum = 
                        %~ term_to_binary(
                            %~ lists:reverse(
                                %~ binary_to_term(
                                    %~ Forward_spectrum
                                %~ )
                            %~ )
                        %~ );
                %~ Other -> 
                    %~ Spectrum = [],
                    %~ {error, Other}
            %~ end,
            Spectrum = spectrum_dispatcher:get_spectrum_for_neuron_initialization({global, spectrum_dispatcher}),
            ets:insert(Neurons_table, #neuron{
                neuron_coordinates = Neuron_coordinates, 
                neuron_vector = Spectrum
                }
            ),
            ets:insert(Coordinate_table, {Neuron_coordinates, Sequence_number})
        end,
        Neuron_worker_state#neuron_worker_state.neuron_indeces
    ),
    {reply, ok, [
            Neuron_worker_state
        ]
    };

handle_call({add_neurons, List_of_neurons}, _From, [Neuron_worker_state]) ->
    Neurons_table = Neuron_worker_state#neuron_worker_state.neuron_table_id,
    Coordinate_table = Neuron_worker_state#neuron_worker_state.coordinate_table_id,
    lists:foreach(fun(Neuron) ->
        ets:insert(Neurons_table, Neuron),
        ets:insert(Coordinate_table, {Neuron#neuron.neuron_coordinates, 1})
    end, 
    List_of_neurons),
    
    New_neurons = [],
    {reply, ok, [New_neurons, Neuron_worker_state]};

handle_call({remove_neurons, Number}, _From, [Neuron_worker_state]) ->
    Neurons_table = Neuron_worker_state#neuron_worker_state.neuron_table_id,
    Coordinate_table = Neuron_worker_state#neuron_worker_state.coordinate_table_id,
    Reply =
        lists:map(fun(_Counter) ->
            Coordinates = ets:first(Coordinate_table),
            ets:delete(Coordinate_table, Coordinates),
            [Neuron] = ets:lookup(Neurons_table, Coordinates),
            ets:delete(Neurons_table, Coordinates),
            Neuron
          end, 
          lists:seq(1,Number)
        ),
    {reply, Reply, [Neuron_worker_state]};

handle_call(
    {set_bmu, BMU_neuron_coordinates, BMU_spectrum_id}, _From, [Neuron_worker_state]) ->
        Neurons_table = Neuron_worker_state#neuron_worker_state.neuron_table_id,
        [Neuron] = ets:lookup(Neurons_table, BMU_neuron_coordinates),
        ets:insert(Neurons_table, Neuron#neuron{bmu_to_spectrum_id = term_to_binary(BMU_spectrum_id) }),
        {reply, ok, [
            Neuron_worker_state
        ]};

handle_call(map_dump, _From, [Neuron_worker_state]) ->
        Neurons_table = Neuron_worker_state#neuron_worker_state.neuron_table_id,
    {
        reply,
        term_to_binary(
            lists:map(fun(Neuron) ->
                [
                    Neuron#neuron.neuron_coordinates, 
                    binary_to_term(Neuron#neuron.bmu_to_spectrum_id)
                ]
            end,
            ets:tab2list(Neurons_table)) %% Fixme make this faster with recursion!
        ),
        [Neuron_worker_state]
    };

handle_call({set_iteration, New_iteration}, _From, [Neuron_worker_state]) ->
        Neurons_table = Neuron_worker_state#neuron_worker_state.neuron_table_id,
        lists:map(
            fun(Neuron) ->
                ets:insert(Neurons_table, 
                    Neuron#neuron{
                        bmu_to_spectrum_id = term_to_binary([-1,-1,-1]),
                        last_spectrum_neuron_vector_difference = []
                    }
                )
            end,
            ets:tab2list(Neurons_table)  %% Fixme make this faster with recursion!
        ),
    {reply, ok, [
        Neuron_worker_state#neuron_worker_state{iteration = New_iteration}
    ]}.


handle_info(Message, State) ->
    io:format("Unexpected message: ~w~n",[{Message, State}]).

%% BMU-Update related functions
alpha(T, T_max, Alpha_begin, Alpha_end) ->
    Alpha_begin * math:pow(Alpha_end / Alpha_begin, T/T_max).

sigma(T, T_max, Sigma_begin, Sigma_end) ->
    Sigma_begin * math:pow(Sigma_end / Sigma_begin, T/T_max).

neighbourhood_function1(T, T_max, Neuron_coordinates, BMU_coordinates) ->
    Sigma_begin = 1.0,
    Sigma_end = 0.0625,
    Alpha_begin = 0.25,
    Alpha_end = 0.01,
    alpha(T, T_max, Alpha_begin, Alpha_end) * 
        math:exp(
            -vector_operations:vector_distance(BMU_coordinates, Neuron_coordinates) 
            / (2 * math:pow(
                            sigma(T, T_max, Sigma_begin, Sigma_end),
                            2
                            )
            )
        )
.
neighbourhood_function2(Two_times_sigma_squared, Alpha, Neuron_coordinates, BMU_coordinates) ->
        Alpha * math:exp(
            -vector_operations:vector_distance(BMU_coordinates, Neuron_coordinates) 
            / Two_times_sigma_squared
        )
.
neighbourhood_function3(Two_times_sigma_squared, Alpha, Neuron_BMU_coordinate_distance) ->
        Alpha * math:exp(
            -Neuron_BMU_coordinate_distance / Two_times_sigma_squared
        )
.

neighbourhood_function4(Exp_one_over_two_times_sigma_squared, Alpha, Neuron_BMU_coordinate_distance) ->
        Alpha*math:pow(Exp_one_over_two_times_sigma_squared, Neuron_BMU_coordinate_distance).

