-module(neuron).
-beaviour(gen_server).

-export([start/5, start_link/5, start/6, start_link/6]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([init/1]).
-export([get_neuron_spectrum_distance/3, get_neuron_spectrum_distance/4]).
-export([set_bmu/2, set_iteration/2]).
-export([update_neuron/3, update_neuron/4]).

%for testing, remove later
-export([alpha/4, sigma/4, neighbourhood_function1/4, neighbourhood_function2/4, neighbourhood_function3/3, neighbourhood_function4/3]).


%%Start a neuron server
start(Server_name, Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration) ->
    gen_server:start(Server_name, ?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration], []).

start_link(Server_name, Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration) ->
    gen_server:start_link(Server_name, ?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration], []).

start(Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration) ->
    gen_server:start(?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration], []).

start_link(Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration) ->
    gen_server:start_link(?MODULE, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration], []).

%%Stop a neuron server
stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).
terminate(_Reason, _Neuron_state) ->
    gen_event:delete_handler(neuron_event_manager, {neuron_event_handler, self()}, []),
    ok.



init(Init_data) ->
    gen_event:add_handler(neuron_event_manager, {neuron_event_handler, self()}, [{pid, self()}]),
    gen_event:add_handler(iteration_event_manager, {iteration_event_handler, self()}, [{pid, self()}, {module, ?MODULE}]),
    {ok, Init_data}.

%% @doc computes the vector distance between neuron and spectrum and sends the result to the caller
get_neuron_spectrum_distance(Server_name, Spectrum, Spectrum_metadata) ->
    gen_server:call(Server_name, {compare, Spectrum, Spectrum_metadata}).
    
%% @doc async vector distance computing
get_neuron_spectrum_distance({async, Result_receiver_name}, Server_name, Spectrum, Spectrum_metadata) ->
    gen_server:cast(Server_name, {{async, Result_receiver_name}, {compare, Spectrum, Spectrum_metadata}}).

set_bmu(Server_name, New_BMU) ->
    gen_server:call(Server_name, {set_bmu, New_BMU}).

%% @doc calls the function to compute the new neuron vector
update_neuron(Server_name, BMU_spectrum, BMU_coordinates) ->
    gen_server:call(Server_name, {update_neuron, BMU_spectrum, BMU_coordinates}).

update_neuron(async, Server_name, BMU_spectrum, BMU_coordinates) ->
    gen_server:cast(Server_name, {update_neuron, BMU_spectrum, BMU_coordinates}).

set_iteration(Server_name, New_iteration) ->
    gen_server:cast(Server_name, {set_iteration, New_iteration}).

handle_cast(
    {{async, Result_receiver_name}, {compare, Spectrum, Spectrum_metadata}}, 
    [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration]) ->
            bmu_manager:neuron_spectrum_distance(Result_receiver_name,
                [Neuron_coordinates, 
                     Spectrum_metadata, 
                     vector_operations:vector_distance(Neuron_vector, Spectrum),
                     Neuron_vector
                ]
            ),
        {noreply, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration]};

handle_cast({update_neuron, BMU_spectrum, BMU_coordinates}, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration]) ->
    {noreply, [Neuron_coordinates, 
             vector_operations:vector_sum(Neuron_vector,
                vector_operations:scalar_multiplication(
                    neighbourhood_function1(Iteration, Max_iteration, Neuron_coordinates, BMU_coordinates),
                    vector_operations:vector_difference(BMU_spectrum, Neuron_vector)
                )
             ),
             BMU,
             Iteration,                
             Max_iteration
            ]
    };
handle_cast({set_iteration, New_iteration}, [Neuron_coordinates, Neuron_vector, BMU, _Old_iteration, Max_iteration]) ->
    {noreply, [Neuron_coordinates, Neuron_vector, BMU, New_iteration, Max_iteration]};

handle_cast(stop, Neuron_state) ->
    {stop, normal, Neuron_state}.

handle_call(
    {compare, Spectrum, Spectrum_metadata}, _From, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration]) ->
        Reply = [Neuron_coordinates, 
                 Spectrum_metadata, 
                 vector_operations:vector_distance(Neuron_vector, Spectrum)
                ],
        {reply, Reply, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration]}
    ;
handle_call(
    {set_bmu, New_BMU}, _From, [Neuron_coordinates, Neuron_vector, _Old_BMU, Iteration, Max_iteration]) ->
        {reply, ok, [Neuron_coordinates, Neuron_vector, New_BMU, Iteration, Max_iteration]}
    ;

handle_call(
    {update_neuron, BMU_spectrum, BMU_coordinates}, _From, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration]) ->
        %% The if is for benchmark. Iteration should not go back to 0, actually
        if
            Iteration < Max_iteration -> Iteration2 = Iteration + 1;
            Iteration == Max_iteration -> Iteration2 = 0
        end,
        {reply, ok, [Neuron_coordinates, 
                 vector_operations:vector_sum(Neuron_vector,
                    vector_operations:scalar_multiplication(
                        neighbourhood_function1(Iteration, Max_iteration, Neuron_coordinates, BMU_coordinates),
                        vector_operations:vector_difference(BMU_spectrum, Neuron_vector)
                    )
                 ),
                 BMU,
                 Iteration2,                
                 Max_iteration
                ]
        }
.



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

