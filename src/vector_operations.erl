-module(vector_operations).
-export([vector_distance_squared/3, vector_distance/2, vector_difference/2, vector_difference/3, vector_sum/2, vector_length/1, scalar_multiplication/2]).
-export([vector_generate/2]).


%% @author Jörg Brünecke <dev@bloerg.de>
%% @doc Basic vector operations module
%% The module provides operations on simple n-dimensional cartesian vectors.



%% The next two lines stop calculation if the two vectors don't have equal dimensions
vector_distance_squared([],[_|_],_) -> error_first_vector_too_short;
vector_distance_squared([_|_],[],_) -> error_second_vector_too_short;
vector_distance_squared([],[],[]) -> error_two_empty_vectors_given;
%% Actual computation
vector_distance_squared([],[],Vector_distance_squared_sum) -> Vector_distance_squared_sum;
vector_distance_squared([Vector1_first_component| Vector1_rest], [Vector2_first_component| Vector2_rest], []) -> 
    vector_distance_squared(Vector1_rest, Vector2_rest, math:pow(Vector1_first_component - Vector2_first_component, 2));
vector_distance_squared([Vector1_first_component| Vector1_rest], [Vector2_first_component| Vector2_rest], Intermediate_result) -> 
    vector_distance_squared(Vector1_rest, Vector2_rest, math:pow(Vector1_first_component - Vector2_first_component, 2) + Intermediate_result).
    

vector_distance([],[]) -> error_two_empty_vectors_given; 
vector_distance(Vector1, Vector2) when is_list(Vector1), is_list(Vector2) -> 
    if 
        length(Vector1) == length(Vector2) -> math:sqrt(vector_distance_squared(Vector1, Vector2, []));
        length(Vector1) < length(Vector2) -> error_first_vector_too_short;
        length(Vector1) > length(Vector2) -> error_second_vector_too_short
    end.
    
    

vector_difference([],[_|_],_) -> error_first_vector_too_short;
vector_difference([_|_],[],_) -> error_second_vector_too_short; 
vector_difference([],[],[]) -> error_two_empty_vectors_given;
vector_difference([Vector1_first_component|Vector1_rest], [Vector2_first_component|Vector2_rest], []) ->
    vector_difference(Vector1_rest, Vector2_rest, [Vector2_first_component - Vector1_first_component]);
vector_difference([Vector1_first_component|Vector1_rest], [Vector2_first_component|Vector2_rest], Intermediate_vector) 
    when is_list(Intermediate_vector) ->
        vector_difference(Vector1_rest, Vector2_rest, [Vector2_first_component - Vector1_first_component| Intermediate_vector]);
vector_difference([],[], Intermediate_vector) when is_list(Intermediate_vector) ->
    lists:reverse(Intermediate_vector).

%Wrapper function
vector_difference(Vector1, Vector2) 
    when 
        is_list(Vector1), 
        is_list(Vector2), 
        length(Vector1) == length(Vector2) 
    ->
        vector_difference(Vector1, Vector2, []).

vector_sum([],[],[]) -> [];
vector_sum([Vector1_first_component|Vector1_rest], [Vector2_first_component|Vector2_rest], []) ->
    vector_sum(Vector1_rest, Vector2_rest, [Vector2_first_component + Vector1_first_component]);
vector_sum([Vector1_first_component|Vector1_rest], [Vector2_first_component|Vector2_rest], Intermediate_vector) 
    when is_list(Intermediate_vector) ->
        vector_sum(Vector1_rest, Vector2_rest, [Vector2_first_component + Vector1_first_component| Intermediate_vector]);
vector_sum([],[], Intermediate_vector) when is_list(Intermediate_vector) ->
    lists:reverse(Intermediate_vector).
%wrapper function
vector_sum(Vector1, Vector2) when is_list(Vector1), is_list(Vector2) ->
    vector_sum(Vector1, Vector2, []).

scalar_multiplication(Scalar, Vector) when is_float(Scalar) or is_integer(Scalar), is_list(Vector) ->
    lists:map(fun(Element) -> Element*Scalar end, Vector).


vector_length(Vector) ->
    vector_length(Vector, []).
vector_length([],[]) -> error_empty_list_is_no_vector;
vector_length([Vector_first_component| Vector_rest],[]) ->
    vector_length(Vector_rest, Vector_first_component * Vector_first_component);
vector_length([Vector_first_component| Vector_rest], Intermediate_result) ->
    vector_length(Vector_rest, Vector_first_component * Vector_first_component + Intermediate_result);
vector_length([], Vector_length_squared) ->
    math:sqrt(Vector_length_squared).



vector_generate(random, Length) when is_integer(Length), Length > 0 ->
    vector_generate_random(Length, []).
    
vector_generate_random(Length, Vector) when Length > 0 ->
    vector_generate_random(Length -1, [math:pow(-1,random:uniform(2)) * random:uniform(1000)|Vector]);
vector_generate_random(0,Vector) -> 
    Vector.
    
