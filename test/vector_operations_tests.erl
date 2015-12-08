-module(vector_operations_tests).
-include_lib("eunit/include/eunit.hrl").

vector_distance_test() ->
    ?assertEqual(error_two_empty_vectors_given, vector_operations:vector_distance([],[])),
    ?assertEqual(error_first_vector_too_short, vector_operations:vector_distance([1],[1,2,3])),
    ?assertEqual(error_second_vector_too_short, vector_operations:vector_distance([1,2,3],[2,3])),
    ?assertEqual(0.0, vector_operations:vector_distance([0.0],[0.0])),
    ?assertEqual(10.0, vector_operations:vector_distance([0.0,3.0,4.0],[0.0,-3.0,-4.0])),
    ?assertEqual(12.0, vector_operations:vector_distance([6.0,12.0,15.0],[-6.0,12.0,15.0])).


vector_distance_squared_test() ->
    ?assertEqual(error_two_empty_vectors_given, vector_operations:vector_distance_squared([],[],[])),
    ?assertEqual(error_first_vector_too_short, vector_operations:vector_distance_squared([1],[1,2,3],[])),
    ?assertEqual(error_second_vector_too_short, vector_operations:vector_distance_squared([1,2,3],[2,3],[])),
    ?assertEqual(0.0, vector_operations:vector_distance_squared([0.0,0.0],[0.0,0.0],[])),
    ?assertEqual(0.0, vector_operations:vector_distance_squared([0.0],[0.0],[])),
    ?assertEqual(100.0, vector_operations:vector_distance_squared([0.0,3.0,4.0],[0.0,-3.0,-4.0],[])),
    ?assertEqual(144.0, vector_operations:vector_distance_squared([6.0,12.0,15.0],[-6.0,12.0,15.0],[])).

vector_difference_test() ->
    ?assertEqual(error_two_empty_vectors_given, vector_operations:vector_difference([],[],[])),
    ?assertEqual(error_first_vector_too_short, vector_operations:vector_difference([1],[1,2,3],[])),
    ?assertEqual(error_second_vector_too_short, vector_operations:vector_difference([1,2,3],[2,3],[])),
    ?assertEqual([0.0,0.0], vector_operations:vector_difference([0.0,0.0],[0.0,0.0],[])),
    ?assertEqual([0.0], vector_operations:vector_difference([0.0],[0.0],[])),
    ?assertEqual([0.0,-6.0,-8.0], vector_operations:vector_difference([0.0,3.0,4.0],[0.0,-3.0,-4.0],[])),
    ?assertEqual([-12.0,0.0,0.0], vector_operations:vector_difference([6.0,12.0,15.0],[-6.0,12.0,15.0],[])).

vector_sum_test() ->
    ?assertEqual([0.0], vector_operations:vector_sum([0.0],[0.0])),
    ?assertEqual([0.0, 0.0, 0.0], vector_operations:vector_sum([0.0,3.0,4.0],[0.0,-3.0,-4.0])),
    ?assertEqual([0.0, 24.0, 30.0], vector_operations:vector_sum([6.0,12.0,15.0],[-6.0,12.0,15.0])).

vector_length_test() ->
    ?assertEqual(error_empty_list_is_no_vector, vector_operations:vector_length([],[])),
    ?assertEqual(0.0, vector_operations:vector_length([0.0],[])),
    ?assertEqual(5.0, vector_operations:vector_length([0.0,3.0,4.0],[])),
    ?assertEqual(5.0, vector_operations:vector_length([-3.0,-4.0,0.0],[])).
    
