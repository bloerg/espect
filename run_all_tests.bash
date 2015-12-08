#!/bin/bash
erlc -o ebin/ src/*.erl test/*.erl
erl -pa ebin/ -noshell -s run_all_tests start -s init stop
