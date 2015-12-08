#!/bin/bash

sine_spectra_test_directory="/var/tmp/sine_spectra_test_directory"
if [ ! -d "$sine_spectra_test_directory" ] ; then
  echo "Error: cannot run tests."
  echo "Please mkdir /var/tmp/sine_spectra_test_directory for testing purposes!"
else
    erlc -o ebin/ src/*.erl test/*.erl
    erl -pa ebin/ -noshell -s run_all_tests start -s init stop
fi

