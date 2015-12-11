-module(sample_spectra_tests).
-include_lib("eunit/include/eunit.hrl").

make_sine_spectrum_test() ->
    ?assertEqual(256, length(sample_spectra:make_sine_spectrum(256)))
.

make_sample_spectrum_bucket_test() ->
    ?assertEqual(ok, sample_spectra:make_sample_spectrum_bucket([2,2], "/var/tmp/sine_spectra_test_directory", 10, plain))
.

