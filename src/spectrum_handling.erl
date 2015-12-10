-module(spectrum_handling).

-export([read_spectrum_from/2]).

% @doc Read and return a spectrum from a given source
read_spectrum_from(filesystem, Path) ->
    {ok, Spectrum} = file:consult(Path),
    hd(Spectrum);
read_spectrum_from(random_sine, Spectrum_length) ->
    sample_spectra:make_sine_spectrum(Spectrum_length)
.

