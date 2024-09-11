function for_freq_to_dens, freq, harmonic=harmonic

; written by Patrick McCauley Dec 2018
;
;input = frequency in Hz
;output = density in cm^-3

const = ( 4.*(!const.pi^2)*!const.me*!const.eps0 ) / ( !const.e^2 )

if keyword_set(harmonic) then dens = const*((freq/2.)^2) $
	else dens = const*(freq^2)

density = dens / (100.^3.) 

return, density
end
