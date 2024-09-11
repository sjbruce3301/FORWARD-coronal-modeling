pro forcodex,r3D,Dens,Temp,Vx,WL1,WL2,wlrat,wlspec,wavelengths

;
; placeholder code for CODEX type white light spectral calculation
;
; INPUTS:
;
;	R3D -- radial distance in units of Rsun
;	DENS -- electron number density, units (cm)^-3
;	TEMP -- electron temperature, units Kelvin
;	VX -- line-of-sight velocity, units km/sec
;
;       Dimensions of the above arrays [NLOS,NPOS]
;	  NLOS -- points along the line of sight-- CRAM program should integrate over this dimension
;	  NPOS -- points in the plane of sky -- CRAM program should return values for each of these points
;
;	WL1, WL2 -- wavelengths (Angstroms) at which to evaluate the WL spectrum, 
;	   and from which to calculate a radio (WLRAT)
;
; OUTPUTS: 
;
;	WLRAT -- ratio of the intensities at the two wavelengths, WL1, WL2
;		dimension [NPOS]
;	WLSPEC -- full spectrum of "white light" intensities vs wavelength
;			integrated over line of sight (and other relevant angles)
;		dimension [NWAVELENGTHS,NPOS]
;	WAVELENGTHS -- wavelengths in Angstrom corresponding to WLSPEC
;		 
;
; Called by FOR_CODEXOUTPUT


;NOTE -- "BAD" points (might be behind the sun, for example) will have value gt 1000
; so you need to screen for them
; for now I am setting density to zero there so they don't contribute to LOS integral

testbad=where(r3D ge 1000.)
dens[testbad]=0.

;
; this next is just to have something to use for debugging
wlrat=total(dens,1)
wavelengths=dindgen(1000.)
wlspec=wavelengths#wlrat


end
