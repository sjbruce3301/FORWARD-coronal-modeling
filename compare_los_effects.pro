pro compare_LOS_effects

  ;~~~~~~~~~~~~~~~~~~~~~~COMPARE SHORT VS LONG LOS INTEGRATION~~~~~~~~~~~~~~~~~~~~~~~~~~
;
  ;LOS_int_absvalue = 0.5

  spectrum = cram_sim_spectrum(3500, 4600, 100, 1.5, 0.1, 7.0)
  wavelengths = spectrum[*,0]
  spec_intensity = spectrum[*,1]

  spec_p = PLOT(wavelengths, spec_intensity, title=TeXtoIDL('Intensity Spectrum, T_e = 0.1MK'), $
    xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity', 'b2', NAME='7.0 R☉ LOS Limit')
  ;COLOR=i+1 ;/YNOZERO


  ;LOS_int_absvalue = 7
  spectrum2 = cram_sim_spectrum(3500, 4600, 100, 1.5, 0.1, 0.5)
  wavelengths = spectrum2[*,0]
  spec_intensity2 = spectrum2[*,1]
  spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, 'r2', NAME='0.5 R☉ LOS Limit');, COLOR=i+1

  ; Add a legend for clarity
  legendItems = ['0.5 R', '7 R']
  legendColors = [2, 1]

  ; Create a legend object
  l = legend(TARGET=[spec_p,spec_p2], POSITION=[3900,0.17], $

    /DATA, /AUTO_TEXT_COLOR)

end
