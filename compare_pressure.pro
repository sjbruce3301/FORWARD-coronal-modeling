pro compare_pressure

  ;~~~~~~~~~~~~~~~~~~~~~~COMPARE SHORT VS LONG LOS INTEGRATION~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;
  ;LOS_int_absvalue = 0.5


  spectrum = cram_sim_spectrum(3500, 4600, 100, 1.5, 0.2, 1.0)
  wavelengths = spectrum[*,0]
  spec_intensity = spectrum[*,1]
  spec_p = PLOT(wavelengths, spec_intensity, title=TeXtoIDL('Integrated Intensity Spectra at 1.0MK by Change in Density \rho'), $
    xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity', 'b', NAME=TeXtoIDL('Change in \rho = 0.2'))


  spectrum2 = cram_sim_spectrum(3500, 4600, 100, 1.5, 0.5, 1.0)
  wavelengths = spectrum2[*,0]
  spec_intensity2 = spectrum2[*,1]
  spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, 'r', NAME=TeXtoIDL('Change in \rho = 0.5'))

  spectrum_1 = cram_sim_spectrum(3500, 4600, 100, 1.5, 1.0, 1.0)
  wavelengths = spectrum_1[*,0]
  spec_intensity_1 = spectrum_1[*,1]
  spec_p21= PLOT(/overplot, wavelengths, spec_intensity_1, color = [0, 100, 65], NAME=TeXtoIDL('Change in \rho = 1.0'))

  spectrum5 = cram_sim_spectrum(3500, 4600, 100, 1.5, 1.5, 1.0)
  wavelengths = spectrum5[*,0]
  spec_intensity5 = spectrum5[*,1]
  spec_p5= PLOT(/overplot, wavelengths, spec_intensity5, color = [0, 200, 120], NAME=TeXtoIDL('Change in \rho = 1.5'))

  spectrum3 = cram_sim_spectrum(3500, 4600, 100, 1.5, 2.0, 1.0)
  wavelengths = spectrum3[*,0]
  spec_intensity3 = spectrum3[*,1]
  spec_p3= PLOT(/overplot, wavelengths, spec_intensity3, color=[141, 196, 235], NAME=TeXtoIDL('Change in \rho = 2.0'))

  spectrum4 = cram_sim_spectrum(3500, 4600, 100, 1.5, 2.5, 1.0)
  wavelengths = spectrum4[*,0]
  spec_intensity4 = spectrum4[*,1]
  spec_p4= PLOT(/overplot, wavelengths, spec_intensity4, color=[222, 141, 235], NAME=TeXtoIDL('Change in \rho = 2.5'))
  
  ; Create a legend object
  l = legend(TARGET=[spec_p, spec_p4], POSITION=[4550,0.08], $
    /DATA, /AUTO_TEXT_COLOR)

  
end