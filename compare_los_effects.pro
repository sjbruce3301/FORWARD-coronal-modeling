pro compare_LOS_effects

  ;~~~~~~~~~~~~~~~~~~~~~~COMPARE SHORT VS LONG LOS INTEGRATION~~~~~~~~~~~~~~~~~~~~~~~~~~
;
  ;LOS_int_absvalue = 0.5
  

  spectrum = cram_sim_spectrum(3500, 4600, 500, 1.5, 0.5, 0.1)
  wavelengths = spectrum[*,0]
  spec_intensity = spectrum[*,1]
  spec_p = PLOT(wavelengths, spec_intensity, title=TeXtoIDL('Integrated Intensity Spectra at 0.5MK'), $
    xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity', 'b', NAME=TeXtoIDL('LOS Int. = 0.1R☉'))
    

  spectrum2 = cram_sim_spectrum(3500, 4600, 500, 1.5, 0.5, 0.5)
  wavelengths = spectrum2[*,0]
  spec_intensity2 = spectrum2[*,1]
  spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, 'r', NAME=TeXtoIDL('LOS Int. = 0.5R☉'))
  
  spectrum3 = cram_sim_spectrum(3500, 4600, 500, 1.5, 0.5, 2.0)
  wavelengths = spectrum3[*,0]
  spec_intensity3 = spectrum3[*,1]
  spec_p3= PLOT(/overplot, wavelengths, spec_intensity3, color=[22, 222, 65], NAME=TeXtoIDL('LOS Int. = 2.0R☉'))
  
  spectrum4 = cram_sim_spectrum(3500, 4600, 500, 1.5, 0.5, 7.0)
  wavelengths = spectrum4[*,0]
  spec_intensity4 = spectrum4[*,1]
  spec_p4= PLOT(/overplot, wavelengths, spec_intensity4, color=[245, 27, 136], NAME=TeXtoIDL('LOS Int. = 7.0R☉'))

  ; Create a legend object
  l = legend(TARGET=[spec_p,spec_p2, spec_p3, spec_p4], POSITION=[3900,0.17], $
    /DATA, /AUTO_TEXT_COLOR)
    
    
  mapped1 = linear_mapping(spec_intensity, 0, 100)
  ;print,mapped1
  mapped2 = linear_mapping(spec_intensity2, 0, 100)
  ;print,mapped2
  mapped3 = linear_mapping(spec_intensity3, 0, 100)
  mapped4 = linear_mapping(spec_intensity4, 0, 100)
  
  mapped_plot = PLOT(wavelengths, mapped1, title=TeXtoIDL('Linear Mapped Intensity Spectra at 0.5MK'), $
    xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity', 'b', NAME=TeXtoIDL('LOS Int. = 0.1R☉')) ;, XRANGE = [3690,3760], YRANGE = [10, 18]
  mapped_plot2 = PLOT(/overplot, wavelengths, mapped2, 'r', NAME=TeXtoIDL('LOS Int. = 0.5R☉'))
  mapped_plot3 = PLOT(/overplot, wavelengths, mapped3, color=[22, 222, 65], NAME=TeXtoIDL('LOS Int. = 2.0R☉'))
  mapped_plot4 = PLOT(/overplot, wavelengths, mapped4, color=[255, 179, 0], NAME=TeXtoIDL('LOS Int. = 7.0R☉'))
  
  l = legend(TARGET=[mapped_plot,mapped_plot2, mapped_plot3, mapped_plot4], POSITION=[3900,95], $
    /DATA, /AUTO_TEXT_COLOR)


  diff1 = (mapped2 - mapped1)
  diff2 = (mapped3 - mapped1)
  diff3 = (mapped4 - mapped1)
  
  diffp = PLOT(wavelengths, diff1, title=TeXtoIDL('Linear Mapped Intensity Spectra Difference at 0.5MK'), $
    xtitle='Wavelength (Å)', ytitle='Spectra Difference (%)', 'b', NAME=TeXtoIDL('LOS Int. = 0.1R☉ to 0.5R☉'))
  diffp2 = PLOT(/overplot, wavelengths, diff2, 'r', NAME=TeXtoIDL('LOS Int. = 0.1R☉ to 2.0R☉'))
  diffp3 = PLOT(/overplot, wavelengths, diff3, color=[22, 222, 65], NAME=TeXtoIDL('LOS Int. = 0.1R☉ to 7.0R☉'))
  
  l = legend(TARGET=[diffp,diffp2,diffp3], POSITION=[4550,1.85], $
    /DATA, /AUTO_TEXT_COLOR)

    
    
  

end
