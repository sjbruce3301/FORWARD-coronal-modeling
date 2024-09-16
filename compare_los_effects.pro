pro compare_LOS_effects

  ;~~~~~~~~~~~~~~~~~~~~~~COMPARE SHORT VS LONG LOS INTEGRATION~~~~~~~~~~~~~~~~~~~~~~~~~~
;
  ;LOS_int_absvalue = 0.5
  

  spectrum = cram_sim_spectrum(3500, 4600, 100, 1.5, 0.5, 0.1)
  wavelengths = spectrum[*,0]
  spec_intensity = spectrum[*,1]
  spec_p = PLOT(wavelengths, spec_intensity, title=TeXtoIDL('Integrated Intensity Spectra at 0.5MK'), $
    xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity', 'b', NAME=TeXtoIDL('LOS Int. = 0.1R☉'))
    

  spectrum2 = cram_sim_spectrum(3500, 4600, 100, 1.5, 0.5, 0.5)
  wavelengths = spectrum2[*,0]
  spec_intensity2 = spectrum2[*,1]
  spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, 'r', NAME=TeXtoIDL('LOS Int. = 0.5R☉'))
  
  spectrum_1 = cram_sim_spectrum(3500, 4600, 100, 1.5, 0.5, 1.0)
  wavelengths = spectrum_1[*,0]
  spec_intensity_1 = spectrum_1[*,1]
  spec_p21= PLOT(/overplot, wavelengths, spec_intensity_1, color = [0, 100, 65], NAME=TeXtoIDL('LOS Int. = 1.0R☉'))
  
  spectrum5 = cram_sim_spectrum(3500, 4600, 100, 1.5, 0.5, 1.5)
  wavelengths = spectrum5[*,0]
  spec_intensity5 = spectrum5[*,1]
  spec_p5= PLOT(/overplot, wavelengths, spec_intensity5, color = [0, 200, 120], NAME=TeXtoIDL('LOS Int. = 1.5R☉'))
  
  spectrum3 = cram_sim_spectrum(3500, 4600, 100, 1.5, 0.5, 2.0)
  wavelengths = spectrum3[*,0]
  spec_intensity3 = spectrum3[*,1]
  spec_p3= PLOT(/overplot, wavelengths, spec_intensity3, color=[22, 222, 65], NAME=TeXtoIDL('LOS Int. = 2.0R☉'))
  
  spectrum4 = cram_sim_spectrum(3500, 4600, 100, 1.5, 0.5, 7.0)
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
  mapped21 = linear_mapping(spec_intensity_1,0,100)
  mapped5 = linear_mapping(spec_intensity5,0,100)
  
  mapped_plot = PLOT(wavelengths, mapped1, title=TeXtoIDL('Linear Mapped Intensity Spectra at 2.0MK'), $
    xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity', 'b', NAME=TeXtoIDL('LOS Int. = 0.1R☉')) ;, XRANGE = [3690,3760], YRANGE = [10, 18]
  mapped_plot2 = PLOT(/overplot, wavelengths, mapped2, 'r', NAME=TeXtoIDL('LOS Int. = 0.5R☉'))
  mapped_plot21 = PLOT(/overplot, wavelengths, mapped21,color = [0, 100, 65], NAME=TeXtoIDL('LOS Int. = 1.0R☉'))
  mapped_plot5 = PLOT(/overplot, wavelengths, mapped5,color = [0, 200, 120], NAME=TeXtoIDL('LOS Int. = 1.5R☉'))
  mapped_plot3 = PLOT(/overplot, wavelengths, mapped3, color=[22, 222, 65], NAME=TeXtoIDL('LOS Int. = 2.0R☉'))
  mapped_plot4 = PLOT(/overplot, wavelengths, mapped4, color=[255, 179, 0], NAME=TeXtoIDL('LOS Int. = 7.0R☉'))
  
  l = legend(TARGET=[mapped_plot,mapped_plot2, mapped_plot3, mapped_plot4], POSITION=[3900,95], $
    /DATA, /AUTO_TEXT_COLOR)


  diff1 = (mapped2 - mapped1)
  diff21 = (mapped21 - mapped1)
  diff2 = (mapped3 - mapped1)
  diff3 = (mapped4 - mapped1)
  diff5 = (mapped5 - mapped1)
  
  
  diffp = PLOT(wavelengths, diff1, title=TeXtoIDL('Linear Mapped Intensity Spectra Difference at 2.0MK'), $
    xtitle='Wavelength (Å)', ytitle='Spectra Difference (%)', 'b', NAME=TeXtoIDL('LOS diff = 0.1 - 0.5R☉'))
  diffp21 = PLOT(/overplot, wavelengths, diff21, 'r', NAME=TeXtoIDL('LOS diff = 0.1 - 1.0R☉'))
  diffp5 = PLOT(/overplot, wavelengths, diff5, color = [0, 100, 65], NAME=TeXtoIDL('LOS diff = 0.1 - 1.5R☉'))
  diffp2 = PLOT(/overplot, wavelengths, diff2,  color=[255, 179, 0], NAME=TeXtoIDL('LOS diff = 0.1 - 2.0R☉'))
  diffp3 = PLOT(/overplot, wavelengths, diff3, color=[22, 222, 65], NAME=TeXtoIDL('LOS diff = 0.1 - 7.0R☉'))
  
  l = legend(TARGET=[diffp,diffp2,diffp3,diffp21], POSITION=[4550,1.85], $
    /DATA, /AUTO_TEXT_COLOR)

    
    
;at 4015A, plot differences for LOS.
;

  LOS_array = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4.0, 4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 5.0, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9, 6.0, 6.1, 6.2, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9, 7.0]
  
  wvl_list = []
  
  FOR i = 0, N_ELEMENTS(LOS_array)-1 DO BEGIN
    spectrum = cram_sim_spectrum(4015, 4015, 1, 1.5, 0.5, LOS_array[i])
    wvl_list = [wvl_list, spectrum]
  ENDFOR

end
