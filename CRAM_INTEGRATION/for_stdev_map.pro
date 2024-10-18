function for_stdev_map, spectrum

  std_dev = STDEV(spectrum)
     
  
;~~~~~~~~~~~~~~~~~~~~~~COMPARE SHORT VS LONG LOS INTEGRATION~~~~~~~~~~~~~~~~~~~~~~~~~~

;LOS_int_absvalue = 0.5
;spectrum = cram_sim_spectrum(3500, 4600, 6, 1.5, 0.1,LOS_int_absvalue)
;wavelengths = spectrum[*,0]
;spec_intensity = spectrum[*,1]

;PLOT, wavelengths, spec_intensity, title=TeXtoIDL('Intensity Spectrum, T_e = 0.1'), $
;  xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity';, $
;  ;COLOR=i+1 ;/YNOZERO


;LOS_int_absvalue = 7
;spectrum2 = cram_sim_spectrum(3500, 4600, 6, 1.5, 0.1,LOS_int_absvalue)
;wavelengths = spectrum2[*,0]
;spec_intensity2 = spectrum2[*,1]
;OPLOT, wavelengths, spec_intensity2;, COLOR=i+1


  ;p_stdev.Save, "stdev_"+strn(temp_points)+".png", BORDER=10, $
     ;RESOLUTION=300, /TRANSPARENT
     
     return,std_dev
end

;  std_deviation = STDDEV(spectrum[*, 1])