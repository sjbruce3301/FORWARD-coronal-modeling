pro stdev_map_test

  temp_min = 0.1
  temp_max = 3
  temp_points = 4
  
  temp_range = temp_min + (temp_max - temp_min) * findgen(temp_points) / (temp_points - 1)
  dev_array = fltarr(n_elements(temp_range), 2)
  
  LOS_int=0.5
  
  for i = 0, (n_elements(temp_range) - 1) do begin
    ; Call cram_sim function
    ;print,lamdaobs_range[i]
    spectrum = cram_sim_spectrum(3500, 4600, 70, 1.5, temp_range[i], LOS_int)
    wavelengths = spectrum[*,0]
    spec_intensity = spectrum[*,1]
    ;print,t

    ; Unpack results
    std_dev = STDDEV(spec_intensity)
    ;    intor = result[1]
    ;    polp = result[2]
    ;print,intot
    ; Store results in spectrum array
    ;print,i
    dev_array[i, 0] = temp_range[i]
    dev_array[i,1] = std_dev
    
    
    spec_plot=PLOT(wavelengths, spec_intensity, $
         linestyle=0, color=0, $
         title=TeXtoIDL('Intensity Spectrum, T_e = '+strn(temp_range[i])), $
         xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity', $
         XRANGE = [3550,4600])
  endfor
  
  
  
  p_stdev=PLOT(dev_array[*, 0], dev_array[*, 1], $
     linestyle=0, COLOR='r', $
     title=TeXtoIDL('Spectral Standard Deviation by Temperature, λ = 3500 - 4600 Å'), $
     xtitle='Temperature (MK)', ytitle='Standard Deviation of Spectra')
     
  
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
end

;  std_deviation = STDDEV(spectrum[*, 1])