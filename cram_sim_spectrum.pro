function cram_sim_spectrum, l_min, $ ;minimum wavelength
  l_max, $ ;maximum wavelength
  num_points, $ ;number of data points between max and min wavelengths
  rho, te  ;radial distance from sun, electron temperature  
  
  ; Initialize default parameters if not provided
  ;if n_params() lt 5 then te = 1
  ;if n_params() lt 4 then rho = 1.5
  ;if n_params() lt 3 then num_points = 20
  ;if n_params() lt 2 then lambda_max = 4600
  ;if n_params() lt 1 then lambda_min = 3500
 
  ; Define the desired wavelength range
  ;lambda_min = 3550.0   ; Minimum wavelength in Angstroms
  ;lambda_max = 4600.0   ; Maximum wavelength in Angstroms
  ;num_points = 10     ; Number of points

  ; Create the lamdaobs_range array
  lamdaobs_range = l_min + (l_max - l_min) * findgen(num_points) / (num_points - 1)
  ; Array to store results
  spectrum = fltarr(n_elements(lamdaobs_range), 2) ; Assuming 2 columns for wavelength and intensity
  
  ; Loop over each wavelength
  for i = 0, (n_elements(lamdaobs_range) - 1) do begin
    ; Call cram_sim function
    ;print,lamdaobs_range[i]
    result = cram_sim(lamdaobs_range[i],rho,te)
    
    ; Unpack results
    intot = result[0]
;    intor = result[1]
;    polp = result[2]
    ;print,intot
    ; Store results in spectrum array
    ;print,i
    spectrum[i, 0] = lamdaobs_range[i]
    spectrum[i,1] = intot
  endfor
  
  ; Print or process spectrum data
  ;print, 'Wavelength (Angstrom)   Intensity   Radial Intensity   Polarization'
  ;print, spectrum

  p=PLOT(spectrum[*, 0], spectrum[*, 1], $
     linestyle=0, color=0, $
     title=TeXtoIDL('Intensity Spectrum, T_e = '+strn(te)+' ρ = '+strn(rho)), $
     xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity', $
     XRANGE = [3550,4600])
     
  ;mean_intensity = MEAN(spectrum[*, 1])
  std_deviation = STDDEV(spectrum[*, 1])
  return,std_deviation


end
