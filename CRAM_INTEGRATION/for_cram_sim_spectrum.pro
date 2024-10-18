;preliminary call: for_cram_sim_spectrum(lambda_min[set by user], lambda_max[set by user], num_points[set by user?], $
;                                         rho[from r3D], te[from ModSolStruct.Temp], file[from user?])

function for_cram_sim_spectrum, lambda_min, $ ;minimum wavelength
  lambda_max, $ ;maximum wavelength
  num_points, $ ;number of data points between max and min wavelengths
  r2, ModSolStruct, $ ;radial distance from sun, electron temperature
  irrad_spectrum_file ;file to use for irradiance spectrum
  
  rho = r2
  te = ModSolStruct.Temp
  
  stdevs = fltarr(n_elements(te), 1)

  ; Create the lamdaobs_range array
  lamdaobs_range = lambda_min + (lambda_max - lambda_min) * findgen(num_points) / (num_points - 1)
  ; Array to store results
  
  
  ;loop over all temps w corresponding radial distances 
  for temp = 0, n_elements(te) do begin   
    spectrum = fltarr(n_elements(lamdaobs_range), 2) ; Assuming 2 rows for wavelength and intensity 
  ; Loop over each wavelength
    for i = 0, (n_elements(lamdaobs_range) - 1) do begin
      ; Call cram_sim function
      result = for_cram_sim(lamdaobs_range[i],rho[temp],te[temp], irrad_spectrum_file)
    
      ; save results
      intot = result[0]
      ;intor = result[1] you can keep track of other parameters by adding rows to spectrum array

      ; Store results in spectrum array
      spectrum[i, 0] = lamdaobs_range[i] ;assign wavelength to column
      spectrum[i,1] = intot ;assign intensity to column
    endfor
    for_std_dev = STDDEV(spectrum[*, 1])
    stdevs[temp] = for_std_dev
  endfor
     
  
  return,stdevs
end
