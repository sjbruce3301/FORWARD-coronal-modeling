pro cram_sim_spectrum, lamdaobs_range, rho, te
  ; Initialize default parameters if not provided
  if n_params() lt 3 then te = 1
  if n_params() lt 2 then rho = 1.5
  if n_params() lt 1 then lamdaobs_range = [7800, 8000, 8200] ; Example range in Angstroms

  ; Example wavelength range
  ;lamdaobs_range = [7800, 8000, 8200]
 
  ; Define the desired wavelength range
  lambda_min = 3800.0   ; Minimum wavelength in Angstroms
  lambda_max = 8000.0   ; Maximum wavelength in Angstroms
  num_points = 10     ; Number of points

  ; Create the lamdaobs_range array
  lamdaobs_range = lambda_min + (lambda_max - lambda_min) * findgen(num_points) / (num_points - 1)
  ; Array to store results
  spectrum = fltarr(n_elements(lamdaobs_range), 2) ; Assuming 3 columns for intot, intor, polp
  
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
  print, spectrum
  
  p=PLOT(spectrum[*, 0], spectrum[*, 1], $
     linestyle=0, color=0, $
     title='Spectrum Plot', $
     xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity')
end
