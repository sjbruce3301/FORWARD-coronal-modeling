pro los_effects_plot

  ;LOS_array = [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4.0, 4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 5.0]
  array2 = [0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2.0, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5, 2.55, 2.6, 2.65, 2.7, 2.75, 2.8, 2.85, 2.9, 2.95, 3.0, 3.05, 3.1, 3.15, 3.2, 3.25, 3.3, 3.35, 3.4, 3.45, 3.5, 3.55, 3.6, 3.65, 3.7, 3.75, 3.8, 3.85, 3.9, 3.95, 4.0, 4.05, 4.1, 4.15, 4.2, 4.25, 4.3, 4.35, 4.4, 4.45, 4.5, 4.55, 4.6, 4.65, 4.7, 4.75, 4.8, 4.85, 4.9, 4.95, 5.0, 5.05, 5.1, 5.15, 5.2, 5.25, 5.3, 5.35, 5.4, 5.45, 5.5, 5.55, 5.6, 5.65, 5.7, 5.75, 5.8, 5.85, 5.9, 5.95, 6.0]

  ;LOS_array = [0.2, 0.3, 0.4, 0.5, 0.7, 0.9, 1.0, 1.3, 1.6, 2.0, 2.3, 2.6, 3.0, 3.5, 4.0, 4.5, 5.0]

  ;LOS_array = [0.200, 0.248, 0.297, 0.345, 0.394, 0.442, 0.491, 0.539, 0.588, 0.636, 0.685, 0.733, 0.782, 0.830, 0.879, 0.927, 0.976, 1.024, 1.073, 1.121, 1.170, 1.218, 1.267, 1.315, 1.364, 1.412, 1.461, 1.509, 1.558, 1.606, 1.655, 1.703, 1.752, 1.800, 1.849, 1.897, 1.946, 1.994, 2.043, 2.091, 2.140, 2.188, 2.237, 2.285, 2.334, 2.382, 2.431, 2.479, 2.528, 2.576, 2.625, 2.673, 2.722, 2.770, 2.819, 2.867, 2.916, 2.964, 3.013, 3.061, 3.110, 3.158, 3.207, 3.255, 3.304, 3.352, 3.401, 3.449, 3.498, 3.546, 3.595, 3.643, 3.692, 3.740, 3.789, 3.837, 3.886, 3.934, 3.983, 4.031, 4.080, 4.128, 4.177, 4.225, 4.274, 4.322, 4.371, 4.419, 4.468, 4.516, 4.565, 4.613, 4.662, 4.710, 4.759, 4.807, 4.856, 4.904, 4.953, 5.000]
  wvl_list = []
  
  LOS_array = [0.4, 0.6, 0.8, 1.0, 1.5, 2.0,3.0,4.0,5.0,6.0]
  
  
  s1 = cram_sim_spectrum(3500, 4350, 14, 1.5, 0.5, 0.1)
  print,'s1'
  print,s1[*,1]
  print,s1[*,0]
  
  mapped1 = linear_mapping(s1[*,1], 0, 100)
  print,'mapped1'
  print,mapped1
  ;stop
  
  ;print,n_elements(mapped1)
  
  wavelengths = LOS_array[*,0]

  FOR i = 0, N_ELEMENTS(LOS_array)-1 DO BEGIN
    ;print,LOS_array[i]
    spectrum = cram_sim_spectrum(3500, 4350, 14, 1.5, 0.5, LOS_array[i])
    ;print,spectrum
    mapped_new = linear_mapping(spectrum[*,1], 0, 100)
    print,'mapped1'
    print,mapped1
    
    print,'mapped_new'
    print,mapped_new
    
    ;print,n_elements(spectrum[*,1])
    
    diffplot = (mapped_new - mapped1)
    print,'diff'
    print,diffplot

    ;print,N_ELEMENTS(diffplot)
    
    wvl4015 = abs(diffplot[7]) ;[2,1]

    print,wvl4015
    ;stop
    
    wvl_list = [wvl_list, abs(wvl4015)]
    ;print,wvl_list
  ENDFOR

  
  wavelengths = LOS_array[*,0]
  spec_p = PLOT(wavelengths, wvl_list, title=TeXtoIDL('Percent Difference of LOS Lengths at 3950Å and T_e = 0.5MK'), $
    xtitle='LOS Length (R☉)', ytitle='Difference WRT 0.1R☉ LOS (%)', 'b', NAME=TeXtoIDL('LOS Int. = 0.1R☉'))
  
end