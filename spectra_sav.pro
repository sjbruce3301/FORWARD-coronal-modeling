pro spectra_sav
  
  print,'start'
  s1 = cram_sim_spectrum(3500, 4600, 600, 1.5, 0.4, 1.0)
  wavelength_scale = s1[*,0]
  i1 = s1[*,1]
  print,'s1'


  s2 = cram_sim_spectrum(3500, 4600, 600, 1.5, 0.7, 1.0)
  w2 = s2[*,0]
  i2 = s2[*,1]
  print,'s2'

  ;LOS_int_absvalue = 7
  s3 = cram_sim_spectrum(3500, 4600, 600, 1.5, 1.0, 1.0)
  w3 = s3[*,0]
  i3 = s3[*,1]

  s4 = cram_sim_spectrum(3500, 4600, 600, 1.5, 1.3, 1.0)
  w4 = s4[*,0]
  i4 = s4[*,1]

  s5 = cram_sim_spectrum(3500, 4600, 600, 1.5, 1.7, 1.0)
  w5 = s5[*,0]
  i5 = s5[*,1]
  print,'s5'

  s6 = cram_sim_spectrum(3500, 4600, 600, 1.5, 2.0, 1.0)
  w6 = s6[*,0]
  i6 = s6[*,1]

  s7 = cram_sim_spectrum(3500, 4600, 600, 1.5, 2.3, 1.0)
  w7 = s7[*,0]
  i7 = s7[*,1]

  s8 = cram_sim_spectrum(3500, 4600, 600, 1.5, 2.7, 1.0)
  w8 = s8[*,0]
  i8 = s8[*,1]
  print,'s8'

  s9 = cram_sim_spectrum(3500, 4600, 600, 1.5, 3.0, 1.0)
  w9 = s9[*,0]
  i9 = s9[*,1]

  
  filename = 'spectra_bytemp.sav'
  
  temps = [0.4, 0.7, 1.0, 1.3, 1.7, 2.0, 2.3, 2.7, 3.0]
  
  SAVE, i1, i2, i3, i4, i5, i6, i7, i8, i9, temps, wavelength_scale, FILENAME=filename


end