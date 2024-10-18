pro spectra_by_temp

spectrum1 = cram_sim_spectrum(3500, 4600, 1500, 1.5, 0.4, 5.0)
wavelengths = spectrum[*,0]
spec_intensity = spectrum[*,1]

spec_p = PLOT(wavelengths, spec_intensity, title=TeXtoIDL('Integrated Intensity Spectra by Electron Temperature'), $
  xtitle='Wavelength (Å)', ytitle='Total Integrated Intensity', color=[20, 13, 214], NAME=TeXtoIDL('T_e = 0.41MK'), $
  XRANGE = [3500,4600])
;COLOR=i+1 ;/YNOZERO

spectrum2 = cram_sim_spectrum(3500, 4600, 600, 1.5, 0.7, 2.0)
wavelengths = spectrum2[*,0]
spec_intensity2 = spectrum2[*,1]
spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, color=[22, 192, 222], NAME=TeXtoIDL('T_e = 0.7MK'));, COLOR=i+1

;LOS_int_absvalue = 7
spectrum2 = cram_sim_spectrum(3500, 4600, 600, 1.5, 1.0, 2.0)
wavelengths = spectrum2[*,0]
spec_intensity2 = spectrum2[*,1]
spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, color=[22, 222, 65], NAME=TeXtoIDL('T_e = 1.0MK'));, COLOR=i+1

spectrum2 = cram_sim_spectrum(3500, 4600, 600, 1.5, 1.3, 2.0)
wavelengths = spectrum2[*,0]
spec_intensity2 = spectrum2[*,1]
spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, color=[131, 245, 24], NAME=TeXtoIDL('T_e = 1.3MK'));, COLOR=i+1

spectrum2 = cram_sim_spectrum(3500, 4600, 600, 1.5, 1.7, 2.0)
wavelengths = spectrum2[*,0]
spec_intensity2 = spectrum2[*,1]
spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, color=[232, 229, 67], NAME=TeXtoIDL('T_e = 1.7MK'));, COLOR=i+1

spectrum2 = cram_sim_spectrum(3500, 4600, 600, 1.5, 2.0, 2.0)
wavelengths = spectrum2[*,0]
spec_intensity2 = spectrum2[*,1]
spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, color=[232, 166, 67], NAME=TeXtoIDL('T_e = 2.0MK'));, COLOR=i+1

spectrum2 = cram_sim_spectrum(3500, 4600, 600, 1.5, 2.3, 2.0)
wavelengths = spectrum2[*,0]
spec_intensity2 = spectrum2[*,1]
spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, color=[245, 139, 122], NAME=TeXtoIDL('T_e = 2.3MK'));, COLOR=i+1

spectrum2 = cram_sim_spectrum(3500, 4600, 600, 1.5, 2.7, 2.0)
wavelengths = spectrum2[*,0]
spec_intensity2 = spectrum2[*,1]
spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, color=[242, 5, 5], NAME=TeXtoIDL('T_e = 2.7MK'));, COLOR=i+1

spectrum2 = cram_sim_spectrum(3500, 4600, 600, 1.5, 3.0, 2.0)
wavelengths = spectrum2[*,0]
spec_intensity2 = spectrum2[*,1]
spec_p2= PLOT(/overplot, wavelengths, spec_intensity2, color=[245, 27, 136], NAME=TeXtoIDL('T_e = 3.0MK'));, COLOR=i+1

; Add a legend for clarity
legendItems = ['T_e = 0.4MK', 'T_e = 0.3MK','T_e = 0.5MK', 'T_e = 0.9MK', 'T_e = 1.3MK','T_e = 1.7MK','T_e = 2.1MK','T_e = 2.5MK','T_e = 3.0MK']
legendColors = [1, 2, 3, 4, 5, 6, 7, 8]

; Create a legend object
l = legend(TARGET=[spec_p,spec_p2], POSITION=[3850,0.17], $

  /DATA, /AUTO_TEXT_COLOR)


end