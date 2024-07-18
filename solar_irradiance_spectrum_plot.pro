pro solar_irradiance_spectrum_plot

  restore,'/Users/sbruce/Documents/GitHub/FORWARD-coronal-modeling/irradiance_solar.sav' ;load irradiance tester file (generated from python script)
  solradii = 6.9599 * 10.^10. ;solar radius (cm)
  au = 1.495979 * 10.^13.   ;earth-sun distance (cm)
  irradtoflux = (au/solradii)^2/!pi
  
  irtot = irtotnl
  lamdasrc = reform(irtot(0,*)) ;wavelength scale of atlas - take out only wavelength values into 1D array
  izero = reform(irtot(1,*)) * irradtoflux  ;convert to solar flux - take out irrad values into 1D array and multiply by conversion factor.
  
  spec_p = PLOT(lamdasrc, izero, title=TeXtoIDL('Irradiance spectrum'), $
    xtitle='Wavelength (Å)', ytitle='Irradiamce', color=[20, 13, 214], NAME=TeXtoIDL('T_e = 0.41MK'), $
    XRANGE = [3500,4600])
  
end