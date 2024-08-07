;file to generate synthetic solar data (not accurate!) for the purpose of testing cram_sim
   ;without a solar irradiance spectrum file

pro irrad_data_gen

; Define the wavelength range and create synthetic data
wavelengths = dindgen(1000) * (12500 - 3300) / 999 + 3300  ; Generate 1000 points from 3300 Å to 12500 Å
irradiance = sin(dindgen(1000) * 2 * !PI / 999) + 10       ; Create synthetic solar irradiance data
;irradiance = 0.5 + 0.5 * sin((wavelengths - 7000) * !PI / 1000)^2

; Combine into a 2D array, first row is wavelengths, second is irradiance
irtotnl = dblarr(2, 1000)   ; Initialize a 2x1000 array
irtotnl(0, *) = wavelengths  ; First row
irtotnl(1, *) = irradiance   ; Second row

; Save to a .sav file
save, irtotnl, filename='/Users/sbruce/Documents/GitHub/FORWARD-coronal-modeling/synthetic_irradiance_idl_corona.sav'

print, 'Synthetic .sav file created successfully.'
;print, irradiance
end