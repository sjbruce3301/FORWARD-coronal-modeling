FUNCTION for_sun_flux,lmbda,dlambda,x2,y2,x1,y1,$
                  flm        = flm,          $
                  expTime    = expTime,      $
                  Aperture   = Aperture,     $
                  Resolution = Resolution,   $
                  Efficiency = Efficiency,   $
                  power      = power,        $
                  noflux      = noflux,        $
                  quiet      = quiet    


; flux table taken from Allen's Astrophysical Quantities
; flm is the spectral flux in 0.001*Watt/m^2/A(nstrong)
; Always use angstrom for input wavelength (lambda and dlambda)
; aperture is in cm (assuming circular aperture)
; return the number of photons at the wavelength given by lambda with the
; telescope aperture
;
; Called by FOR_FIXUNITS, FOR_PERSONALITY, FOR_READCOMPFITS
;
; Created 2000/09/10
; H. Lin, IfA
;
; Added to FORWARD SEG 2013/12/09
;
; Version 2.0 July 2014
;   fixed bug where lambda was being passed as 1-point array, messing
;   up fit to Allen curve
;   Also changed Rsun to 959 arcsec (was 975)
;
; December 2021 -- put in kluge for UV, flm flat .1-.2 microns
; March 2022 -- expanded/un-kluged UV using SORCE data
; Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
; January 2023-- removed kluge for UV; replaced with use of chromorad
;	by passing through noflux and getting information on 
;	telescope weighting only
; October 2023 -- updated h
; February 2024 -- changed parameters from float to double to be consistent with labeling in structure
;--

lambda=lmbda[0]

; next line for debugging
;if (lambda GT 50000 OR lambda LT 1000) THEN BEGIN
if (lambda GT 50000 OR lambda LT 2000) and keyword_set(noflux) eq 0 THEN BEGIN

  print,'Input wavelength must be between 0.2 to 5.0 microns (2000 to 50000 A)'
  return,0

ENDIF

IF (NOT keyword_set(Aperture))   THEN Aperture   = 1.
IF (NOT keyword_set(Resolution)) THEN Resolution = 1.
IF (NOT keyword_set(expTime))    THEN expTime    = 0.01
IF (NOT keyword_set(Efficiency)) THEN Efficiency = 0.1

; Make sure all the numbers are real...
; H.Lin, 20131124
Aperture=double(Aperture)
Resolution=double(Resolution)
expTime=double(expTime)
Efficiency=double(Efficiency)

h=6.6262d-27 		; Planck constant in ergs*second
                        ; 1 Watt = 10^7 ergs/sec = 1 Joule/sec
c=2.9979d10			; speed of light in cm/sec
                        ; 1 A = 10^-8 cm 

tele_area     = !dpi*Aperture^2/4.d0
photon_energy = h*c/lambda/1.0d-8
;
; photon_energy is in units of ergs
;
; Note that this table is given in microns and 0.001Watt/m^2/A
;

;
; next lines for debugging
;flm=[[0.120,   .01],$
;     [0.1214,   .2],$
;     [0.1216,   5.6],$
;     [0.1217,   .2],$
;     [0.122,   0.02],$
;     [0.123,   0.01],$
;     [0.14,   0.01],$
;     [0.16,   0.02],$
;     [0.18,   0.4],$
;     [0.20,   0.65],$

flm=[[0.20,   0.65],$
     [0.22,   4.50],$
     [0.24,   5.20],$
     [0.26,  13.00],$
     [0.28,  23.00],$
     [0.30,  56.00],$
     [0.32,  76.00],$
     [0.34,  91.00],$
     [0.36,  97.00],$
     [0.37, 113.00],$
     [0.38, 107.00],$
     [0.39, 103.00],$
     [0.40, 148.00],$
     [0.41, 170.00],$
     [0.42, 173.00],$
     [0.43, 159.00],$
     [0.44, 184.00],$
     [0.45, 200.00],$
     [0.46, 205.00],$
     [0.48, 203.00],$
     [0.50, 192.00],$
     [0.55, 188.00],$
     [0.60, 177.00],$
     [0.65, 159.00],$
     [0.70, 141.00],$
     [0.75, 127.00],$
     [0.80, 114.00],$
     [0.90,  94.00],$
     [1.00,  75.00],$
     [1.10,  61.00],$
     [1.20,  52.00],$
     [1.40,  35.00],$
     [1.60,  25.50],$
     [1.80,  16.90],$
     [2.00,  11.60],$
     [2.50,   5.20],$
     [3.00,   2.60],$
     [4.00,   0.90],$
     [5.00,   0.40]]

x1=flm[0,*]*10000.d0
y1=flm[1,*]*0.001d0*1d7/1d4/(!dpi*959.d0^2)	; Convert into ergs/cm^2/A/sec/arcsec^2
					; 0.001 was from 0.001 Watt (See Allen's)
					; 1d7 is Watt to erg 
					; 1d4 is m^2 to cm^2
					; !dpi*959^2 is the solar disk area in arcsec

x1=reform(x1)
y1=reform(y1)
yy=spl_init(x1,y1)
x2=findgen(500)*100.d0
;
; next line for debugging
;x2=x2[10:499]
x2=x2[20:499]
y2=spl_interp(x1,y1,yy,x2)

junk=min(abs(x2-lambda),idx)
flux=y2[idx]
flux=double(flux[0])
			
;
; note flux is the flux for the sun central disk brightness
; at the wavelength inputted
;
; if noflux set -- flux and dlambda are set to 1 and the N_Photons or power
;  needs to be multiplied by total line-integrated flux externally

if keyword_set(noflux) then begin
 N_Photons=tele_area*expTime*Efficiency*Resolution^2
; units cm^2 s arcsec^2
endif else begin
 N_Photons=flux*dlambda*tele_area*expTime*Efficiency*Resolution^2/photon_energy
endelse

IF (NOT keyword_set(quiet)) and (NOT keyword_set(noflux)) THEN BEGIN
print,'-----------------------------'
print,'Observing Parameters  '
print,'-----------------------------'
print,'Wavelength          = ',lambda,' A'
print,'Spectral Resolution = ',dlambda,' A'
print,'Telescope Aperture  = ',Aperture,' cm'
print,'Exposure Time       = ',expTIme,' sec'
print,'Resolution          = ',Resolution,' arcsec'
print,'Efficiency          = ',Efficiency
print,''
print,'Solar Flux    = ',flux,' ergs/cm^2/A/sec/arcsec^2'
print,'Photon Energy = ',photon_energy,' ergs'
print,''
print,'Max. Photon Flux   = ',max(N_Photons),' Photons'
print,'Max. Signal/Noise  = ',sqrt(max(N_Photons)),1./sqrt(max(N_photons))
print,'Max. Energy Flux   = ',max(N_Photons)*photon_energy*1d-7,' Watts'

; !x.title='Wavelength [A]'
; !y.title='Flux [ergs/cm^2/A/sec/arcsec^2]'
; plot,x2,y2
; oplot,[lambda,lambda],[0,max(y2)*10]
; oplot,[0,50000],[y2(idx),y2(idx)]
endif

IF (NOT keyword_set(power)) THEN RETURN,N_Photons $
                            ELSE RETURN,N_Photons*photon_energy*1d-7

END
