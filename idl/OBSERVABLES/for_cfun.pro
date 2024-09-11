;**********************************************************************
      function for_cfun,r,ulimb=ulimb

;  Name:  FOR_CFUN
;  
;  Calculates scattering function needed for Brightness calculation
;   and also polarized Brightness
;
; See Billings 1966
; Also Guhathakurta thesis, equations 2.10, 2.11, 2.12, 2.13, 2.14, 2.18, 2.20, 2.21
;
; NOTE ERRORS IN GUHATHAKURTA THESIS EQNS 2.10 and 2.11
; -- 2.10 should be written arcsin(Rsun/r)
; -- 2.11 should have sin^2 instead of sin
;  see Billings chapter 6 eqns 20-23
;
; Called by FOR_PBBCALC, FOR_PINPOINT
;
; Code original written by Fran Bagenal (possibly Lika Guhathakurta),
; adopted by Sarah Gibson
; Back when dinosaurs roamed the Earth.
;
; July 2024 -- added keyword for limb darkening

;
; Thomson cross section sigma=7.95d-26 cm^2sr-1
;   constant = pi*sigma*.5*Rsun(in cm)*1d8
       a1 = 8.69d-7
;        leaving pB;B in units of 1d8 Io (sun central brightness)
;      note: normalized to solar brightness at sun center 
;       Bsun_center = Io(Billings notation) 
;      renormalization to Bsun_mean can be done via program FOR_FIXUNITS
;	Bsun_mean = mean solar brightness (over the entire disk)

; ulimb
; (an empirical function of wavelength, see Billings 1996 Chapter 6, page 151) 
; 1-u limb darkening Allen

       a2 = 1.d0-ulimb; 0.37 for default
       a3 = ulimb
;
; eq 2.11 -2.12
;
; r * sin(omega) = 1
; omega is angle between radius and point grazing photosphere
;

; note discrepancy with Guhathakurta thesis in the following line, see comments above 
       afunc = sqrt(1. - 1./r/r)*(1./r/r)

       bf = (r - 1./r)*(1 + 3./r/r)*alog((1.+1./r)/sqrt(1.-1./r/r))
       bfunc = -1./8 * (1-3/r/r - bf)
;
; from e.g. eq 2.18b, 2.21
; a1*((1-u)*A + uB)
;
       cf = a1 * (a2*afunc + a3*bfunc)

       return,cf
       end
;************************************************************************
