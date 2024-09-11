;**************************************************************************
      function for_bfun,r,ulimb=ulimb

;
;  Name:  FOR_BFUN
;  
;  Calculates scattering function needed for Brightness calculation
;
; See Billings 1966
; Also Guhathakurta thesis, equations 2.13, 2.14, 2.20, 2.21
; NOTE change to equation 2.14 as described below
;   See Billings Chapter 6 equations 20-23
;
; Called by FOR_INTENSINT
;
;; Called by FOR_PBBCALC, FOR_PINPOINT
;
; Code original written by Fran Bagenal (possibly Lika Guhathakurta),
; adopted by Sarah Gibson
; Back when dinosaurs roamed the Earth.
;
; see comments FOR_CFUN.PRO
;
; Version 2.0 July 2014
; July 2024 -- added keyword for limb darkening

       a1 = 8.69d-7
       a2 = 1.d0-ulimb; 0.37 for default
       a3 = ulimb

       cfunc = 4./3. -sqrt(1. - 1./r/r) - ((1. - 1./r/r)^(1.5))/3.
       df = (r - 1./r)*(5 - 1./r/r)*alog((1.+1./r)/sqrt(1.-1./r/r))
       dfunc = 1./8 * (5.+1./r/r - df)
       bf = a1 * (a2*cfunc + a3*dfunc)
;
; note -- this corrects a typo in Guhathakurta thesis which had negative sign in front of a3
;

       return,bf
       end
;************************************************************************
