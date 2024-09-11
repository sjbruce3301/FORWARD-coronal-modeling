function gbl_velradprof,r,velscale

; Name: GBL_VELRADPROF
;
; Purpose: Calculate the velocity as a function of radius
;	   based on Cranmer 1999 -- different for isotropic or anisotropic
;		WAIT NOW WE ONLY USE ANISOTROPIC
;
; Inputs:
;          rpB -- radial position in 3D space 
;                               r in units of RSUN
;
;	   velscale -- scaling on velocity
;
; Outputs: velocity same dimensions as r
;		units km/sec
;
; Called by GIBBAGLOW
;
;  Written: S. Gibson S. Fineschi R. Susino
;       July 2018
;----------------------------------------


;
; anisotropic from cranmer et al - use
  newfun=1./(1.+exp(-6.5*(r-1.5)))
  vel = newfun*(110.+445.*(1. - 1./r)^3.47) 

; isotropic from cranmer et al - don't use
;  newfun=1./(1.+exp(-6.5*(r-1.5)))
;  vel = newfun*(112.+1450.*(1. - 1./r)^5.46)

vel=velscale*vel
return, vel

end
