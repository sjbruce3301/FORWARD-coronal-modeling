pro for_atom1
; purpose: converts "multi" radiative atomic transition probabilities
;          to those needed for formalism of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539
;          
; inputs:
;
; outputs (common):
;          aj            real quantum number j of a level
;          ecoeff(i,j)   einstein a-coefficient (i > j)
;          ecoeff(i,j)   2*pi* b coefficient (i < j)
;
; CALLED BY FOR_ICLESTART
@include.icle
@include.icle.atom
@include.icle.cse
; define the cse structure containing variables to be sent to SE
; procedures
;
cse={cse,rho:dblarr(param.mjtot+1,param.mjtot+1), scoeff:dblarr(param.mjtot,param.mjtot),$
     e:dblarr(atom.nk), aj:dblarr(atom.nk), ecoeff:dblarr(atom.nk,atom.nk), radj:dblarr(3,atom.nline),$
     weight:dblarr(param.mjtot)}
;
;     set aj, total angular momentum quantum number
;     and energy in cm-1
;
cse.aj=(lvl.g-1.d0)/2.0
cse.e=lvl.ev*const.ee/const.hh/const.cc  
;
;  set emission coefficients
;
cse.ecoeff[trn.jrad,trn.irad]=trn.a
cse.ecoeff[trn.irad,trn.jrad]=trn.bij*2.d0*const.pi
;
return
end
