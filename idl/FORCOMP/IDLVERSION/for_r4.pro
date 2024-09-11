pro for_r4,ij,ij1,k,r4coeff
;
;  this subroutine calculates the radiative rates r(4)  (ij1 > ij)
;
; purpose: compute radiative rates r(4) for eqn (20) of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539
;          the complete coefficient needed for eq. (20) is 
;          built by summing outside of this routine in routine
;          se0_build
;
; inputs:
;       
; outputs:
;       r4coeff
;
; CALLED BY FOR_SE0_BUILD
; CALLS FOR_FUN6J
;

@include.icle
@include.icle.cse
r4coeff=0.d0
if(ij ge ij1) then return
rj=cse.aj[ij]
rj1=cse.aj[ij1]
rk=double(k)
r4coeff=const.sg(round(rj+rj1)+k+1.d0)*(2.0d0*rj1+1.d0)*cse.ecoeff[ij1,ij] *for_fun6j(rj,rj,rk,rj1,rj1,1.d0)
return
end
