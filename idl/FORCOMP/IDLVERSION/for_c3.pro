pro  for_c3,ij,ij1,k,c3coeff
;
; purpose: compute collision rates c(3) for eqn (20) of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539
;          the complete coefficient needed for eq. (20) is 
;          built by summing outside of this routine in routine
;          se0_build
; inputs:
;          ij       index of level.  
;          ij1      index of lower level
;          k        multipolar coefficient
; outputs:
;          c3coeff  
; CALLED BY FOR_SE0_BUILD

@include.icle.cse
c3coeff=sqrt((2.d0*cse.aj[ij1]+1.d0)/(2.d0*cse.aj[ij]+1.d0))*for_ci(ij,ij1,k)
return
end
