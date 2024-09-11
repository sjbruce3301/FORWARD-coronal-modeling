pro for_r1,ij,r1coeff
;
; purpose: compute radiative rates r(1) for eqn (20) of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539
;          r1 is initialized to zero and computed.
; inputs:
;       ij
; outputs:
;       r1coeff
;
; CALLED BY FOR_SE0_BUILD
;
@include.icle.cse
r1coeff=0.d0
if(ij gt 0) then begin 
   index=indgen(ij)
   ecoeff=cse.ecoeff
   r1coeff=total ( ecoeff[ij,index], 2) 
endif
return
end

