pro for_fieldb,kr
;
;  this subroutine calculates irreducible tensor components
;  of the average radiation field from the photosphere in the b
;  frame
;
; purpose: calculates the b-frame irred. tensor components of avg radn field
;          from the photosphere in the "b" frame (see fig 5 of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539)
; inputs:
;
; outputs:
;
; CALLS FOR_FIELD_INT
; CALLED BY FOR_SE0_BUILD
;
@include.icle
@include.icle.cse
@include.icle.corona
amu_b=cos(thetab)
for_field_int,kr,radj_0,radj_1,radj_2
;
cse.radj(0,kr)=radj_0
cse.radj(1,kr)=radj_1
cse.radj(2,kr)=0.5*(3.0*amu_b*amu_b-1.0)*radj_2
;
return
end
