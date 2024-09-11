pro for_r3,kr,ij,ij1,k,k1,iq,r3coeff
;  this subroutine calculates the radiative rates r(3)  (ij1<ij)
;
;  the radiation field is assumed to be cylindrically
;  symmetric in the reference frame in which the s.e.
;  equations are expressed.
;
; purpose: compute radiative rates r(3) for eqn (20) of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539
;          the complete coefficient needed for eq. (20) is 
;          built by summing outside of this routine in routine
;          se0_build
;
; inputs:
;       
; outputs:
;       r3coeff
; common:
;
; CALLS FOR_FUN3J, FOR_FUN9J
; CALLED BY FOR_SE0_BUILD
;
@include.icle
@include.icle.cse
rj=cse.aj[ij]
rj1=cse.aj[ij1]
rk=double(k)
rk1=double(k1)
q=double(iq)
r3tmp=0.d0
for  k2=0,2 do begin 
   rk2=double(k2)
   r3tmp+=sqrt(2.d0*rk2+1.d0) *for_fun3j(rk,rk1,rk2,q,-q,0.d0)  *for_fun9j(1.d0,rj,rj1,1.d0,rj,rj1,rk2,rk,rk1) $
         *cse.radj[k2,kr]
endfor
r3coeff=const.sg[k1-iq]*sqrt(3.d0*(2.d0*rk+1.d0)*(2.d0*rk1+1.d0)) $
        *(2.d0*rj1+1.d0)*cse.ecoeff[ij1,ij]*r3tmp 

return
end
