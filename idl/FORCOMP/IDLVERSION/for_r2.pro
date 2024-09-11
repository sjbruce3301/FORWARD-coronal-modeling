pro for_r2,ij,k,k1,iq,r2coeff
;
; purpose: compute radiative rates r(2) for eqn (20) of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539
;          the complete coefficient needed for eq. (20) is 
;          built by summing outside of this routine in routine
;          se0_build
;
; inputs:
;       ij
; outputs:
;       r2coeff
;
;  the radiation field is assumed to be cylindrically
;  symmetric in the reference frame in which the s.e.
;  equations are expressed.
;
; CALLS FOR_FUN3J, FOR_FUN6J
; CALLED BY FOR_SE0_BUILD
;
@include.icle
@include.icle.atom
@include.icle.cse
;
rj=cse.aj[ij]
rk=double(k)
rk1=double(k1)
q=double(iq)
r2tmp=0.d0
for  k2=0,2 do begin 
   if(const.sg(k+k1+k2) gt 0.d0) then begin ; \zeta+ = 1
      rk2=double(k2)
      r2tmp1=0.d0
      for ij1=0,ij-1 do begin 
         kr=krad(ij,ij1)
         if(kr ge 0) then begin 
            rj1=cse.aj[ij1]
            r2tmp1=const.sg[round(rj-rj1)] *(2.d0*rj1+1.0)*cse.ecoeff[ij1,ij] $
                   *for_fun6j(1.0d0,1.0d0,rk2,rj,rj,rj1) *cse.radj[k2,kr]+r2tmp1
         endif
      endfor
      r2tmp=sqrt(2.d0*rk2+1.0d0) *for_fun3j(rk,rk1,rk2,q,-q,0.d0) *for_fun6j(rk,rk1,rk2,rj,rj,rj)*r2tmp1+r2tmp
   endif
endfor
;
r2coeff=const.sg[-iq+1]*sqrt(3.d0*(2.d0*rk+1.0d0)*(2.d0*rk1+1.0d0)) *r2tmp
;
return
end
