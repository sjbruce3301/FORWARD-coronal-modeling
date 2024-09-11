pro  for_r6,nj,ij,k,k1,iq,r6coeff
;  this subroutine calculates the radiative rates r(6)
;
;  the radiation field is assumed to be cylindrically
;  symmetric in the reference frame in which the s.e.
;  equations are expressed.
;
; purpose: compute radiative rates r(6) for eqn (20) of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539
;          the complete coefficient needed for eq. (20) is 
;          built by summing outside of this routine in routine
;          se0_build
;
; inputs:
;       
; outputs:
;       r6coeff
;
; CALLS FOR_FUN3J, FOR_FUN6J
; CALLED BY FOR_SE0_BUILD

@include.icle
@include.icle.atom
@include.icle.cse
rj=cse.aj[ij]
rk=double(k)
rk1=double(k1)
q=double(iq)
r6tmp=0.d0
for k2=0,2 do begin 
   if(const.sg[k+k1+k2] gt 0.d0) then	begin ; \zeta+ = 1
      rk2=double(k2)
      r6tmp1=0.d0
      for ij1=ij,nj-1 do begin 
         kr=krad(ij,ij1)
         if(kr ge 0) then begin 
            rj1=cse.aj[ij1]
            r6tmp1=const.sg[round(rj-rj1)]*cse.ecoeff[ij,ij1] *for_fun6j(1.d0,1.d0,rk2,rj,rj,rj1) *cse.radj[k2,kr]+r6tmp1
         endif
      endfor
      r6tmp=sqrt(2.0d0*rk2+1.d0) *for_fun3j(rk,rk1,rk2,q,-q,0.d0) $ 
            *for_fun6j(rk,rk1,rk2,rj,rj,rj)*r6tmp1+r6tmp
   endif
endfor
r6coeff=const.sg[k+k1-iq+1] * sqrt(3.d0*(2.0d0*rk+1.d0)*(2.0d0*rk1+1.d0)) $
        * (2.0d0*rj+1.d0)*r6tmp
return
end
