function for_ce,ij,k
;+
; purpose: compute elastic collision rates ce for eqn (20) of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539
;          the complete coefficient needed for eq. (20) is 
;          built by summing outside of this routine in routine
;          se0_build
; inputs:
;          ij       index of level.  
;          k        multipolar coefficient
; outputs:
;          ce 
; common:
;          cse, sgnm
; comments: october 6, 1999, p. judge
; 
;  this function calculates the elastic collisional rates c_e(ij,k)
; 
; CALLS FOR_GAMMAE, FOR_FUN6J
; CALLED BY FOR_SE0_BUILD
;-
;
@include.icle
@include.icle.const
@include.icle.cse
rj=cse.aj[ij]
rk=double(k)
jj=2*round(rj)
ce=0.d0
;
; sum over the very last index to fun6j
;
;  if the fun6j and gammae can handle array inputs at all then
; this sum can be vectorized 
;
;
for k1=0,jj do begin 
   rk1=double(k1)
   ce+=const.sg[k1]*for_fun6j(rj,rj,rk,rj,rj,rk1)*for_gammae(ij,k1)
endfor
;
ce*=const.sg[jj-k]*(double(jj)+1.d0)
;
return,ce
end
