function for_ci,ij,ij1,k
;  this function calculates the inelastic collisional rates 
;
; purpose: computes inelastic collision rates 
;          from the 
; inputs:
;          ij       index of upper level 
;          ij1      index of lower level
;          k        multipolar coefficient
;
; outputs:
;          ci       inelastic rate from multipolar component k
; common:
;
; CALLS FOR_FUN6J, FOR_GAMMAI
; CALLED BY FOR_C6, FOR_C3
; comments: october 6, 1999, p. judge
; 
;  c_i(ij,ij1,k) (for ij1 < ij)
;
@include.icle
@include.icle.cse
rj=cse.aj(ij)
rj1=cse.aj(ij1)
rk=double(k)
jp=round(rj+rj1)
jm=round(abs(rj-rj1))
ci=0.d0
for k1=jm,jp do begin 
   rk1=double(k1)
   ci+=const.sg(k1)*for_fun6j(rj,rj,rk,rj1,rj1,rk1)*for_gammai(ij,ij1,k1) 
endfor
return,const.sg(jp-k)*sqrt((2.d0*rj+1.d0)*(2.d0*rj1+1.d0))*ci
end
