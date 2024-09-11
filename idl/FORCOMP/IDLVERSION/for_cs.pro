function for_cs,ij,ij1,k
;
;  this function calculates the inelastic collisional rates 
;  c_s(ij,ij1,k) (for ij1 > ij)
;
; purpose: computes inelastic collision rates 
;          from the 
; inputs:
;          ij       index of lower level 
;          ij1      index of upper level
;          k        multipolar coefficient
;
; outputs:
;          cs       superelastic rate from multipolar component k
;
; CALLS FOR_FUN6J, FOR_GAMMAS
; CALLED BY FOR_C2
;
; common:
; 
@include.icle
@include.icle.cse
rj=cse.aj(ij)
rj1=cse.aj(ij1)
rk=double(k)
jp=round(rj+rj1)
jm=round(abs(rj-rj1))
cs=0.d0
for  k1=jm,jp do begin 
   rk1=double(k1)
   cs+=const.sg(k1)*for_fun6j(rj,rj,rk,rj1,rj1,rk1)*for_gammas(ij,ij1,k1)
endfor
return,const.sg(jp-k)*sqrt((2.d0*rj+1.d0)*(2.d0*rj1+1.d0))*cs
end
