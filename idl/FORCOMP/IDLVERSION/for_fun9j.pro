function for_fun9j,rj11,rj12,rj13,rj21,rj22,rj23,rj31,rj32,rj33
;
; purpose: calculates 9j value
;
; inputs:
;       rj11,rj12,rj13  real j quantum numbers in 1st row of 9j
;       rj21,rj22,rj23  real j quantum numbers in 2nd row of 9j
;       rj31,rj32,rj33  real j quantum numbers in 3rd row of 9j
;
; outputs:
;       fun9j 9j symbol
;
;
; CALLED BY FOR_EMISSION, FOR_R3, FOR_R5
; CALLS FOR_FUN6J
;
@include.icle

ij11=round(rj11+rj11)
ij12=round(rj12+rj12)
ij21=round(rj21+rj21)
ij23=round(rj23+rj23)
ij32=round(rj32+rj32)
ij33=round(rj33+rj33)

kmin1=abs(ij11-ij33)
kmin2=abs(ij32-ij21)
kmin3=abs(ij23-ij12)

;kmin1=kmin2 > kmin1
;kmin1=kmin3 > kmin1
kmin=max([kmin1,kmin2,kmin3])

kmax1=ij11+ij33
kmax2=ij32+ij21
kmax3=ij23+ij12

;kmax1=kmax2 < kmax1
;kmax1=kmax3 < kmax1
kmax=min([kmax1,kmax2,kmax3])

if(kmin gt kmax) then return,.0D

fun9j=0.d0

for k=kmin,kmax,2 do begin 
   fk5=double(k)/2.D
   fun9j+=const.sg(abs(k))*double(k+1) $
   	    *for_fun6j(rj11,rj21,rj31,rj32,rj33,fk5) $
   	    *for_fun6j(rj12,rj22,rj32,rj21,fk5,rj23) $
   	    *for_fun6j(rj13,rj23,rj33,fk5,rj11,rj12)
endfor

return,fun9j
end
