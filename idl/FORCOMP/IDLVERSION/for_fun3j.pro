function for_fun3j,rj1,rj2,rj3,rm1,rm2,rm3
;+
; purpose: calculates 3j value, each rj/rm is real
;
; CALLS FOR_FN1
; CALLED BY FOR_GAMMAE, FOR_GAMMAI, FOR_GAMMAS, FOR_R2, FOR_R3, FOR_R5, FOR_R6
;-
@include.icle
;
ij1=round(rj1+rj1)
ij2=round(rj2+rj2)
ij3=round(rj3+rj3)
im1=round(rm1+rm1)
im2=round(rm2+rm2)
im3=round(rm3+rm3)

;print,ij1,ij2,ij3
;print,im1,im2,im3
;
; some triangle rules etc
;
if( (ij1+ij2-ij3) lt 0 or $
    (ij2+ij3-ij1) lt 0 or $
    (ij3+ij1-ij2) lt 0 or $
    (im1+im2+im3) ne 0 or $
    abs(im1) gt ij1 or $
    abs(im2) gt ij2 or $
    abs(im3) gt ij3 ) then return,.0D

kmin=(ij3-ij1-im2)/2
kmin1=kmin
kmin2=(ij3-ij2+im1)/2

;if(kmin2 lt kmin) then kmin=kmin2
;kmin=-kmin
;if(kmin lt 0) then kmin=0
kmin=-min([kmin,kmin2]) > 0 

kmax=round(rj1+rj2-rj3)
kmax1=kmax
kmax2=round(rj1-rm1)
kmax3=round(rj2+rm2)

;if(kmax2 lt kmax) then kmax=kmax2
;if(kmax3 lt kmax) then kmax=kmax3
kmax=min([kmax,kmax2,kmax3])

if(kmin gt kmax) then return,.0D

term1=for_fn1(rj1,rj2,rj3,rm1,rm2,rm3)
sgn=const.sg(abs((ij1-ij2-im3)/2))

fun3j=.0D

for i=kmin,kmax do begin 
   term2=const.fl(i)+const.fl(kmin1+i)+const.fl(kmin2+i) $
        +const.fl(kmax1-i)+const.fl(kmax2-i)+const.fl(kmax3-i)
   fun3j+=sgn*const.sg(abs(i))*exp(term1-term2)
endfor

return,fun3j
end
