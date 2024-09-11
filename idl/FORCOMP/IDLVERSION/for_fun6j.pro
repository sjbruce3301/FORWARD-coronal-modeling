function for_fun6j,rj11,rj12,rj13,rj21,rj22,rj23
;
;  purpose: calculates 6j value
;
; inputs:
;       rj11,rj12,rj13  real j quantum numbers in 1st row of 6j
;       rj21,rj22,rj23  real j quantum numbers in 2nd row of 6j
;
; outputs:
;       fun6j
;
;
; CALLS FOR_FN2
; CALLED BY FOR_R2, FOR_R4, FOR_R6, FOR_FUN9J, FOR_EMISSION, FOR_CS, FOR_CI, FOR_CE
;
@include.icle

ij1=round(rj11+rj11)
ij2=round(rj12+rj12)
ij3=round(rj13+rj13)
ij4=round(rj21+rj21)
ij5=round(rj22+rj22)
ij6=round(rj23+rj23)

ijm1=(ij1+ij2+ij3)/2
ijm2=(ij1+ij5+ij6)/2
ijm3=(ij4+ij2+ij6)/2
ijm4=(ij4+ij5+ij3)/2

;ijm=ijm1
;
;if(ijm2 gt ijm) then ijm=ijm2
;if(ijm3 gt ijm) then ijm=ijm3
;if(ijm4 gt ijm) then ijm=ijm4
ijm=max([ijm1,ijm2,ijm3,ijm4])

ijx1=(ij1+ij2+ij4+ij5)/2
ijx2=(ij2+ij3+ij5+ij6)/2
ijx3=(ij3+ij1+ij6+ij4)/2

;ijx=ijx1
;if(ijx2 lt ijx) then ijx=ijx2
;if(ijx3 lt ijx) then ijx=ijx3
ijx=min([ijx1,ijx2,ijx3])

if (ijm gt ijx) then return,.0D

term1=for_fn2(ij1,ij2,ij3)+for_fn2(ij1,ij5,ij6)+for_fn2(ij4,ij2,ij6)+for_fn2(ij4,ij5,ij3)
	  
fun6j=.0D

for i=ijm,ijx do begin 
   term2=const.fl(i+1)-const.fl(i-ijm1)-const.fl(i-ijm2) $
	  -const.fl(i-ijm3)-const.fl(i-ijm4)-const.fl(ijx1-i) $
	  -const.fl(ijx2-i)-const.fl(ijx3-i)
   fun6j+=const.sg(abs(i))*exp(term1+term2)
endfor

return,fun6j
end
