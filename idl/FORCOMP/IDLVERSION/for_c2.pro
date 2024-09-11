pro for_c2,ij,c2coeff
;
; purpose: compute collision rates c(2) eqn (20) of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539
;          a sum of superelastic collisions is made for levels lower
;          than ij, for the case k=0 only.  
; inputs:
;          ij       index of level.  
;       
; outputs:
;          c2coeff  
;
;
; following does not work because of the internal workings of 
;  cs
;  c2coeff = total(for_cs(indgen(ij),ij,0), 1 )
;
; CALL FOR_CS
; CALLED BY FOR_SE0_BUILD
;
c2coeff=0.
for ij1=0,ij-1 do c2coeff+=for_cs(ij1,ij,0)
;print,ij1,ij,c2coeff
return
end
