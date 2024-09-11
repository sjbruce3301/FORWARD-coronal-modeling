pro for_c6,nj,ij,c6coeff
;
; purpose: compute collision rates c(6) for eqn (20) of 
;          casini, r. & judge, p. g., 1999. ap j 522, 524-539
;          a sum of inelastic collisions is made for levels greater
;          than ij, for the case k=0 only.  
;          the complete coefficient needed for eq. (20) is 
;          built by summing outside of this routine in routine
;          se0_build
; inputs:
;          nj       number of levels in atom
;          ij       index of level.  
;       
; CALLED BY FOR_SE0_BUILD
;
; following does not work because of the internal workings of 
;index=ij+1 + indgen(nj-ij-1)
;;
;c6coeff=total(for_ci[index,ij,0],1 )
c6coeff=0.d0
for ij1=ij+1,nj do begin
  c6coeff+=for_ci(ij1,ij,0)
;  print,ij1,ij,c6coeff
endfor
return
end

