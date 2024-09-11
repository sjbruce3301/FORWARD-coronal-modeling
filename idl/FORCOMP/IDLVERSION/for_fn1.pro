function for_fn1,rj1,rj2,rj3,rm1,rm2,rm3
;+
;   used in 3j calculation
;   inputs are assumed to be real/double and are converted to integer.
;   sum done avoiding loops using weights [1,1,.... -1]
;
; CALLED BY FOR_FUN3J
; -
@include.icle  ; get access to fl
;
l=round([rj1+rj2-rj3,rj2+rj3-rj1,rj3+rj1-rj2,rj1+rm1,$
         rj1-rm1,rj2+rm2,rj2-rm2,rj3+rm3,rj3-rm3])
l0=round(rj1+rj2+rj3+1.D)

return,.5D*(total(const.fl[l])-const.fl[l0])

end
