function for_fn2,ij1,ij2,ij3
;+
;   used in 6j calculation
;   inputs are assumed to be real/double and are converted to integer.
;
; CALLED BY FOR_FUN6J
; -
@include.icle  ; get access to fl
;
l=[ij1+ij2-ij3,ij2+ij3-ij1,ij3+ij1-ij2]/2
l0=(ij1+ij2+ij3)/2+1

return,.5D*(total(const.fl[l])-const.fl[l0])
end
