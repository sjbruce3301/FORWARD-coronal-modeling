pro for_trap,wt
;
; CALLED BY M1SYNTH

@include.icle.input
@include.icle.atom
@include.icle.emerge
;
ok=where(trn.alamb ge input.wlmin and trn.alamb le input.wlmax,n)
if(n lt 1) then message,'no wavelengths outputted'
emerge.emerge(ok,*,*)+=   emerge.emiss(ok,*,*)*wt
return
end



