pro for_profil
;
;  calculate line profiles
;
; CALLED BY M1SYNTH
;
@include.icle
@include.icle.atom
@include.icle.input
@include.icle.corona

sqrtpi=sqrt(const.pi)
;
;  calculate doppler width
;
      dnyd=sqrt(2.d0*const.bk*temp/atom.awgt)*1.d-5/input.qnorm
;
;  include microturbulence
;
dnyd=sqrt(dnyd*dnyd+vturb*vturb)

for kr=0,atom.nline-1 do begin 
   mx=trn[kr].nq-1
   v=(trn[kr].q[0:mx]-vel)/dnyd
   trn[kr].phi[0:mx]=exp(-v*v)/(sqrtpi*dnyd)
   trn[kr].phip[0:mx]=(-2.0*v/dnyd)*trn[kr].phi[0:mx]
;   print, 'profil: phi(kr,*)',trn[kr].phi[0:mx]
endfor
return
end

