PRO for_ltepop
;+
; CALLED BY FOR_M1SYNTH
;-
;
@include.icle
@include.icle.atom
@include.icle.thermal
@include.icle.corona
;message,'must pass totn nne and temp to ltepop, and nstar from ltepop ',/inf
;   
ek=const.ee/const.bk
;
; normalization
;
pstar=dblarr(atom.nk)

;st=size(totn)
;if(st(0) eq 0 or st(1) gt 1) then begin
;   toth = 0.8*ed
;
; this is defined differently than in for_corona - commenting
; to avoid problems
;
;   totn=10.^(atom.abnd-12.0)*toth 
;
;endif
;

xxx = (const.hh/sqrt(2.d0*const.pi*const.em)/sqrt(const.bk))
ccon=0.5d0*xxx*xxx*xxx
conl=alog(ccon*ed)-1.5*alog(temp)
sumn=1.d0
tns=lvl.ev*0.d0 & pstar=tns
glog = DOUBLE(alog(lvl.g))
;
tnsl = glog-glog(0)-ek/temp*lvl.ev
nk = atom.nk
FOR i=1,nk-1 DO BEGIN
  if(lvl(i).ion le lvl(0).ion) then goto, hundred
  l=lvl(i).ion-lvl(0).ion
  tnsl(i)=tnsl(i)-float(l)*conl
  hundred:
  tns(i)=exp(tnsl(i))
  sumn=sumn+tns(i)
endfor
;
;
pstar(0)=totn/sumn
pstar(1:nk-1)=tns(1:nk-1)*pstar(0)
;print,pstar/lvl.g
return
end
