pro for_field_int,kr,rj0,rj1,rj2
;+
; purpose: calculates the s-frame irred. tensor components of avg radn
; field
;
; this subroutine calculates the irreducible tensor components
; of the average radiation field from the photosphere in the
; s frame, given the limb-darkening function, and assuming
; cylindrical symmetry of the radiation field.
;
; h is the height of the observed point over the limb, expressed
; in solar radii
;
; In this version the assumption of cylindrical symmetry is made
; so there is no explicit integration done 
;
; inputs:
; kr index of the transition
; (flimb external function that has to be defined)
;
; outputs:
; rj0 k=0 tensor component of the radiation field
; rj1 k=1 tensor component of the radiation field
; (this is zero in cylindrical symmetry)
; rj2 k=2 tensor component of the radiation field
; see equations 22 and 23 of
; casini, r. & judge, p. g., 1999. ap j 522, 524-539
;
; CALLS FOR_UV
; CALLED BY FOR_FIELDB
;-
; 
@include.icle
@include.icle.atom
@include.icle.corona

sm=1.d0/(h+1.d0)
sm2=sm*sm
cm2=1.d0-sm2
cm=sqrt(cm2)
;
rln=0.d0
if (cm2 ne 0.d0) then rln=alog((1.d0+sm)/cm)
;
a1=1.d0-cm
a2=.5d0*(1.d0-rln*cm2/sm)-a1
a3=(cm*(3.d0-cm2)-2.d0)/(3.d0*sm2)
;
b1=cm*sm2
b2=.125d0*(3.d0*sm2+rln*cm2*(3.d0*sm2+1.d0)/sm-1.d0)-b1
b3=((cm2*(20.d0-9.d0*cm2)-15.d0)*cm+4.d0)/(15.d0*sm2)
;
wl=trn[kr].alamb
wm=wl/1.d4
for_uv,wm,u,v
xnu=1.d8*(2.d0*const.pi)*(const.cc/wl)
;
; now add the absolute intensity factor pint
; xt is disk center radiation temperature for i (allen 1973, sec 80)
;
xt=6050.d0
pint=for_planck(xnu,xt) 
; 
; print *,'no limb darkening' 
; u=0.d0 
; v=0.d0 
;
rj0=(a1+a2*u+a3*v)*pint/2.d0
rj1=0.d0
rj2=(b1+b2*u+b3*v)/sqrt(2.d0)*pint/4.d0
return
end
