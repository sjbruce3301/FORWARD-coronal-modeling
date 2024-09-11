pro for_t0tens,t0
;
;
; purpose: calculates the (complex) spherical components of geometric tensor 
;       t_0^k(i) in the b-frame, for k=0,1,2
;
; inputs: t0
;
; outputs:
;
; common:
;
; comments: october 6, 1999, p. judge
;           equation numbers refer to 
;           casini, r. & judge, p. g., 1999. ap j 522, 524-539
; 
; CALLED BY FOR_M1SYNTH

@include.icle
@include.icle.corona


t0=dblarr(4,3)
sqrt2=.14142135623730950488d1
sqrt3=.17320508075688772935d1
sth=sin(theta)
cth=cos(theta)
s2g=sin(2.d0*pgamma)
c2g=cos(2.d0*pgamma)
sthb=sin(thetab)
cthb=cos(thetab)
sphb=sin(phib)
cphb=cos(phib)
;
;  eq. (42)
;
cosf=cth*cthb+sth*sthb*cphb
;
;  eq. (44)
;
sincosf=sth*cthb-cth*sthb*cphb
;
;   calculate  t0(i,k)
;   these are all from table 1 of casini & judge, with 
;   appropriate substitutions 
;
;
;   k=0
;
t0(0,0)=1.d0
;
;   k=1
;
t0(3,1)=(sqrt3/sqrt2)*cosf
;
;   k=2
;
t0(0,2)=(0.5/sqrt2)*(3.d0*cosf*cosf-1.d0)
tmp1=-(3.d0/sqrt2)*(sthb*sthb*sphb*sphb -0.5*(1.d0-cosf*cosf))
tmp2=-(3.d0/sqrt2)*sthb*sphb*sincosf
t0(1,2)=tmp1*c2g+tmp2*s2g
t0(2,2)=-tmp1*s2g+tmp2*c2g
; PRINT, COSF  , ' T0 tensor cosf'
return
end
