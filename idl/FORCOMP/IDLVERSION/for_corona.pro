pro for_corona,r3D,theta3D,phi3D,Br3d,Bth3d,Bph3d,Dens,Temperature,Velocity,vturbul
;
; purpose: get coronal thermal and magnetic data for each grid point
;
; inputs: none
;      r3D,theta3D,phi3D,Br3d,Bth3d,Bph3d,Dens,Temperature,Velocity,vturbul
;
; outputs: 
;     temp,ed,vel,vturb,h,alpha,bfield,thetab,phib,theta,pgamma,alarmor,dop
;
; CALLS FOR_EQION
;
; CALLED BY FOR_M1SYNTH
;

@include.icle
@include.icle.input
@include.icle.atom
@include.icle.corona
;
;if(n_elements(vturbul) eq 0) then vturbul=0.*velocity
;
; changed this to match original format
if(n_elements(vturbul) eq 0) then vturbul=0.*velocity + 10.
;
gx2=gx*gx
gy2=gy*gy
gz2=gz*gz
;     
alpha=-atan(gx,sqrt(gy2+gz2))
;     
salpha=sin(alpha)
calpha=cos(alpha)
;     
beta=atan(gy,gz)
;     
sbeta=sin(beta)
cbeta=cos(beta)
;     
;     observer's geometry in the "s-frame" (phi=0), 
;     with reference direction always parallel to z
;     
theta=0.5*const.pi+alpha
pgamma=beta
;     
;     transform to polar coordinates for b-field calculation
;     
rb=sqrt(gx2+gy2+gz2)
tb=atan(sqrt(gx2+gy2),gz)
pb=atan(gy,gx)
;     
;     compute magnetic parameters
;     inputs here are from jim dove's file.
;     convert his inputs to bs,bt,bp
bx= br3d*sin(theta3d)*cos(phi3d)+ $
    bth3d*cos(theta3d)*cos(phi3d)- bph3d*sin(phi3d)
by= br3d*sin(theta3d)*sin(phi3d)+ $
   bth3d*cos(theta3d)*sin(phi3d)+ bph3d*cos(phi3d)
;
bz=  br3d*cos(theta3d) - bth3d*sin(theta3d)
bs=sqrt(bx*bx+by*by+bz*bz)
;
;
; watch out for null points
; (sg edit nov 2012)
;
; also see below adjust to thetab, phib
; to make field radial if B is small
; (sg edit aug 2015)
;
    if bs eq 0.d0 then begin
         ps=0.d0
         ts=0.d0
    endif else begin
      ps=atan(by,bx)
      ts=acos(bz/bs)
    endelse
;
;     end of dove's input
;
bfield=bs
btheta=ts
bphi=ps
;     
xb=sin(btheta)*cos(bphi)
yb=sin(btheta)*sin(bphi)
zb=cos(btheta)
;     
yzb=sbeta*yb+cbeta*zb
;     
xxb=calpha*xb+salpha*yzb
yyb=cbeta*yb-sbeta*zb
zzb=-salpha*xb+calpha*yzb
;     
;     compute polar angles of b in the "s'-frame"
;     (van vleck angle = 54.7356103173)
;     
thetab=atan(sqrt(xxb*xxb+yyb*yyb),zzb)	
phib=atan(yyb,xxb)

;
; make field radial if B very small 
; (sg edit aug 2015)
;
    if bs lt 1d-3 then begin
      thetab=0.d0
      phib=0.d0
    endif

;     
;     height above limb
;     
h=rb-1.0
;     
;     larmor frequency 
;     
alarmor=(1.0e-8*(0.25d0/const.pi)*(const.ee/const.em)*bfield)/input.qnorm
;
; normalize velocities
;
vel=velocity/input.qnorm
vturb=vturbul/input.qnorm
;
ed=dens
temp=temperature
;
;  normalize atomic populations
;
toth=0.8*ed  ; assumed fully ionized, so N(H) = 0.8 N_e
totn=10.^(atom.abnd-12.)*toth*for_eqion(temp)
;
; hydrogen populations: 0.d0 if neutral, toth for protons
;
hd=[0.d0,toth]
;
if input.idebug ne 0 then print,'MAG: THETAB, PHIB, ALARMOR',thetab,phib,alarmor
return
end

