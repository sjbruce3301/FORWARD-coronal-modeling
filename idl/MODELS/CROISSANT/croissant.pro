PRO croissant,r,theta,phi,ModPramsStruct,ModSolStruct
  
;+
; Name:
;      CROISSANT
;
; Purpose: Calculate a density distribution consistent with a croissant-shaped CME.
; The Croissant CME model is a large tube with
; footpoints at the Sun. In this model, the density is peaked at the boundaries
; of the flux tube, decreasing with distance from the boundaries. The tube is
; narrow near the Sun and widest at the CME apex.
;
; This module allows users to adjust various parameters that control the geometry
; of the Croissant-shaped flux tube, described in the CROISSANTPRAMS procedure.
;
; This implementation of the Croissant model is a simple, general model for large CMEs, and is
; similar to the model developed by Thernisien et al, see
; https://iopscience.iop.org/article/10.1088/0067-0049/194/2/33/meta
; and references within. See also
; https://iopscience.iop.org/article/10.1088/0004-637X/813/1/35/meta
; https://www.aanda.org/articles/aa/abs/2017/03/aa29516-16/aa29516-16.html
; for brief descriptions of similar models. Please cite the above if you use this module for
; published work.
;
; Calling sequence:
;      CROISSANT,r,theta,phi,ModPramsStruct, ModSolStruct
;
;
; Inputs:
;          r, theta, phi -- position in 3D space where model is to be evaluated
;                               r in units of RSUN, th, ph in RADIANS
;
;          ModPramsStruct - structure associated with model, containing
;                           model name (CROISSANT), model parameters
;                           set up in croissantprams.pro
;
; Outputs: ModSolStruct - Solution of model, containing density,temperature,pressure
;
; Called by FOR_INTENSINT and FOR_POSNOINT (via call_procedure)
; Calls FOR_HYDROCALC
;
; Author and history:
;      Written by Huw Morgan Aug-Sept 2020
;       edited for FORWARD code consistency (SEG) Sept. 2020
;	Feb 2022 -- added hydro=5 capability for imposing single radial power law for background
;		also defined densprof vector in for_hydrodefaults so not needed below
;		and change Te, Densprof to OT0, ODensprof
;	Dec 2022 -- passed through EDGEWIDTH parameter
;		to allow thicker boundaries
;		(having trouble with LOS resolution "missing"
;		the shell
;-
COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

;extract parameter variables from params structure

t=tag_names(ModPramsStruct)
for i=0,n_elements(t)-1 do void=execute(t[i]+'=ModPramsStruct.(i)')

;spherical to cartesian
x=double(r*cos(phi)*sin(theta))
y=double(r*sin(phi)*sin(theta))
z=double(r*cos(theta))

;define axis of tube shape
t=interpol([1,-1]*!dpi,nloop)
zloop=1+cos(t/2)*(cme_distance-1)
xloop=((cme_distance-1)*sin(t)*angular_extent+interpol([1,-1],nloop))*legsqueeze
yloop=dblarr(nloop)

if twist ne 0 then begin
  twist_angle=croissant_scale_values(zloop)*twist*2*!pi
  xloop2=cos(twist_angle)*xloop-sin(twist_angle)*yloop
  yloop=sin(twist_angle)*xloop+cos(twist_angle)*yloop
  xloop=xloop2
endif

rloop=sqrt(xloop^2+yloop^2+zloop^2)
wf=rloop/max(rloop)
htwidthfact=htwidthfact gt 0?exp((wf-1)/htwidthfact):1
w=width[0]*htwidthfact*cme_distance/3.15

;rotate CME to FORWARD standard
longitude=90.
colatitude=90.
croissant_rot_3d,orientation*!dtor,0,0,xloop,yloop,zloop,xloop2,yloop2,zloop2,/vector
;rotate CME loop axis from North to required central longitude,latitude
croissant_rot_3d,0,colatitude*!dtor,0,xloop2,yloop2,zloop2,xloop3,yloop3,zloop3
croissant_rot_3d,longitude*!dtor-!pi/2,0,0,xloop3,yloop3,zloop3,xloop,yloop,zloop

;restrict our CME density calculations to a subregion, for efficiency
maxw=max(w)*4
indbox=where( x ge min(xloop)-maxw and x le max(xloop)+maxw and $
  y ge min(yloop)-maxw and y le max(yloop)+maxw and $
  z ge min(zloop)-maxw and z le max(zloop)+maxw,nbox)

;main density array
dens=x*0.d

if nbox gt 0 then begin

 s=croissant_get_path_length(xloop,yloop,zloop,dy=dyc,dx=dxc,dz=dzc,ds=ds)
 a=atan(dzc,dxc);*(abs(dzc) gt 1.d-20)
 ind=where(abs(a) gt !pi/2,cnt)
 if cnt gt 0 then a[ind]=-a[ind]
 b=atan(dyc,dxc);*(abs(dyc) gt 1.d-20)
 c=atan(dyc,dzc);*(abs(dyc) gt 1.d-20)

 ;calculate electron density as function of distance within CME
 ;total volume of CME tube
 rsunincm=6.9570D+10;solar radius in cm
 sigma=edgewidth*rloop/3.15;width of flux tube boundary
 vol=total(sqrt(!pi*sigma)*2*!pi*w*ds*(rsunincm^3));total CME volume, in cm^3
 emass =1.974d-24;mass per electron in grams assuming 10% Helium
 einvol=mass/emass;total number of electrons within CME
 meandens=einvol/vol;mean electron density within CME
 ;distance along CME central axis used to define density profile
 ;density increases as we move up the legs towards apex,
 ;else CME legs are too bright compared to main CME body
 densnorm=exp(-abs((rloop-cme_distance)/(heightprof*cme_distance/3.15))^2)
 cme_densprof=meandens*densnorm/mean(densnorm);density profile along CME

 cmebox=dblarr(nbox)
 ;loop through points along CME central axis, calculating and recording
 ;the density distribution associated with each point
 for i=0,nloop-1 do begin
  croissant_rot_3d,b[i],a[i],c[i], $
                  x[indbox]-xloop[i],y[indbox]-yloop[i],z[indbox]-zloop[i], $
                  xn,yn,zn
  d=sqrt(yn^2+zn^2)
  cmenow=cme_densprof[i]*exp(-((d-w[i])^2)/sigma[i])*exp(-((xn^2)/(0.5*ds[i])))
  ind=where(cmenow gt 0.1,n)
  if n eq 0 then continue
  cmebox[ind]=cmebox[ind]>cmenow[ind];keep highest density values
 endfor

 dens[indbox]=cmebox;store CME densities in main density array

endif;nbox gt 0

;
; now figure out background and also temperature,pressure
;  assuming hydrostatic equilibrium for non CME plasma
;  note densprof is set up as a vector in for_hydrodefaults
;   for all HYDRO>1
;  and is a tag on ModPramsStruct
;

if hydro eq 0 then begin
 k=1.38d-16; boltzmann constant cgs
 Pres=2.d0*Dens*k*OT0
 Temp=OT0+Dens*0.;isothermal
endif else begin
 for_hydrocalc,r,Densopen,Presopen,Tempopen,hydro=hydro,isothermal=OT0,densprof=odensprof
 Pres=Presopen
 Temp=Tempopen
 Dens=Dens+Densopen
endelse

ModSolStruct={Pres:Pres,Dens:Dens,Temp:Temp}

END

