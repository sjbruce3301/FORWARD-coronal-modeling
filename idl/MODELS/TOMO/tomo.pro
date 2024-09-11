PRO tomo,r,theta,phi,ModPramsStruct,ModSolStruct
  
;+
; Name:
;      TOMO
;
; Purpose: This main tomo module will open a set of tomography density maps gained from coronagraph observations,
; and interpolate/extrapolate these densities (based on simple physical assumptions) throughout the required region
; of the corona specified by the input spherical coordinates r, theta, phi.
;
; The tomography method is described by
;         https://iopscience.iop.org/article/10.3847/1538-4357/ab7e32/meta
;         https://iopscience.iop.org/article/10.3847/1538-4365/ab125d/meta
;         https://iopscience.iop.org/article/10.1088/0067-0049/219/2/23/meta
; Please reference these works if you use this module for public work.
;
; The mean tomographical densities within streamers and coronal holes, as functions of height, are fitted to
; to a model velocity/acceleration profile assuming mass flux constant. This allows the extrapolation of
; densities and velocities throughout any coronal region. The velocity profile is given by the simple
; equation
;               v(r) = v_user * ( 1 - exp(-h/r_h) )
; where   v_user is the user-supplied vfast or vslow,
;         h is height ( = r - 1)
;         r_h is a scale height calculated by the tomo module to best fit the tomographical densities.
; Note this velocity allows acceleration up to the maximum v_user, over scale heights of r_H
;
; Based on this velocity profile, the densities at any given height are given by
;               p(r) = (p_f/( 1 - exp(-h/r_h) ) * (r_user/r)^2
; where   r_user is a user-supplied height where the outflow velocity reaches its asymptotic value
;         p_f is the density at r_user, calculated by the tomo module to best fit the tomographical densities.
;
; These equations are fitted separately to the mean streamer densities as given by the tomography maps, and
; the mean coronal hole densities. The final extrapolation then uses the longitude-latitude distribution
; of tomographical densities, plus the streamer/coronal hole height profiles, to calculate densities throughout
; the required coronal region. So, if, in the original tomography map a point has a density which is halfway between
; the maximum streamer and minimum coronal hole densities, it will be a mean of the streamer and coronal hole height profiles.
;
; Note that the module, given a required date, searches the FORWARD database for the tomographical map structure made
; closest in time. If this time is longer than 30 days a warning is given, but the module continues.
;
; Calling sequence:
;      TOMO,r,theta,phi,ModPramsStruct, ModSolStruct
;
;
; Inputs:
;          r, theta, phi -- position in 3D space where model is to be evaluated
;                               r in units of RSUN, th, ph in RADIANS
;
;          ModPramsStruct - structure associated with model, containing
;                           model name (TOMO), model parameters
;                           set up in tomoprams.pro
;
; Outputs: ModSolStruct - Solution of model, containing density,temperature,pressure
;
; Called by FOR_INTENSINT and FOR_POSNOINT (via call_procedure)
;
; Author and history:
;      Written by Huw Morgan Aug-Sept 2020
;       edited for FORWARD code consistency (SEG) Sept. 2020
;	updated to allow simple density falloff Feb. 2022
;-

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

;extract parameter variables from params structure

t=tag_names(ModPramsStruct)
for i=0,n_elements(t)-1 do void=execute(t[i]+'=ModPramsStruct.(i)')

; one of the ModPramsStruct parameters is filename
; which is where the tomographic data live
;
restore,filename

tomo_sizearr,d.dens,nlon,nlat,nr

ilon=interpol(findgen(nlon*3),[d.lon-2*!pi,d.lon,d.lon+2*!pi],phi)
ilat=interpol(findgen(nlat),d.lat,theta)

;
; allow simple option with power law decrease with distance
;

if simple eq 1 then begin

 dens=interpolate([d.dens[*,*,0],d.dens[*,*,0],d.dens[*,*,0]],ilon,ilat)*((d.r[0]/r)^(-1*rpwr))
 vel=dens*0.d0

endif else begin

;index the streamers and coronal holes regions
 ind_str=where(d.dens[*,*,0] gt mean(d.dens[*,*,0],/nan)*1.5,complement=ind_ch)

;find mean density at each height separately for streamers and coronal holes
 mn_str=dblarr(nr)
 mn_ch=dblarr(nr)
 for ir=0,nr-1 do begin
  mn_str[ir]=mean((d.dens[*,*,ir])[ind_str],/nan)
  mn_ch[ir]=mean((d.dens[*,*,ir])[ind_ch],/nan)
 endfor

;fit these mean density height profiles to function of speed
;assuming mass flux constant and radial expansion
 functargs={ht_asympt:ht_asympt_slow,rinrs:d.r,dens:mn_str}
 start_parms=[min(mn_str)*0.5d,5.d]
 pstr=mpfit('tomo_speedfit',start_parms,functargs=functargs,/quiet)

 functargs={ht_asympt:ht_asympt_fast,rinrs:d.r,dens:mn_ch}
 start_parms=[min(mn_ch)*0.5d,2.d]
 pch=mpfit('tomo_speedfit',start_parms,functargs=functargs,/quiet)

 nrmod=101
 rmod=interpol(minmax(r,/nan),nrmod)
 densst=(pstr[0]/(1-exp(-((rmod-1)/pstr[1]))))*((ht_asympt_slow/rmod)^2)
 densch=(pch[0] /(1-exp(-((rmod-1)/pch[1])))) *((ht_asympt_fast/rmod)^2)
 vmodslow=vslow*(1-exp(-((rmod-1)/pstr[1])))
 vmodfast=vfast*(1-exp(-((rmod-1)/pch[1])))

 nrmod=5001
 rmod=10^interpol(alog10([1.01,200]),nrmod)
 densst=(pstr[0]/(1-exp(-((rmod-1)/pstr[1]))))*((ht_asympt_slow/rmod)^2)
 densch=(pch[0] /(1-exp(-((rmod-1)/pch[1])))) *((ht_asympt_fast/rmod)^2)
 vmodslow=vslow*(1-exp(-((rmod-1)/pstr[1])))
 vmodfast=vfast*(1-exp(-((rmod-1)/pch[1])))

 ir=interpol(dindgen(nrmod),rmod,r)

 mnmx=minmax(d.dens[*,*,0])
 weight=interpolate([d.dens[*,*,0],d.dens[*,*,0],d.dens[*,*,0]],ilon,ilat,missing=!values.f_nan)
 weight=(weight-mnmx[0])/(mnmx[1]-mnmx[0])

 dens=weight*interpolate(densst,ir)+(1-weight)*interpolate(densch,ir)
 vel=weight*interpolate(vmodslow,ir)+(1-weight)*interpolate(vmodfast,ir)

endelse

k=1.38d-16; boltzmann constant cgs
Pres=2.d0*Dens*k*Te
Temp=Te+Dens*0.;isothermal

ModSolStruct={Pres:Pres,Dens:Dens,Temp:Temp,Vr:vel,Vth:0.*vel,Vph:0.*vel}

END
