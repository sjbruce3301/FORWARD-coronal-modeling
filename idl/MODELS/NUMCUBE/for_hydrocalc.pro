pro for_hydrocalc,r,dens_he,pres_he,temp_he,hydro=hydro,isothermal=isothermal,densprof=densprof

; calculate hydrostatic balanced density/pressure distribution
; assume r is in units of Rsun
;
; Inputs:
;   R - radial height (Rsun)
;
; Keyword inputs
;
;   HYDRO -- choice of hydrostatic model
;   ISOTHERMAL - choice of isothermal temperature
;   DENSPROF -- choice of power law density profile
;
; Outputs:
;  DENS_HE, PRES_HE, TEMP_HE
;

; Called by PFSSMOD, FOR_MAKEPLASMA, FOR_INTERP_CUBE
;	GIBBAGLOW, MYDIPOLE, CROISSANT
;
; Written by Laurel Rachmeler, Sarah Gibson 2011-2012
; Version 2.0 July 2014
;
;  added HYDRO = 3, Vasquez et al 2003 option
;  updated May 2019
;
;  added HYDRO = 4, Cranmer et al 1999 option for solar wind
;  updated August 2019, Jie Zhao
;  Feb 2022 -- added HYDRO=5, simple far field falloff
;  Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;  Nov 2022 -- added negative hydro option - only for in/out
;         of bounds in numericl cubes. Take absolute value here-
;	   may be needed e.g. for topology stuff



g=6.67d-8 ;gravitational constant cgs
ms=1.99d33 ;grams in sun
mp=1.67d-24 ; grams in proton
k=1.38d-16; boltzmann constant cgs
rs=6.9570d10 ;cm solar rad

; HYDRO = 1: exponential isothermal case

if abs(hydro) eq 1 then begin
 scaleheight=rs*2.*k*double(isothermal)/(g*ms*mp)
 dens_he = double(densprof)*exp(1./scaleheight/r - 1./scaleheight)
 pres_he = 2.d0*dens_he*k*double(isothermal)
 temp_he = dens_he*0.d0 + double(isothermal)
endif

; HYDRO = 2: power law density

if abs(hydro) eq 2 then begin
 A=double(densprof[0])
 B=double(densprof[1])
 C=double(densprof[2])
 D=double(densprof[3])
 E=double(densprof[4])
 F=double(densprof[5])
 dens_he= A*r^(-B) + C*r^(-D) + E*r^(-F)
 pres_he= (A/(B+1))*r^(-B-1.) + (C/(D+1))*r^(-D-1.) + (E/(F+1))*r^(-F-1)
 pres_he=pres_he*(g*ms*mp)/rs
 temp_he = pres_he/2.d0/dens_he/k
;
; temperature is constrained by ideal gas law and hydrostatic equilirium
; so isothermal variable is not used
;
endif

; HYDRO = 3: Vasquez et al 2003 density/temperature profiles

if abs(hydro) eq 3 then begin
 A1=double(densprof[0])
 A2=double(densprof[1])
 A3=double(densprof[2])
 A4=double(densprof[3])
 A5=double(densprof[4])
 aa=double(densprof[5])
 bb=double(densprof[6])
 alpha=double(densprof[7])
 beta=double(densprof[8])
 dens_he= A1*1d8*exp(A2/r)*r^(-2)*(1.d0 + A3/r + A4/r/r + A5/r/r/r)
;
 temp_he= double(isothermal)*(8e5/1.5e6)*(aa+1.d0)/(aa+bb*r^(alpha)+(1.d0-bb)*r^(-beta))
;
; renormalized so that input isothermal can have same default (1.5e6) as for exponential hydrothermal
; and still end up with default Vasquez values
;
 pres_he = temp_he*2.d0*dens_he*k
endif

; HYDRO = 4: Cranmer et al 1999 density/temperature profiles in the solar wind (coronal hole)

if abs(hydro) eq 4 then begin
 
 da=double(densprof[0])
 db=double(densprof[1])
 dc=double(densprof[2])
 dd=double(densprof[3])
 de=double(densprof[4])    
 ta=double(densprof[5])
 tb=double(densprof[6])
 tc=double(densprof[7])
 td=double(densprof[8])

 dens_he= da*1e5*(db*(1./r)^dc + dd*(1./r)^de)

 temp_he= double(isothermal)*(1e6/1.5e6)*(ta*r^tb + tc*r^td)^(-1)

;
; renormalized so that input isothermal can have same default (1.5e6) as for exponential hydrothermal
; and still end up with default Cranmer's values
;
 pres_he = temp_he*2.d0*dens_he*k
endif

;  HYDRO = 5: simple far-field radial density falloff

if abs(hydro) eq 5 then begin

  A=double(densprof)
  B=-1.*double(isothermal)
  dens_he= A*r^(-B)
  pres_he= (A/(B+1))*r^(-B-1.)
  pres_he=pres_he*(g*ms*mp)/rs
  temp_he = pres_he/2.d0/dens_he/k
;
; temperature is constrained by ideal gas law and hydrostatic equilirium
;

endif
end
