PRO MYDIPOLE,r,theta,phi,ModPramsStruct,ModSolStruct

;+
; Name:
;      MYDIPOLE
;
; Purpose: Calculate a dipole field with hydrostatic density
;          differentiated by open vs. closed field
;          at a location r, theta, phi (true spherical coordinates)
;
; Calling sequence:
;      MYDIPOLE,r,theta,phi,ModPramsStruct, ModSolStruct 
;
;
; Inputs:
;          r, theta, phi -- position in 3D space where model is to be evaluated
;                               r in units of RSUN, th, ph in RADIANS
;
;          ModPramsStruct - structure associated with model, containing
;                           model name (DIPOLE), model parameters
;                           set up in dipoleprams.pro
;
; Outputs: ModSolStruct - Solution of model, containing density,temperature,
;                         pressure, magnetic field (Br, Bth, Bph) and velocity
;
;
; Called by FOR_INTENSINT and FOR_POSNOINT (via call_procedure)
; Calls FOR_HYDROCALC
;
; Author and history:
;      Written by Sarah Gibson October 2017
;	adjusted hydrostatic Sep 2018
;	added Vasquez optoin Oct 2018
;	added Cranmer option Sep 2019
;	updated to interface with for_hydrodefaults Feb 2022
;		also changed Te,densprof to cT0, cdensprof
;      April 2024 -- changed name to MYDIPOLE
;-

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

;extract parameter variables from params structure

t=tag_names(ModPramsStruct)
for i=0,n_elements(t)-1 do void=execute(t[i]+'=ModPramsStruct.(i)')

;
; get dipole field

const=Bnorm/(2*r*r*r)
Br=const*2.0*COS(theta)
Bth=const*SIN(theta)
Bph=0.d0

; 
; magnetic field line equation
; S is height
;

thetopen=ThetOpen*!dtor

openpts = where(r/sin(theta)/sin(theta) gt 1./sin(thetopen)/sin(thetopen))
open=Br*0.d0
open[openpts]=1

; now figure out plasma
;  note cdensprof, odensprof are set up as a vector in for_hydrodefaults
;   for all HYDRO>1
;  and is a tag on ModPramsStruct

hydrosave=hydro
hydrouse=hydro
if hydro eq 4 then hydrouse=3

for_hydrocalc,r,Densopen,Presopen,Tempopen,hydro=hydro,isothermal=oT0,densprof=odensprof
for_hydrocalc,r,Dens,Pres,Temp,hydro=hydrouse,isothermal=cT0,densprof=cdensprof
hydro=hydrosave

; get velocity along open field lines

Vel=Br*0.d0

; commenting this because it was probably a leftover diagnostic
;bmag=sqrt(Br*Br+Bth*Bth)

if min(openpts) ne -1 then begin
  Dens[openpts]=Densopen[openpts]
  Pres[openpts]=Presopen[openpts]
  Temp[openpts]=Tempopen[openpts]
  Vel[openpts]=velimpose
endif

ModSolStruct={Pres:Pres,Dens:Dens,Temp:Temp,Open:open,$
                      Br:Br,Bth:Bth,Bph:Bph,Vel:Vel}

END
