PRO cirtoy,r,theta,phi,ModPramsStruct,ModSolStruct

;+
; Name:
;      CIRTOY
;
; Purpose: Create a simple density model of an archimedian spiral
;
; Calling sequence:
;      CIRTOY,r,theta,phi,ModPramsStruct,ModSolStruct 
;
;
; Inputs:
;          r, theta, phi -- position in 3D space where model is to be evaluated
;                               r in units of RSUN, th, ph in RADIANS
;
;          ModPramsStruct - structure associated with model, containing
;                           model name (CIRTOY), model parameters
;                           set up in CIRTOYPRAMS.pro
;
; Outputs: ModSolStruct - Solution of model, containing density,temperature,
;                         pressure
;
;
; Called by FOR_INTENSINT and FOR_POSNOINT (via call_procedure)
; Calls FOR_HYDROCALC
;
; Author and history:
;      Written by Sarah Gibson and Craig DeForest July 2024
;-

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

;extract parameter variables from params structure

t=tag_names(ModPramsStruct)
for i=0,n_elements(t)-1 do void=execute(t[i]+'=ModPramsStruct.(i)')

;
; get CIR density
; parameters: alfa,bta,n_o,wwidth,time,densprof,T0,hydro,vsw,phimin,phimax

; Omega is solar rotation rate radians per hour
;  fast wind stream for exactly one solar rotation, then at time 0,
;  turns off the stream but lets the pattern expand out radially
; a Vsolar wind speed
;  If we want to have a shorter period of the stream, then
;   we would need to restrict the phi allowed to within +/- from some phio
;   2 or 3 more parameters
;   and the spirl will form through the radial outward motion 
;   of the piece of pattern

Rsun=6.96d5 ;km
Vsw_rsunperhour=Vsw*3600./Rsun
Omega=2.*!dpi/28./24.; units radian/hour)
alfap=alfa*Vsw_rsunperhour/Omega ; units Rsun/radian
;
; Archimedian spiral equation
;
r_o_p=((sin(theta))^bta)*alfap*(2.*!dpi-phi) ; units Rsun
r_o=r_o_p+time*Vsw_rsunperhour ; units Rsun

; scale width with time/distance
;wwp=(Vsw_rsunperhour*time + r_o_p)*wwidth/r_o_p
; NO - this is not right- going back to fixed width
wwp=wwidth

; remove the inner shell (otherwise expands as a sphere outward from the origin)
test=where(r_o_p le 1.,nt)
if nt ne 0 then begin
  r_o[test]=0.d0
  wwp[test]=wwidth
endif

;  less extended
test=where(phi lt phimin or phi gt phimax,nt)
if nt ne 0 then begin
 r_o[test]=0.d0
 wwp[test]=wwidth
endif

;ncir=n_o*(r_o)*exp((-(r-r_o)^2)/(wwidth*(time+1))^2)
ncir=n_o*exp((-(r-r_o)^2)/(wwp^2))


;
; now figure out background and also temperature,pressure
;  assuming hydrostatic equilibrium for non CME plasma
;  note densprof is set up as a vector in for_hydrodefaults
;   for all HYDRO>1
;  and is a tag on ModPramsStruct
;

; for now, temperature of CIR is same as background wind, set by pressure balance and r^-OTO falloff

for_hydrocalc,r,Densopen,Presopen,Tempopen,hydro=5,isothermal=OT0,densprof=odensprof
Pres=Presopen
Temp=Tempopen
Dens=Densopen+ncir*Densopen


ModSolStruct={Pres:Pres,Dens:Dens,Temp:Temp}

END
