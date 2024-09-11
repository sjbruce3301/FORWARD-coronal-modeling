pro gibbaglow,rpB,thetapB,phipB,ModPramsStruct,ModSolStruct
 
; Name: GIBBAGLOW
;
; Purpose: Calculate the density, temperature, magnetic field of the
;          Gibson-Bagenal-Low model at a location rpB, thetapB, phipB 
;	(true spherical coordinates)
;
; Inputs:
;          rpB, thetapB, phipB -- position in 3D space where model is to be evaluated
;                               r in units of RSUN, th, ph in RADIANS
;
;          ModPramsStruct - structure associated with model, containing
;                           model name (GIBBAGLOW), model parameters
;                           set up in gibbaglowprams.pro
;
; Outputs: ModSolStruct - Solution of model, containing density,temperature,
;                         pressure, magnetic field (Br, Bth, Bph)
;
; Calls GBL_STREAMFUNC
; Calls GBL_DIFF_STREAMFUNC
; Calls GBL_DIFFDIFF_STREAMFUNC
; Calls GBL_VELRADPROF
; Calls FOR_HYDROCALC
;
;  Written: Jie Zhao and S. Gibson
;	June 2018; updated for new hydro scaling - Sept 2018
;		added hydro=3 - Oct 2018
;                  updated for new hydro scaling - Aug-Sept 2019
;               added hydro=4
;	Feb 2022 updted to interface with for_hydrodefaults
;		and changed Te, Densprof to CT0, CDensprof
;----------------------------------------

;print,'hello model'

;
; set physical parameters
;
g=6.67d-8 ;gravitational constant cgs
ms=1.99d33 ;grams in sun
mp=1.67d-24 ; grams in proton
k=1.38d-16; boltzmann constant cgs
rs=6.9570d10 ;cm solar rad

numpts=n_elements(rpB)

;extract parameter variables from params structure

t=tag_names(ModPramsStruct)
for i=0,n_elements(t)-1 do void=execute(t[i]+'=ModPramsStruct.(i)')

; now figure out plasma
;  note cdensprof, odensprof are set up as a vector in for_hydrodefaults
;   for all HYDRO>1
;  and is a tag on ModPramsStruct

hydrosave=hydro
hydrouse=hydro
if hydro eq 4 then hydrouse=3

for_hydrocalc,rpB,Denso_P0,Po0,To0,hydro=hydro,isothermal=oT0,densprof=odensprof
for_hydrocalc,rpB,Densc_P0,Pc0,Tc0,hydro=hydrouse,isothermal=cT0,densprof=cdensprof
hydro=hydrosave

; 
; for testing only -- comparing normalizations from original GBL paper
;   and original Low paper.  
;   should also change to commented values of gamma1,gamma3,bpar 
;		in gibbaglowprams.pro
;
;  norm1=1.
;  norm3=1.
;

norm1=2.d0/!pi/bpar/bpar
norm3=-1.d0/45.d0/!pi/bpar/bpar/bpar/bpar

if norm1*gamma1*norm3*gamma3 le 0 then balance=1 else balance = 0

;
; set up shifted (bulk-current) coordinate system
;

rb=(rpB+apar)/(1.+apar)

;
; Calculate stream function 
;

Streamfunc=gbl_streamfunc(rb,thetapB,bpar,norm1=norm1,norm3=norm3)

strfunc1=Streamfunc.strfunc1
strfunc3=Streamfunc.strfunc3

strfunc=gamma1*strfunc1+gamma3*strfunc3

;======================calculate physical parameters=============
;============== Magnetic field Density Pressure Temperature =====
;

; for this we need derivatives in both the physical and stretched (rb)
;  coordinate systems

Dstrfunc_rb=gbl_diff_streamfunc(rb,thetapB,bpar,norm1=norm1,norm3=norm3)     

Dsf1_rb=Dstrfunc_rb.Dsf1_r
Dsf1_thrb=Dstrfunc_rb.Dsf1_th
Dsf3_rb=Dstrfunc_rb.Dsf3_r
Dsf3_thrb=Dstrfunc_rb.Dsf3_th
Dsf_thrb = gamma1*Dsf1_thrb+gamma3*Dsf3_thrb
Dsf_rb = (gamma1*Dsf1_rb+gamma3*Dsf3_rb)

Dstrfunc_r=gbl_diff_streamfunc(rpB,thetapB,bpar,norm1=norm1,norm3=norm3)
Dsf1_r=Dstrfunc_r.Dsf1_r
Dsf1_thr=Dstrfunc_r.Dsf1_th
Dsf3_r=Dstrfunc_r.Dsf3_r
Dsf3_thr=Dstrfunc_r.Dsf3_th
Dsf_thr = gamma1*Dsf1_thr+gamma3*Dsf3_thr
Dsf_r = (gamma1*Dsf1_r+gamma3*Dsf3_r)

;
; Second derivatives
;

; first stretched coordinate rb

DDstrfunc_rb = gbl_diffdiff_streamfunc(rb,thetapB,bpar,norm1=norm1,norm3=norm3)       
DDsf1_rrb = DDstrfunc_rb.DDsf1_rr
DDsf3_rrb = DDstrfunc_rb.DDsf3_rr
DDsf1_thrb = DDstrfunc_rb.DDsf1_thr
DDsf3_thrb = DDstrfunc_rb.DDsf3_thr

DDsf_thrb = (gamma1*DDsf1_thrb+gamma3*DDsf3_thrb)
DDsf_rrb = (gamma1*DDsf1_rrb+gamma3*DDsf3_rrb)

; now physical coordinate rpB

DDstrfunc_r = gbl_diffdiff_streamfunc(rpB,thetapB,bpar,norm1=norm1,norm3=norm3) 
DDsf1_rr = DDstrfunc_r.DDsf1_rr
DDsf3_rr = DDstrfunc_r.DDsf3_rr
DDsf1_thr = DDstrfunc_r.DDsf1_thr
DDsf3_thr = DDstrfunc_r.DDsf3_thr

DDsf_thr = (gamma1*DDsf1_thr+gamma3*DDsf3_thr)
DDsf_rr = (gamma1*DDsf1_rr+gamma3*DDsf3_rr)


; --------------------identify open vs closed field --------------------

if numpts gt 1 then begin
 type1pts= where(((strfunc lt 0) and (thetapB lt !dpi/2.d0) and (Dsf_thr gt 0)) or ((strfunc lt 0) and (thetapB ge !dpi/2.d0) and (Dsf_thr le 0)),complement=type2pts)
 type3pts= where(((strfunc ge 0) and (thetapB lt !dpi/2.d0) and (Dsf_thr le 0)) or ((strfunc ge 0) and (thetapB ge !dpi/2.d0) and (Dsf_thr gt 0)),complement=type4pts)
 if gamma1 le 0 then begin
  openpts=type1pts
  closedpts=type2pts
 endif else begin
  openpts=type3pts
  closedpts=type4pts
 endelse
endif else begin
 if(((strfunc lt 0) and (thetapB lt !dpi/2.d0) and (Dsf_thr gt 0)) or ((strfunc lt 0) and (thetapB ge !dpi/2.d0) and (Dsf_thr le 0))) then begin
   if gamma1 lt 0 then begin
     openpts=1
     closedpts=0
   endif else begin
     openpts=0
     closedpts=1
   endelse
 endif
 if(((strfunc ge 0) and (thetapB lt !dpi/2.d0) and (Dsf_thr le 0)) or ((strfunc ge 0) and (thetapB ge !dpi/2.d0) and (Dsf_thr gt 0))) then begin
   if gamma1 lt 0 then begin
     openpts=0
     closedpts=1
   endif else begin
     openpts=1
     closedpts=0
   endelse
 endif
endelse

if balance eq 0 then begin
  openpts=-1
  closedpts=-1
endif

Open=rpb*0.+1.
if min(closedpts) ne -1 then Open[closedpts]=0.


;===================================Pressure===================================
eta_r=(1.+apar/rpB)^2

;bulk current
Pb = 1./(8.*!pi)*(1./eta_r-1.)*(1./rpB^4/sin(thetapB)^2)*(Dsf_thrb)^2  

P_closed= Pc0 + Pb
P_open = Po0 + Pb

Pres=P_closed

if numpts gt 1 then begin
   if min(openpts) ne -1 then Pres[openpts]=P_open[openpts] 
endif else begin
  if openpts eq 1 then Pres=P_open
endelse

;Calculate the thermal pressure jump across the open and closed boundary
dP = Pc0-Po0

;===================================Magnetic field=============================

Br = (1./rpB^2/sin(thetapB))*Dsf_thrb
Bth = (-1./rpB/sin(thetapB)/(1.+apar))*Dsf_rb
Bph = 0.*Br

B_closed = sqrt(Br^2+Bth^2)

;This is to make sure that the total pressure is consistent between the open and closed region 
;The jump should be depend on the r only with the same dimensions as pressure and B ...

;
; watch out for zero B
;

nonzerob=where(B_closed ne 0.)
C_jump=B_closed*0. + 1.

if min(nonzerob) ne -1 then C_jump[nonzerob]=sqrt(1.+ dP[nonzerob]/((B_closed[nonzerob])^2/(8.*!pi)))

Bmag=B_closed
B_open=C_jump*B_closed

if numpts gt 1 then begin
  if min(openpts) ne -1 then Bmag[openpts]=B_open[openpts] 
  if min(openpts) ne -1 then Br[openpts]=C_jump[openpts]*Br[openpts] 
  if min(openpts) ne -1 then Bth[openpts]=C_jump[openpts]*Bth[openpts] 
  if min(openpts) ne -1 then Bph[openpts]=C_jump[openpts]*Bph[openpts] 
endif else begin
  if openpts eq 1 then begin
    Bmag=B_open
    Br=C_jump*Br
    Bth=C_jump*Bth
    Bph=C_jump*Bph
  endif
endelse

;====================================Density===================================

Dens_first= (rs/g/ms/mp)*((eta_r-1)/(8.*!pi)*((2./sin(thetapB)^2/(1.+apar)^3)*Dsf_rb*DDsf_rrb))

Dens_Pb=(rs/g/ms/mp)*(-rpB^2/(8.*!pi)*(2.*((1+apar/rpB)^(-3))*apar*rpB^(-6)*sin(thetapB)^(-2)*(Dsf_thrb)^2 $
                   - 4.*(1./eta_r-1.)*sin(thetapB)^(-2)*rpB^(-5)*(Dsf_thrb)^2 $
                   + 2.*(1./eta_r-1.)*rpB^(-4)*sin(thetapB)^(-2)*Dsf_thrb*DDsf_thrb/(1.+apar)))

Dens_closed =  Dens_first + Densc_P0 + Dens_Pb
Dens_open = Dens_first*C_jump^2 + Denso_P0 + Dens_Pb
Dens=Dens_closed

if numpts gt 1 then begin
   if min(openpts) ne -1 then Dens[openpts]=Dens_open[openpts] 
endif else begin
  if openpts eq 1 then Dens=Dens_open
endelse

;=============================Temperature===============================
Temp=Pres/Dens/k/2.

test=where(Temp*0. ne 0.)
if min(Test) ne -1 then Temp(test) = 0.d0

;
; used for plasma beta
;

Ptot = Pres + Bmag*Bmag/(8.d0*!dPi)

;=======================Velocity===================================

Vel=gbl_velradprof(rpB,vscale)
;
; only have velocity in open region
;
Vel=Open*Vel

;=========================Diagnostics==============================
;
; uncomment if want
; 
; can calculate critical points here -- but mostly for diagnostic purposes
;
; 
gbl_calc_crit,apar,bpar,norm1,norm3,gamma1,gamma3

;
; in case we need the second derivative of the stream function with respect to theta
; for diagnostic purposes (can replace Stream as below)
;

;DDsf1_thth = DDstrfunc_r.DDsf1_thth
;DDsf3_thth = DDstrfunc_r.DDsf3_thth
;DDsf_thth = (gamma1*DDsf1_thth+gamma3*DDsf3_thth)

;
;  this is a diagnostic of magnetic field vs height
;  (could also replace Stream below)
;

;DBsq_r = -4.*(1./rpB^5/sin(thetapB)^2)*(Dsf_thr)^2+(2./rpB^4/sin(thetapB)^2)*(Dsf_thr)*(DDsf_thr)$
;         -2.*(1./(1+apar)^2/rpB^3/sin(thetapB)^2)*(Dsf_r)^2 + (2./(1.+apar)^2/rpB^2/sin(thetapB)^2)*(Dsf_r)*(DDsf_rr)
;
;this is for diagnostic purposes
;Brdip = gamma1*(1./rpB^3)*2.*cos(thetapB)
;Bthdip = gamma1*(1./rpB^3)*sin(thetapB)
;Bmagdip=sqrt(brdip*brdip+bthdip*bthdip)

;=======================Save data===================================

Stream=strfunc
;Stream=DDsf_thth
;Stream=Dsf_thr

ModSolStruct={Pres:Pres,Dens:Dens,Temp:Temp,Br:Br,Bth:Bth,Bph:Bph,Stream:Stream,Open:Open,Vel:Vel}

;print,'goodbye model'
end
