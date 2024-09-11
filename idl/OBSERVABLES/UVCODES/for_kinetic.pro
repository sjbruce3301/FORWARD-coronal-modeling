pro for_kinetic,r3D,isotropic,wpar_open,wpar_closed,aniso_open,ModSolStruct,ObsPramsStruct,w_par,aniso

;
; Generates model of kinetic temperature
; STILL BEING TESTED
;
;  REQUIRED INPUTS
;
;  R3D - radial height of plasma
;
;  ISOTROPIC - whether or not to be isotropic
;	If set, overrules any ModSolStruct.AnIso
;  WPAR_OPEN, WPAR_CLOSED -- multiplicative factors to establish W_PAR if not set in ModSolStruct
;	multiply W_MIN which depends on temperature, ion max
;       If info on open vs closed not available, will assume all is open
;  ANISO_OPEN -- model choice for ANISO for OVI (max positive nonzero, min zero)
;	or if negative becomes value of constant anisotropy
;       not used if set in ModSolStruct (UV spectro)
;       If info on open vs closed not available, will assume all is open
;		WARNING -- this overestimates polarization in closed regions!!
;
;  OBSPRAMSSTRUCT
;	INSTRUMENT
;
;  MODSOLSTRUCT
;  	TEMP model electron temperatere
;	(Maybe) W_PAR, ANISO
;
; OUTPUTS
;
;  W_PAR -- 1/e Gaussian width for velocity distribution of the coronal ion along the magnetic field (KM/SEC)
;	Dimensionality same as R3D
;  ANISO -- parameter describing anisotropy of coronal velocity distribution:
;       1/e Gaussian width for velocity distribution of the coronal ion    
;       perpendicular to the magnetic field W_PERP=W_PAR*ANISO; [1 = isotropic distribution]
;	Dimensionality same as R3D
;
; Called by FOR_UVMODEL
;
; Written by Roberto Susino, Silvano Fineschi, Sarah Gibson 2017
; Add constant anisotropy option in Sept 2019 Jie Zhao
;
; Oct 2021 fixed ug where said that if no info on closed/open assume all closed -- wasn't doing that
;	but instead made it assume all open for anisotropic
; 
; Jul 2022 -- updating treatment of aniso 
; Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
; Nov 2022 - added NEVIII
; Feb 2024 -- fixed bug where OVI and NEVIII had ionmass in terms of atomic number, not mass (no neutrons included)
;	Added MGIX
;--

k=1.38d-16
mp=1.67d-24

if strupcase(ObsPramsStruct.instrument) eq 'LYA' then ionmass=mp

if strpos(strupcase(ObsPramsStruct.instrument),'OVI') ge 0 then ionmass=16.*mp

if strpos(strupcase(ObsPramsStruct.instrument),'NEVIII') ge 0 then ionmass=20.*mp

if strpos(strupcase(ObsPramsStruct.instrument),'MGIX') ge 0 then ionmass=24.*mp

Temp_e=ModSolStruct.Temp

w_min=sqrt(2.*k*Temp_e/ionmass)

;
; make KM/SEC
;

w_min=w_min*1d-5

;
; note - be careful if using the 
; ModSolStruct content -- that would be in model
; frame, and odds are there needs to be 
; a vector coordinate transform as was done
; for B and V in for_fieldcalls
;
; so the preliminary text below probably needs editing.
; if tag_exist(ModSolStruct,'w_par') then begin
;   w_par=ModSolStruct.w_par + r3D*0.d0
; endif 
; if tag_exist(ModSolStruct,'aniso') then begin
;   aniso=ModSolStruct.aniso + r3D*0.d0
; endif

;
; If one or both not set in ModSolStruct:
;

if exist(w_par) eq 0 or exist(aniso) eq 0 then begin

  if tag_exist(ModSolStruct,'open') then begin
    openfield=where(ModSolStruct.Open eq 1)
    closefield=where(ModSolStruct.Open ne 1)
  endif else begin 
    closefield=-1
;
; assume everything open
    openfield=where(is_number(ModSolStruct.Temp))
  endelse

  if isotropic eq 0 then begin

;=====================constant value of anisotropy======================
    if aniso_open lt 0. then begin
      sz=size(r3D)
      aniso_wind=fltarr(sz[1],sz[2])+abs(aniso_open)
; default to minimum width or w_par anisotropic
      w_par=w_min*wpar_open
      if min(closefield) ne -1 then begin
; default in closed field to same as isotropic
        w_par[closefield]=w_min[closefield]*wpar_closed
        aniso_wind[closefield]=1.
      endif
;=====================anisotropy models======================
    endif else begin
      w_min_cranmer=w_min
      if strupcase(ObsPramsStruct.instrument) eq 'LYA'  then begin
        if aniso_open gt 0 then begin
; 2022-06 RS: this is the original approach using an analytic function for aniso
;  SG: we will need to make the next two variables FORWARD keywords once we are sure of the approach
          switch_r = 2.5 ; height at which half of the maximum anisotropy (aniso_open) is reached
          slope = 5. ; the greater this parameter, the steeper the slope of the logistic
          aniso_wind=r3D*0.+aniso_open
          aniso_wind=1.+(aniso_wind-1.)/(1.+exp(-slope*(r3D-switch_r)))
          w_par=w_min
        endif else begin
;
; This next can be accessed by setting aniso_open=0
;
; 2022-06 RS: this part set wperp to Cranmer's model A1 wperp and calculates aniso_wind as wperp/wmin,
; where wmin is taken from the input model
;
; originally added by RS to have a more realistic anisotropy profile as in Cranmer et al. 1999
; the aniso profile is obtained by dividing the w_perp and w_par=w_min functions given in the paper
; as long as we are using hydro=4, w_min=w_min_cranmer, and Temperature=temperature cranmer
;
          w_perp_cranmer=174.+200./(5.7*(1./r3D)^0.499+5.86e4*(1./r3D)^14.3) ; model A1
          aniso_wind=w_perp_cranmer/w_min_cranmer
          w_par=w_min
        endelse
      endif

      if strpos(strupcase(ObsPramsStruct.instrument),'OVI') ge 0 $
        or strpos(strupcase(ObsPramsStruct.instrument),'NEVIII') ge 0 $
        or strpos(strupcase(ObsPramsStruct.instrument),'MGIX') ge 0 $
        then begin
; 2022-06: I left the OVI part unchanged for the moment
        if aniso_open gt 0 then begin
; model B1 - maximum anisotropy
          w_perp_cranmer=71.6+200./(2.07*(1./r3D)^1.15+6970.*(1./r3D)^15.6)
          aniso_wind=w_perp_cranmer/w_min_cranmer
          w_par=w_min
        endif else begin
; model B2 - minimum anisotropy
;  this can be accessed by aniso_open = 0
          w_max_cranmer=1.49e5*(1./r3D)^8.27+1.3*r3D^3.15
          w_perp_cranmer=74.3+200./(0.23*(1./r3D)^0.0327+4.69*(1./r3D)^2.36+2.58e5*(1./r3D)^22.9)
          diff=(w_perp_cranmer-w_max_cranmer) le 0.
          w_par_cranmer=diff*w_perp_cranmer+(1-diff)*w_max_cranmer
          aniso_wind=w_perp_cranmer/w_par_cranmer
          w_par=w_min*(w_par_cranmer/w_min_cranmer)
        endelse
      endif
      if min(closefield) ne -1 then aniso_wind[closefield]=1.
    endelse
  
    default,aniso,aniso_wind
    default,w_par,w_min

  endif else begin
;========Isotropic===============================================================================
    aniso=1. + r3D*0.
    w_par_wind=w_min*wpar_closed
    if min(openfield) ne -1 then w_par_wind[openfield]=w_min[openfield]*wpar_open
    default,w_par,w_par_wind
  endelse

endif

; Be sure ANISO is set to one if ISOTROPIC flag is set

if isotropic eq 1 then begin
  aniso = 1. + r3D*0.
endif

end
