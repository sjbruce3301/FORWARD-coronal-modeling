pro for_uvmodel,ObsPramsStruct,ModSolStruct,Pop2TRegime,r3D,theta3D,phi3D,Br,Bth,Bph,VelR,VelTh,VelPh,StokesUV_I,StokesUV_Q,StokesUV_U

;
; generates UV forward-modeled Stokes parameters based on physical state input
; STILL BEING TESTED
;
;  REQUIRED INPUTS
;
;  R3D,THETA3D,PHI3D -- plasma position in spherical coordinates
;	R3D  in solar radii
;		passed to FOR_UV_STOKES,
;	Phi3D measured clockwise from Sun-observer line (x axis)
;	Theta3D measured clockwise from plane-of-sky vertical (z axis)
;		used in FOR_FIXANGLE transform into Hanle coordinate
;		BIGTHETA and BIGCHI passed to FOR_UV_STOKES
;	points are along a line of sight, or an array of lines of sight
;	  with at least 2 points for each LOS vector
;	IN RADIANS
;
; VELR, VELTH, VELPH - solar wind velocity
;	same dimensions as R3D, THETA3D, PHI3D
;	Magnitude calculated below and passed as VWIND into FOR_UV_STOKES
;	(KM/SEC)
;
; BR, BTH, BPH -- magnetic field vector in spherical coordinates
;	in observer's frame of reference
;	IN GAUSS
;		used in FOR_FIXANGLE transform into Hanle coordinate
;		resulting magnetic field vector THETAB, CHIB
;		which is passed into FOR_UV_STOKES
;		along with magnitude BMAG
;
; OBSPRAMSSTRUCT: 
; 	INST - instrument
;
; MODSOLSTRUCT
; 	DENS -- electron density
; 	TEMP -- electron temperature
; 	POP2TREGIME -- if there are more than one population of density
;
; ADDITIONAL INPUTS CAN BE DEFINED AS PART OF MODSOLSTRUCT
;	(so far this has not been implemented -- edits needed in FOR_KINETIC
;	   and probably make_my_cube.pro)
;  or if not, calculated in FOR_KINETIC
; 	used as input to FOR_UV_STOKES

;  W_PAR -- 1/e Gaussian width for velocity distribution of the coronal ion 
;	parallel to the magnetic field (KM/SEC)
;	either passed in as part of ModSolStruct or calculated in FOR_KINETIC
;	Dimensionality same as R3D
;  ANISO -- parameter describing anisotropy of coronal velocity distribution:
;       1/e Gaussian width for velocity distribution of the coronal ion 
;	perpendicular to the magnetic field W_PERP=W_PAR*ANISO; [1 = isotropic distribution]
;	either passed in as part of ModSolStruct or calculated in FOR_KINETIC
;	Dimensionality same as R3D
;
; INPUTS passed through OBSPRAMSSTUCT.SPECTSTRUCT
;	ion-specific used as input to FOR_UV_STOKES
;
;  W_ANG -- 1/e Gaussian width for velocity distribution 
;       of the chromospheric/transition region ion (Angstrom)
;	converted to KM/S to become W_D and used in Doppler dimming, 
;	also used in getting physical units for intensity
;  CHROMORAD -- chromospheric radiation
;  BLEND -- line blending factor due to transition
;  EINSTEINA - Einstein coefficient for spontaneous emission (1/SEC)
;  EINSTEINB - Einstein coefficient for absorption/stimulated emission (CM^2/SEC)
;  GJ -  Lande factor
;
; INPUTS passed through OBSPRAMSSTUCT.SPECTSTRUCT
;   	Used by FOR_KINETIC
;
;  COLLKEY - whether or not to turn on collisions for total intensity
;	if negative, will return only collisions on I (wont calculate Stokes)
;
;  ISOTROPIC - whether to do isotropic (1) or anisotropic (0) model
;	If set, will overwrite any ModSolStruct.AnIso
;  WPAR_OPEN, WPAR_CLOSED -- multiplicative factors to establish W_PAR if not set in ModSolStruct
;       multiply W_MIN which depends on temperature, ion maxx
;       If info on open vs closed not available, will assume all is closed
;  keyword ANISO_OPEN -- model choice for ANISO for OVI (see for_kinetic.pro)
;       not used if set in ModSolStruct (UV spectro)
;       If info on open vs closed not available, will assume all is closed
;
;  NANGLEINT -- how many steps to take in solid angle integration
;               of the incoming integration
;
; OUTPUTS StokesUV -- Linear polarization, I, Q, U
;
; Called by FOR_UVCALC
;
; Calls FOR_UV_STOKES
;	FOR_FIXANGLE
;	FOR_KINETIC 
;
; Written by Silvano Fineschi, Sarah Gibson 2013
;	Significant update October 2017; SF, SG also Roberto Susino
;	Also May 2019 
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Nov 2022 -- added EinsteinB etc and updated EinsteinA for 1037
;		TO BE VERIFIED!
;		Also changed 1032 blend to 1 
;	Dec 2022 -- added NeVIII (STILL TESTING)
;		also updated NeVIII collisions file in DEFAULTS 
;		  and moved windows file naming correction up to where it mattered
;		also updated EinsteinB and w_ang for NEVIII
;	Jan 2023
;       restructured so abundance, LWidth (w_ang)
;       chromorad,blend, einsteina, einsteinb, and gj 
;       defaults are defined for UV/EUV here
;        (in SpecPrams not here in FOR_UVMODEL.PRO or through ModSolStruct).
;       restructured so w_ang (LWidth), blend, einsteina, and gj defaults are defined for UV/EUV h
;	Also re-ordered code for simplicity
; 
;      May 2023-- allowed for collemiss=-1 to plot just collisional emission no scattering
;	Sep 2023-- 
;		added chromorad explicitly on disk since it will
;		be part of the intensity observed there.
;		This required passing through the CMER central meridian observer position to define the disk.
;		Also, updted Pop2 to include collisions.
;		Also, used abs(chromorad) allowing negative value
;		 to be passed through -- if that happens, won't be added
;		 to the Iline on disk.
;		**NO this was adding it to every integrand. Removed all this and now it is done
;		   in FOR_INTEGRATE
;		-- kept absollute value on chromorad because now it can be negative
;	** also added collisions to pop2 
;     Oct 2023
;	updated for consistency with chianti usage elsewhere in FORWARD
;		updated h, c, Rsun
;     Dec 2023 -- updated to call new rates file including OVI1037 and NEVIII780
;     Feb 2024 -- added rates file and ionization for MGIX
;
usewindows=0
if strupcase(!version.os_family) eq 'WINDOWS' then usewindows=1

;
; WIDTH CORONA PARALLEL
;  AND ANISOTROPY defaults
;  set in FOR_SPECDEFAULTS 
; 

SpectStructCheck=ObsPramsStruct.SpecPrams
collkey=SpectStructCheck.CollKey
isotropic=SpectStructCheck.Isotropic
nangleint=SpectStructCheck.NAngleInt
wpar_open=SpectStructCheck.Wpar_Open
wpar_closed=SpectStructCheck.Wpar_Closed
aniso_open=SpectStructCheck.AnIso_Open

;
;  VWIND -- magnitude of solar wind speed (direction assumed to be along field)
;     this will be calculated from VelR,VelTh,Velph 
; 	(KM/SEC)
 
Vwind=sqrt(VelR*VelR+VelTh*VelTh*VelPh*VelPh)

; generate model of kinetic temperature

for_kinetic,r3D,isotropic,wpar_open,wpar_closed,aniso_open,ModSolStruct,ObsPramsStruct,w_par,aniso

; ion specific quantities set in FOR_SPECDEFAULTS
;	or FOR_OBSDEFAULTS
;
w_ang=SpectStructCheck.LWidth
chromorad=SpectStructCheck.ChromoRad
; units of photons cm^-2 s^-1 sr^-1 
blend=SpectStructCheck.Blend
einsteina=SpectStructCheck.EinsteinA
einsteinb=SpectStructCheck.EinsteinB
GJ=SpectStructCheck.GJ
abundance=SpectStructCheck.abundance
wavelength=ObsPramsStruct.Wavelength_Ang

;
; get element abundance from Chianti



if strupcase(ObsPramsStruct.instrument) eq 'OVI1032' $
 or strupcase(ObsPramsStruct.instrument) eq 'OVI1037' then iz=8
if strupcase(ObsPramsStruct.instrument) eq 'LYA' then iz=1
if strupcase(ObsPramsStruct.instrument) eq 'NEVIII770' $
 or strupcase(ObsPramsStruct.instrument) eq 'NEVIII780' then iz=10
if strupcase(ObsPramsStruct.instrument) eq 'MGIX706' then iz=12

ffexist=file_exist(form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance')))
if ffexist then $
         abund_file=form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance')) else $
        ;if abundance file is not in main abundance directory, assume it is in 
        ;abundance/archive
         abund_file=form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance/archive'))

read_abund,abund_file,abundances,ref
elabund=(abundances(iz-1))[0]

;
; LINE WIDTH CHROMO
;   (band width over which signal is integrated)
;   w_d = 1/e most probable speed equivalent 
;	 to the chromospheric line-width [km/s]

w_d=(w_ang/wavelength)*2.9979d10

; units CM/SEC
;  convert to KM/SEC
w_d=w_d*1d-5

;
;  rotate spherical vector into local vertical Hanle coordinates

for_uvangle,r3D,theta3D,phi3D,Br,Bth,Bph,thetaB,chiB,bigtheta,bigchi
Bmag=sqrt(Br^2+Bth^2+Bph^2)

; need to call FOR_UV_STOKES one point at a time, since
; it loops over solid angle which has limits with height dependence
;
; radiative component only
; collisions only affect Stokes I 
; and are done below

dim=size(r3d)
dim1=dim[1]
if dim[0] ne 1 then dim2=dim[2] else dim2=1

StokesUV_I=r3d*0.d0
StokesUV_Q=r3d*0.d0
StokesUV_U=r3d*0.d0

; if collkey is negative, will return collisions only
;  for diagnostics purposes

if collkey ge 0 then begin
 for j = 0,dim2-1 do begin
  for i = 0,dim1-1 do begin
    ruse=r3d[i,j] 
    thbuse=thetaB[i,j] 
    chbuse=chiB[i,j]
    bthuse=bigtheta[i,j] 
    bchuse=bigchi[i,j]
    bmuse=Bmag[i,j] 
    vwuse=Vwind[i,j] 
    wpuse=w_par[i,j] 
    anuse=aniso[i,j] 
    for_uv_stokes,ruse,thbuse,chbuse,bthuse,bchuse,bmuse,vwuse,wpuse,anuse,nangleint,w_d,blend,einsteinA,gJ,Iuse,Quse,Uuse
    StokesUV_I[i,j]=Iuse
    StokesUV_Q[i,j]=Quse
    StokesUV_U[i,j]=Uuse
;    print,i,j
  endfor
 endfor
endif

;
; put into physical units
;

; collisional rates (from CHIANTI)

colldir=concat_dir(GET_ENVIRON('FORWARD'),'DEFAULTS')
if strupcase(ObsPramsStruct.instrument) eq 'OVI1032' then collfile=concat_dir(colldir,'OVI1032_rates.txt')
if strupcase(ObsPramsStruct.instrument) eq 'OVI1037' then collfile=concat_dir(colldir,'OVI1037_rates.txt')
;if strupcase(ObsPramsStruct.instrument) eq 'OVI1032' then collfile=concat_dir(colldir,'OVI1032_rates.txt')
if strupcase(ObsPramsStruct.instrument) eq 'LYA' then collfile=concat_dir(colldir,'LYA_rates.txt')
if strupcase(ObsPramsStruct.instrument) eq 'NEVIII770' then collfile=concat_dir(colldir,'NEVIII770_rates.txt')
if strupcase(ObsPramsStruct.instrument) eq 'NEVIII780' then collfile=concat_dir(colldir,'NEVIII780_rates.txt')
if strupcase(ObsPramsStruct.instrument) eq 'MGIX706' then collfile=concat_dir(colldir,'MGIX706_rates.txt')

if usewindows eq 1 then collfile=str_replace(collfile,'/','\')

;
;if strupcase(ObsPramsStruct.instrument) ne 'OVI1037' $
;   and strupcase(ObsPramsStruct.instrument) ne 'NEVIII780' $
;   then begin
readcol,collfile,t,c,/silent
crates=interpol(c,t,alog10(ModSolStruct.Temp)) 
; replaced with actual rates for other ions
;   endif else crates = 1.; units of cm^3 s^-1

;
; ionization ratio
;

ioneq_file=form_filename(SpectStructCheck.IonEq,'.ioneq',dir=concat_dir(!xuvtop,'ioneq'))
read_ioneq,ioneq_file,ioneq_temp,ion_fractions,ref

if strupcase(ObsPramsStruct.instrument) eq 'OVI1032' $
    or strupcase(ObsPramsStruct.instrument) eq 'OVI1037' then begin
        ;Resample to ionization fraction for our arrays
	ionization=interpol(ion_fractions[*,7,5],ioneq_temp,alog10(ModSolStruct.Temp))
endif

if strupcase(ObsPramsStruct.instrument) eq 'NEVIII770' $
    or strupcase(ObsPramsStruct.instrument) eq 'NEVIII780' then begin
        ;Resample to ionization fraction for our arrays
	ionization=interpol(ion_fractions[*,9,7],ioneq_temp,alog10(ModSolStruct.Temp))
endif

if strupcase(ObsPramsStruct.instrument) eq 'LYA' then begin
        ;Resample to ionization fraction for our arrays
	ionization=interpol(ion_fractions[*,0,0],ioneq_temp,alog10(ModSolStruct.Temp))
endif

if strupcase(ObsPramsStruct.instrument) eq 'MGIX706' then begin
        ;Resample to ionization fraction for our arrays
	ionization=interpol(ion_fractions[*,11,8],ioneq_temp,alog10(ModSolStruct.Temp))
endif

frequency=2.9979d10/(wavelength*1d-8) ; units of s^-1

; integrate over time *of level transition?* 
normchromorad=abs(chromorad)/(w_d*frequency/2.9979d5) ; units of photons cm^-2 sr^-1

emissconst=0.83*elabund*ionization*einsteinB*normchromorad  ; units of photons s^-1 sr^-1
;
; .83 is proton number density 
; for temperatures above 4.5 logT

;
; this introduces back a timescale from EinsteinB

; set up filling factor
; population 2
;
; NEED TO take into consideration if population 2
;  a different temperature input to FOR_KINETIC and IONIZATION ABOVE
;  also need to put warnings in -- only good for pop2tregime1 
; second coronal population -- a chromospheric population will not
; be treated correctly.
; TALK TO TERRY
;

fill=1.
fillp2=0.
if tag_exist(ModSolStruct,'FillingFactor') then fill=ModSolStruct.FillingFactor
if Pop2TRegime gt 0 then fillp2=ModSolStruct.Pop2FillingFactor

h=6.6262d-27 ; ergs*s
photon_energy = h*frequency ; ergs
RSun_cm = 6.95700d+10  ;solar radius in cm

StokesUV_I=StokesUV_I*ModSolStruct.Dens*fill*emissconst*photon_energy ; units of ergs s^-1 cm^-3 sr^-1
StokesUV_Q=StokesUV_Q*ModSolStruct.Dens*fill*emissconst*photon_energy
StokesUV_U=StokesUV_U*ModSolStruct.Dens*fill*emissconst*photon_energy

;Add Pop2 electrons

if Pop2TRegime gt 0 then begin
  StokesUV_I=StokesUV_I+StokesUV_I*ModSolStruct.Pop2Dens*fillp2*emissconst*photon_energy
  StokesUV_Q=StokesUV_Q+StokesUV_Q*ModSolStruct.Pop2Dens*fillp2*emissconst*photon_energy
  StokesUV_U=StokesUV_U+StokesUV_U*ModSolStruct.Pop2Dens*fillp2*emissconst*photon_energy
endif

; here we should add collisions 
collemiss=photon_energy*0.83*elabund*ionization*ModSolStruct.Dens^2*crates*fill/(4.*!pi) ; units of ergs s^-1 cm^-3 sr^-1
if Pop2TRegime gt 0 then begin
 collemiss2=photon_energy*0.83*elabund*ionization*ModSolStruct.Pop2Dens^2*crates*fill/(4.*!pi) ; units of ergs s^-1 cm^-3 sr^-1
 collemiss+=collemiss2
endif

; to add collisions
if collkey gt 0 then StokesUV_I += collemiss ; units of ergs s^-1 cm^-3 sr^-1
; or to test just collisions 
;  (scattering will not have been calculated for I, Q, U)
if collkey lt 0 then StokesUV_I = collemiss 
; if collkey lt 0 then Q, U = 0
;
; if collkey = 0 then collemiss not used
;  	scattering only

;
; Finally, we have to multiply by Rsun in cm, to make it an integrand
; in the right units of ergs cm^-2 s^-1 sr^-1
;

StokesUV_I=StokesUV_I*RSun_cm
StokesUV_Q=StokesUV_Q*RSun_cm
StokesUV_U=StokesUV_U*RSun_cm

end
