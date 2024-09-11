;+
pro make_my_cube,simname=simname0,axisym=axisym,hydro=hydro,isothermal=isothermal,densprof=densprof,$
		rsun=rsun,global=global,date=date,mheat=mheat,OutFile=OutFile,OutDir=OutDir,$
		CubeFile=CubeFile,modelname=modelname,cubestr=cubestr,magmod=magmod,topology=topology
;
; Project: FORWARD
;
; Name MAKE_MY_CUBE
;
; Explanation:
; This code will generate a numerical datacube that can be used in the forward
; models given a numerical model. The output from make_my_cube will be
; in the form of an IDL save set (called for_'simname'.dat or 
; with hydro model extension
;
; 	ALL SIMULATION VARIABLES SHOULD BE IN CGS
;	EXCEPT!! VELOCITIES SHOULD BE IN KM/SEC
;	ALSO  - if /rsun is set, r will be expected in units of solar radii
;
; The input should be a save set named 'simname'.dat which should include 
; at a minimum (for 3D simulations) these
; six quantities with these same names:
;
; 	r = fltarr(NR) (r=0 is center of sun)
; 	th = fltarr(NTH)
; 	ph = fltarr(NPH)
;
; (NOTE ALL GRIDS MUST BE REGULAR AND ORDERED THEY DO NOT HAVE TO BE
; UNIFORM) Theta and phi should be in RADIANS, where acceptable values
; for theta are (0 LE theta LE pi, zero is the north pole ) 
; and phi should have values (-pi LE phi LE pi). 
; For non-global simulations, the central longitude (phi)
; should be zero, and the central colatitude (theta) should be
; pi/2. Note global simulations do not have to 
; have the center of the data cube at ph=0; for example,
; if a cube is ordered from 0 to 2pi it can simply be
; changed to 0 to pi and then carry on with ph values
; from -pi to 0
;
; 
; Magnetic field is required UNLESS keyword MAGMOD=0
;
; 	Br = fltarr(NR, NTH, NPH)
; 	Bth = fltarr(NR, NTH, NPH)
; 	Bph = fltarr(NR, NTH, NPH)
;
; (note, float used as example; double is fine too):
;
; The FORWARD codes also require specification of electron number
; density, temperature, and pressure. There are several ways to
; proceed.
;
; A) - If you want to use the plasma data from your cube, include the
;      following three arrays
;
; 	dens = fltarr(NR, NTH, NPH), electron # density
; 	pres = fltarr(NR, NTH, NPH), dyne/cm^2
; 	temp = fltarr(NR, NTH, NPH), kelvin
;
;      If you do not have all three, you can use the ideal gas law,
;      P=2NKT where Boltzman's constant is 1.3807d-16 [erg/K] to
;      calculate the others. 
;
; 	NOTE: We will add instructions for adding a second, e.g.
;		cooler population here in future -- watch this space!
;
; B) - If the simulation does NOT have a density distribution, or if
;      one wants to ignore that distribution and just consider the
;      magnetic field in a spherically symmetric hydrostatic
;      atmosphere (reasonable for low beta plasmas),
;
;      keyword HYDRO should be set to:
;
;		HYDRO = 1; exponential isothermal atmosphere
;		HYDRO = 2; power law density/pressure in hydrostatic balance
;               HYDRO=3; Vasquez et al 2003 hydrostatic atmosphere
;		HYDRO=4; Cranmer 1999 empirical model
;		(note HYDRO=0 by default, and it will look for 
;			your dens,pres,temp arrays)
;
; For HYDRO = 1, the density and pressure will  be calculated by
; program FOR_HYDROCALC.PRO. Define your temperature with the keyword
;
;               ISOTHERMAL [10^6 kelvin]
;                (default 1.5MK)
;      but make sure that it is in the range of around 1-2 MK. 
;      
;      Base electron number density is defined by keyword 
;
;               DENSPROF  [electrons/CC] 
;               (default 1d9 electrons/cc)
;
;       The pressure follows from ideal gas.
;
;       For HYDRO=2, DENSPROF can be array [A,B,C,D,E,F]
;                       or multiplier of array [densprof*A,B,densprof*C,D,densprof*E,F]
;                    dens = A*r^-B + C*r^-D + E*r^-F
;                  and in FOR_MAKEPLASMA A,B,C,D,E,F defaults to WSM streamer values
;       Pressure follows by integrating to solve radial hydrostatic
; balance, and Temperature from ideal gas (it can no longer be
; isothermal) 
;
;        For HYDRO=3, DENSPROF can be array [A1,A2,A3,A4,A5,aa,bb,alpha,beta]
;                       or multiplier of array 
;                     dens=A1*exp(A2/r)*r^-2*(1+A3/r+A4/r^2+A5/r^3)
;                     temp=TE*(aa+1)/(aa+bb*r^alpha + (1-bb)*r^-beta)
;                  and in FOR_MAKEPLASMA A1,A2,A3,A4,A5,aa,bb,alpha,beta
;                       defaults to Vasquez streamer values
;            For HYDRO=4, DENSPROF,ODENSPROF can be array [da,db,dc,dd,de,ta,tb,tc,td]
;                       or multipliers of da
;                       and inside GIBBAGLOW program da,db,dc,dd,de
;                       defaults to Cranmer coronal hole values
;                       and ta,tb,tc,td
;                       defaults to Cranmer coronal hole and Vasquez streamer values
;                  dens_he= da*1e5*(db*(1./r)^dc + dd*(1./r)^de)
;                  temp_he= TE*(1e6/1.5e6)*(ta*r^tb + tc*r^td)^(-1)
;                  DEFAULT DENSPROF=6,ODENSPROF=1
;
;
; TOPOLOGY -- for cubes that have no plasma, there is an option
; 	to pass information about whether the points in the cube lie
; 	along open or closed magnetic field lines, and also what
;	the magnetic field vector is at the footpoint and the field line length.  This
;	will be used in FORWARD to assign different density/temperature
;	models for open vs closed points. It adds a requirement for the
;	following information to be passed in
;
;  	NOTE -- Brphot1 is the photospheric footpoint for open field lines
;
; 
;       Convention as in deRosa PFSS code:
;       FROM PRSS_TRACE_FIELD.PRO: 
;		 (by default, closed field lines are oriented so that the
;		     end with negative polarity comes first and open 
;		     field lines are oriented so that the photospheric
; 		    end comes first)
; 		open = fltarr(NR, NTH, NPH)  -- 0 or 1 for closed vs. open
;						-1 for detached
;		fllen = fltarr(NR,NTH,NPH) -- length in Rsun of field line
; 		Brphot1 = fltarr(NR, NTH, NPH) -- value at fieldline footpoint
; 		Bthphot1 = fltarr(NR, NTH, NPH) -- value at fieldline footpoint
; 		Bphphot1 = fltarr(NR, NTH, NPH) -- value at fieldline footpoint
; 		Brphot2 = fltarr(NR, NTH, NPH) -- value at fieldline footpoint
; 		Bthphot2 = fltarr(NR, NTH, NPH) -- value at fieldline footpoint
; 		Bphphot2 = fltarr(NR, NTH, NPH) -- value at fieldline footpoint
;	
;
; VELOCITY -- if velocity exists, it will be passed through
;	otherwise a scalar of zero velocity VEL will be assumed
;	If varible VEL exists, it will be assumed this is the
;	velocity magnitude, with direction defined by the field
;	 (this is applied in FOR_FIELDCALLS)
;	if VR, VTH, VPH exist, these will be assumed to be
;	vector velocity field
;
;	NOTE ALL VELOCITIES IN UNITS OF KM/S
;
; TWO POPULATIONS ARE ALLOWED
;
;    Top-level FORWARD parameter (POP2TREGIME - set in
;      FOR_SPECDEFAULTS but described in FOR_OBSDEFAULTS) 
;    allows treating distinct chromospheric and coronal populations
;  	(for POP2TREGIME=2; keyword POP2IONFRAC is set and discussed in FOR_SPECDEFAULTS) 
;    or two coronal populations which may have different abundances or temperatures,
;	(for POP2TREGIME=1; keyword POP2ABUNDANCE is set and discussed in FOR_SPECDEFAULTS)
;    if unset or set to zero, even if cube has second population, it will be ignored
;
;   TAGS ON CUBE THAT CAN BE SET HERE:
;
;   Pop2Dens - density values for second plasma population in cm^-3
;	if unset (and pop2tregime is nonzero), main (population 1) DENS will be used
;   Pop2Temp - temperature values for second plasma population in kelvin
;	if unset (and pop2tregime is nonzero), main (population 1) TEMP will be used
;   FillingFactor - filling factor for main plasma population
;	if unset (and pop2dens and/or pop2temp are/is set), will be set to 1 in NUMCUBE
;   Pop2FillingFactor - filling factor for second plasma population
;	if unset (and pop2dens and/or pop2temp are/is set), will be set to 1-FillingFactor in NUMCUBE
;
;  Note, these quantities can be further adjusted via 
;  	NUMCUBEPRAMS parameters, COLDPTS, DTHRES, and P2FILL
;	which will (temporarily) overwrite Pop2Dens, Pop2Temp, FillingFactor, and Pop2FillingFactor
;	(see NUMCUBE.PRO)
;
;  BEING TESTED:   ion density IONDENS

;
; FOR AXISYMMETRIC SIMULATIONS
;
; As above, except everything in arrays of (NR, NTH) PH is not
; necessary, and will be defined to be zero below. Set the keyword
;
;               AXISYM = 1
;
; Other Input Keywords:
;	global - specifies that cube is intended to cover all values of theta and phi
;	OutFile - specifies output file. Otherwise default names 
;			(simname or modelname) will be used.
;	OutDir - specifies directory for output file. Otherwise file will be put in 
;			same directory as simname.
;   ModelName- if cube should contain a model name which is different than the 
;				one in the input filename (simname)
;
;
; DATE -- if simulation is a fit to a particular date, enter in form
;	e.g '2010-05-05T00:00:00'
;		default,''
;
; MHEAT -- specific to PSI MAS model, see description in MAKEMASCUBE
;	in PSIMAS directory
;
; MAGMOD -- whether or not there is magnetic field
;	default MAGMOD 1
;
;Output Keywords:
;	CubeFile - name of IDL save file used for output
;	CubeStr - cube structure placed in CubeFile
;
; Called by MAKEMASCUBE
; Calls FOR_MAKEPLASMA, FOR_SAVECUBE
;
; HISTORY
;       Created: Laurel Rachmeler, 2011
;       14-May-2012 Added sorting so that coordinates and dependent arrays are monotonic
;                       Added a check so that the same coordinate value is not repeated
;                       Added GLOBAL keyword. If this keyword is set extra ph and th coordinates are 
;                               added at the end just above and below coordinate ranges to make sure 
;                               there are no gaps. physical parameters are set to be identical to edged is input cube values.
;                       Added checks associated with RSun and th and ph ranges
;                       Added keywords UseFileName and OutDir to allow user specification of 
;                               output file name and directory
;                       Added keywords CubeFile and Cube to allow user to obtain the name of the 
;                               Output file and the cube structure.  T.A. Kucera
;       6-Jul-2012 Added MODELNAME keyword so that model can be renamed if needed.
;       23-Oct-2012 Small change to check for identical consecutive coordinates. TAK
;       24-Oct-2013 Added velocity SEG
;
; Version 2.0 July 2014
;		27-Jan-2016 added fillingfactor, pop2fillingfactor, pop2dens, pop2temp. TAK
;	July 2017 - reduced size of global angle pad from 1d-1 to 1d-5
;	Feb 2018 -- fixed comment where Boltzmann's constant was slightly off
;	Oct 2018 -- added hydrostatic options
;	Sep 2019 -- added hydrostatic option 4
;	Aug 2020 -- fixed it to pass iondens and ionname through to the cube
;			also added magdens pass through
;	Sep 2020 -- added magmod as a keyword
;	Oct 2023 -- updated Rsun
;	Apr 2024 -- updated definition of topology open=-1 for detached
;
; DEFAULT KEYWORDS
;  default 3d, isothermal, not hydrostatic

default,simname,'numcube'
default,axisym,0
default,rsun,0
default,hydro,0
default,topology,0
default,magmod,1

;
; input cube
;check if data cube ends in .dat
if strlowcase(strmid(simname0,strlen(simname0)-4)) eq '.dat' $
	then simname=strmid(simname0,0,strlen(simname0)-4) $
		else simname=simname0
restore,simname+'.dat'

;Make sure simname seems to have the right contents!
if n_elements(r) eq 0 or n_elements(th) eq 0 then begin
	print,simname+'.dat does not contain the required variables.'
	return
endif

if max(r) lt 1d10  and rsun eq 0 then $
	message,/info,'The r coordinate does not seem to be in cm. Do you want to set the RSUN keyword?'

; convert r to rsun if necessary
if rsun eq 0 then r=r/6.957d10

if axisym eq 1 then ph=0.

;
; check to be sure there is plasma
;

if hydro eq 0 then begin
    IF n_elements(dens) LT 1 THEN BEGIN
        print, "OOPS! no density specified. Cube not created. Try again or set hydro to 1, 2 or 3 or 4."
        return
    ENDIF
    IF n_elements(pres) LT 1 THEN BEGIN
        print, "OOPS! no pressure specified. Cube not created. Try again or set hydro to 1, 2 or 3 or 4."
        return
    ENDIF
    IF n_elements(temp) LT 1 THEN BEGIN
        print, "OOPS! no temperature specified. Cube not created. Try again or set hydro to 1, 2 or 3 or 4."
        return
    ENDIF
endif

if n_elements(Brphot1) lt 1 then Brphot1=0.d0
if n_elements(Bthphot1) lt 1 then Bthphot1=0.d0
if n_elements(Bphphot1) lt 1 then Bphphot1=0.d0
if n_elements(Brphot2) lt 1 then Brphot2=0.d0
if n_elements(Bthphot2) lt 1 then Bthphot2=0.d0
if n_elements(Bphphot2) lt 1 then Bphphot2=0.d0
if n_elements(open) lt 1 then open=0.d0
if n_elements(fllen) lt 1 then fllen=0.d0

if n_elements(Vr) lt 1 then begin
 if n_elements(Vel) lt 1 then Vel=0.d0
 Vr=0.d0
 Vth=0.d0
 Vph=0.d0
endif else Vel=0.d0

if magmod eq 0 then begin
 Br=0.d0*Dens
 Bth=0.d0*Dens
 Bph=0.d0*Dens
endif 

if n_elements(fillingfactor) lt 1 then fillingfactor=0
if n_elements(pop2fillingfactor) lt 1 then pop2fillingfactor=0
if n_elements(pop2dens) lt 1 then pop2dens=0
if n_elements(pop2temp) lt 1 then pop2temp=0
if n_elements(iondens) lt 1 then iondens=0
if n_elements(ionname) lt 1 then ionname=''

;make sure th between 0 and pi and ph between -pi and +pi
if min(th) lt 0 or max(th) gt !pi then message,'Theta coordinates should be in radians between 0 and pi'
if min(ph) lt -!pi or max(ph) gt !pi then message,'Phi coordinates should be in radians between -pi and +pi'

;Make sure r, th, and ph are monotinically ascending. 
Nr=n_elements(r)	&	Nth=n_elements(th)	& Nph=n_elements(ph)
rsrt=sort(r)		& 	thsrt=sort(th)		& phsrt=sort(ph)
r=r[rsrt]			& 	th=th[thsrt]		& ph=ph[phsrt]
SBr=size(Br)		& 	SBth=size(Bth) 		& SBph=size(Bph)
SBrphot1=size(Brphot1)	& 	SBthphot1=size(Bthphot1)& SBphphot1=size(Bphphot1) & Sopen=size(open)
SBrphot2=size(Brphot2)	& 	SBthphot2=size(Bthphot2)& SBphphot2=size(Bphphot2) & Sfllen=size(fllen)
SVr=size(Vr)		& 	SVth=size(Vth) 		& SVph=size(Vph)
SDens=size(Dens)	& 	STemp=size(Temp)    & SPres=size(Pres)  & SVel=size(Vel)
SFillingFactor=size(FillingFactor)   &  SPop2FillingFactor=size(Pop2FillingFactor)
SPop2Dens=size(Pop2Dens)             &  SPop2Temp=size(Pop2Temp)
SIonDens=size(IonDens) 

;Now check for identical consecutive coordinates
if Nph gt 1 then begin
	DPhNonZero=where((ph-shift(ph,1)) ne 0) 
	ph=ph[DPhNonZero]
endif else DPhNonZero=0
DThNonZero=where((th-shift(th,1)) ne 0)
	th=th[DThNonZero]
DRNonZero=where((r-shift(r,1)) ne 0)
	r=r[DRNonZero]

if max(abs(SBr-SBth)) ne 0 or max(abs(SBr-SBph)) ne 0 then $
		message,'All magnetic field arrays must have the same dimensions'

		
if SBr[0] gt 1 then begin		;resort B field 
		Br=Br[rsrt[DRNonZero],*,*]		& 	Bth=Bth[rsrt[DRNonZero],*,*] 	&	Bph=Bph[rsrt[DRNonZero],*,*]
		Br=Br[*,thsrt[DThNonZero],*]	& 	Bth=Bth[*,thsrt[DThNonZero],*] 	&	Bph=Bph[*,thsrt[DThNonZero],*]
		Br=Br[*,*,phsrt[DPhNonZero]]	& 	Bth=Bth[*,*,phsrt[DPhNonZero]] 	&	Bph=Bph[*,*,phsrt[DPhNonZero]]
endif

if SBrphot1[0] gt 1 then begin		;resort Bphot1 field and open
		Brphot1=Brphot1[rsrt[DRNonZero],*,*]		& 	Bthphot1=Bthphot1[rsrt[DRNonZero],*,*] 	&	Bphphot1=Bphphot1[rsrt[DRNonZero],*,*] 	& 	open=open[rsrt[DRNonZero],*,*]
		Brphot1=Brphot1[*,thsrt[DThNonZero],*]	& 	Bthphot1=Bthphot1[*,thsrt[DThNonZero],*] 	&	Bphphot1=Bphphot1[*,thsrt[DThNonZero],*] 	& 	open=open[*,thsrt[DRNonZero],*]
		Brphot1=Brphot1[*,*,phsrt[DPhNonZero]]	& 	Bthphot1=Bthphot1[*,*,phsrt[DPhNonZero]] 	&	Bphphot1=Bphphot1[*,*,phsrt[DPhNonZero]] 	& 	open=open[*,*,phsrt[DRNonZero]]
endif

if SBrphot2[0] gt 1 then begin		;resort Bphot2 field and open
		Brphot2=Brphot2[rsrt[DRNonZero],*,*]		& 	Bthphot2=Bthphot2[rsrt[DRNonZero],*,*] 	&	Bphphot2=Bphphot2[rsrt[DRNonZero],*,*] 	& 	fllen=fllen[rsrt[DRNonZero],*,*]
		Brphot2=Brphot2[*,thsrt[DThNonZero],*]	& 	Bthphot2=Bthphot2[*,thsrt[DThNonZero],*] 	&	Bphphot2=Bphphot2[*,thsrt[DThNonZero],*] 	& 	fllen=fllen[*,thsrt[DRNonZero],*]
		Brphot2=Brphot2[*,*,phsrt[DPhNonZero]]	& 	Bthphot2=Bthphot2[*,*,phsrt[DPhNonZero]] 	&	Bphphot2=Bphphot2[*,*,phsrt[DPhNonZero]] 	& 	fllen=fllen[*,*,phsrt[DRNonZero]]
endif

if SVr[0] gt 1 then begin		;resort V field 
		Vr=Vr[rsrt[DRNonZero],*,*]		& 	Vth=Vth[rsrt[DRNonZero],*,*] 	&	Vph=Vph[rsrt[DRNonZero],*,*]
		Vr=Vr[*,thsrt[DThNonZero],*]	& 	Vth=Vth[*,thsrt[DThNonZero],*] 	&	Vph=Vph[*,thsrt[DThNonZero],*]
		Vr=Vr[*,*,phsrt[DPhNonZero]]	& 	Vth=Vth[*,*,phsrt[DPhNonZero]] 	&	Vph=Vph[*,*,phsrt[DPhNonZero]]
endif

if SVel[0] gt 1 then begin	 ;resort velocity magnitude (if not vector)
	Vel=Vel[rsrt[DRNonZero],*,*]
	Vel=Vel[*,thsrt[DThNonZero],*]
	Vel=Vel[*,*,phsrt[DPhNonZero]]	
endif

if SDens[0] gt 1 then begin	 ;resort density
	Dens=Dens[rsrt[DRNonZero],*,*]
	Dens=Dens[*,thsrt[DThNonZero],*]
	Dens=Dens[*,*,phsrt[DPhNonZero]]	
endif
if STemp[0] gt 1 then begin	 ;resort temperature
	Temp=Temp[rsrt[DRNonZero],*,*]
	Temp=Temp[*,thsrt[DThNonZero],*]
	Temp=Temp[*,*,phsrt[DPhNonZero]]	
endif
if SPres[0] gt 1 then begin	 ;resort pressure
	Pres=Pres[rsrt[DRNonZero],*,*]
	Pres=Pres[*,thsrt[DThNonZero],*]
	Pres=Pres[*,*,phsrt[DPhNonZero]]	
endif

if SFillingFactor[0] gt 1 then begin	 ;resort FillingFactor
	FillingFactor=FillingFactor[rsrt[DRNonZero],*,*]
	FillingFactor=FillingFactor[*,thsrt[DThNonZero],*]
	FillingFactor=FillingFactor[*,*,phsrt[DPhNonZero]]	
endif

if SPop2FillingFactor[0] gt 1 then begin	 ;resort Pop2FillingFactor
	Pop2FillingFactor=Pop2FillingFactor[rsrt[DRNonZero],*,*]
	Pop2FillingFactor=Pop2FillingFactor[*,thsrt[DThNonZero],*]
	Pop2FillingFactor=Pop2FillingFactor[*,*,phsrt[DPhNonZero]]	
endif

if SPop2Dens[0] gt 1 then begin	 ;resort Pop2Density
	Pop2Dens=Pop2Dens[rsrt[DRNonZero],*,*]
	Pop2Dens=Pop2Dens[*,thsrt[DThNonZero],*]
	Pop2Dens=Pop2Dens[*,*,phsrt[DPhNonZero]]	
endif

if SPop2Temp[0] gt 1 then begin	 ;resort Pop2Temperature
	Pop2Temp=Pop2Temp[rsrt[DRNonZero],*,*]
	Pop2Temp=Pop2Temp[*,thsrt[DThNonZero],*]
	Pop2Temp=Pop2Temp[*,*,phsrt[DPhNonZero]]	
endif

if SIonDens[0] gt 1 then begin	 ;resort IonDens
	IonDens=IonDens[rsrt[DRNonZero],*,*]
	IonDens=IonDens[*,thsrt[DThNonZero],*]
	IonDens=IonDens[*,*,phsrt[DPhNonZero]]	
endif


			;cube should cover all of theta and phi, so add extra points at the end
;
; this basically assumes that the grid given has endpoints in theta and phi very close to
;	[0,pi], [-pi,pi] 
;  because it takes the first and last point and clones them and all the arrays then have
;  cloned points there.
;   This could be problematic if the array inputted goes, e.g. from 0, .9pi -- because the last point will be stretched in a way
;     that is not symmetric with the first point
;   Or if it goes from .1pi to .9pi, it will be symmetric, but will have a flat propagation of the value of .1pi down to 0, and .9pi up to pi
; if done in theta and phi, could have an issue since for a given phi, the value of density could change with theta and vice verse
;  so coud make some weird artifacts at the pole. As long as the structures are smooth at the poles, and or the cube
;   gets quite close to the poles (and ideally, is symmetric N-S, E-W) then this is not a huge problem

if keyword_set(global) then begin
	th=[-1d-5,th,!dpi+1d-5]
	if Nph gt 1 then ph=[-!dpi-1d-5,ph,!dpi+1d-5]
	if SDens[0] gt 1 then begin
		if SDens[0] eq 3 then $
			Dens=interpolate(Dens,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) $
			else Dens=interpolate(Dens,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
	endif
	if STemp[0] gt 1 then begin
		if STemp[0] eq 3 then $
			Temp=interpolate(Temp,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) $
			else Temp=interpolate(Temp,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
	endif
	if SPres[0] gt 1 then begin
		if SPres[0] eq 3 then $
			Pres=interpolate(Pres,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) $
			else Pres=interpolate(Pres,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
	endif
	if SBr[0] gt 1 then begin
		if SBr[0] eq 3 then begin
				Br=interpolate(Br,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
				Bth=interpolate(Bth,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
				Bph=interpolate(Bph,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
		endif else begin
				Br=interpolate(Br,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
				Bth=interpolate(Bth,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
				Bph=interpolate(Bph,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
		endelse
	endif
	if SBrphot1[0] gt 1 then begin
		if SBrphot1[0] eq 3 then begin
				Brphot1=interpolate(Brphot1,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
				Bthphot1=interpolate(Bthphot1,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
				Bphphot1=interpolate(Bphphot1,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
				open=interpolate(open,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
		endif else begin
				Brphot1=interpolate(Brphot1,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
				Bthphot1=interpolate(Bthphot1,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
				Bphphot1=interpolate(Bphphot1,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
				open=interpolate(open,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
		endelse
	endif
	if SBrphot2[0] gt 1 then begin
		if SBrphot2[0] eq 3 then begin
				Brphot2=interpolate(Brphot2,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
				Bthphot2=interpolate(Bthphot2,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
				Bphphot2=interpolate(Bphphot2,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
				fllen=interpolate(fllen,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
		endif else begin
				Brphot2=interpolate(Brphot2,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
				Bthphot2=interpolate(Bthphot2,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
				Bphphot2=interpolate(Bphphot2,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
				fllen=interpolate(fllen,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
		endelse
	endif
	if SVr[0] gt 1 then begin
		if SVr[0] eq 3 then begin
				Vr=interpolate(Vr,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
				Vth=interpolate(Vth,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
				Vph=interpolate(Vph,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) 
		endif else begin
				Vr=interpolate(Vr,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
				Vth=interpolate(Vth,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
				Vph=interpolate(Vph,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
		endelse
	endif
	if SVel[0] gt 1 then begin
		if SVel[0] eq 3 then $
			Vel=interpolate(Vel,indgen(Nr),[0,indgen(Nth),Nth-1],[0,indgen(Nph),NPh-1],/grid) $
			else Vel=interpolate(Vel,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
	endif
	if SFillingFactor[0] gt 1 then begin
		if SFillingFactor[0] eq 3 then $
			FillingFactor=interpolate(FillingFactor,indgen(Nr),[0,indgen(Nth),Nth-1],$
			              [0,indgen(Nph),NPh-1],/grid) $
			else FillingFactor=interpolate(FillingFactor,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
	endif
    if SPop2FillingFactor[0] gt 1 then begin
		if SPop2FillingFactor[0] eq 3 then $
			Pop2FillingFactor=interpolate(Pop2FillingFactor,indgen(Nr),[0,indgen(Nth),Nth-1],$
			              [0,indgen(Nph),NPh-1],/grid) $
			else Pop2FillingFactor=interpolate(Pop2FillingFactor,indgen(Nr),$
			              [0,indgen(Nth),Nth-1],/grid)
	endif
	if SPop2Dens[0] gt 1 then begin
		if SPop2Dens[0] eq 3 then $
			Pop2Dens=interpolate(Pop2Dens,indgen(Nr),[0,indgen(Nth),Nth-1],$
			              [0,indgen(Nph),NPh-1],/grid) $
			else Pop2Dens=interpolate(Pop2Dens,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
	endif
	if SPop2Temp[0] gt 1 then begin
		if SPop2Temp[0] eq 3 then $
			Pop2Temp=interpolate(Pop2Temp,indgen(Nr),[0,indgen(Nth),Nth-1],$
			              [0,indgen(Nph),NPh-1],/grid) $
			else Pop2Temp=interpolate(Pop2Temp,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
	endif
	if SIonDens[0] gt 1 then begin
		if SIonDens[0] eq 3 then $
			IonDens=interpolate(IonDens,indgen(Nr),[0,indgen(Nth),Nth-1],$
			              [0,indgen(Nph),NPh-1],/grid) $
			else IonDens=interpolate(IonDens,indgen(Nr),[0,indgen(Nth),Nth-1],/grid)
	endif


endif   		
    		

;
; if hydro not zero, fill in plasma
;

if hydro ne 0 then for_makeplasma,r,th,ph,dens,pres,temp,hydro,isothermal=isothermal,densprof=densprof 

;
; date
;
default,date,''

;*Output*


if topology eq 0 then begin

     if Svr[0] gt 1 then for_savecube,r, th, ph, dens, pres, temp, br, bth, bph, $
		axisym, hydro, simname,vr=vr,vth=vth,vph=vph,$
		cubefile=cubefile,outfile=outfile,outdir=outdir,cubestr=cubestr,$
		modelname=modelname,global=global,date=date,mheat=mheat,$
		fillingfactor=fillingfactor,pop2fillingfactor=pop2fillingfactor,$
		pop2dens=pop2dens,pop2temp=pop2temp,iondens=iondens,ionname=ionname,magmod=magmod $
            else for_savecube,r, th, ph, dens, pres, temp, br, bth, bph, $
		axisym, hydro, simname,vr=vel,cubefile=cubefile,outfile=outfile,outdir=outdir,$
		cubestr=cubestr,modelname=modelname,global=global,date=date,mheat=mheat,$
		fillingfactor=fillingfactor,pop2fillingfactor=pop2fillingfactor,$
		pop2dens=pop2dens,pop2temp=pop2temp,$
		iondens=iondens,ionname=ionname,magmod=magmod

endif else begin

     if Svr[0] gt 1 then for_savecube,r, th, ph, dens, pres, temp, br, bth, bph, $
		axisym, hydro, simname,vr=vr,vth=vth,vph=vph,$
		brphot1=brphot1,brphot2=brphot2,bthphot1=bthphot1,bthphot2=bthphot2,bphphot1=bphphot1,bphphot2=bphphot2,open=open,fllen=fllen,$
		cubefile=cubefile,outfile=outfile,outdir=outdir,cubestr=cubestr,$
		modelname=modelname,global=global,date=date,mheat=mheat,$
		fillingfactor=fillingfactor,pop2fillingfactor=pop2fillingfactor,$
		pop2dens=pop2dens,pop2temp=pop2temp,iondens=iondens,ionname=ionname,magmod=magmod $
            else for_savecube,r, th, ph, dens, pres, temp, br, bth, bph, $
		axisym, hydro, simname,vr=vel,cubefile=cubefile,outfile=outfile,outdir=outdir,$
		brphot1=brphot1,brphot2=brphot2,bthphot1=bthphot1,bthphot2=bthphot2,bphphot1=bphphot1,bphphot2=bphphot2,open=open,fllen=fllen,$
		cubestr=cubestr,modelname=modelname,global=global,date=date,mheat=mheat,$
		fillingfactor=fillingfactor,pop2fillingfactor=pop2fillingfactor,$
		pop2dens=pop2dens,pop2temp=pop2temp,$
		ionname=ionname,iondens=iondens,magmod=magmod

endelse

end
