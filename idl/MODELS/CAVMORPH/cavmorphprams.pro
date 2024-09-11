;**********************************************************************
;+
pro cavmorphprams,outarray,date=date,working_dir=working_dir,$
	tcav=tcav,trim=trim,tnoug=tnoug,trgam=trgam,tcgam=tcgam,$
	tngam=tngam,nougat=nougat,TCPar=TCPar,TRPar=TRPar,TMod=TMod,$
	densscal=densscal,usemod=usemod,streamscales=streamscales,$
	cavscales=cavscales,nougscales=nougscales,$
	holescales=holescales,thcs=thcs0,phcs=phcs0,csheight=csheight,$
	photowidth=photowidth,$
	newkirk=newkirk,cswidth=cswidth,alpha=alpha0,mang=mang,photolength=photolength,$
	depbase=depbase,depslope1=depslope1,depslope2=depslope2,rdepmid=rdepmid,$
	cavtop_r=cavtop_r,cavtop_th=cavtop_th,cavwidth=cavwidth,$
	cavheight=cavheight,cavlength=cavlength,nofmax=nofmax,tunnel=tunnel,tunslope=tunslope,$
	nougthcs=nougthcs0,nougphcs=nougphcs0,noug_mang=noug_mang,$
	nougtop_r=nougtop_r,nougtop_th=nougtop_th,nougwidth=nougwidth,$
	nougheight=nougheight,nouglength=nouglength,nougmult=nougmult,zitipram=zitipram,$
	FFPar=FFpar,CDens=CDens,Pop2Hole=pop2hole,Pop2Cav=pop2cav,Pop2Noug=pop2noug,$
	CFF_Streamscales=CFF_Streamscales,CFF_Cavscales=CFF_Cavscales,CFF_Nougscales=CFF_Nougscales,$
	saveprams=saveprams,readprams=readprams
;+
; Name: cavmorphprams
;
;Purpose: To create structure containing information concerning the
;         CavMorph model; To be called by driver routine with keyword inputs 
;          with resulting structure OUTARRAY assigned to ModPramsStruct 
;	(with ModPramsStruct.name='cavmorph')
;
; Keyword Inputs:
;
;      (NOTE: * indicates directly measurable from observations, no fit required)
;
;	MORPHOLOGY STREAMER
;
; 		**THCS - Streamer central colatitude (DEGREES)
;				NO LONGER POLAR ANGLE BECAUSE NOW WE HAVE PHCS
;				Also defined for cavity in EGGS program as intersection
;				of ellipse axis closest to radial (not necessarily semi-major axis)
;                               with limb.
;                               Can also be thought of as drawing a line of symmetry down from
;                               the cavity/streamer center top to the photosphere.
;				Note-- this IS distinct from thetao in forward codes.
;				Changing it effectively shifts the structure but
;				keeps the m slope the same (thus, an equatorial
;				streamer belt becomes an equatorially parallel
;				higher-latitude streamer belt).  Changing
;				thetao introduces the interesting possibility of having
;				a streamer aligned along a great circle other than the equator.
;				DEFAULT 90 DEGREES (equator)
;		**PHCS -- Streamer central longitude (DEGREES)
;				DEFAULT 0 DEGREES (Central Meridian)
;
; 		**MANG - the angle the streamer neutral line makes to equator  (DEGREES)
;				positive means a neutral line that moves northward
;				with increasing longitude
;				(for parallel to equator set to zero)
;		 		DEFAULT 0 DEGREES
;
; 		*PHOTOLENGTH - streamer gaussian half length X 2 at photosphere (DEGREES)
;				DEFAULT 100 DEGREES
;
; 		*PHOTOWIDTH - streamer gaussian half width X 2 at photosphere (DEGREES)
;				DEFAULT 40 DEGREES
;
; 		*CSWIDTH  - streamer gaussian half width X 2 at streamer top (DEGREES)
;				DEFAULT 3 DEGREES
;
; 		*CSHEIGHT - streamer current sheet height (RSUN)
;				DEFAULT 2.5 RSUN
;
; 		*ALPHA - streamer non-radiality (DEGREES)
;				DEFAULT 0 DEGREES
;				If set to 999, will take whatever value cavity GAMMA has
;
;		NOFMAX -- this turns off the "sinking mountain" effect done to avoid discontinuities at the
;				poles. Good for testing sensitivity to this effect (which arises due to long 
;				streamers with big mang. DEFAULT 1 (so discontinuities possible)
;
;
;	MORPHOLOGY CAVITY
;
;               **CAVTOP_R,TH - 
;                               in EGGS program, this is the height (in solar radii) 
;                               and POLAR ANGLE (in degrees) of the point
;                               at the top of the ellipse axis closest to radial (not necessarily semi-major)
;                               DEFINED AT CENTER OF STREAMER GAUSSIAN
;                               DEFAULT R=1.2 RSUN
;                               DEFAULT TH=THCS
;
; 		**CAVLENGTH  -  TOTAL length of cavity (DEGREES)
;				MUST be smaller than PHOTOLENGTH
;				DEFAULT 50 DEGREES
;				
;               **CAVWIDTH  -   width of cavity (RSUN)
;                               in EGGS program, this is the width (in solar radii)
;                               of the ellipse axis closest to normal to radial (not necessarily semi-minor)
;                               DEFINED AT CENTER OF STREAMER GAUSSIAN
;                               DEFAULT 0.25 RSUN
;
;               **CAVHEIGHT  -   height of cavity (RSUN)
;                               in EGGS program, this is the height (in solar radii)
;                               of the ellipse axis closest to radial (not necessarily semi-major)          
;                               DEFINED AT CENTER OF STREAMER GAUSSIAN
;                               DEFAULT 0.25 RSUN
;
; 		DEPBASE, DEPSLOPE1, DEPSLOPE2, RDEPMID - 
;				density depletion radial profile for cavity
;				DEPBASE is depletion at photosphere 
;					DEFAULT 0.5
;				DEPSLOPE1 is slope of depletion to RDEPMID point (can be positive or negative)
;					DEFAULT 0.0
;				DEPSLOPE1 is slope of depletion above RDEPMID (can be positive or negative)
;					DEFAULT 0.0
;				RDEPMID is point of inflection for profile 
;					DEFAULT 1.0
;				NOTE if abs(DEPBASE) set > 1, CAVSCALES are used to set cavity radial profile
;
;		TUNNEL - if set assume cavity is a long tunnel with constant height and width.
;		TUNSLOPE - slope for the Tunnel height, so that it is linear in phi 
;					rather than constant.
;
;	MORPHOLOGY NOUGAT
;
;		**NOUGAT - keyword indicating a nougat will be embedded
;				DEFAULT 0
;
;		**NOUGMULT - multiply by cavity density to get nougat density
;				DEFAULT 1
;				If set to less than zero, nougat density defined by NOUGSCALES
;
; 		**NOUGTHCS,NOUGPHCS - Nougat central limb-intersecting colatitude, longitude (DEGREES)
;				NOUGTHCS defined in EGGS program as intersection
;				of ellipse axis closest to radial (not necessarily semi-major axis)
;                               with limb.
;				DEFAULT THCS,0
;
;               **NOUGTOP_R,TH - 
;                               in EGGS program, this is the height (in solar radii) 
;                               and POLAR ANGLE (in degrees) of the point
;                               at the top of the ellipse axis closest to radial (not necessarily semi-major)
;                               DEFINED AT CENTER OF STREAMER GAUSSIAN
;                               DEFAULT R=0.75*CAVTOP_R 
;                               DEFAULT TH=CAVTOP_TH
;
;		**NOUG_MANG -- Nougat slope
;				DEFAULT MANG
;				
; 		**NOUGLENGTH  -  TOTAL length of nougat (DEGREES)
;				CANT BE BIGGER than CAVLENGTH
;				DEFAULT cavlength/2
;
;               **NOUGWIDTH  -   width of nougat (RSUN)
;                               in EGGS program, this is the width (in solar radii)
;                               of the ellipse axis closest to normal to radial (not necessarily semi-minor)
;                               DEFINED AT CENTER OF STREAMER GAUSSIAN
;                               DEFAULT half cavwidth
;				MUST be smaller than CAVWIDTH
;
;               **NOUGHEIGHT  -   height of nougat (RSUN)
;                               in EGGS program, this is the height (in solar radii)
;                               of the ellipse axis closest to radial (not necessarily semi-major)          
;                               DEFINED AT CENTER OF STREAMER GAUSSIAN
;                               DEFAULT half cavheight
;				MUST be smaller than CAVHEIGHT
;
;	 	**ZITIPRAM -- makes nougat hollow, value of ZITIPRAM
;			is fraction of radius of hollow tube wall
;			DEFAULT 0
;
;	TEMPERATURE
;
; 		TCAV - Cavity temperature, DEFAULT 1.6d6
;			NOTE if TCAV=0 or TRIM=0 or TNOUG=0, 
;			use hydrostatic scale-height (based on density radial falloff)
;			NOTE this requires USEMOD=POWER-- if not set, defaults will take over
;
; 		TRIM - Rim temperature, DEFAULT 1.6d6
;			NOTE if TCAV=0 or TRIM=0 or TNOUG=0, 
;			use hydrostatic scale-height (based on density radial falloff)
;			NOTE this requires USEMOD=POWER-- if not set, defaults will take over
;
; 		TNOUG - Nougat temperature, DEFAULT 1.6d6
;			NOTE if TCAV=0 or TRIM=0 or TNOUG=0, 
;			use hydrostatic scale-height (based on density radial falloff)
;			NOTE this requires USEMOD=POWER-- if not set, defaults will take over
;
; 		TRGAM - Rim gamma, DEFAULT 1. (isothermal)
;
; 		TCGAM - Cavity gamma, DEFAULT 1. (isothermal) 
;
; 		TNGAM - Nougat gamma, DEFAULT 1. (isothermal) 
;
;		TCPar - Cavity Temperature Parameter Array  (listed at end of parameters in widget)
;			if non-zero, will allow freely varying temperature function or 1-8 elements
;
;		TRPar - Rim Temperature Parameter Array  (listed at end of parameters in widget)
;			if non-zero, will allow freely varying temperature function or 1-8 elements
;
;		TMod - tempeature Model. Default a Power law.
;			choices POLY POWER POW1 (see CAVMORPH.PRO)
;			only used if TCPar or TRPar used
;
;	DENSITY
; 		DENSSCAL - scaling factor times radial profile of streamer and c. hole density
;				default SPARTAN and WSM values, for varying model for use 
;				in different parts of solar cycle
;				DEFAULT 1.
;
;		USEMOD - coronal hole/streamer radial density model to use
;				EXPONENT - based on Gibson et al., 2003) model, but with streamer
;				 profile determined independent of coronal hole profile
;				EXPDEL - as in multistreamer Gibson et al., 2003) model, SPARTAN fit default
;			         here the streamer coefficients refer to a delta above the coronal hole fit	
;				or POWER (Guhathakurta et al., 1999; Gibson et al., 1999 - WSM fits)
;				DEFAULT POWER
;
;		STREAMSCALES,HOLESCALES,CAVSCALES,NOUGSCALES --  (listed at end of parameters in widget)
;			these are 6-D arrays of radial profile parameters streamer and hole
;			used either in EXPONENT or POWER function-- if not set, default to 
;			SPARTAN and WSM value respectively
;			IF POWER-- will be multipled by 1d5 (coronal hole), and 1d8 (streamer)
;			CAVSCALES used only if abs(DEPBASE) (see below) is set to greater than one
;				DEFAULT then is scaled to DEPBASE (and this will be forced if DEPBASE is positive)
;			NOUGSCALES used only if NOUGMULT (see below) is set to less than zero
;				DEFAULT then is CAVSCALES
;
;	Filling Factor
;
;		FFPAR - parameters for filling factor. If not present FF=1
;			If present, Population 1 filling factor set to it
;			If FF=1, and Pop2FillFact set via CFF_*SCALES below, set to 1-Pop2FillFact
;			Can also be set (line command only) to 2 or 3 element array, see CAVMORPH.PRO
;
;      Second Population: Chromospheric - forces Pop2TRegime=2 at top level
;		can use the following four parameters, requires CDENS set:
;
;               CDENS -  electron density for cool (chromospheric temp) material (Lyman absorbing). Default=0
;                       For cool particles (neutral H, neutral and +1 He) used for Lyman continuum absorption calculation
;
;               CFF_STREAMSCALES - filling factor for cool material (Pop2) in streamer (listed at end of parameters in widget)
;               CFF_CAVSCALES - filling factor for cool material (Pop2) in cavity (listed at end of parameters in widget)
;               CFF_NOUGSCALES - filling factor of  cool material (Pop2) in nougat (listed at end of parameters in widget)
;
;      Second Population: Coronal - forces Pop2TRegime=1 at top level
;		can use the following three parameters (requires at least one set):
;
;		POP2HOLE
;			if set, coronal hole is treated as population 2, allowing different abundance setting
;				default,0
;		POP2CAV
;			if set, cavity is treated as population 2, allowing different abundance setting
;				default,0
;		POP2NOUG
;			if set, nougat is treated as population 2, allowing different abundance setting
;				default,0
;
;
;       BOOKKEEPING
;
;               SAVEPRAMS - if keyword set to a string, write parameters to filename
;                       (which is the keyword value saveprams)
;			or if set to 1, then replace with pramarray
;			
;
;               READPRAMS - if keyword set, read in parameters from filename
;                       (which is the keyword value filename)
;			or if a structure than read directly
;			NOTE, flagged keywords will overwrite
;

; ModPramsStruct Outputs:
;
;
;               NAME -- cavmorph-- so that procedure can be called in intensint
;               LABEL -- CavMorph -- for plot label
;
;		TCAV, TRIM, TNOUG, TRGAM, TCGAM, TNGAM, USEMOD, CSHEIGHT
; 			DEPBASE, DEPSLOPE1, DEPSLOPE2, CAVWIDTH, NOUGWIDTH,
;			CAVHEIGHT,NOUGHEIGHT, RDEPMID
;			NOUGAT, NOUGMULT
;			as inputted or default
;
;               NOUGBOT  --
;                       defined as radial height at bottom point
;                       in coord system tilted radially by GAMMANOUG
;
;               CAVBOT  --
;                       defined as radial height at bottom point
;                       in coord system tilted radially by GAMMA
;
;		GAMMA  --
;			nonradial tilt of cavity
;
;		GAMMANOUG  --
;			nonradial tilt of nougat
;
;
;		S0-S5; H0-H5, C0-C5, N0-N5 -- background radial profiles
; 			for streamer (S) and hole (H)
;			used for EXPDEL, EXPONENT or POWER profiles
;			note that POWER assumes radial power law and 
;			EXPONENT/EXPDEL a combination exponential/radial power law
;			this will be dealt with in CAVMORPH.PRO
;			DENSSCAL simply multiplies default profiles  (SPARTAN/WSM)
;			C0-C5 only used if ABS(DEPBASE) > 1, if not set, defaults
;			to DEPBASE*S0-S5 (and this is forced if DEPBASE positive)
;			N0-N5 only used if NOUGMULT < 0, if not set, defaults
;			to C0-C5
;			if streamscales, holescales, cavscales,nougscales set by user, these are passed through
;			if keyword NEWKIRK set, coronal hole profile repalced by Newkirk 1961 profile
;			helpful for radio forward modeling
;			
;
;		THCS, NOUGTHCS, NOUGPHCS, ALPHA -- converted to RADIANS
;
;		AWIDTH, BWIDTH-- streamer Gaussian half width variation with height
;			determined from CSWIDTH, PHOTOWIDTH, CSHEIGHT
;
;		M, NOUG_M - slope of of latitude vs. longitude on a Carrington map
;			determined from tan(MANG*!dtor)
;
; 		WMULT  - multiple of PHOTOWIDTH that determines streamer length 
;			at photosphere
;			determined from PHOTOWIDTH, PHOTOLENGTH
;
; 		FRACLENGTH - fractional length of the cavity relative to streamer
;			determined from PHOTOLENGTH and CAVLENGTH
;
; 		NFRACLENGTH - fractional length of the nougat relative to streamer
;			determined from PHOTOLENGTH and NOUGLENGTH
;
;

;
; Written: S. Gibson
; Modifications: 
;	2-Jan-2010 Removed and added common blocks, added keyword inputs 
;			for remaining variables  TAK
;	9-Feb-2010 Adapted for FORWARD tree structure, SEG
;	22-Apr-2010	CAVWIDTH input changed to solar radii, not degrees polar angle, SEG
;	23-Apr-2010	Modifications to ellipse calculation to be consistent with EGGS, SEG
;		 	Added checks for floating point, TAK
;	20-May-2010 Changed THCS to be COLAT not POLAR ANGLE;  Introduced PHCS, SEG
;			Changed to POWER, EXPONENT, EXPDEL profiles, with WSM and SPARTAN as defaults for these
;	1-Aug-2010	Added NOFMAX, SEG
;	10-09-10   Added NOUGAT SEG
;	5-Nov-2010 If inarray has no nougat parameters then add them, TAK
;	12-Dec-2010	 Added ZITI, SEG
;	12-Dec-2010	 Added NOUGTHCS, MANG, made corrections to to add tags, 12-12-10 SEG
;		 	Added check for structure in readprams
;	28-Feb-2011	Adding Parameter arrays for Cavity and Rim Temperatures (TCPar, TRPar)  TAK
;	25-Apr-2011 Added Tunnel keyword. SEG & TAK
;	9-May-2011 Added TunSlope keyword
;	23-Sep-2011 Adjusted default for TMOD, TAK
;	27-Oct-2011 Added FFPar, TAK
; 	changed to procedure and adjusted readprams/saveprams I/O Jan 2013 SEG
;	changed floats to doubles Jan 2013 SEG
;	made newkirk apply to streamer as well as hole Mar 2013 SEG
;	October 2013 - fixed bug where it was saving information before
;		changes were made to stream/hole/cav/nougscales so input
;		parameters would altered
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;
; Version 2.0 July 2014
;
;  2-Jan-2015 Added Lyman continuum absorption related parameters,
;       CDens, CFF_STREAMSCALES, CFF_CAVSCALES, CFF_NOUGSCALES. TAK
;  2-Feb-2016 Updated Population 2, and added possibility of coronal pop2
;  1-Jun-2019 Removed usewindows, used slash instead for PC compatibility
;-

mdtor=!dpi/180d0


if n_elements(thcs0) ne 0 then thcs=thcs0
if n_elements(nougthcs0) ne 0 then nougthcs=nougthcs0
if n_elements(nougphcs0) ne 0 then nougphcs=nougphcs0
if n_elements(phcs0) ne 0 then phcs=phcs0
if n_elements(alpha0) ne 0 then alpha=alpha0
default,FFpar,1.d0

if keyword_set(readprams) then begin
; read parameter file (a structure file)

   case datatype(readprams) of
     'STR': restgen,inarray,file=readprams
     'STC': inarray=readprams
     else: message, 'must provide a named readprams file or a structure'
   endcase
   
;If the inarray is missing certain parameters, add them 
	tags=tag_names(inarray)
	if not tag_exist(inarray,'Nougat')  then inarray=add_tag(inarray,0,'Nougat')
	if not tag_exist(inarray,'TNoug')  then inarray=add_tag(inarray,inarray.TCav,'TNoug')
	if not tag_exist(inarray,'NougMult')  then inarray=add_tag(inarray,0.,'NougMult')
	if not tag_exist(inarray,'NougScales')  then inarray=add_tag(inarray,inarray.CavScales,'NougScales')
	if not tag_exist(inarray,'TNGam')  then inarray=add_tag(inarray,inarray.TCGam,'TNGam')
	if not tag_exist(inarray,'NougThCS')  then inarray=add_tag(inarray,inarray.ThCs,'NougThCS')
	if not tag_exist(inarray,'NougPhCS')  then inarray=add_tag(inarray,inarray.PhCs,'NougPhCS')
	if not tag_exist(inarray,'Noug_Mang')  then inarray=add_tag(inarray,inarray.Mang,'Noug_Mang')
	if not tag_exist(inarray,'NougTop_R')  then inarray=add_tag(inarray,inarray.CavTop_R,'NougTop_R')
	if not tag_exist(inarray,'NougTop_Th')  then inarray=add_tag(inarray,inarray.CavTop_Th,'NougTop_Th')
	if not tag_exist(inarray,'NougHeight')  then inarray=add_tag(inarray,inarray.CavHeight,'NougHeight')
	if not tag_exist(inarray,'NougWidth')  then inarray=add_tag(inarray,inarray.CavWidth,'NougWidth')
	if not tag_exist(inarray,'NougLength')  then inarray=add_tag(inarray,inarray.CavLength,'NougLength')
	if not tag_exist(inarray,'ZitiPram')  then inarray=add_tag(inarray,0,'ZitiPram')
	if not tag_exist(inarray,'TCPar')  then inarray=add_tag(inarray,dblarr(6),'TCPar')
	if not tag_exist(inarray,'TRPar')  then inarray=add_tag(inarray,dblarr(6),'TRPar')
	if not tag_exist(inarray,'TMod')  then inarray=add_tag(inarray,'','TMod')
	if not tag_exist(inarray,'tunnel')  then inarray=add_tag(inarray,0,'tunnel')
	if not tag_exist(inarray,'TunSlope')  then inarray=add_tag(inarray,0.,'tunslope')
	if not tag_exist(inarray,'FFPar')  then inarray=add_tag(inarray,FFpar,'FFPar')
        if not tag_exist(inarray,'CDens')  then inarray=add_tag(inarray,0.,'CDens')
        if not tag_exist(inarray,'Pop2Hole')  then inarray=add_tag(inarray,0,'Pop2Hole')
        if not tag_exist(inarray,'Pop2Cav')  then inarray=add_tag(inarray,0,'Pop2Cav')
        if not tag_exist(inarray,'Pop2Noug')  then inarray=add_tag(inarray,0,'Pop2Noug')
        if not tag_exist(inarray,'CFF_STREAMSCALES')  then inarray=add_tag(inarray,dblarr(6),'CFF_STREAMSCALES')
        if not tag_exist(inarray,'CFF_CAVSCALES')  then inarray=add_tag(inarray,dblarr(6),'CFF_CAVSCALES')
        if not tag_exist(inarray,'CFF_NOUGSCALES')  then inarray=add_tag(inarray,dblarr(6),'CFF_NOUGSCALES')

;
; default to inarray values
;
   default,usemod,inarray.UseMod
   default,tcav,inarray.TCav
   default,trim,inarray.Trim
   default,tnoug,inarray.TNoug
   default,depbase,inarray.DepBase
   default,densscal,inarray.DensCal
   default,alpha,inarray.Alpha

   default,trgam,inarray.TRGam
   default,tcgam,inarray.TCGam
   default,tngam,inarray.TNGam
   default,TCpar,inarray.TCPar
   default,TRpar,inarray.TRPar
   default,TMod,inarray.TMod
  
   default,thcs,inarray.ThCs
   default,nougat,inarray.Nougat
   default,nougmult,inarray.NougMult
   default,zitipram,inarray.ZitiPram
   default,tunnel,inarray.Tunnel
   default,tunslope,inarray.TunSlope
   default,nougthcs,inarray.NougThCs
   default,phcs,inarray.PhCs
   default,nougphcs,inarray.NougPhCs
   default,csheight,inarray.CsHeight
   default,photowidth,inarray.PhotoWidth
   default,cswidth,inarray.CsWidth
   default,nofmax,inarray.NoFMax
   default,mang,inarray.Mang
   default,noug_mang,inarray.Noug_Mang
   default,photolength,inarray.PhotoLength

   default,depslope1,inarray.DepSlope1
   default,depslope2,inarray.DepSlope2
   default,rdepmid,inarray.RDepMid
   default,cavtop_r,inarray.CavTop_R
   default,cavtop_th,inarray.CavTop_Th
   default,cavheight,inarray.CavHeight
   default,cavwidth,inarray.CavWidth
   default,cavlength,inarray.CavLength
   default,nougtop_r,inarray.NougTop_R
   default,nougtop_th,inarray.NougTop_Th
   default,nougheight,inarray.NougHeight
   default,nougwidth,inarray.NougWidth
   default,nouglength,inarray.NougLength

   default,streamscales,inarray.StreamScales
   default,holescales,inarray.HoleScales
   default,cavscales,inarray.CavScales
   default,nougscales,inarray.NougScales
   default,FFpar,inarray.FFpar
   default,CDens,inarray.CDens
   default,Pop2Hole,inarray.Pop2Hole
   default,Pop2Cav,inarray.Pop2Cav
   default,Pop2Noug,inarray.Pop2Noug
   default,CFF_Streamscales,inarray.CFF_Streamscales
   default,CFF_Cavscales,inarray.CFF_Cavscales
   default,CFF_Nougscales,inarray.CFF_Nougscales

endif else begin

  default,usemod,'POWER'
  default,tcav,1.6d6
  default,trim,1.6d6
  default,tnoug,1.6d6
  default,depbase,0.5d0
  default,densscal,1.d0
  default,alpha,0.d0

  default,trgam,1.d0
  default,tcgam,1.d0
  default,tngam,1.d0
  default,TCPar,dblarr(6)
  default,TRPar,dblarr(6)
  default,TMod,'POWER'

  default,thcs,90.d0
  default,nougat,0
  default,nougmult,1.d0
  default,zitipram,0
  default,tunnel,0
  default,tunslope,0.d0
  default,nougthcs,thcs
  default,phcs,0.d0
  default,nougphcs,phcs
  default,csheight,2.5d0
  default,photowidth,40.d0
  default,cswidth,3.d0
  default,nofmax,1.
  default,mang,0.d0
  default,noug_mang,mang
  default,photolength,100.d0

  default,depslope1,0.0d0
  default,depslope2,0.0d0
  default,rdepmid,1.d0 
  default,cavtop_r,1.20d0
  default,cavtop_th,thcs
  default,cavheight,25.d-2
  default,cavwidth,25.d-2
  default,cavlength,50.d0
  default,nougtop_r,1.d0+(cavtop_r-1.d0)*0.75d0
  default,nougtop_th,cavtop_th
  default,nougheight,0.5d0*cavheight
  default,nougwidth,0.5d0*cavwidth
  default,nouglength,0.5d0*cavlength

   default,CDens,0.
   default,Pop2Hole,0
   default,Pop2Cav,0
   default,Pop2Noug,0
   default,CFF_StreamScales,dblarr(6)
   default,CFF_Cavscales,dblarr(6)
   default,CFF_Nougscales,dblarr(6)
endelse

if strupcase(usemod) ne 'POWER' and strupcase(usemod) ne 'EXPONENT' and strupcase(usemod) ne 'EXPDEL' then begin
	print,'wrong model, assuming EXPONENT'
	usemod = 'EXPONENT'
endif
	
flag=0
if trim eq 0. or tcav eq 0. or tnoug eq 0. then begin
	if strupcase(usemod) ne 'POWER' then begin
 	  print,'Hydrostatic case: requires POWER law profile'
	  usemod='POWER'
	  flag=1
	endif
        if alpha ne 0 then begin
    	  print,'Oops! no pivot allowed for hydrostatic!!'
     	  alpha = 0
        endif
endif

;
; DEFAULT WSM VALUES
;

if strupcase(usemod) eq 'POWER' then begin
   h0=densscal*1736.9d0
   h1=13.72d0
   h2=densscal*19.95d0
   h3=4.09d0
   h4=densscal*1.316d0
   h5=2.d0
   s0=densscal*3.6d0
   s1=15.3d0
   s2=densscal*0.99d0
   s3=7.34d0
   s4=densscal*0.365d0
   s5=4.31d0
endif

;
; DEFAULT SPARTAN VALUES
;

if strupcase(usemod) eq 'EXPDEL' then begin
   h0=densscal*726898.51d0
   h1=-3.7715119d0
   h2=13.196889d0
   h3=0.86212339d0
   h4=-5.5238750d0
   h5=4.0225836
   s0=densscal*244985.87d0
   s1=4.4445688d0
   s2=0.58698320d0
   s3=5.5598960d0
   s4=21.305183d0
   s5=-3.1475979d0
endif

;
; DEFAULT SPARTAN VALUES (MODIFIED FOR NON DELTA)
;  also note the coronal hole values have been modified
;

if strupcase(usemod) eq 'EXPONENT' then begin
   h0=densscal*726898.51d0
   h1=-3.7715119d0
   h2=13.196889d0
   h3=0.86212339d0
   h4=-5.5238750d0
   h5=4.0225836
   s0=densscal*1000000.d0
   s1=4.4445688d0
   s2=0.58698320d0
   s3=5.5598960d0
   s4=21.305183d0
   s5=-3.1475979d0
endif

; 
; Cavscale defaults
;

cuse=dblarr(6)
if depbase ne 0. then dpm=depbase
if depbase eq 0. then dpm=1.
if strupcase(usemod) eq 'POWER' then cuse=[dpm*s0,s1,dpm*s2,s3,dpm*s4,s5]
if strupcase(usemod) eq 'EXPONENT' then cuse=[dpm*s0,s1,s2,s3,s4,s5]

; note there is no easy way to set a default for 'EXPDEL' -- this is just a kluge
;
if strupcase(usemod) eq 'EXPDEL' then cuse=[dpm*(s0+h0),s1,s2,s3,s4,s5]

default,streamscales,[s0,s1,s2,s3,s4,s5]
default,holescales,[h0,h1,h2,h3,h4,h5]
if depbase lt 0 then default,cavscales,cuse else cavscales=cuse
default,nougscales,cavscales

if flag eq 1 then begin
;
; If user asked for hydrostatic but requested USEMOD=EXPONENT, assume any inputted
; SCALES are wrong, and overwrite with default
;
	print,'Overwriting cavscales,nougscales,holescales,streamscales with POWER defaults'
	streamscales=[s0,s1,s2,s3,s4,s5]
	holescales=[h0,h1,h2,h3,h4,h5]
	cavscales=cuse
	nougscales=cuse
	densscal=1
endif       

if trim eq 0. or tcav eq 0. or tnoug eq 0. and abs(depbase) le 1 then begin
    print,'Oops! have to use parameters of cavity/nougat scaling for hydrostatic!! Defaults to constant cavity/nougat depletion =',depbase
    dpm=depbase
    cavscales=[dpm*s0,s1,dpm*s2,s3,dpm*s4,s5]
    nougscales=cavscales
endif

;
; for the purpose of diagnostics and easy access, we put these input parameters into a structure
;

pramarray={$
	Name:'cavmorph',ThCs:thcs,PhCs:phcs,$
        Mang:mang,PhotoLength:photolength,PhotoWidth:photowidth,$
	CsWidth:cswidth,CsHeight:csheight,Alpha:alpha,NoFMax:nofmax,$
	CavTop_R:cavtop_r,CavTop_Th:cavtop_th,$
	CavLength:cavlength,CavWidth:cavwidth,CavHeight:cavheight,$
    	DepBase:depbase,DepSlope1:depslope1,DepSlope2:depslope2,$
    	RDepMid:rdepmid,$
        Tunnel:tunnel,TunSlope:TunSlope,$
	Nougat:nougat,$
        NougMult:nougmult,$
	NougThCs:nougthcs,NougPhCs:nougphcs,$
	NougTop_R:nougtop_r,NougTop_Th:nougtop_th,$
	Noug_Mang:noug_mang,NougLength:nouglength,NougWidth:nougwidth,$
	NougHeight:nougheight,$
        ZitiPram:zitipram,$
	TCav:tcav,TRim:trim,TNoug:tnoug,TRGam:trgam,TCGam:tcgam,TNGam:tngam,$
        TMod:TMod,DensCal:1.0d0,UseMod:usemod,$
        FFPar:FFpar,CDens:CDens,Pop2Hole:pop2hole,Pop2Cav:pop2cav,Pop2Noug:pop2noug,$
	StreamScales:streamscales,HoleScales:holescales,$
	CavScales:cavscales,NougScales:nougscales,$
        CFF_Streamscales:CFF_Streamscales,$
        CFF_Cavscales:CFF_Cavscales,CFF_Nougscales:CFF_Nougscales,$
	TCPar:TCPar,TRPar:TRPar}

; if requested, save input parameters to a file (stored in file named saveprams if it is a string)

   if keyword_set(saveprams) then begin

    savefilename=saveprams
    if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then begin
     slash=path_sep()
     if working_dir ne '' then savefilename=working_dir+slash+saveprams
    endif

     case 1 of
	 datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
	 else: saveprams=pramarray
     endcase
   endif

; 
; now calculate output structure values (the ones the model uses)
;

if n_elements(streamscales) ne 0 then begin
  streamscales[0]=densscal*streamscales[0]
  if strupcase(usemod) eq 'POWER' then begin
    streamscales[2]=densscal*streamscales[2]
    streamscales[4]=densscal*streamscales[4]
  endif
endif
    
if n_elements(holescales) ne 0 then begin
  holescales[0]=densscal*holescales[0]
  if strupcase(usemod) eq 'POWER' then begin
    holescales[2]=densscal*holescales[2]
    holescales[4]=densscal*holescales[4]
  endif
endif

if n_elements(cavscales) ne 0 then begin
  cavscales[0]=densscal*cavscales[0]
  if strupcase(usemod) eq 'POWER' then begin
    cavscales[2]=densscal*cavscales[2]
    cavscales[4]=densscal*cavscales[4]
  endif
endif

if n_elements(nougscales) ne 0 then begin
  nougscales[0]=densscal*nougscales[0]
  if strupcase(usemod) eq 'POWER' then begin
    nougscales[2]=densscal*nougscales[2]
    nougscales[4]=densscal*nougscales[4]
  endif
endif

;
; determine streamer and coronal hole radial density profiles
;

   s0=streamscales[0]
   s1=streamscales[1]
   s2=streamscales[2]
   s3=streamscales[3]
   s4=streamscales[4]
   s5=streamscales[5]
   h0=holescales[0]
   h1=holescales[1]
   h2=holescales[2]
   h3=holescales[3]
   h4=holescales[4]
   h5=holescales[5]
   c0=cavscales[0]
   c1=cavscales[1]
   c2=cavscales[2]
   c3=cavscales[3]
   c4=cavscales[4]
   c5=cavscales[5]
   n0=nougscales[0]
   n1=nougscales[1]
   n2=nougscales[2]
   n3=nougscales[3]
   n4=nougscales[4]
   n5=nougscales[5]

; if keyword Newkirk set, replace with Newkirk 1961 profile

  if keyword_set(newkirk) then begin
    h0=4.2d4
    h1=4.32d0
    s0=4.2d4
    s1=4.32d0
    newkirk=1
  endif else begin
    newkirk=0
  endelse

; convert angles to radians

  nougthcs=nougthcs*mdtor
  nougphcs=nougphcs*mdtor
  thcs=thcs*mdtor
  phcs=phcs*mdtor

; convert angle to slope

  m = tan(mang*mdtor)
  noug_m = tan(noug_mang*mdtor)

; now we need to use streamer width at photosphere
; (which is for now assumed to not be directly based on
; observation, and so not need deprojection from m)
; to determine half-width variation with r
; awidth+bwidth*r
; NOTE: if photowidth,cswidth are more standard normal variance
; Gaussian half-width, it needs an addition sqrt(2) factor
;
  w=sqrt(2.d0)*photowidth*mdtor/2.d0
  csw=sqrt(2.d0)*cswidth*mdtor/2.d0
  bwidth=(csw-w)/(csheight-1.d0)
  awidth=w-bwidth

; We now determine the multipler that relates 
; 1/e-length to  1/e-width 
; or equivalently 1/2 length to 1/2 width

  wmult=photolength/photowidth

;
; calculate nonradial angle gamma for cavity
;
 
  xtop=cavtop_r*sin(cavtop_th*mdtor)
  ytop=cavtop_r*cos(cavtop_th*mdtor)
  xcs=sin(thcs)
  ycs=cos(thcs)
  dist=sqrt((xtop-xcs)^2+(ytop-ycs)^2)
  gamma=0.
  if dist ne 0.d0 then begin
	gamma=asin(cavtop_r*sin(cavtop_th*mdtor-thcs)/dist)
	hyp=sqrt(1.+dist*dist)
        if cavtop_r lt hyp then gamma = !dpi-gamma
  endif

  if alpha eq 999. then alpha = gamma $
  else alpha=alpha*mdtor

;  print,hyp,cavtop_r,dist,gamma/mdtor
 
  xtopn=nougtop_r*sin(nougtop_th*mdtor)
  ytopn=nougtop_r*cos(nougtop_th*mdtor)
  xcsn=sin(nougthcs)
  ycsn=cos(nougthcs)
  distn=sqrt((xtopn-xcsn)^2+(ytopn-ycsn)^2)
  gammanoug=0.
  if distn ne 0. then gammanoug=asin(nougtop_r*sin(nougtop_th*mdtor-nougthcs)/distn)

; calculate cavity bottom radius in tilted gamma coords
;
  cavbot=1.d0+dist-cavheight

; calculate nougat bottom radius in tilted gamma coords
;
  nougbot=1.d0+distn-nougheight

;
; calculate the fraction of streamer length that
; should be occupied by cavity, based on number of cavity days vs. streamer days
;

  fraclength=double(cavlength)/double(photolength)

;
; calculate the fraction of streamer length that
; should be occupied by nougat, based on number of nougat days vs. nougat days
;

  nfraclength=double(nouglength)/double(photolength)

  outarray={Name:'cavmorph',Label:'CavMorph',TCav:double(tcav),TRim:double(trim),TRGam:double(trgam),TCGam:double(tcgam),$
	TCPar:double(TCPar),TRPar:double(TRPar),TMod:TMod,TNoug:double(tnoug),TNGam:double(tngam),$
	Nougat:nougat,ZitiPram:zitipram,Tunnel:tunnel,TunSlope:double(TunSlope),NougMult:double(nougmult),$
	UseMod:usemod,DensScal:double(densscal),CsHeight:double(csheight),ThCs:double(thcs),$
	NougThCs:double(nougthcs),PhCs:double(phcs),NougPhCs:double(nougphcs),$
	Alpha:double(alpha),M:double(m),Noug_M:double(noug_m),Awidth:double(awidth),Bwidth:double(bwidth),$
	WMult:double(wmult),DepBase:double(depbase),DepSlope1:double(depslope1),DepSlope2:double(depslope2),RDepMid:double(rdepmid),$
	CavHeight:double(cavheight),CavBot:double(cavbot),CavWidth:double(cavwidth),$
	Gamma:double(gamma),$
 	NougHeight:double(nougheight),NougBot:double(nougbot),NougWidth:double(nougwidth),$
	GammaNoug:double(gammanoug),FracLength:double(fraclength),NFracLength:nfraclength,$
	NoFMax:nofmax,$
	H0:double(h0),H1:double(h1),H2:double(h2),H3:double(h3),H4:double(h4),H5:double(h5),$
	S0:double(s0),S1:double(s1),S2:double(s2),S3:double(s3),S4:double(s4),S5:double(s5),$
	C0:double(c0),C1:double(c1),C2:double(c2),C3:double(c3),C4:double(c4),C5:double(c5),$
	N0:double(n0),N1:double(n1),N2:double(n2),N3:double(n3),N4:double(n4),N5:double(n5),$
	Newkirk:newkirk,FFPar:double(FFpar),MagMod:0,$
        CDens:CDens,Pop2Hole:pop2hole,Pop2Cav:pop2cav,Pop2Noug:pop2noug,CFF_Streamscales:CFF_Streamscales,$
        CFF_Cavscales:CFF_Cavscales,CFF_Nougscales:CFF_Nougscales}



end

;************************************************************************
