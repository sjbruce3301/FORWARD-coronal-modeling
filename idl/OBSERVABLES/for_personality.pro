function for_personality,quantmap,ObsPramsStruct,StokesStruct

;
; Program to calculate photon noise on an input QUANTITY.
; So far only set up for spectropolarimeters like CoMP/DKIST/COSMO
;			and UV spectropolarimeters
; Currently, quantity can be  I, Q, U, V, L (linear polarization magnitude), 
;		(Q,U,V,L)/I, Az, Linewidth and Vlos
;		calculated by FORCOMP.F (program called CLE.F in original Judge distribution)
; 	         input intensities (integrated over lambda) are in units of erg/cm2/s/sr
;
; NOTE -- various assumptions are made. See "WARNING"s below.
;
; References are made throughout to Tomczyk technical note: "TN 1 Technical Errors"
;  http://mlso.hao.ucar.edu/COSMO/Sections/Technical%20Note%20Series/TN%2001%20Measurement%20Errors.pdf
; 
; And to a Roberto Technical note
;  Available at 
; https://drive.google.com/file/d/12c0vs9fHJp5GlgDh8e8tQAL7mV7ZTT_n/
;
; INPUTS: Structure QUANTMAP, includes quantity=quantmap.data 
;	  Structure STOKESSTRUCT, includes StokesStruct.I = intensity
;		(which is identical to quantity if quantity happens to be intensity)
;		this is needed for all quantities, to establish sigma_Int
;	  Structure OBSPRAMSSTRUCT
;		includes tag LINENAME
;		Also tag structure NOISEPRAMS with tags (more description below), including-
; 		 user option variables: 
;		  integration time, resolution (which is really a spatial sampling)
; 		 telescope specific variables: 
;                 aperture, telescope throughput (efficiency), 
;	          scattered light (background)
; 		 instrument specific variables: 
;	          modeffint-- modulation efficiency on Stokes I noise
;		  modeffquant -- modulation efficiency on Quantity noise
;			(e.g.CoMP: for I, modeffint=1; for Q,U, modeffquant=1/sqrt(3))
;			NOTE this varies with line -- so if Quantity = I
;			modeffquant = modeffint
;
; OUTPUT: returned value will be relative error amplitude: noise/signal, unitless
;
; Called by FOR_POS_MAP, FOR_DRIVE
; 
; Calls FOR_SUN_FLUX
;
; Written by Sarah Gibson 2013
; 
; Version 2.0 July 2014
;               06-Feb-2015 - Bug fix dlambda (was set to I, should be 0)
;		06-Sep-2016 - SEG updated to simplify/document program
;				generalize modulation efficiency via modeffint, modeffquant
;				and add non intensity noise calculations
;		12-May-2017 - SEG changed output to be magnitude of relative error, not multiplied by randomizer
;				random number option in plot is now done in FOR_PLOT
;		June 2017 -- fixed bug where Azimuth relative error divided noise in radians units
;				by data in degrees units
;		Sept 2021 -- 	expanded conditional test for STOKESQOI/UOI etc
;		Dec 2021 -- included comments re UV 
;		Jun 2022 -- fixed bug where Azimuth units were passed through to fixunits for I
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Jan 2023 -- changed UV SPECPOL approach to using for_sun_flux
;		simplified approach - no need to go through for_fixunits
; 	Sep 2023 -- commented out StokesP because confusing
;		updated background noise calculation - in particular sigma
;		now is sqrt(I+B) instead of sqrt(I)
;		added clarifying notes and simplified equations
;		also fixed error where Doppler VLOS sigma did not
;		select for real points
;	Oct 2023 -- updated c and h
;       Nov 2023 -- used abs(chromorad) since it can be passed through negative if not adding to disk
; --


; ----
; FIRST set up the variables
;  Note- defaults are described in FOR_NOISEDEFAULTS.PRO

;
; USER VARIABLES
;

; 
; INTEGRATION
;
; This is the total integration time, 
;  the more integration, the higher the signal/noise 
;     units: seconds
; 
; WARNING: we assume NO EVOLUTION of a model over
;  timescale INTEGRATION.  If the user wants to average
;  over a moving structure, it is best to determine the dynamic time scale
;  of the system (largest amount of time with no significant change; 
;  e.g., resolution/model velocity),
;  to run FORWARD setting INTEGRATION equal to that dynamic time scale,
;  and then do any further averaging/beating down of noise for longer integration
;  times as part of an external code.

integration = ObsPramsStruct.NoisePrams.Integration

; RESOLUTION: spatial size of image grid element
;	This is user set, not telescope dependent. If one wants
;	to test the telescope resolution, RESOLUTION can be chosen
;	as appropriate for a telescope pixel size (see Note)
;    units: arcsec
;
; Note: RESOLUTION is set by choices of 
; NGRID, NGY and XXMIN, XXMAX, YYMIN, YYMAX 
; which are model parameters that live in 
; Structure GRIDPRAMSSTRUCT. 
; Program FOR_NOISEDEFAULTS.PRO calculates the pixel size 
; from these, converts to arcsec, and stores this in tag RESOLUTION
 
resolution = ObsPramsStruct.NoisePrams.Resolution

;
; TELESCOPE-SPECIFIC VARIABLES
;

; APERTURE
; units centimeters

aperture = ObsPramsStruct.NoisePrams.Aperture

; EFFICIENCY (system efficiency, telescope throughput)
;   unitless (percentage, expressed as fraction < 1)
;
; WARNING -- this is likely to be a function of wavelength
; but for now there is one number per telescope
;	

efficiency = ObsPramsStruct.NoisePrams.Efficiency

; BACKGROUND noise
; units of millionths of Bsun_center
; (PPM ; need it in photons - will calculate below)

B_ppm = ObsPramsStruct.NoisePrams.Background

;
; INSTRUMENT-SPECIFIC VARIABLES
;

;
; MODEFFINT, MODEFFQUANT - efficiency associated with Modulation Cycle for Stokes I, Quantity
;	See Tomczyk technical note, pages 5 and 6
;	noise is divided by this
;       e.g. for CoMP, original modulation cycle is
;       (Io+Qo, Io-Qo, Io+Uo, Io-Uo, Io+Vo, Io-Vo)
;       MODEFFINT = 1, MODEFFQUANT is 1/sqrt(3)
;       but even for CoMP, one might want to choose 
;		MODEFFQUANT=MODEFFINT=1 e.g. for [Io+Vo,Io-Vo]); 
;		or MODEFFINT=1, MODEFFQUANT=1/sqrt(2) for (Io+Qo, Io-Qo, Io+Uo, Io-Uo)
;   	unitless 
;
; (not used in these code) N_MOD -- how many states used in a polarization scheme
;	e.g., for first original modulation cycle above, N_MOD = 6
;	for second N_MOD =2
;	for third N_MOD =4

modeffint = ObsPramsStruct.NoisePrams.ModEffInt
modeffquant = ObsPramsStruct.NoisePrams.ModEffQuant

;
; WARNING duty cycle for instrument observing is not explicitly set
;  it can be applied as an external factor, or incorporated within telescope efficiency
;

; ----
; DONE setting up variables
; ----

;---
; Now translate everything into units of photons
; using Haosheng's code FOR_SUN_FLUX which gives us Bsun_center in photons
; collected under telescope conditions

;
; need wavelength range
; lambda is central wavelength
;  units Angstrom
;

;lmbda=StokesStruct.CentWave
; temporary kluge to fix old version of UV code which passed a single number through for CentWave
;       however, it should still work even after that is fixed
; (note, for_polstruc fills CentWave with ObsPramStruct.Wavelength_Ang)

lmbda=StokesStruct.CentWave + 0.*StokesStruct.I

;
; note this will be an array of same dimension as StokesStruct.I etc.
; the variation induced by velocity should not greatly affect
; Bsun_center so we will just use a median value
; But be careful! under occult and above upoccult set to zero
; so need to disregard these
;

findnonzero=where(lmbda ne 0)

if n_elements(lmbda) gt 1 then lmbda=median(lmbda[findnonzero])
lmbda=lmbda[0]

;
; dlambda is spectral resolution
; for which we use effective line width 
; determined by integral-over-lambda-of-I 
;  (which is what is returned by FORCOMP) 
; divided by Ipeak
;	units Angstrom
;
; This is equivalent to sqrt(pi)*Lambda in eq 12 of Tomczyk tech note
;

dlambda=stokesstruct.I*0.
findnonzero=where(stokesstruct.CentI ne 0)
dlambda[findnonzero]=StokesStruct.I[findnonzero]/StokesStruct.CentI[findnonzero]

;
; some things we'll need
;

arcsec2_to_SR=4.25d10
h=6.6262d-27 ; ergs*s
frequency=2.9979d10/(lmbda*1d-8) ; units of s^-1
photon_energy = h*frequency ; ergs

; telescope contribution
; /noflux ignores dlambda and flux (Allen) in for_sun_flux.pro
;  just uses it as a convenience for calculating telescope contribution

tele_contribution = for_sun_flux(lmbda,dlambda,x2,y2,x1,y1,flm=flm,expTime=integration,Aperture=aperture,Resolution=resolution,Efficiency=efficiency,quiet=1,/noflux)
; UNITS CM^2 S ARCSEC^2

;
; Now calculate Bsun_center in photons
;   if UV SPECPOL will only get telescope weighting - need to multiply by flux in chromorad
;	which lives in SpecPrams.LLim

if strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' then begin
 chromorad=abs(ObsPramsStruct.SpecPrams.ChromoRad)
; chromorad units= photons cm^-2 s^-1 sr^-1 
; tele_contribution is cm^2 s arcsec^2
 Bsun_phot=tele_contribution*chromorad/arcsec2_to_SR
; UNITS PHOTONS
; NOTE -- on disk ALL of this chromorad will get through and be part of I
;  - this will be explicitly dealt with in FOR_INTEGRATE
; but in addition there may be some stray light/instrumental background
; that will be preserved inside B_ppm and set via keyword or widget
;  and this will be in PPM relative to this sun central background
;  as defined here by chromorad
; (note, we may want to consider including Thomson scattering of coronal continnum
;  i.e., K corona - modeling it with model density and treating as another background
;  term; also potentially the Planckian continuum on disk at line wavelength)
;
endif else begin
;
; for VIR spectropolarimeters use Allen flux spectrum
 Bsun_phot = for_sun_flux(lmbda,dlambda,x2,y2,x1,y1,flm=flm,expTime=integration,Aperture=aperture,Resolution=resolution,Efficiency=efficiency,quiet=1)
; UNITS PHOTONS
endelse

;
; convert INSTRUMENTAL BACKGROUND from PPM (1/1d-6/Bsun_center) to photons
;  for the telescope specs
;

B_phot=B_ppm*Bsun_phot*1d-6

; 
; convert intensity into photons for the telescope specs
; intensity for both CoMP and UV will be 'ERG/CM2/S/SR'
;     (if we add WL, need to update)
; tele_contribution is cm^2 s arcsec^2

I_phot = StokesStruct.I*tele_contribution/arcsec2_to_SR/photon_energy


; If QUANTITY is an intensity (e.g., not a ratio or Az, Linewidth, Doppler)
;  need also to convert to photons
;

quantity=quantmap.data

quantint=0
Case 1 of
	strupcase(ObsPramsStruct.LineName) eq 'STOKESI'  or $
	 strupcase(ObsPramsStruct.LineName) eq 'STOKESQ' or $
	 strupcase(ObsPramsStruct.LineName) eq 'STOKESU' or $
	 strupcase(ObsPramsStruct.LineName) eq 'STOKESV' or $
	 strupcase(ObsPramsStruct.LineName) eq 'STOKESW' or $
;	 strupcase(ObsPramsStruct.LineName) eq 'STOKESP' or $
; removing because confusing (P later means L/I) and only there for limited backward compatability
	 strupcase(ObsPramsStruct.LineName) eq 'STOKESL' : quantint=1
	else:
endcase

if quantint eq 1 then begin
 Quant_phot = quantity*tele_contribution/arcsec2_to_SR/photon_energy
endif

;
; Azimuth and L/I error will require calculation of linear polarization fraction
; no units conversion needed since it is dimensionless

if strupcase(ObsPramsStruct.LineName) eq 'STOKESAZ' then begin
 I_phys=StokesStruct.I
 Q_phys=StokesStruct.Q
 U_phys=StokesStruct.U
 L_phys=sqrt(Q_phys*Q_phys+U_phys*U_phys)
 P_phys=L_phys/I_phys
endif

;
; We don't want to include bad points, occulted points in noise calculation
;

realpts=where(quantity ne -8888. and quantity ne -9999.)
numrealpts=size(realpts)
numrealpts=numrealpts[1]

B_real=B_phot[realpts]
I_real=I_phot[realpts]
if quantint eq 1 then Quant_real=Quant_phot[realpts] else Quant_real=quantity[realpts]
if strupcase(ObsPramsStruct.LineName) eq 'STOKESAZ' then P_real=P_phys[realpts]

; ----
; DONE setting up units of photons
; ----

;---
; Now calculate base noise SIGMA for Stokes I
;
; from Tomczyk technical note
;  TN 1 Technical Errors
;  eq. 31
;
; sigma = sigma_i*sqrt(1 + 2 B/N) 
;
; B is number of photons detected in background (B_phot aka B_real above)
; N is number of photons detected in corona (I_phot aka I_real above)
; sigma_i is sqrt(N) = sqrt(I_phot aka I_real above)
;
; WARNING - if using spectrograph have background info s.t.
; the factor of 2 might go away (see tech note for discussion)
; - it is there for filter instrument because effectively we are adding (I+B)-B
;
; ********
; THIS HAS BEEN REPLACED WITH CALCULATION FROM Casini Technical Note, 
; dealing with the noise that cannot be subtracted out.
;
; We are assuming signal I, and background B (or I_bg)  -- see eqn 12 Casini TN
;
; I_bg = B_ppm + (B_TS + B_disk) ; last two terms not implemented yet
;	= B_real in this program (units, photons)
;
;*B_ppm = sky brightness + telescope stray light [set as keyword in FORWARD, telesceop/site dependent]
;*B_TS = Thomson scattering of coronal continuum (K corona) -- depends on model density
;*B_disk -- Planckian continuum from below corona (chrom or TR) at line wavelength on disk [typically ignored for UV]
;
;I_line= I_scatt + I_coll + (I_disk) ; last term only in EUV calculation
;***THIS IS WHAT ROBERTO CALLS I IN S_i Stokes vector
;	I_real this program (units, photons)
;
;* I_scatt = e.g. UVModel Silvano code r. scat
;*I_coll = collisional excitation of the line, calculated from density only if collkey set
;*I_disk = line emission from below corona (chromo or TR) - only relevant for non coronagraphs (on disk) -- this is set by chromorad keyword and specific to line
;
; so all the light is 
;I_obs = B_tot + I_line
; (Casini TN eqn 11)
;

I_obs = I_real+B_real

; sigma is defined first as generally following Poisson distribution
;   sigma_N = sqrt(N)
;   where as above N is total number of photons -- Iobs 
;   and we divide by modeffint in order to account for the
;   efficiency of the modulation scheme for intensity
;
;   note that we are assuming the sqrt(n) (number of states) in eqn 1
;   is implicitly accounted for in sigma by integration time entering as
;   sqrt(integration) through sqrt(I). Also assuming the factor of 2
;   for single beam polarimeter is part of instrument throughput
;   so sigma_I (observed) =sigma/modeffint (generally modeffint=1)
; 

sigma_obs=sqrt(I_obs)/modeffint

; 
; and now establish background noise correction as in Casini TN equation 16
; 
; (note old Tomczyk version TN had f_bg= (1. + 2*B_real/I_real))
; (the change here comes from a reinterpretation of what is "knowable"
; (or "subtractible" part of SNR. We are assuming now (with CASINI TN)
; (that I_obs and B_tot are the knowable parts; Tomczyk TN (and Penn paper?)
; (effectively assumed all three I_obs I_line and B_tot are knowable eqn 29)
;

f_bg = sqrt((1.+B_real/I_real)*(1.+2.0*B_real/I_real))

sigma_I = I_real*f_bg*(sigma_obs/I_obs)

; eqn 15 Casini TN
;
; ----
; DONE getting SIGMA_I
; ----
 
;---
; Now determine SIGMA_QUANT for Quantity
;
; assume same noise for all exposures in a modulation cycle (dominated by noise on I)
;	so noise is the same for all intensities except divided by modulation efficiency
;
; this is sigma_obs for I, Q, U, V
;   note if line=I, modeffquant=modeffint

if quantint eq 1 then begin 

 sigma_quant=sigma_I*modeffint/modeffquant

endif else begin

;
; now do the more complicated quantities
;

;  Casini equations 17-18 use SNR, meant signal to noise I_obs/sigma_obs, 
;   and multiply back background correction. These are both accounted for in:
;      sigma_I/I_real =  f_bg*(sigma_obs/I_obs)
; 	= f_bg/SNR_obs
       fSNR_obs=sigma_I/I_real

;
; First ratios
;

 Case 1 of
	strpos(strupcase(ObsPramsStruct.LineName),'QOI') ge 0 or $
	strpos(strupcase(ObsPramsStruct.LineName),'UOI') ge 0 or $
	strpos(strupcase(ObsPramsStruct.LineName),'VOI') ge 0 or $
; removing because confusing (P later means L/I) and only there for limited backward compatability
;	strpos(strupcase(ObsPramsStruct.LineName),'POI') ge 0 or $
	strpos(strupcase(ObsPramsStruct.LineName),'LOI') ge 0 : begin

; Casini TN eqn 6/17, 
;      sigma_quant=f_bg*(sigma_obs/I_obs)*sqrt(Quant^2+(modeffint/modeffquant)^2)
;      sigma_quant=fSNR_obs*sqrt(Quant^2+(modeffint/modeffquant)^2)
; 
	  Sq=Quant_real
	  sigma_quant= fSNR_obs*sqrt(Sq*Sq + (modeffint/modeffquant)^2)
	end
	else:
 endcase

; Now Azimuth

 if strupcase(ObsPramsStruct.LineName) eq 'STOKESAZ' then begin
;
; from eqn 26 in Tomczyk technical manual
; OR eqn 8/16 of Casini TN
;
; sigma_quant=f_bg*(sigma_obs/I_obs)*(modeffint/modeffquant)*(1/P)*(1/2)
;   (where modeffquant= efficiency of linear polarization)
; so sigma_quant=fSNR_obs*(modeffint/modeffquant)/P/2

	sigma_quant=fSNR_obs*(modeffint/modeffquant)/2./P_real
;
; note -- this is in radians, but azimuth quantity is in degrees
;  so we change error to degrees for consistency
;
	sigma_quant=sigma_quant/!dtor

 endif

; Now Linewidth and DopplerVlos
;
; both use e-folding halfwidth (see eq 12 Tomczyk tech note)
; need to convert from Angstrom to nm
	Lambda_gauss=dlambda/sqrt(!pi)/10.
        Lambda_real=Lambda_gauss[realpts]
	lambda_nm=lmbda/10.

;Linewidth

 if strupcase(ObsPramsStruct.LineName) eq 'LINEWIDTH' then begin
;
; from eqn 20,21 in Tomczyk technical manual
 
;
; WARNING assuming Lande G factor 1.5
;    only applicable to 10747
;
	glande=1.5
	sigma_quant=fSNR_obs*Lambda_real/lambda_nm/lambda_nm/glande/6.6d-12
 endif

; Now DopplerVlos

 if strupcase(ObsPramsStruct.LineName) eq 'DOPPLERVLOS' then begin
;
; from eqn 18 in Tomczyk technical manual
;
; speed of light cgs
  clight = 2.9979d10
; divide by 1d5 to get solution in units of km/sec
  sigma_quant = fSNR_obs*clight*Lambda_real/lambda_nm/sqrt(2.)/1d5

 endif
endelse

; ----
; DONE getting SIGMA_QUANT appropriate to quantity 
; ----
 
;---
; Now we calculate the Relative Error (noise/signal)
; need to divide SIGMA_QUANT by Quantity
; and randomize
;  (no longer randomizing here-- doing in for_plot)
; 

RelErrReal=sigma_quant/Quant_real

;
; this is removed because now randomizing is done in FOR_PLOT
;if norandom eq 1 then RelErrRandom=RelErrReal else RelErrRandom=RelErrReal*randomn(seed,numrealpts)

RelErr=quantity*0.d0
RelErr[realpts]=RelErrReal

;RelErr[realpts]=RelErrRandom

nandata=where(RelErr*0. ne 0.)
if min(nandata) ne -1 then RelErr[nandata]=0.d0

return,RelErr

end
