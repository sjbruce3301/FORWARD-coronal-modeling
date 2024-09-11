pro for_fixunits,units,Map,ObsPramsStruct,StokesStruct=StokesStruct,model=model,ulimb=ulimb

;
; program to convert to/from Map units using keyword UNITS
;
;  Inputs:  UNITS - to be converted to
;   	    MAP - map, either data or model with current unit identified through tag BUnit
;           OBSPRAMSSTRUCT - needed for noise info in PHOTONS case, also instrument type and name
;
;
;  Keywords
;	    STOKESSTRUCT - needed for central wavelength and equivalent width,
;			also used to identify CoMP input
;	     MODEL -- on for model (as opposed to observations)
;	
;	
;  Output:  MAP.DATA and MAP.BUNIT will be changed, if MAP.BUNIT ne UNITS
;
; For Coronal Polarimeters 
;
;               NOTE -- PPM is in units relative to Bsun_center, and so
;		is independent of telescope.  
;		For cases ERG/CM2/S/SR and ERG/CM2/S/ARCSEC2
;               NO TELESCOPE INFORMATION will be taken into consideration.
;		However, PHOTONS will be calculated taking into
;		consideration telescope information in ObsPramsStruct.NoisePrams
;
;		Also - although the means of converting from data units
;		is included below, it requires knowledge of equivalent width
;		of line. Right now there is a place holder of 1.4 Angstrom for 10747 and 10798
;		CoMP which is not generally correct, and so in FOR_PLOTDEFAULTS
;		the choice of changing from (native) PPM units is taken away.
;		If eventually we calculate eqwidth appropriately this can be changed back.
;		*update -- UCoMP does in fact provide both I and centI so we 
;		could perhaps add this capability now, with appropriate testing**
;
;
; Called by FOR_PLOT, FOR_DRIVE
;
; Can be called standalone to convert units in a map made by FORWARD
;
; Calls FOR_SUN_FLUX
;
; Written by Sarah Gibson 2013
; Version 2.0 July 2014
;       Modifications
;               13-Jan-2013, Added pop2colden and pop2losem units
;		06-Feb-2015 - Bug fix dlambda (was set to I, should be 0)
;		June-2018, Added benergy,etc.
;	 	July-2018 - removed UV spectropolarimetry from call to 
;			for_sun_flux for now, and changed units of intensity
;			to NoUnit -- since we do not have physical units yet
;		May-2019 - made native units of UV  photons s^-1 cm^-3 sr^-1
;		Sept 2021 - expanded conditional for STOKESVOI
;		May 2022 -- modified to allow UV spec to vary
;		Jun 2022 -- commented out special UV rules -- fixed in for_personality
;			 also changed STOKESAZ --> AZ for generality
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Dec 2022 -- updated units for P=PB/TB
;	Jan 2023 -- updated approach for UV SPECPOL
;		cleaned up some unnecessary lines of code
;		got rid of modeffquant^2 multiplier on integrtion
;	Aug 2023 -- added IONDENS
;	Sep 2023 -- updated c and h
;	Nov 2023 -- used abs(chromorad) since it can be passed through negative if not adding to disk
;       Jun 2024 -- added BPOS column variables
;	Jul 2024 -- removed PPM as option for WL as ambiguous
;		and replace with Bsun_mean
;		Also added some notes about I, centI for data
;		and passed through ulimb
;--

;
; old data might not have BUnit -- if so they will be in original units
;

arcsec2_to_SR=4.25d10
;
h=6.6262d-27          ; Planck constant in ergs*second
                        ; 1 Watt = 10^7 ergs/sec = 1 Joule/sec
c=2.9979d10                  ; speed of light in cm/sec
                        ; 1 A = 10^-8 cm
;

getunit=0
if tag_exist(Map,'BUnit') eq 0 then getunit=1
if tag_exist(Map,'BUnit') then if strupcase(Map.BUnit) eq 'NULL' then getunit=2
;if strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' then getunit=2

if getunit gt 0 then begin
  
 BUnit=' '

 if strupcase(ObsPramsStruct.IClass) eq 'EUV/XRAY IMAGERS' then begin
  if keyword_set(model) then begin
   if strpos(strupcase(GridPramsStruct.GridType),'USER') lt 0 then BUnit='DN/S/MAPPIX' $
       else BUnit='DN/S/CM^2'
  endif else BUnit='DN/S/DETPIX'
 endif

 if strupcase(ObsPramsStruct.IClass) eq 'UV/EUV SPECTROMETERS' then begin
   if strupcase(ObsPramsStruct.Instrument) eq 'IONDENS' then BUnit='CM^-3' $
    else BUnit='ERG/CM2/S/SR'
 endif

 if strupcase(ObsPramsStruct.Instrument) eq 'RADIO' then if $
        strpos(strupcase(ObsPramsStruct.LineName), 'VOI') lt 0 then $
          BUnit='degrees Kelvin' else BUnit='fraction circular polarization'
 if strupcase(ObsPramsStruct.Instrument) eq 'FARADAY' then $
        if strupcase(ObsPramsStruct.LineName) eq 'RM' then BUnit='radians/m^2' else BUnit='radians'

 if strpos(strupcase(ObsPramsStruct.Instrument),'OMP') ge 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' $
  or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' $
  then begin
   if strpos(strupcase(ObsPramsStruct.LineName),'OI') lt 0 then begin
    if strpos(strupcase(ObsPramsStruct.LineName),'DOPPLERVLOS') lt 0 and strpos(strupcase(ObsPramsStruct.LineName),'LINEWIDTH') lt 0 and strpos(strupcase(ObsPramsStruct.LineName),'AZ') lt 0 then begin
      if keyword_set(model) then BUnit='ERG/CM2/S/SR' else begin
;       if strpos(strupcase(ObsPramsStruct.Instrument),'OMP') ge 0 then BUnit='PPM' else BUnit='DN'
       BUnit='PPM'
      endelse
    endif else BUnit='KM/SEC'
    if strpos(strupcase(ObsPramsStruct.LineName),'AZ') ge 0 then BUnit='DEGREES'
   endif else BUnit='FRACTION INTENSITY'
 endif

 if strupcase(ObsPramsStruct.Instrument) eq 'WL' then BUnit='10^-8 Bsun_center'
 if strupcase(ObsPramsStruct.Instrument) eq 'WL' and strupcase(ObsPramsStruct.LineName) eq 'P' then BUnit='Fraction Polarized (pB/TB)'
 if strupcase(ObsPramsStruct.Instrument) eq 'LOSEM' then BUnit='cm^-5 '
 if strupcase(ObsPramsStruct.Instrument) eq 'COLDEN' then BUnit='cm^-2 '
 if strupcase(ObsPramsStruct.Instrument) eq 'BENERGY' then BUnit='Gauss^2*cm '
 if strupcase(ObsPramsStruct.Instrument) eq 'BEN_DENS_INT' then BUnit='Gauss^2*cm^-2 '
 if strupcase(ObsPramsStruct.Instrument) eq 'B_INT' then BUnit='Gauss*cm '
 if strupcase(ObsPramsStruct.Instrument) eq 'B_DENS_INT' then BUnit='Gauss*cm^-2 '
 if strupcase(ObsPramsStruct.Instrument) eq 'B_POS_INT' then BUnit='Gauss*cm '
 if strupcase(ObsPramsStruct.Instrument) eq 'B_POS_DENS_INT' then BUnit='Gauss*cm^-2 '
 if strupcase(ObsPramsStruct.Instrument) eq 'POP2LOSEM' then BUnit='cm^-5 '
 if strupcase(ObsPramsStruct.Instrument) eq 'POP2COLDEN' then BUnit='cm^-2 '

;
; now take care of physical parameters
;

 if BUnit eq ' ' then begin
    left=strpos(ObsPramsStruct.Label,'(')
    right=strpos(ObsPramsStruct.Label,')')
    if left ne -1 and right ne -1 then begin
      BUnit=strmid(ObsPramsStruct.Label,left+1,right-left-1)
    endif
 endif

 if getunit eq 1 then Map=add_tag(Map,BUnit,'BUnit')  else Map.BUnit=BUnit

endif

if strupcase(units) ne strupcase(Map.BUnit) and strupcase(units) ne 'NULL' and strpos(strupcase(Map.BUnit),'FILTER') lt 0 then begin

  unitmult=1.d0

;
; ComP model or data
;  or UV specpol
;

 if (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') ge 0 or $
        strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' or $
	(strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' and keyword_set(model))) and strpos(strupcase(ObsPramsStruct.LineName),'OI') lt 0 and strpos(strupcase(ObsPramsStruct.LineName),'DOPPLERVLOS') lt 0 and strpos(strupcase(ObsPramsStruct.LineName),'LINEWIDTH') lt 0 and strpos(strupcase(ObsPramsStruct.LineName),'AZ') lt 0 then begin

;  lmbda=StokesStruct.CentWave
; temporary kluge to fix old version of UV code which passed a single number through for CentWave
;	however, it should still work even after that is fixed
; (note, for_polstruc fills CentWave with ObsPramStruct.Wavelength_Ang)
;  (and EQWIDTH with SpecPrams.LWidth)
;
  lmbda=StokesStruct.CentWave + 0.*StokesStruct.I
  
;
; note this will be an array of same dimension as Map.data
; and StokesStruct.I etc.
; for model, although it will be a scalar for CoMP data
; the variation induced by velocity should not greatly affect
; Bsun_center so we will just use an average value
; But be careful! under occult and above upoccult set to zero
;

  findnonzero=where(lmbda ne 0)

  if n_elements(lmbda) gt 1 then lmbda=median(lmbda[findnonzero])
  lmbda=lmbda[0]

  photon_energy = h*c/lmbda/1.0d-8

;
; equivalent width is just integrated I divided by central I
;
; WARNING- this is fine for model because it is in physical units
; for data I carry through a value of Icent (defined in READCOMPFiTS) 
; --- and now also in for_polstruc for UV
;	(note this is chromospheric line width also carried in ObsStruct.SpecPrams as LWidth )
; to back out the line width so that when comparing to Bsun_center it is integrated
; over something comparable (but UV is done differently, using chromorad,
;  and so dlambda is ignored, see below)
;
; Its ok if this (dlambda) is an array
;

  dlambda=StokesStruct.I*0.
  findnonzero=where(StokesStruct.CentI ne 0)
  dlambda[findnonzero]=StokesStruct.I[findnonzero]/StokesStruct.CentI[findnonzero]

;
;  For going to/from PPM calculation, or for calculating PHOTONS, 
;  we use Haosheng's code to calculate central solar flux at 
;  central wavelength lambda, with equivalent width determined from ratio of integrated
;  to central intensity from model, or specified width if coming from observation.
;  We choose telescope values stored in ObsPramsStruct 
;
 
  integration = ObsPramsStruct.NoisePrams.Integration
  aperture=ObsPramsStruct.NoisePrams.Aperture
  resolution=ObsPramsStruct.NoisePrams.Resolution
  efficiency=ObsPramsStruct.NoisePrams.Efficiency

; bsun_notele is the "ideal" sun central brightness (no telescope filter function)
; basically just presents Bsun_center at central wavelength (integrated over line equivalent width
; dlambda) in units of ERG/CM2/S/ARCSEC2 

; next two lines for debugging
;  test=1
;  if test eq 0 then begin
  if strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' then begin
   chromorad=abs(ObsPramsStruct.SpecPrams.ChromoRad)
; chromorad units= photons cm^-2 s^-1 sr^-1 
   Bsun_notele=chromorad*photon_energy/arcsec2_to_SR
;  units= ERG/CM2/S/ARCSEC2
  endif else begin
;    use Allen table, for a CM2 S ARCSEC2 telescope
;      (note the 4/pi is to end up with telescope area = 1, see for_sun_flux)
   Bsun_notele=1d7*for_sun_flux(lmbda,dlambda,x2,y2,x1,y1,expTime=1.,Aperture=sqrt((4.d0/!dpi)),Resolution=1.,Efficiency=1.,quiet=1,/power)
;  units= ERG/CM2/S/ARCSEC2
  endelse

; if units of PHOTONS, we will need to take into account filter through telescope
; /noflux ignores dlambda and flux (Allen) in for_sun_flux.pro
;  just uses it as a convenience for calculating telescope contribution

   tele_contribution=for_sun_flux(lmbda,dlambda,x2,y2,x1,y1,expTime=integration,Aperture=aperture,Resolution=resolution,Efficiency=efficiency,quiet=1,/noflux)
; UNITS CM^2 S ARCSEC^2

  Bsun_tele_photons=tele_contribution/photon_energy
; UNITS CM^2 S ARCSEC^2 / ERG 

;
; coming from physical units
;
  if strupcase(Map.BUnit) eq 'ERG/CM2/S/SR' or strupcase(Map.BUnit) eq 'ERG/CM2/S/ARCSEC2' then begin

   if strupcase(Map.BUnit) eq 'ERG/CM2/S/SR' then unitmult=unitmult/arcsec2_to_SR
		; convert to ERG/CM2/S/ARCSEC2

   case strupcase(units) of
   'ERG/CM2/S/SR': unitmult=unitmult*arcsec2_to_SR
   'ERG/CM2/S/ARCSEC2': unitmult=unitmult
   'PPM': begin
; convert to PPM BSUN_CENTER (relative units)
	 unitmult=unitmult/(1d-6*Bsun_notele)
	end
   'PHOTONS': begin
; multiply by telescope contribution and divide by photon energy to get absolute photons units
	 unitmult=unitmult*Bsun_tele_photons
	end
   else: message,/info,"Allowed units 'ERG/CM2/S/SR','ERG/CM2/S/ARCSEC2','PPM','PHOTONS'. Keeping original "+Map.Bunit
   end
  endif 

; 
; Now coming from Photons or PPM
;

  if strupcase(Map.BUnit) eq 'PPM' or strupcase(Map.BUnit) eq 'PHOTONS' then begin
   if strupcase(Map.BUnit) eq 'PHOTONS' then unitmult=1d6/Bsun_tele_photons/Bsun_notele
	; multiply by photon energy and divide by telescope contribution
        ;  --> ERG/CM2/S/ARCSEC2
	; then convert to PPM
   case strupcase(units) of
   'PPM': unitmult=unitmult
   'ERG/CM2/S/SR': begin
	 unitmult=unitmult*1d-6*Bsun_notele
	 unitmult=unitmult*arcsec2_to_SR
	end
   'ERG/CM2/S/ARCSEC2':  begin
	 unitmult=unitmult*1d-6*Bsun_notele
	end
   'PHOTONS': begin
	 unitmult=unitmult*1d-6*Bsun_tele_photons*Bsun_notele
       end
   else: message,/info,"Allowed units 'ERG/CM2/S/SR','ERG/CM2/S/ARCSEC2','PPM','PHOTONS'. Keeping original "+Map.BUnit

   end
  endif

  
 endif

;
; now EUV imagers
;
 
 if strupcase(ObsPramsStruct.IClass) eq 'EUV/XRAY IMAGERS' then begin

  instr=ObsPramsStruct.Instrument
  detpix=for_detpix_lookup(instr)
  detpix=detpix*detpix
;
; detector pixel size in arcseconds^2
;
  if tag_exist(Map,'Dx') then mappix=Map.Dx*Map.Dy else mappix=Map.Dth*Map.Dph
;
; map pixel in units of Rsun^2
;
  earth=1
  soho=0
  stereo=0
  if strupcase(instr) eq 'EIT' then begin
   soho=1
   earth=0
  endif
  if strupcase(instr) eq 'EUVIA' then begin
   soho=0
   earth=0
   stereo='A'
  endif
  if strupcase(instr) eq 'EUVIB' then begin
   soho=0
   earth=0
   stereo='B'
  endif
  if keyword_set(stereo) then begin
    if strupcase(stereo) eq 'A' then RSun=pb0r(Map.Time,soho=soho,earth=earth,/ahead,/arcsec) 
    if strupcase(stereo) eq 'B' then RSun=pb0r(Map.Time,soho=soho,earth=earth,/behind,/arcsec) 
  endif else RSun=pb0r(Map.Time,soho=soho,earth=earth,/arcsec)
  RSun=Rsun[2]
  mappix=mappix*Rsun*Rsun

  if strupcase(units) eq 'DN/S/MAPPIX' then begin
    unitmult=mappix/detpix
  endif else begin
    unitmult=detpix/mappix
    if strupcase(units) ne 'DN/S/DETPIX' then print,'something odd about units: debug'
  endelse
 endif

 if strupcase(ObsPramsStruct.Instrument) eq 'WL' then begin
;  if strupcase(units) eq 'PPM' then begin
;   unitmult=.01
  if strupcase(units) eq '10^-8 BSUN_MEAN' then begin
; default limb darkening coefficient u = .63
   if tag_exist(ObsPramsStruct,'ULimb') then ulimb=ObsPramsStruct.Ulimb else ulimb=0.63
   unitmult=1.d0/(1.d0-ulimb*(1.d0 - 2.d0/!dpi))
  endif
 endif

;print,'dx=',map.dx,'dy=',map.dy,'rsun=',rsun
;print,'mappix=',mappix,'detpix=',detpix

;
; take care of arrays with occulted disks which may have NaNs
;

 test=where(unitmult*0. ne 0.)
 if min(test) ne -1 then unitmult[test]=1.

 Map.Data=double(Map.Data)*unitmult
; if Map.BUnit ne 'NoUnit' then Map.BUnit=units
; 
; commenting next line because I need to be able to change UV SPEC
; and adding the one to follow
;
;  if strupcase(ObsPramsStruct.IClass) ne 'UV SPECTROPOLARIMETERS' then Map.BUnit=units
  Map.BUnit=units
endif
; else no change needed
; or in case of filtered data, none appropriate

end
