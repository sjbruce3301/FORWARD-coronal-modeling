PRO NUMCUBEPRAMS,outarray,date=date,working_dir=working_dir,$
		   nreinit=nreinit,getmas=getmas,getawsom=getawsom,$
		   cubename=cubename,cuberot=cuberot0,$
                   bout=bout0, bnorm=bnorm0,$
	           hydro=hydro0,densprof=densprof,te=te,t0=t0,$
 		   odensprof=odensprof,ot0=ot0,topology=topology0,$
 		   cdensprof=cdensprof,ct0=ct0,$
		   deltar=deltar0,velimpose=velimpose0,$
		   coldpts=coldpts0,dthres=dthres0,p2fill=p2fill0,$
		   magmod=magmod,mheat=mheat,choosehigh=choosehigh,$
		   nowidgmess=nowidgmess,nofield=nofield0,$
		   saveprams=saveprams,readprams=readprams
;+
;
;Name: 
;   NUMCUBEPRAMS
;
;Purpose:
;          To set up the structure containing the information needed
;          to run a data cube interpolation. This procedure is to  be
;          called by the driver routine with keyword inputs that
;	   with resulting structure OUTARRAY assigned to ModPramsStruct 
;	   (with ModPramsStruct.name='numcube') 
;
;Keyword inputs:
;
;  CUBENAME: Filename (inluding extention and path) of the numerical datacube
;            you want to look at. File must be in the correct format,
;            use make_my_cube.pro in this directory to convert your
;            cube. 
;
;  BOUT: how to handle the magnetic field ouside the
;        cube. The options are:
;
;             BOUT=1; no magnetic field outside the cube (DEFAULT)
;             BOUT=2; radial open field. default strength. 
;		     Changes sign at equator.
;			note that
;                     there will probably be a discontionuity at the
;                     edge of the cube.  
;             BOUT=3; dipole field. default strength. note that
;                     there will probably be a discontionuity at the
;                     edge of the cube. 
;
;  BNORM: magnitude of external field for BOUT = 2 or 3
;			DEFAULT 10 Gauss
;
;
;  VELIMPOSE - impose a velocity of constant magnitude VELIMPOSE directed along the field
;	 	overwrites any velocity field already loaded in if nonzero
;			DEFAULT 0.d0
;
;  NOFIELD - turn off magnetic field in simulation
;		*OR if not 0 or 1, can act as a multiplier on field
;            DEFAULT 0 (model field)
;
;  DTHRES    - DOUBLE  : Identify POP2 coronal points that are dense and can be treated
;			as different population
;                        If unset or set to 0, will be ignored; if set to non-zero
;                        will be interpreted as density above which points are
;                        designated population 2.  (If set to 1 will default to 5d8 cgs)
;			 This will result in creation of POP2DENS zeroed everywhere except 
;			 D>THRESD - this will be set up in NUMCUBE.PRO
;			 This will overwrite any POP2DENS tag already in CUBE.
;			 Note if COLDPTS also set, DTHRES will be ignored.
;                        DEFAULT 0
;  COLDPTS     - DOUBLE  : Identify points that are cold as different population
;                        If unset or set to 0, will be ignored; if set to non-zero
;                        will be interpreted as temperature below which points are
;                        designated population 2.  (If set to 1 will default to 3d4 K)
;			 This will result in creation of POP2DENS zeroed everywhere except 
;			 T<COLDPTS - this will be set up in NUMCUBE.PRO
;			 This will overwrite any POP2DENS tag already in CUBE.
;                        DEFAULT 0
;
; P2FILL   - DOUBLE :  A single number between 0 and 1 to multiply population 2 density by
;			so that POP2FILLINGFACTOR=P2FILL in POP2 voxels, and 0 elsewhere, and
;			FILLINGFACTOR=1-P2FILL in POP2 voxels, and 1 elsewhere.
;			Will overwrite any FILLINGFACTOR and/or POP2FILLINGFACTOR tag on CUBE.
;			Note if there is no POP2DENS or POP2TEMP in cube and both COLDPTS and DTHRES
;			are not set, P2FILL will be ignored if set to anything other than default
;			DEFAULT 1.d0
;
;  DELTAR - step for radial derivative (e.g. LINE=NINST)
;                       DEFAULT 0.01d0
;
;  CUBEROT: Rotation angle in degrees. Amount the numerical datacube is to be
;        rotated around the radial direction in a counterclockwise manner. The
;        cube is centered at longitude zero at the equator.(The
;        default viewing position for a plane-of-sky map set in
;        for_get_grid is -90, so that model structures centered on
;        longitude zero appear at the West limb) The z-axis points
;        north-south.
;        Won't work on global or axisymmetric data sets, but will work with
;        datasets that are only a subset of lat/lon where outside is
;        filled in by defaults (see below).
;        DEFAULT 0
;
;  GETMAS - only used when called via PSIMAS programs
;		forces run of MAKEMASCUBE based on inputted date
;
;  GETAWSOM - only used when called via AWSOM programs
;		forces run of MAKEAWSOMCUBE based on inputted date
;
;  MHEAT - only used when called via PSIMAS programs
;		defineds MAS heating, magnetic boundary
;		MHEAT within CUBENAME will overwrite any input
;
;  MAGMOD: Indicates numerical model contains magnetic fields.  Default yes (1).
;
;
; HYDRO, DENSPROF, CDENSPROF, ODENSPROF, TE/T0, CT0, OT0:
;  how to handle the plasma outside the box and (if TOPOLOGY set within datacube) 
;	in closed vs open regions
;       DEFAULT HYDRO 3 in closed vs open, Vasquez 2003
;		HYDRO 0 outside the box
;		(or -1, continuing inward/outward with r**2 falloff
;			if global)
;       FOR DEFAULTS SEE NUMCUBE/FOR_HYDRODEFAULTS
;	   (note that DENSPROF/ODENSPROF are treated the same)
;
; BOOKKEEPING
;
;
;		NREINIT - if set to zero, assume the datacube is already in the common block
;
;               SAVEPRAMS - if keyword set to a string, write parameters to filename
;                       (which is the keyword value saveprams)
;                       or if set to 1, then replace with pramarray
;
;
;               READPRAMS - if keyword set, read in parameters from filename
;                       (which is the keyword value filename)
;                       or if a structure than read directly
;                       NOTE, flagged keywords will overwritten
;
;Output:  ModPramsStruct to be useed by numcube.pro
;
;		DATE	If numerical cube has a date within it, it will overwrite
;			keyword DATE.  If GETMAS or GETAWSOM set, DATE inputed will be 
;			used to retrieve MAS/AWSOM CUBE if they exist
;               DENSPROF, ODENSPROF, CDENSPROF -- may be converted to vectors by FOR_HYDRODEFAULTS
;               TE, OT0, CT0 -- may be changed in for_hydrodefaults
;
; Called by FOR_MODELDEFAULTS, PSIMASPRAMS, AWSOMPRAMS
;
;
; Author and history:
;	Written by Laurel Rachmeler, Sarah Gibson
;	LR: original version
; 	modified for hydrostatic model consistency SEG March 2011
;       changed to procedure and add readprams/saveprams Jan 2013 SEG
;	fixed bug where saveprams was not same as original input Oct 2013 SEG
; Version 2.0 July 2014
;	added POP2 capability Jan 2015 SEG
;       Sep 2018 SEG - fixed bug where odensprof was multiplied by 1d8 instead of 1d5
;		also bug where densprof hydro=2 did not work from widget
;	Oct 2018 SEG - added Vasquez hydrostatic profiles
;	Jun 2019 - used slash for PC compatibility
;               removed flduse option for torus inst 
;		will be set to BHOR (horizontal-Bth^2+Bph^2)
;               made default OT0 1.5d6 for HYDRO=3
;		added keyword T0 so if someone uses that instead of Te for outside
;		cube temperature (which also impacts default of cT0) it still works
;	Sep 2019 -- added hydro=4
;	Sep 2020 -- updated treatment of magmod
;	Sep 2021- passed through nowidgmess
;	Feb 2022 - cleaned up hydro etc
;		simplified defaults
;	  made TE default to open field OT0
;	 changed B0 to Bnorm to avoid conflict with solar B angle
;	Nov 2022 - allowed for negative hydro for numcube out of box
;		r**2 falloff
;		and made it default for global models
;		Also forced defaults if nreinit set
;	Aug 2023 -- changed cuberot to cuberot0 in call 
;	Sept 2023 -- passed through choosehigh keyword for PSI
;		also fixed bug where magmod not passed to widget for PSI
;		Added AWSOM hooks
;	Oct 2023- put in checks for getmas/getawsom eq 1 -- if set to -1 will still build
;		parameter files appropriately
;       Dec 2023- added nofield parameter
;-

slash=path_sep()

  COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

;
; set parameter defaults
;

if keyword_set(readprams) then begin
  ; read parameter file (a structure file or structure)
  case datatype(readprams) of
    'STR': restgen,inarray,file=readprams
    'STC': inarray=readprams
    else: message, 'must provide a named readprams file or a structure'
  endcase
  t=tag_names(inarray)
  for i=0,n_elements(t)-1 do void=execute(t[i]+'_rd=inarray.(i)')
endif

;Parameters
;If keyword set for a given parameter, then this is used.
;If keyword not specified, then use value from readparams if set. If readparams not set,
;uses the default values as listed in these following statements

cuberot=n_elements(cuberot0) eq 0?(n_elements(cuberot_rd) eq 0?0.0:cuberot_rd):cuberot0

Bout=n_elements(Bout0) eq 0?(n_elements(Bout_rd) eq 0?1.:Bout_rd):Bout0
; nothing outside

bnorm=n_elements(bnorm0) eq 0?(n_elements(bnorm_rd) eq 0?10.:bnorm_rd):bnorm0
;10 Gauss, default strength of the external field

velimpose=n_elements(velimpose0) eq 0?(n_elements(velimpose_rd) eq 0?0.0:velimpose_rd):velimpose0
; no field-aligned velocity

nofield=n_elements(nofield0) eq 0?(n_elements(nofield_rd) eq 0?0.0:nofield_rd):nofield0
; turn off B-field

dthres=n_elements(dthres0) eq 0?(n_elements(dthres_rd) eq 0?0.:dthres_rd):dthres0
coldpts=n_elements(coldpts0) eq 0?(n_elements(coldpts_rd) eq 0?0.:coldpts_rd):coldpts0
p2fill=n_elements(p2fill0) eq 0?(n_elements(p2fill_rd) eq 0?1.:p2fill_rd):p2fill0

if p2fill lt 0.d0 or p2fill gt 1.d0 then begin
 p2fill=1.d0
 message,/info,'filling factor between 0 and 1; changing to 1'
endif

; deltar
deltar=n_elements(deltar0) eq 0?(n_elements(deltar_rd) eq 0?0.01:deltar_rd):deltar0

if keyword_set(getmas) then if getmas eq 1 then makemascube,date=date,working_dir=working_dir,mheat=mheat,cubename=cubename,choosehigh=choosehigh
if keyword_set(getawsom) then if getawsom eq 1 then makeawsomcube,date=date,working_dir=working_dir,cubename=cubename

;which data cube
; THE STRUCTURE INSIDE CUBENAME.DAT BETTER BE CALLED CUBE

IF NOT KEYWORD_SET(cubename) THEN BEGIN
     cubename=file_dirname(GET_ENVIRON('FORWARD'))+slash+file_basename(GET_ENVIRON('FORWARD'))+slash+'MODELS'+slash+'NUMCUBE'+slash+'cube1.dat'
     pramcubename=''
ENDIF else pramcubename=cubename

  ;if you forgot the .dat on your cubename, then add it in. 
IF STRPOS(cubename,'.dat') LT 0 THEN cubename=cubename+'.dat' 

;Restore the data cube. When it comes out, there will be a structure
;called 'cube'

;only restore the cube once. Otherwise, use the common block
;where the cube is stored

IF nreinit EQ 1 THEN BEGIN
   RESTORE, cubename
   COMMON THECUBE, cube
ENDIF

default,date,''
if tag_exist(cube,'date') then if cube.date ne '' then date=cube.date
default,mheat,''
if tag_exist(cube,'mheat') then mheat=cube.mheat
default,magmod,1
if tag_exist(cube,'magmod') then magmod=cube.magmod

;
; topology
 
if tag_exist(cube,'topology') then topset=1 else topset=0

topology=n_elements(topology0) eq 0?(n_elements(topology_rd) eq 0?topset:topology_rd):topology0

if tag_exist(cube,'global') then begin
  if cube.global eq 1 then hydrodefault=-1 else hydrodefault=0
endif else hydrodefault=0
; nothing outside box is default for non-global
; r^2 is default for global
; but note will use hydro=3 if topology open/closed done and hydro=0
; and also note that if nreinit set will not use hydro0 but be set to hydrodefault

;Plasma parameters
if nreinit NE 1 then hydro=n_elements(hydro0) eq 0?(n_elements(hydro_rd) eq 0?hydrodefault:hydro_rd):hydro0 $
   else hydro=hydrodefault


;
; first do inside the box if topology is set
; openfield
;
hydrosave=hydro
if hydro ne 0. then signhydro=hydro/abs(hydro) 
if topology eq 1 then begin
 if hydro eq 0 then hydrouse=3 else hydrouse=abs(hydro)
 if exist(cdensprof) then cdensprofsave=cdensprof
 if exist(cT0) then cT0save=cT0
 for_hydrodefaults,$
        hydro=hydrouse,cdensprof=cdensprof,cT0=cT0,$
        odensprof=odensprof,oT0=oT0,$
        rcdensprof=cdensprof_rd,rcT0=cT0_rd,$
        rodensprof=odensprof_rd,roT0=oT0_rd,$
        vodensprof=vodensprof,vcdensprof=vcdensprof

;
; save this as it may be overwritten below
;

 vodensprofsave=vodensprof
;
; note the HYDRO = 4 requires rerunning for_hydrodefaults
;  with HYDRO=3 for the closed field regions
;

 if hydrouse eq 4 then begin
  for_hydrodefaults,$
        hydro=3,cdensprof=cdensprofsave,ct0=cT0save,$
        rcdensprof=cdensprof_rd,rct0=cT0_rd,$
        vcdensprof=vcdensprof
  cdensprof=cdensprofsave
  ct0=ct0save
 endif
endif else begin
 vcdensprof='NULL'
 vodensprof='NULL'
 oT0='NULL'
 cT0='NULL'
endelse

;
; now do outside the box
;  this will default to open field HYDRO profiles
;

; in case someone uses T0 instead of Te
;
if keyword_set(T0) eq 1 then Te=string(T0)

hydro=abs(hydrosave)
for_hydrodefaults,$
        hydro=hydro,$
        odensprof=densprof,oT0=Te,$
        rodensprof=densprof_rd,rot0=Te_rd,$
        vodensprof=vodensprof
vdensprof=vodensprof
if exist(vodensprofsave) then vodensprof=vodensprofsave
if hydro ne 0. then begin
 if hydro/abs(hydro) ne signhydro then hydro=signhydro*hydro
endif

crot=''
if (n_elements(getmas) ne 0 or n_elements(getawsom) ne 0) and date eq '' then begin
  break_file,cubename,disk,dir,fil,ext
  crot=strmid(fil,0,4)
  print,'crot=',crot
;
; maybe this should be shifted by 14 days?
;
  date=anytim(carr2ex(crot+.5),/ccsds)
endif

;
; don't allow cube rotation for global or axisymmetric cubes
;

cuberotnouse=0
if cube.axisym eq 1 then cuberotnouse=1
if tag_exist(cube,'global') then if cube.global eq 1 then cuberotnouse=1

if cuberotnouse and cuberot ne 0.d0 then begin
   if keyword_set(nowidgmess) then message,/info,' cuberot has no effect on global or axisymmetric simulations. Note, viewer position may be changed via LOS keywords BANG, CMER.' else d=dialog(/WARNING,'cuberot has no effect on global or axisymmetric simulations. Note, viewer position may be changed via LOS keywords BANG, CMER.')
endif

if cuberotnouse then cuberot=0.d0

;
; set up  pram input array
;
pramarray={name:'numcube',cubename:pramcubename,bout:double(bout),bnorm:double(bnorm),nofield:nofield,$
	           hydro:double(hydro),densprof:densprof,te:te,velimpose:velimpose,dthres:dthres,coldpts:coldpts,p2fill:p2fill,deltar:deltar,magmod:magmod} 

if cuberotnouse eq 0 then pramarray=add_tag(pramarray,cuberot,'CubeRot')

if tag_exist(cube,'topology') then begin
 pramarray=add_tag(pramarray,topology,'topology')
 pramarray=add_tag(pramarray,odensprof,'ODensProf')
 pramarray=add_tag(pramarray,oT0,'OT0')
 pramarray=add_tag(pramarray,cdensprof,'CDensProf')
 pramarray=add_tag(pramarray,cT0,'CT0')
endif
;
; MAS doesn't need deltar for potential field
; (also don't need o/cdensprof or o/cT0)
; does need magmod
;

if n_elements(getmas) ne 0 then  $
 pramarray={name:'psimas',cubename:pramcubename,nofield:nofield,$
		   bout:double(bout),bnorm:double(bnorm),$
	           hydro:double(hydro),densprof:densprof,te:te,$
		   velimpose:velimpose,dthres:dthres,coldpts:coldpts,p2fill:p2fill,magmod:magmod} 
if n_elements(getawsom) ne 0 then  $
 pramarray={name:'awsom',cubename:pramcubename,nofield:nofield,$
		   bout:double(bout),bnorm:double(bnorm),$
	           hydro:double(hydro),densprof:densprof,te:te,$
		   velimpose:velimpose,dthres:dthres,coldpts:coldpts,p2fill:p2fill,magmod:magmod} 

; if requested, save input parameters to a file (stored in file named saveprams if it is a string)

if keyword_set(saveprams) then begin

     savefilename=saveprams
     if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then if working_dir ne '' then savefilename=working_dir+slash+saveprams

     case 1 of
         datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
         else: saveprams=pramarray
     endcase
endif

;Rotation

cuberot=double(cuberot)*!dpi/180.0 ;turn into a double and in radians

;Magnetic field

IF (Bout GT 3) OR (Bout LT 1) THEN BEGIN
     print, 'invalid Bout parameter in numcubeprams.pro. Now setting Bout=1, such that there is no ambient field outside the cube.'
     Bout=1
ENDIF

;Finishing

label=cube.title
name='numcube'
if n_elements(getawsom) ne 0 then begin
  if crot eq '' then crot=fix(tim2carr(date,/dc))
  label=strtrim(string(crot),2)+'_AWSOM'
  name='awsom'
endif
if n_elements(getmas) ne 0 then begin
  if crot eq '' then crot=fix(tim2carr(date,/dc))
;  label=strtrim(string(crot),2)+'_MAS_'+mheat 
;  label=label[0]
  name='psimas'
endif
outarray={Name:name,$
          Label:label,$
          Cuberot:Cuberot,$
          Bout:double(Bout),$
          bnorm:double(bnorm),$
          Hydro:double(Hydro),$
	  DensProf:VDensProf,$
	  NoField:NoField,$
          Te:Te,VelImpose:VelImpose,DThres:DThres,ColdPts:ColdPts,P2Fill:p2fill,MagMod:double(magmod),$
          DeltaR:DeltaR,CubeName:CubeName};cubename changes for each possible data cube, this parameter is an input

if tag_exist(cube,'topology') then begin
 outarray=add_tag(outarray,topology,'topology')
 outarray=add_tag(outarray,vodensprof,'ODensProf')
 outarray=add_tag(outarray,oT0,'OT0')
 outarray=add_tag(outarray,vcdensprof,'CDensProf')
 outarray=add_tag(outarray,cT0,'CT0')
endif

;print,cubename
;print,'crot=',crot,'date=',date

END
