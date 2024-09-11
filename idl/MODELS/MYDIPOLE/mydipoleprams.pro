PRO MYDIPOLEPRAMS,outarray,date=date,working_dir=working_dir,$
                   bnorm=bnorm0,thetopen=thetopen0,$
	           hydro=hydro0,$
		   cdensprof=cdensprof,ct0=ct0,$
 		   odensprof=odensprof,ot0=ot0,$
		   velimpose=velimpose0,$
		   saveprams=saveprams,readprams=readprams
;+
;
;Name: MYDIPOLEPRAMS
;
;Purpose: To create structure containing information about a magnetic dipole model;
; To be called by driver routine and resulting structure will be named 
; ModPramsStruct (with ModPramsStruct.name='mydipole')
;
; Called by FOR_MODELDEFAULTS
;
;Keyword Inputs:
;
; PHYSICAL PROPERTIES
;
;  BNORM: magnitude of dipole at photosphere
;                       DEFAULT 10 Gauss
;
;  THETOPEN: latitude at photosphere (north and south)
;	above which field is open
;
;
;  VELIMPOSE - impose a velocity of constant magnitude VELIMPOSE directed along the field
;               in open regions.
;                       UNITS KM/SEC
;                       DEFAULT 0.d0
;
; HYDRO, CDENSPROF, ODENSPROF, CT0, OT0:
;  how to handle the plasma throughout the corona.
;       DEFAULT HYDRO 3
;               Vasquez 2003
;
;       FOR CDENSPROF, ODENSPROF, CTO, OT0 DEFAULTS SEE NUMCUBE/FOR_HYDRODEFAULTS

;
; BOOKKEEPING
;
;
;
; SAVEPRAMS - if keyword set to a string, write parameters to filename
;                       (which is the keyword value saveprams)
;                       or if set to 1, then replace with pramarray
;
;
; READPRAMS - if keyword set, read in parameters from filename
;                       (which is the keyword value filename)
;                       or if a structure than read directly
;                       NOTE, flagged keywords will overwritten
;
; ModPramsStruct Outputs:
;
;               As above, plus
;
;               NAME -- mydipole-- so that procedure can be called in intensint
;               LABEL -- Dipole Field -- for plot label
;               MAGMOD -- 1 -- meaning it is a magnetized model
;               CDENSPROF, ODENSPROF -- may be converted to vectors by FOR_HYDRODEFAULTS
;               CT0, OT0 -- may be changed in for_hydrodefaults

;
;Output: outarray - structure containing keyword output model parameters
;
;Common Blocks: None
;
; Author and history:
;	Written by Sarah Gibson October 2017
;	 Sep 2018 adjusted hydrostatic model
;	 Oct 2018 - added hydro=3, Vasquez model option
;        1-Jun-2019 Removed usewindows, used slash instead for PC compatibility
;               made default OT0 1.5d6 for HYDRO=3
;	 Sep 2019 added Cranmer Hydro=4
;	 Feb 2022 cleaned up hydro etc
;		passed through 6-D vector for densprof/odensprof for hydro > 1
; 	   also simplified default statements
;	   and changed TE, DENSPROF to CT0, CDENSPROF
;		changed B0 to Bnorm to avoid conflict with solar B angle
;	Apr 2024 -- changed name to MYDIPOLE
;
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

bnorm=n_elements(bnorm0) eq 0?(n_elements(bnorm_rd) eq 0?10.0:bnorm_rd):bnorm0
;10 Gauss, default strength of the external field

thetopen=n_elements(thetopen0) eq 0?(n_elements(thetopen_rd) eq 0?40.0:thetopen_rd):thetopen0
;40 Gauss, default strength of the external field

velimpose=n_elements(velimpose0) eq 0?(n_elements(velimpose_rd) eq 0?0.0:velimpose_rd):velimpose0
; no field-aligned velocity

;Plasma parameters
hydro=n_elements(hydro0) eq 0?(n_elements(hydro_rd) eq 0?3:hydro_rd):hydro0

if hydro eq 0 then begin
  print,'hydro = 0 not allowed. Resetting to default hydro=3 (Vasquez profiles)'
  hydro=3
endif

if exist(cdensprof) then cdensprofsave=cdensprof 
if exist(ct0) then ct0save=ct0
for_hydrodefaults,$
        hydro=hydro,cdensprof=cdensprof,ct0=ct0,$
        odensprof=odensprof,oT0=oT0,$
        rcdensprof=cdensprof_rd,rct0=cT0_rd,$
        rodensprof=odensprof_rd,rot0=oT0_rd,$
        vodensprof=vodensprof,vcdensprof=vcdensprof

;
; note the HYDRO = 4 requires rerunning for_hydrodefaults 
;  with HYDRO=3 for the closed field regions
;

hydrosave=hydro
if hydro eq 4 then begin
  for_hydrodefaults,$
        hydro=3,cdensprof=cdensprofsave,ct0=ct0save,$
        rcdensprof=cdensprof_rd,rct0=cT0_rd,$
        vcdensprof=vcdensprof
  cdensprof=cdensprofsave
  ct0=ct0save
endif
hydro=hydrosave

pramarray={name:'mydipole',bnorm:double(bnorm),thetopen:double(thetopen),$
	           hydro:double(hydro),cdensprof:cdensprof,ct0:ct0,velimpose:velimpose,odensprof:odensprof,ot0:ot0} 

; if requested, save input parameters to a file (stored in file named saveprams if it is a string)

if keyword_set(saveprams) then begin

     savefilename=saveprams
     if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then begin
      if working_dir ne '' then savefilename=working_dir+slash+saveprams
     endif

     case 1 of
         datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
         else: saveprams=pramarray
     endcase
endif


label='Dipole field'
name='mydipole'
outarray={Name:name,$
          Label:label,$
          bnorm:double(bnorm),$
	  thetopen:double(thetopen),$
          Hydro:double(Hydro),$
	  MagMod:1,$
	  CDensProf:VCDensProf,$
	  ODensProf:VODensProf,$
          CT0:CT0,OT0:OT0,VelImpose:VelImpose}

END
