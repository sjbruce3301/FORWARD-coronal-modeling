PRO TOMOPRAMS,outarray,tomofile=tomofile,$
          vfast=vfast0,vslow=vslow0, $
          ht_asympt_fast=ht_asympt_fast, ht_asympt_slow=ht_asympt_slow, $
	  simple=simple0, rpwr=rpwr0, te=te0,$
          date=date,working_dir=working_dir,$   
	        saveprams=saveprams,readprams=readprams0,nowidgmess=nowidgmess
;+
;
;Name: TOMOPRAMS
;
;Purpose: To create structure containing parameters that the tomography module uses;
; to extrapolate the density datacubes (based on measurements) to other heights.
; To be called by driver routine and resulting structure will be named 
; ModPramsStruct (with ModPramsStruct.name='tomo')
;
; The tomography method is described by
;         https://iopscience.iop.org/article/10.3847/1538-4357/ab7e32/meta
;         https://iopscience.iop.org/article/10.3847/1538-4365/ab125d/meta
;         https://iopscience.iop.org/article/10.1088/0067-0049/219/2/23/meta
; Please reference these works if you use this module for public work.
;
; The mean tomographical densities within streamers and coronal holes, as functions of height, are fitted to
; to a model velocity/acceleration profile assuming mass flux constant. This allows the extrapolation of
; densities and velocities throughout any coronal region. The velocity profile is given by the simple
; equation
;               v(r) = v_user * ( 1 - exp(-h/r_h) )
; where   v_user is the user-supplied vfast or vslow,
;         h is height ( = r - 1)
;         r_h is a scale height calculated by the tomo module to best fit the tomographical densities.
; Note this velocity allows acceleration up to the maximum v_user, over scale heights of r_H
;
; Based on this velocity profile, the densities at any given height are given by
;               p(r) = (p_f/( 1 - exp(-h/r_h) ) * (r_user/r)^2
; where   r_user is a user-supplied height where the outflow velocity reaches its asymptotic value
;         p_f is the density at r_user, calculated by the tomo module to best fit the tomographical densities.
;
; These equations are fitted separately to the mean streamer densities as given by the tomography maps, and
; the mean coronal hole densities. The final extrapolation then uses the longitude-latitude distribution
; of tomographical densities, plus the streamer/coronal hole height profiles, to calculate densities throughout
; the required coronal region. So, if, in the original tomography map a point has a density which is halfway between
; the maximum streamer and minimum coronal hole densities, it will be a mean of the streamer and coronal hole height profiles.
;
; Note that the module, given a required date, searches the FORWARD database for the tomographical map structure made
; closest in time. If this time is longer than 30 days a warning is given, but the module continues.
;
; 
;Keyword Inputs:
;
;Parameters to control the density extrapolation and velocity calculation.
;
;   TOMOFILE: filename (including extension) of the save file of 
;	tomographically determined densities found in $FORWARD_DB/TOMO directory 
;	Default will be set to $FORWARD_DB/tomo_20080301_densfix.dat
;
;     NOTE -- TOMOFILE OVERWRITES DATE IF SET AND DIFFERENT
;               BE CAREFUL FOR EXAMPLE IF YOU WANT CMER AND BANGLE TO BE FOR
;               A SOMEWHAT DIFFERENT DATE THAN THE ONE IN THE CUBE - in this
;               case, you should explicitly define CMER, and BANG as keywords
;               In other words -- if you use keyword TOMOFILE, DATE will
;               be completely ignored and actually replaced by date from name of TOMOFILE.
;               Note widget should not send TOMOFILE explicitly, that is,
;               for most calls it will send DATE but not TOMOFILE;
;               unless TOMOFILE as a file is selected via the widget
;               or if READPRAMS is set it will use that TOMOFILE
;               (if there is one; generally, it will not be saved in READPRAMS
;               unless it is an original keyword)
;
;  DATE: if this is set,  it will look for datacube
;       for this date (or close to it) in $FORWARD_DB/TOMO
;       If TOMOFILE set, it will overrule and overwrite DATE.
;
; **TO SUMMARIZE ** there are three different potential observer point of view inputs
;
;       1) CMER/BANG - if these are set as keywords, they take precedence and
;               *define* the observer's point of view
;               These keywords are not defined or used in this subroutine, however.
;
;       2) TOMOFILE - if explicitly set, this will be the datacube used
;               for the TOMO model-- the date associated with its filename is
;               the date the tomography is centered on. This will
;               overwrite/replace DATE even if it is explicitly set, and will define
;               CMER/BANG if they are *not* explicitly set in the function call
;                 (note, if TOMOFILE is changed in the widget, CMER/BANG will be updated)
;
;       3) DATE - if this is the only thing set, it will define CMER/BANG
;               as the Earth's view on that date;
;               also TOMOFILE as described above.
;                 (note, if DATE is changed in the widget, CMER/BANG and TOMOFILE
;                    will be updated)
;
; *So, for example, one might want to know what STEREO saw on a particular day and time:
;       one would set DATE to that day and time, CMER and BANG to STEREO's view for that day and
;       time, and then TOMOFILE would be a close-by time (but not exactly the same as DATE)
;       that represented the time of the boundary condition.
;       The DATE itself would not have a huge impact on the result, except to the extent that
;       some issues of instrument calibration have a dependency on DATE, and of course in
;       picking the TOMOFILE to use.
;
;       The FORWARD plot will indicate all three of these points of view in the plot title,
;       via  "TOMO Cube" (the tomographic reconstruction date associated with TOMOFILE),
;       "observer's date" (DATE), and then explicit CMER + BANG
;
;
;   VFAST: asymptotic speed of the fast solar wind in km/s. Default 700 km/s
;   
;   VSLOW: asymptotic speed of the slow solar wind in km/s. Default 300 km/s
;   
;   HT_ASYMPT_FAST: the distance at which the fast solar wind reaches its 
;                   maximum speed of VFAST. Default 1.5 solar radii (heliocentric)
;   
;   HT_ASYMPT_SLOW: the height at which the slow solar wind reaches its 
;                   maximum speed of VSLOW. Default 20 solar radii (heliocentric)
; 
;   SIMPLE: if set, just has density fall off with power rpwr
;		   DEFAULT 0 (unset)
;
;   RPWR: only used if SIMPLE is set, and then controls density falloff
;		   if simple set DEFAULT -2 (falls off as 1/r^2)
;		      otherwise set to 'NULL'
;
;   TE
;              isothermal temperature parameter value
;                       DEFAULT 1.5D6
;
;	
;   The above defaults are educated guesses. See https://iopscience.iop.org/article/10.3847/1538-4357/ab7e32/meta
;
; BOOKKEEPING
;
; SAVEPRAMS - if keyword set to a string, write parameters to filename
;                       (which is the keyword value saveprams)
;                       or if set to 1, then replace with pramarray
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
;               NAME -- tomo-- so that procedure can be called in intensint
;               LABEL -- Tomographical density -- for plot label
;               MAGMOD -- 0 -- meaning it is NOT a magnetized model
;
;Output: outarray - structure containing keyword output model parameters
;
;Common Blocks: None
;
; Called by FOR_MODELDEFAULTS
;
; Author and history:
;      Written by Huw Morgan Aug-Sept 2020
;       edited for FORWARD code consistency (SEG) Sept. 2020
;    Sept 2021 - passed through nowidgmess
;    Feb 2022 - added simple/rpwr keywords
;-

slash=path_sep()

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

if keyword_set(tomofile) then tomofilesave=tomofile else tomofilesave=''

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
;If keyword not specified, then use value from readprams if set. If readprams not set, 
;uses the default values as listed in these following statements

;
; if simple is set, a different set of parameters is used
;

simple=n_elements(simple0) eq 0?(n_elements(simple_rd) eq 0?0.:simple_rd):simple0

if simple eq 0 then begin

 vfast=n_elements(vfast0) eq 0?(n_elements(vfast_rd) eq 0?'700.d':string(vfast_rd)):$
       (strupcase(vfast0) eq 'NULL'?'700.d':string(vfast0));maximum speed

 vslow=n_elements(vslow0) eq 0?(n_elements(vslow_rd) eq 0?'300.d':string(vslow_rd)):$
       (strupcase(vslow0) eq 'NULL'?'300.d':string(vslow0))

 ht_asympt_fast=n_elements(ht_asympt_fast0) eq 0? $
          (n_elements(ht_asympt_fast_rd) eq 0?'1.5d':string(ht_asympt_fast_rd)):$
          (strupcase(ht_asympt_fast0) eq 'NULL'?'1.5d':string(ht_asympt_fast0))
		;height where wind reaches maximum speed

 ht_asympt_slow=n_elements(ht_asympt_slow0) eq 0? $
          (n_elements(ht_asympt_slow_rd) eq 0?'20.d':string(ht_asympt_slow_rd)):$
          (strupcase(ht_asympt_slow0) eq 'NULL'?'20.d':string(ht_asympt_slow0))

 rpwr='NULL'

endif else begin
 vfast='NULL'
 vslow='NULL'
 ht_asympt_fast='NULL'
 ht_asympt_slow='NULL'
 rpwr=n_elements(rpwr0) eq 0?(n_elements(rpwr_rd) eq 0?'-2.':string(rpwr_rd)):$
       (strupcase(rpwr0) eq 'NULL'?'-2.':string(rpwr0))
endelse

te=n_elements(Te0) eq 0?(n_elements(Te_rd) eq 0?1.5d6:Te_rd):Te0

;
; need to be careful with TOMOFILE and DATE
;

if not keyword_set(tomofile) and not keyword_set(date) then begin
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'No file or date entered, so will use default 20080301 datacube'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
endif

usetomo=0
if keyword_set(tomofile) then begin
 if file_exist(tomofile) then usetomo = 1 else begin
   if file_exist(file_dirname(GET_ENVIRON('FORWARD_DB'))+slash+file_basename(GET_ENVIRON('FORWARD_DB'))+slash+'TOMO_DB'+slash+tomofile) then usetomo=1 else begin
    tomofile=0
    if not keyword_set(date) then begin
     if keyword_set(nowidgmess) then message,/info,' The TOMO datacube referred to in this parameter file is not in the local directory or $FORWARD_DB/TOMO_DB, so will use default (20080301) datacube' else d = dialog('The TOMO datacube referred to in this parameter file is not in the local directory or $FORWARD_DB/TOMO_DB, so will use default (20080301) datacube',/warning) 
    endif else begin
     if keyword_set(nowidgmess) then message,/info,'The TOMO datacube referred to in this parameter file is not in the local directory, so will use date provided ' else d = dialog('The TOMO datacube referred to in this parameter file is not in the local directory, so will use date provided',/warning)
    endelse
   endelse
 endelse
endif

if not keyword_set(date) and usetomo eq 0 then begin
    tomofile=file_dirname(GET_ENVIRON('FORWARD_DB'))+slash+file_basename(GET_ENVIRON('FORWARD_DB'))+slash+'TOMO_DB'+slash+'tomo_20080301_densfix.dat'
    usetomo=1
endif

;
; TOMOFILE overwrites all
;

if usetomo eq 1 then begin

;
; use date ssociated with tomofile
;
 dateread=strmid(file_basename(tomofile),5,8)
 year=strmid(dateread,0,4)
 month=strmid(dateread,4,2)
 day=strmid(dateread,6,2)
 dateuse=year+'-'+month+'-'+day
 now=dateuse

endif else begin

;
; if DATE set and no TOMOFILE, get it from $DB
; use the closest available
;

 dateuse=date

 date_exist_db=file_search(file_dirname(GET_ENVIRON('FORWARD_DB'))+slash+file_basename(GET_ENVIRON('FORWARD_DB'))+slash+'TOMO_DB'+slash+'*',count=nfiles)
 date_array=(long(strmid(file_basename(date_exist_db),5,8)))
 dateread=strtrim(string(long(date_array)),2)
 year=strmid(dateread,0,4)
 month=strmid(dateread,4,2)
 day=strmid(dateread,6,2)

 nowarray=year+'-'+month+'-'+day

 datesec=anytim(date)
 datesec_array=anytim(nowarray)
 datediff=abs(datesec_array-datesec)
 test=where(datediff eq min(datediff))

 now=nowarray[test]
 now=now[0]
 tomofile=date_exist_db[test]
 tomofile=tomofile[0]
 print,'User date is:',date
 print,'Nearest date is:',now
 print,'Using nearest file:',tomofile

endelse

;
; information for label
;

exlab='!c observer date='+dateuse

label='Tomographical density, cube='+now+exlab
name='tomo'
magmod=0

;
; set up  pram input array
;

pramarray= {Name:name,$
  tomofile:tomofilesave,$
  vfast:vfast, $
  vslow:vslow, $
  ht_asympt_fast:ht_asympt_fast, $
  ht_asympt_slow:ht_asympt_slow,Te:Te,$
  simple:simple,rpwr:rpwr $
  }  

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

outarray= {Name:name,$
  Label:label,$
  vfast:vfast, $
  vslow:vslow, $
  ht_asympt_fast:ht_asympt_fast, $
  ht_asympt_slow:ht_asympt_slow, $
  simple:simple,rpwr:rpwr, $
  filename:tomofile,Te:Te,$
  magmod:magmod $
  }  

date=dateuse

END
