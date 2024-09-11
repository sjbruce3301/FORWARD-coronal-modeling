PRO PFSSMODPRAMS,outarray,pfssfile=pfssfile,$
                      date=date,working_dir=working_dir,$
		      rtop=rtop0, $
		      deltar=deltar0,$
                      hydro=hydro0,velimpose=velimpose0,topology=topology0,$
		      densprof=densprof,t0=t0,$
		      odensprof=odensprof,ot0=ot0,$
		      rindex=rindex0,$
		      nowidgmess=nowidgmess,verbose=verbose,$
                      readprams=readprams,saveprams=saveprams
;+
;Name: 
;   PFSSMODPRAMS
;
;Purpose:
;          To set up the structure containing the information needed
;          to run a pfss in the forward model. To  be
;          called by the driver routine, and the resulting structure
;          will be named ModPramsStruct 
;
;Keyword inputs:
;
;  PFSSFILE: Filename (inluding extention) of the save file you want to
;            look at  This can be gotten with the pfss_viewer SSWIDL
;            command, or by setting DATE and then later referring to
;	     saved file. You have to have the pfss package installed in
;            SSWIDL. Default is set to file in $FORWARD_DB$ directory. 
;		Inputted as a string.
;		PFSSFILE OVERWRITES RTOP and DATE AND TOPOLOGY IF SET and DIFFERENT.
;		BE CAREFUL FOR EXAMPLE IF YOU WANT CMER AND BANGLE TO BE FOR
;		A SOMEWHAT DIFFERENT DATE THAN THE ONE IN THE CUBE - in this
;		case, you should explicitly define CMER, and BANG as keywords
;		In other words -- if you use keyword PFSSFILE, DATE will
;		be completely ignored and actually replaced by NOW in PFSSFILE.
;		Note widget should not send PFSSFILE explicitly, that is,
;		for most calls it will send date but not pfssfile;
;		unless pfssfile as a file is selected via the widget
;		or if READPRAMS is set it will use that PFSSFILE
;		(if there is one; generally, it will not be saved in READPRAMS
;		unless it is an original keyword)
;
;  DATE: if this is set,  it will look for datacube
;	for this date (or close to it). If not in WORKING_DIR (or local if WORKING_DIR not
;	set) will download.  If PFSSFILE set, it will overrule and overwrite DATE.
;
; **TO SUMMARIZE ** there are three different potential observer point of view inputs
;
;	1) CMER/BANG - if these are set as keywords, they take precedence and
;		*define* the observer's point of view
;		These keywords are not defined or used in this subroutine, however.
;
;	2) PFSSFILE - if explicitly set, this will be the datacube used
;		for the PFSS model-- the date associated with it (NOW) is 
;		the date the boundary condition is centered on. This will
;		overwrite/replace DATE even if it is explicitly set, and will define
;		CMER/BANG if they are *not* explicitly set in the function call
;		  (note, if PFSSFILE is changed in the widget, CMER/BANG will be updated)
;
;	3) DATE - if this is the only thing set, it will define CMER/BANG 
;		as the Earth's view on that date;
;		also PFSSFILE as described above.
;		  (note, if DATE is changed in the widget, CMER/BANG and PFSSFILE
;		     will be updated)
;
; *So, for example, one might want to know what STEREO saw on a particular day and time:
;  	one would set DATE to that day and time, CMER and BANG to STEREO's view for that day and
; 	time, and then PFSSFILE would be a close-by time (but not exactly the same as DATE)
;	that represented the time of the boundary condition.
;	The DATE itself would not have a huge impact on the result, except to the extent that
;	some issues of instrument calibration have a dependency on DATE, and of course in 
;	picking the PFSSFILE to use.
;
;	The FORWARD plot will indicate all three of these points of view in the plot title,
;	via  "PFSS Cube" (NOW; the boundary condition date associated with PFSSFILE), 
;	"observer's date" (DATEUSE; will be DATE as set UNLESS the data cube doesn't exist and revert to 2005 default)
;	and then explicit CMER + BANG (either set by user or determined from DATE)
;	   note FORWARD variable DATE will be set to DATEUSE at bottom of file
;	   and there are print commands just before this that can be uncommented to track a change, e.g. to default
;
;  WORKING_DIR: see above
;
;  RTOP: the location of the pfss source surface. it defaults to 2.5
;	and only can be something different if you specify a DATE
;	and no PFSSFILE, or if it is different in the PFSSFILE 
;	from 2.5
;	NOTE: if set to negative -- will use abs. value for cube top,
;		but, will call PFSS_GET_POTL_COEFFS with no RTOP which forces
;		source surface at infinity
;
;  VELIMPOSE - impose a velocity of constant magnitude VELIMPOSE directed along the field
;               overwrites any velocity field already loaded in if nonzero
;		UNITS KM/SEC
;                       DEFAULT 0.d0
;
;  TOPOLOGY - preprocess PFSS datacube by drawing field lines through every point
;               takes a while, but allows storage of information about closed vs open
;               which then allows different hydrostatic models on each
;               also stores info needed for quick calculation of expansion factor (line=EXPFAC)
;            it defaults to 0 and only can be something different if you specify a DATE
;            and no PFSSFILE, or if it is different in the PFSSFILE from 0
;               If PFSSFILE set, it will overrule TOPOLOGY
;
;  DELTAR - step for radial derivative (e.g. LINE=NINST)
;			DEFAULT 0.01d0
;
;  RINDEX - array of r values for custom grid -- only used if rtop ne 2.5
;			also only usable with command line (not widget)
;		DEFAULT UNSET		
;
; HYDRO, DENSPROF, ODENSPROF, T0, OT0:
;  how to handle the plasma and (if TOPOLOGY set)in closed vs open regions
;       DEFAULT HYDRO 3 
;               Vasquez 2003
;       FOR DEFAULTS SEE NUMCUBE/FOR_HYDRODEFAULTS
;          (note that DENSPROF/T0 is treated as CDENSPROF/CT0)
;
;       BOOKKEEPING
;
;               SAVEPRAMS - if keyword set to a string, write parameters to filename
;                       (which is the keyword value saveprams)
;                       or if set to 1, then replace with pramarray
;
;
;               READPRAMS - if keyword set, read in parameters from filename
;                       (which is the keyword value filename)
;                       or if a structure than read directly
;                       NOTE, keywords set in for_drive call overwrite 
;				those set by READPRAMS
;
;;Output:  ModPramsStruct to be useed by numcube.pro
;
;               DATE    If numerical cube has a date within it, it will overwrite
;                       keyword DATE. 
;
;               DENSPROF, ODENSPROF -- may be converted to vectors by FOR_HYDRODEFAULTS
;               T0, OT0 -- may be changed in for_hydrodefaults
;
;
;  Calls: FOR_TRACEBACK
;  Called by FOR_MODELDEFAULTS
;
;-
; HISTORY written Laurel Rachmeler, Sarah Gibson
;
;       modified for hydrostatic model consistency SEG March 2011
;       changed to procedure and add readprams/saveprams Jan 2013 SEG
;	updated for rtop and date 	Apr 2013 SEG
;	updated again to change precedence of pfssfile, date, and rtop July 2013 SEG
;
; Version 2.0 July 2014
;   	changed flduse default to BPOS
;		added negative RTOP to signify SS at infinity
;		added messaging
;	Nov 2016 SEG
;	 changed order of variables saved in pfssfile -- should not make a difference
;	 also added pfss_restore call for final case of finding no file 
;	 - should not make a difference,before this was covered with call 
;	   to pfss_restore in pfssmod, which has now been removed	
; 	 also added TOPOLOGY
;	Jan 2017 SEG 
;	  added clarifying comments regarding DATE; 
;	  changed output of PRAMARRAY (used for READPRAMS/SAVEPRAMS) 
;	  so that PFSSFILE is only saved if a keyword input -- this avoids
;	  the widget unnecessarily sending PFSSFILE as a keyword and conflicting
;	  with DATE
;	  Also changed label to include Observer date, but only if it is not overwritten
;	   by PFSSFILE date
;	  Also made search window tighter and more flexible
;       Apr 2017 SEG - put in check for date before MDI
;	May 2017 SEG - updated to match changes to FOR_TRACEBACK
;       Sep 2018 SEG - fixed bug where odensprof was multiplied by 1d8 instead of 1d5
;		also fixed bug where hydro=2 crashed in widget because densprof was a scalar
;	Oct 2018 SEG - -added Vasquez hydrostatic model
;	Jun 2019  - used slash for PC compatibility
;		removed flduse options for torus instability
;		will be set to BHOR (horizontal-Bth^2+Bph^2)
;		made default OT0 1.5d6 for HYDRO=3
;	Sep 2019 -- expanded search to +/- 3 hours around input date instead of +/- 1 hour
;	          -- added hydro=4 option
;	Sep 2021 -- passed through nowidgmess
;	Jan 2022 -- added default 2005 10/30 topo file to dbase directory
;		added code below to go to that if download/local file fails
;		changed order of if usepfss conditional to allow change if no file found
;		made default date consistent throughout at 12:04 not 00:00
;	 Feb 2022 -- 
;				---> for_hydrodefaults
; 			Also moved defaults for topology and rtop to be with other defaults
;			also fixed a bug where date could be completely unset
;			 added default ''
;			edited pramarray/outarray to only display ot0,odensprof if topology=1
;	Dec 2022 -- fixed bug where input to pfssmodprams was cdensprof,cT0 
;		instead of odensprof, oT0
;	        And another one where dumarray was not being filled with
;			VC/VO Densprof
;	Jun 2023 -- fixed bug where pfssfile was overwritten by default date in widget
;		added ability to ingest STABLE output files
;	May 2024 -- added verbose keyword-- for now only controled by
;	        changing inside this code, default 0
;
;-

; COMMON BLOCKS: uses @pfss_data_block to access PFSS package common blocks

@pfss_data_block

;
; make sure network can be called
;
slash=path_sep()

use_network

; when downloading PFSS files, window number gets lost
if !d.name eq 'X' then winnumsave=!d.window


COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

;if keyword_set(pfssfile) then print,pfssfile
;if keyword_set(date) then print,date

if keyword_set(pfssfile) then pfssfilesave=pfssfile else pfssfilesave=''

;
; set parameter defaults
;

default,verbose,0

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

; this will register as unset, but not crash when printing
default,date,''

deltar=n_elements(deltar0) eq 0?(n_elements(deltar_rd) eq 0?0.01:deltar_rd):deltar0
rtop=n_elements(rtop0) eq 0?(n_elements(rtop_rd) eq 0?2.5:rtop_rd):rtop0
; source surface

velimpose=n_elements(velimpose0) eq 0?(n_elements(velimpose_rd) eq 0?0.0:velimpose_rd):velimpose0
; no field-aligned velocity

topology=n_elements(topology0) eq 0?(n_elements(topology_rd) eq 0?0.0:topology_rd):topology0

;Plasma parameters
hydro=n_elements(hydro0) eq 0?(n_elements(hydro_rd) eq 0?3:hydro_rd):hydro0

if exist(densprof) then densprofsave=densprof
if exist(T0) then T0save=T0
for_hydrodefaults,$
        hydro=hydro,cdensprof=densprof,cT0=T0,$
        odensprof=odensprof,oT0=oT0,$
        rcdensprof=densprof_rd,rcT0=T0_rd,$
        rodensprof=odensprof_rd,roT0=oT0_rd,$
        vodensprof=vodensprof,vcdensprof=vcdensprof
;
; note the HYDRO = 4  requires rerunning for_hydrodefaults
;  with HYDRO=3 for the closed field regions
;

hydrosave=hydro
if hydro eq 4 then begin
  for_hydrodefaults,$
        hydro=3,cdensprof=densprofsave,ct0=T0save,$
        rcdensprof=densprof_rd,rct0=T0_rd,$
        vcdensprof=vcdensprof
  densprof=densprofsave
  T0=T0save
endif
hydro=hydrosave

if not keyword_set(pfssfile) and not keyword_set(date) then begin
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'No file or date entered, so will use default (2005) datacube'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
endif

if keyword_set(date) then if anytim(date) lt anytim('1996-07-01T06:04:00.000') then begin
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'Date' + date + ' pre MDI,  so will use default (2005) datacube'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
  print,'*****************************************************************'
  date='2005-10-30T12:04'
  if keyword_set(nowidgmess) then message,/info,'Date pre MDI, so will use default (2005) datacube' else d=dialog('Date pre MDI, so will use default (2005) datacube',/warning)
endif

usepfss=0
if keyword_set(pfssfile) then begin
 if file_exist(pfssfile) then usepfss = 1 else begin
   pfssfile=0
   if not keyword_set(date) then begin
     if keyword_set(nowidgmess) then message,/info,'The PFSS datacube referred to in this parameter file is not in the local directory, so will use default (2005) datacube' else d = dialog('The PFSS datacube referred to in this parameter file is not in the local directory, so will use default (2005) datacube',/warning) 
   endif else begin
     if keyword_set(nowidgmess) then message,/info,'The PFSS datacube referred to in this parameter file is not in the local directory, so will use date provided' else d = dialog('The PFSS datacube referred to in this parameter file is not in the local directory, so will use date provided',/warning)
   endelse
 endelse
endif

if keyword_set(date) then timediffdefault= abs(anytim('2005/10/30T120400')-anytim(date)) else timediffdefault=0

if (not keyword_set(date) and usepfss eq 0) or (timediffdefault eq 0 and usepfss eq 0) then begin
  if not keyword_set(topology) then begin
        pfssfile=file_dirname(GET_ENVIRON('FORWARD_DB'))+slash+file_basename(GET_ENVIRON('FORWARD_DB'))+slash+'Bfield_20051030_120400.sav'
  endif else begin
        pfssfile=file_dirname(GET_ENVIRON('FORWARD_DB'))+slash+file_basename(GET_ENVIRON('FORWARD_DB'))+slash+'Bfield_20051030_120400_topo.sav'
  endelse
  usepfss=1
endif 

if usepfss eq 0 then begin

;
; if DATE set and no PFSSFILE, get it, from
; local directory or internet
;
      dateuse=str_replace(date,' ','_')
      time0=anytim(date,/ccsds)
      time0split=strsplit(time0,'T',/regex,/extract)
      time0date=strsplit(time0split[0],'-',/regex,/extract)
      year=time0date[0]
      month=time0date[1]
      day=time0date[2]
      time0time=str_replace(time0split[1],'.',':')
      time0time2=strsplit(time0time,':',/regex,/extract)
      hour=time0time2[0]
      minute=time0time2[1]
      second=time0time2[2]
;
; will first look for exact file, then within an hour
; note -- not robust to month end yet; will force reupload
;
      dateexact=year+month+day+'T'+hour+minute+second
      datehour=year+month+day+'T'+hour+'*'

      dayplus=day
      dayplustwo=day
      dayplusthree=day
      hourplus=strtrim(string(hour+1),2)
      hourplustwo=strtrim(string(hour+2),2)
      hourplusthree=strtrim(string(hour+3),2)
      if hourplus eq 24 then begin
       dayplus=strtrim(string(day+1),2)
       if dayplus le 9 then dayplus='0'+dayplus
       hourplus='0'
      endif
      if hourplustwo ge 24 then begin
       dayplustwo=strtrim(string(day+1),2)
       if dayplustwo le 9 then dayplustwo='0'+dayplustwo
       hourplustwo = strtrim(string(hourplustwo-24),2)
      endif
      if hourplusthree ge 24 then begin
       dayplusthree=strtrim(string(day+1),2)
       if dayplusthree le 9 then dayplusthree='0'+dayplusthree
       hourplusthree = strtrim(string(hourplusthree-24),2)
      endif
      if hourplus le 9 then hourplus='0'+hourplus
      if hourplustwo le 9 then hourplustwo='0'+hourplustwo
      if hourplusthree le 9 then hourplusthree='0'+hourplusthree

      dayminus=day
      dayminustwo=day
      dayminusthree=day
      hourminus=strtrim(string(hour-1),2)
      hourminustwo=strtrim(string(hour-2),2)
      hourminusthree=strtrim(string(hour-3),2)
      if hourminus eq -1 then begin
       dayminus=strtrim(string(day-1),2)
       if dayminus le 9 then dayminus='0'+dayminus
       hourminus='23'
      endif
      if hourminustwo le -1 then begin
       dayminustwo=strtrim(string(day-1),2)
       if dayminustwo le 9 then dayminustwo='0'+dayminustwo
       hourminustwo=strtrim(string(hourminustwo+24),2)
      endif
      if hourminusthree le -1 then begin
       dayminusthree=strtrim(string(day-1),2)
       if dayminusthree le 9 then dayminusthree='0'+dayminusthree
       hourminusthree=strtrim(string(hourminusthree+24),2)
      endif
      if hourminus le 9 then hourminus='0'+hourminus
      if hourminustwo le 9 then hourminustwo='0'+hourminustwo
      if hourminusthree le 9 then hourminusthree='0'+hourminusthree

      datehourplus=year+month+dayplus+'T'+hourplus+'*'
      datehourminus=year+month+dayminus+'T'+hourminus+'*'
      datehourplustwo=year+month+dayplustwo+'T'+hourplustwo+'*'
      datehourminustwo=year+month+dayminustwo+'T'+hourminustwo+'*'
      datehourplusthree=year+month+dayplusthree+'T'+hourplusthree+'*'
      datehourminusthree=year+month+dayminusthree+'T'+hourminusthree+'*'
      datearray=[dateexact+'*',datehour,datehourplus,datehourminus,datehourplustwo,datehourminustwo,datehourplusthree,datehourminusthree]

      if verbose eq 1 then begin
       print,'Searching for local file'
       print,datearray
      endif

      for idate=0,7 do begin
       checkfile='Bfield_'+datearray[idate]
       if n_elements(working_dir) eq 1 then if working_dir ne '' then checkfile=working_dir+slash+'Bfield_'+datearray[idate]

       if rtop ne 2.5 then begin
        if rtop gt 0 then checkfile=checkfile+'_'+strmid(strtrim(string(rtop),2),0,4)
        if rtop lt 0 then checkfile=checkfile+'_'+strmid(strtrim(string(abs(rtop)),2),0,4)+'_inf'
       endif
       if topology ne 0 then begin
        checkfile=checkfile+'_topo'
       endif 

      if verbose eq 1 then $
       print,checkfile

       if file_exist(checkfile+'.sav') then pfssfile=findfile(checkfile+'.sav')
       if file_exist(checkfile+'.h5') then pfssfile=findfile(checkfile+'.h5')

       if keyword_set(pfssfile) then begin
        if verbose eq 1 then $
	 print,'found it'
         idate=8
       endif
      endfor

      if keyword_set(pfssfile) then begin
          pfssfile=pfssfile[0] 
	  pfss_restore,pfssfile
      endif else begin
        if year gt 2999 and topology ne 0 then begin
         pfssfile='Bfield_'+dateexact+'.sav'
         print, 'special case, STABLE file'
         print,pfssfile
         pfss_restore,pfssfile
         now=dateuse
        endif else begin 
 	 print,'No luck, downloading'
	 if rtop eq 2.5 then begin
  	   pfss_restore,pfss_time2file(dateuse,/ssw_cat,/url) 
	 endif else begin
	   smap=pfss_surffield_restore(pfss_time2file(dateuse,/ssw_cat,/url,/surffield))
	   pfss_mag_create,magmap,0,nlat,file=smap
	   if rtop gt 0 then begin
            pfss_get_potl_coeffs,magmap,rtop=rtop 
            if keyword_set(rindex) eq 0 then pfss_potl_field,rtop,2,/trunc else $
                             pfss_potl_field,rtop,3,rindex=rindex,/trunc
           endif else begin
; infinity source surface
	     pfss_get_potl_coeffs,magmap
             if keyword_set(rindex) eq 0 then pfss_potl_field,abs(rtop),2,/trunc else $
                             pfss_potl_field,abs(rtop),3,rindex=rindex,/trunc
           endelse
	 endelse
	endelse
        if keyword_set(now) then begin
          nowset = 1 
          timediff= abs(anytim(now)-anytim(dateuse))
	endif else begin
          nowset = 0
	  timediff = 99999.
	endelse
        if nowset eq 1 and timediff lt 10800. then begin
;print,'created now ',now
	 nownow=str_replace(now,'T','_')
	 nownow=str_replace(now,' ','_')
	 nownow=str_replace(nownow,'-','')
	 nownow=str_replace(nownow,':','')
	 nownow=str_replace(nownow,'/','')
	 nowsplit=strsplit(nownow,'.',/extract)
         checkfile='Bfield_'+nowsplit[0]
         if n_elements(working_dir) eq 1 then if working_dir ne '' then checkfile=working_dir+slash+'Bfield_'+nowsplit[0]
         if rtop ne 2.5 then begin
          if rtop gt 0 then checkfile=checkfile+'_'+strmid(strtrim(string(rtop),2),0,4)
          if rtop lt 0 then checkfile=checkfile+'_'+strmid(strtrim(string(abs(rtop)),2),0,4)+'_inf'
         endif
         pfssfile=checkfile+'.sav'
	 if topology ne 0 then begin
 	  if keyword_set(nowidgmess) then message,/info,'Doing a run with keyword topology set takes a looooooong time (like, 24 hours) because it traces field lines through every point, but, it will resave the PFSS datacube with _topo added to its name so that you should not have to rerun in future' else d = dialog('Doing a run with keyword topology set takes a looooooong time (like, 24 hours) because it traces field lines through every point, but, it will resave the PFSS datacube with _topo added to its name so that you should not have to rerun in future',/warning)
	  nrix=size(rix)
	  nrix=nrix[1]
	  ntheta=size(theta)
	  ntheta=ntheta[1]
	  nphi=size(phi)
	  nphi=nphi[1]
	  rcube=fltarr(nphi,ntheta)
	  thcube=fltarr(nphi,ntheta)
	  phcube=fltarr(nphi,ntheta)
	  Brphot1=Br*0.
	  Bthphot1=Br*0.
	  Bphphot1=Br*0.
	  Brphot2=Br*0.
	  Bthphot2=Br*0.
	  Bphphot2=Br*0.
	  llen=Br*0.
	  open=Br*0.
;
; note, this is ModPramsStruct, but
; we need to set topology=0 because FOR_TRACEBACK
; calls PFSSMOD and PFSSMOD will try to look for OPEN
; field lines in datacube which has not yet been saved
; if TOPOLOGY is set. This only affects density and pressure,
; which FOR_TRACEBACK does not use.
;
; FROM PRSS_TRACE_FIELD.PRO: 
; (by default, closed field lines are oriented so that the
;     end with negative polarity comes first and open 
;     field lines are oriented so that the photospheric
;     end comes first

          dumarray={Name:'pfssmod',$
           Label:'PFSS '+now,$
           RTop:RTop,$
           Hydro:double(Hydro),$
           DensProf:VCDensProf,T0:T0,$
           ODensProf:VODensProf,OT0:OT0,$
           velimpose:velimpose,topology:0,$
           filename:pfssfile,DeltaR:DeltaR,MagMod:fix(1)}

          for i=0,nrix-1 do begin
 	   rcube[*,*]=rix[i]
	   print,'height=',rix[i]
           for j=0,ntheta-1 do thcube[*,j]=replicate(theta[j],nphi) 
           for k=0,nphi-1 do phcube[k,*]=replicate(phi[k],ntheta) 
	   Br2d=Br[*,*,i]
	   Bth2d=Bth[*,*,i]
	   Bph2d=Bph[*,*,i]
           for_traceback,rcube,thcube,phcube,Br2d,Bth2d,Bph2d,Brphot12d,Brphot22d,Bthphot12d,Bthphot22d,Bphphot12d,Bphphot22d,linelengths2d,open2d,dumarray
	   Brphot1[*,*,i]=Brphot12d
	   Bthphot1[*,*,i]=Bthphot12d
	   Bphphot1[*,*,i]=Bphphot12d
	   Brphot2[*,*,i]=Brphot22d
	   Bthphot2[*,*,i]=Bthphot22d
	   Bphphot2[*,*,i]=Bphphot22d
	   open[*,*,i]=open2d
	   llen[*,*,i]=linelengths2d
          endfor
          checkfile=checkfile+'_topo'
          pfssfile=checkfile+'.sav'
 	  save,BR,BTH,BPH,NLAT,NLON,NR,LAT,LON,RIX,THETA,PHI,L0,B0,NOW,PHIAT,PHIBT,Brphot1,Brphot2,Bthphot1,Bthphot2,Bphphot1,Bphphot2,llen,open,filename=pfssfile
         endif else begin
          save,BR,BTH,BPH,NLAT,NLON,NR,LAT,LON,RIX,THETA,PHI,L0,B0,NOW,PHIAT,PHIBT,filename=pfssfile
	 endelse
        endif else begin
         if keyword_set(nowidgmess) eq 0 then message,/info,'No PFSS file found online, going to default (2005-10-30T12:04).' else d=dialog('no PFSS file found online, going to default (2005-10-30T12:04).',/WARNING)
         if topology eq 0 then begin
           pfssfile=file_dirname(GET_ENVIRON('FORWARD_DB'))+slash+file_basename(GET_ENVIRON('FORWARD_DB'))+slash+'Bfield_20051030_120400.sav'
         endif else begin
           pfssfile=file_dirname(GET_ENVIRON('FORWARD_DB'))+slash+file_basename(GET_ENVIRON('FORWARD_DB'))+slash+'Bfield_20051030_120400_topo.sav'
         endelse
	 usepfss = 1
        endelse
      endelse
endif

;
; PFSSFILE overwrites all
;

if usepfss eq 1 then begin

	  pfss_restore,pfssfile
;
; check rtop in pfssfile for radial field
;  bearing in mind if set to infinity
;  will fail this test (for radial field at top of box)
;  so first figure out if set to infinity ("inf" should be in file name)
;
        if strpos(strupcase(pfssfile),'INF') gt 0 then begin
	  rtop=-1*rix[nr-1]
;
; this is really box top -- will be the same unless set to infinity
;
        endif else begin
	 rtop=rix[nr-1]
 	 btest=bth^2+bph^2
	 if max(abs(btest[*,*,nr-1])) gt 1d-4 then begin
 	   if keyword_set(nowidgmess) then message,/info,'source surface does not appear to be at top of box' else d = dialog('source surface does not appear to be at top of box',/warning)
 	 endif
        endelse

;
; check topology in pfssfile
; and force consistency
; including looking for "topo" in file name
;
        if strpos(strupcase(pfssfile),'TOPO') gt 0 then begin
	 topology=1
        endif else topology=0
; 
; use date set in pfssfile
;
; Be careful here-- for the standard de Rosa data cubes the format
; will have a T splitting date and time, but some custom generated files
; won't.  If you make a custom generated file using FORWARD, this will be fixed.
; But if for example you make the potential field custom cube using PFSS IDL codes
; outside of FORWARD you may need to go in and change now so that this works.
;
;print,'read-from-file now', now

	nownow=str_replace(now,'T',' ')
	nowsplit=strsplit(nownow,' ',/regex,/extract)
	nowmin=str_replace(nowsplit[1],'.','_')
	nowtime=strsplit(nowmin,'_',/regex,/extract)
 	nowdate=nowsplit[0]+'_'+nowtime[0]
	dateuse=nowdate
endif

;put all parameters into the output array, which will be called ModPramsStruct

;
; information for label
;

;if str_replace(date,' ','_') eq dateuse then exlab='!c observer date='+dateuse else exlab=''
exlab='!c observer date='+dateuse 

 outarray={Name:'pfssmod',$
          Label:'PFSS cube='+now+exlab,$
          RTop:RTop,$
          Hydro:double(Hydro),$
          DensProf:VCDensProf,T0:T0,$
          velimpose:velimpose,topology:topology,$
          filename:pfssfile,DeltaR:DeltaR,MagMod:fix(1)}

pramarray={Name:'pfssmod',pfssfile:pfssfilesave,rtop:rtop,deltar:deltar,hydro:double(hydro),densprof:densprof,T0:T0,topology:topology,velimpose:velimpose}

if keyword_set(rindex) eq 1 then begin
 pramarray=add_tag(pramarray,rindex,'rindex')
 outarray=add_tag(outarray,rindex,'rindex')
endif

if topology eq 1 then begin
 pramarray=add_tag(pramarray,odensprof,'odensprof')
 pramarray=add_tag(pramarray,oT0,'oT0')
 outarray=add_tag(outarray,vodensprof,'odensprof')
 outarray=add_tag(outarray,oT0,'oT0')
endif

; if requested, save input parameters to a file (stored in file named saveprams if it is a string)

if keyword_set(saveprams) eq 1 then begin

     savefilename=saveprams
     if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then if working_dir ne '' then savefilename=working_dir+slash+saveprams

     case 1 of
         datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
         else: saveprams=pramarray
     endcase
endif

if verbose eq 1 then begin
 print,'now= ',now
 print,'date= ',date
 print,'dateuse= ',dateuse
endif

date=dateuse

if !d.name eq 'X' then wset,winnumsave
;print,'d_window 2',!d.window

END
