PRO for_plotfits, filename=filename,dodisk=dodisk, noplots=noplots,ImageMap=ImageMap,$
              psplot=psplot,gif=gif,tiff=tiff,jpeg=jpeg,eps=eps,$
              usecolor=usecolor,docont=docont,colortable=colortable,cpos=cpos,plotlog=plotlog,$
	      winnumber=winnumber,nwinx=nwinx,nwiny=nwiny,units=units,$
              whitedisk=whitedisk,occult=occult,upoccult=upoccult,$
	      axiscolor=axiscolor,bgcolor=bgcolor,useid=useid,nlev=nlev,cgsmult=cgsmult,$
              rotate_im=rotate_im,sunedge=sunedge,oldcomp=oldcomp,ucomp=ucomp,$
	      dispexp=dispexp,dispgamma=dispgamma,rpow=rpow,nobangletitle=nobangletitle,$
	      stklines=stklines,stkcolor=stkcolor,stkthick=stkthick,sminuse=sminuse,pscale=pscale,nstarrow=nstarrow,arrowave=arrowave,$
              xxmax=xxmax,xxmin=xxmin,yymax=yymax,yymin=yymin,limb=limb,rheight=rheight,$
              mapname=mapname,extratitle=extratitle,title=title,xtitle=xtitle,ytitle=ytitle,$
              imax=imax,imin=imin,ngrid=ngrid,gridtype=gridtype,redomap=redomap,typecomp=typecomp,typekcor=typekcor,wlcomp=wlcomp,$
              charsize=charsize,charthick=charthick,nocontcolor=nocontcolor,c_charsize=c_charsize,$
              noerase=noerase,noclabel=noclabel,moreplots=moreplots,$
	      nulldatacolor=nulldatacolor,blake=blake,rfilter=rfilter,savescale=savescale,$
	      euvia=euvia,euvib=euvib,eit=eit,comp=comp,aia=aia,swap=swap,kcor=kcor,cormag=cormag,xrt=xrt,doppler=doppler,date=date,working_dir=working_dir,nowidgmess=nowidgmess,$
	      instrument=instrument,line=line,GridPramsStruct=GridPramsStruct,savemap=savemap
;+
;
; This program plots a fits file so that it looks
; like the output from the forward model.
;
;
; NOTE: Only confirmed to work with SDO/AIA, SOHO/EIT, and STEREO/EUVIA&B data,
; 		Hinode/XRT, SWAP, MLSO/KCOR also COMP linear polarization and velocity.
;		Currently testing capabilities for synoptic maps
;
; 	For EIT - having a local copy of calibration files in SSWDB is required (otherwise the image will look crummy).
;		see http://umbra.nascom.nasa.gov/eit/eit_guide/software.htm
;
;	When calling this program with FILENAME input,  you might run into trouble if the name 
;		of the instrument is not part of the filename. This can be fixed by explicitly setting the
;		INSTRUMENT as an input.
;
; Inputs:
;
;      FILENAME: the name of the fits file as a string.
;		or the name of the save file
;		NOTE: KCOR Carrington save maps will have an extra string of "EAST" or "WEST" for limb
;			but fits file will include both limbs
;
;      types of save files: PSPLOT (post script file same) with EPS option, GIF, TIFF,
;      JPEG
;
;      USECOLOR, default 3 (heatscale), set to 0 for grayscale, or
;      number of color table for something else.
;
;      DOCONT, draw contours on the image. default, 1, yes
;
;      PLOTLOG, set if you want the plot in log
;
;      COLORTABLE show color table intesnity bar
;
;      WINNUMBER, window number in which plot is drawn
;
;	WHITEDISK: plot disk white (1) or black (0 or 2 - min dat vs imin)
;
;      OCCULT, UPOCCULT - only show data in between
;
;      AXISCOLOR, axis color
;      BGCOLOR, background color
;
;      ROTATE_IM, angle in degrees to rotate image CLOCKWISE. default, 0
;		NOT CURRENTLY IMPLEMENTED
;      
;      NWINX, width of viewing window, pixel size of image
;
;      NWINY, height of viewing window, pixel size of image
;
;      XXMAX,XXMIN,YYMAX,YYMIN, ranges for x(horizontal) and
;      y(vertical) of your plot in solar radii. default, size of full
;      image
;
;      MAPNAME, name of saved plot. default filename of image
;
; 	GRIDTYPE, plane of sky or Carrington map (PLANEOFSKY or CARRMAP)
;          LIMB, RHEIGHT-- for Carrington map
;
;      EXTRATITLE, stuff to add to the plot title, must be string
;
;      TITLE,XTITLE,YTITLE as in FOR_PLOT, allows overwriting of standard title, or
;	 if title='ZZ', allows no title
;
;      IMAX,IMIN, max and min levels for plot. if plotlog is set,
;      these are max and min exponents.
;
;      CHARSIZE, size of character in plot
;
;      CHARTHICK, thickness of character in plot
;
;      NOERASE, dont erase (if turned on after a previous plot will overplot contours on that one)
;
;      NOCLABEL, dont label contours
;
;      C_CHARSIZE contour label size
;
;      MOREPLOTS, turn on if want to make ps with multiple plots using
;      noerase;
;
;      SUNEDGE: plot yellow sun edge
;
;  	NULLDATACOLOR: color to make NAN data
;
;	NOPLOT: no call to for_plot
;
;	DATE, WORKING_DIR
;
;       LINE: wavelength. Defaults set below for different instruments.
;
;       REDOMAP: make save set fresh from fits, even if a version exists
;
;       INSTRUMENT: name of instrument to call VSO by date 
;		default 'AIA'
;
;      STKCOLOR, color for the stokeslines. default 4,blue
;      STKTHICK, thickness for the stokeslines. default 1.5
;      STKLINES, set if you want the stokes lines plotted
;
;      PSCALE, size of stokes lines. see for_plotstokeslines for more
;      info. default varies with number of lines plotted
;
;      NSTARROW, ngrid/nstarrow is the number of points to skip when
;      plotting stokeslines. default, 50
;
;      ARROWAVE - option to average over skipped points - default, average
;
;      TYPECOMP - type of CoMP file to look for - QUICKINVERT, DYNAMICS, or POLARIZATION
;		default QUICKINVERT
;		*note, UCoMP data will have combined dynamics/polarization typecomp=L1
;		* and will also vary depending on WLCOMP-- only 10747/10798 have polarization
;
;      CGSMULT - convert CoMP data from PPM to ERG/CM2/S/SR
;		default 1 
;
;      OLDCOMP - use old (2005) CoMP quickinvert file
;
;      UCOMP - assume UCoMP fits file format
;
;      WLCOMP - CoMP wavelength, right now just 10747 and 10798 for polarization
;		also will hold CORMAG wavelength 5303
;
;      NOWIDGMESS - don't show messages in DIALOG widget mode,
;		rather, use MESSAGE

;
; OUTPUTS, a lovely plot of your fits data
;	if ImageMap set, also the map is passed backe
;	and if SaveMap - the map is saved in file of name MapName
;	The structure GridPramsStruct can also be explicitly returned.
;
; Called by FOR_WIDGETEVENT, FOR_VIEWFROMDATA
; 
; Calls FOR_SETMINMAXDATE, FOR_NAMECHECKFILE, FOR_DIRECTDOWNLOAD
;       FOR_READCOMPFITS, FOR_PLOTSTOKESLINES, FOR_GETFILEINFO, 
;	DOBLAKE 	
;	VSO_SEARCH, VSO_GET, READ_SDO, READ_EIT, AIA_PREP, SECCHI_PREP
;
; NEEDS PACKAGES ONTOLOGY (for VSO), AIA, SECCHI, EIT
;
; Written by Sarah Gibson, Laurel Rachmeler 2012-2014
; Version 2.0 July 2014
;--
;
; Feb 2018 -- updated call to for_directdownload to include keywords line and nadd
;	also added nadd to ID
; July 2018 - added BGCOLOR
; June 2019 - used file_move, file_copy, file_delete slash for PC compatibility
;  forced overwrite for file_copy
;  (some heritage usewindows to be safe)
; May 2020 - added carrington map capability -- testing with MLSO data
; July 2020 -- added working_dir to call to for_directdownload
;	enabled BY DATE KCOR download
;	also added minmax, filter choices for KCOR
; Sept 2020 -- edited line making negative data for intesnity = -8888
;		so didn't apply to KCOR	
; Oct 2020 -- edited for Carrington map capability
;		passed through limb and rheight
;		moved some lines of code around
; Jan 2021 -- forced VSO server 
;	but then commented out because fixed - so back to default
;   also commented out COMPILE_OPT because it was breaking in fmt_tag
; July 2021 -- made noise structure empty except for donoise -- removed call to 
;	for_noisedefaults
; August 2021 -- moved plotting defaults out to subroutine for_plotfitsdefaults
;  and moved call to within for_plot call (helped with e.g. KCOR typekcor changing
;  also added dispgamma, dispexp, and rpow to call to for_plot
;  also moved 'ps' stuff inside the noplots=0 conditional (although not really working)
;  also forced overwrite for file_move -- this wasnt and issue before - if file 
;   exists shouldnt re download -- but the issue is that old data might be reassigned
;   typekcor = 'FIRSTL1' in which case won't be checked for. This problem will go away
;   when KCOR calibration is imposed across all data.
;   fixed some mis-indentations:~ line 646
;   removed for_noisedefaults call
;  also simplified foundsavemap search; no need for explicilty excluding _SUB
;   because not part of checkfile; also added check for CARRMAP both there and in the breakfile below
; Sept 2021 -- expanded NoisePrams and added direct calls to for_specdefaults and for_compdefaults to fill structures
;	made ObsPramsSTruct complete so wouldn't create errors with printresult list
;	also made sure ngrid was integer in file naming
;       also put in check for BYFILE if gridtype changes and overwrite
;	 and same for LIMB if SAV file
;	also commented out lines saving copy with our naming convention since it is done
;	earlier, and since if direct for_plotfits run caused problems
;       replaced all "if ne 'CARRMAP'" with "if eq 'PLANEOFSKY'" because there should be no 'USER' here
;	Added file_delete of IONEQ 
;       changed inst to instr in for_specdefaults call because of potential for confusion with system var
;	passed through more nowidgmess
; Oct 2021 -- updated defaults on occult/upoccult
;		added yeskcor variable to allow setting of occulter for both KCor and CoMP to 1.05
; Nov 2021 -- added print statement after saving map
; Dec 2021 -- added * to typecomp QUICKINVERT to allow additional naming information
;		also passed through nadd name
; Mar 2022 -- changed xoffset --> losoffset
; May 2022 -- checked backward compatibility for LOSOFFSET, AZEQUI, DISTOBS, NOSTRETCH, added AZEQUI and DISTOBS to gridpramsstruct and nostretch 
; Jun 2022 -- added keyword bangletitle
; Feb 2023- changed instr to instrument in call to for_specdefaults
; Apr 2023 -- took out stop for KCOR file type check to allow non average plane of sky images to go through
;    		hopefully this wont cause trouble?
; Aug 2023 -- added UCoMP hooks
; Oct 2023 -- passed line through for_specdefaults
; Dec 2023 -- more ucomp updates (call to for_directdownload include ucomp)
; Feb 2024 -- changed pointer to MLSO calendar
;	changed default imin imax for linewidth for mlso
;	saved PlotSave to save file allow recovery of plotting options 
;	more ucomp updates
; Jun 2024 -- added BPOS column variables
; Jul 2024 -- removed StokesStruct from call to getfileinfo because no longer uses it
;		also edited treatment of subcentI and removed eqwidth as unnecessary
;		And changed dummy vars in LOSPRAMSSTRUCT to 'NULL' instead
;		 of 9999, and to 'NULL' in LOSINPUTS
; Aug 2024 -- added WLRAT hooks
;

;COMPILE_OPT IDL2 ;default long and square brackets for array subscripts


common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops


if n_elements(flag) eq 0 then flag={magmod:'',tab:0,reset:-1,widgregen:0,noerase:0,dta:0,mdl:0,rmp:0,obs:0,noise:0,rfil:0}

mdtor=!dpi/180d0

; when downloading data, sometimes window number gets lost
winnumsave=!d.window

usewindows=0
if strupcase(!version.os_family) eq 'WINDOWS' then usewindows=1
slash=path_sep()

;set defaults
; (note plotting defaults moved into for_plotfitsdefaults
;   and called just before for_plot)

if keyword_set(aia) then instrument='aia'
if keyword_set(xrt) then instrument='xrt'
if keyword_set(euvia) then instrument='euvia'
if keyword_set(euvib) then instrument='euvib'
if keyword_set(eit) then instrument='eit'
if keyword_set(comp) then instrument='comp'
if keyword_set(swap) then instrument='swap'
if keyword_set(instrument) then if strupcase(instrument) eq 'SWAP' then swap=1
if keyword_set(kcor) then instrument='kcor'
if keyword_set(cormag) then instrument='cormag'
if keyword_set(instrument) then if strupcase(instrument) eq 'KCOR' then kcor=1
if keyword_set(instrument) then if strupcase(instrument) eq 'CORMAG' then cormag=1

default,gridtype,'PLANEOFSKY'

if keyword_set(swap) then line='174'
if keyword_set(doppler) then begin
;  instrument='COMP'
  line='DOPPLERVLOS'
endif

forcemapname='UNSET'
if keyword_set(mapname) then forcemapname=mapname 

problem=0
if keyword_set(filename) then begin
 if exist(instrument) eq 0 then problem=1
 if file_exist(filename) eq 0 then problem=2
 if problem eq 1 then print,'please specify instrument as keyword'
 if problem eq 2 then print,'bad filename'
 if problem ne 0 then begin
   mapname=''
   goto,label3
 endif
endif

yescomp = -1
if keyword_set(instrument) then yescomp=strpos(strupcase(instrument),'COMP')
if yescomp ge 0 then yescomp=1
if yescomp lt 0 then yescomp=0
testcormag=-1
if keyword_set(instrument) then testcormag=strpos(strupcase(instrument),'CORMAG')
if testcormag ge 0 then yescomp=2

yeskcor=-1
if keyword_set(instrument) then yeskcor=strpos(strupcase(instrument),'KCOR')
if yeskcor ge 0 then yeskcor=1
if yeskcor lt 0 then yeskcor=0

if keyword_set(filename) eq 0 then begin
 default,instrument,'aia'
 if strupcase(instrument) eq 'AIA' then default,line,193  
 if strupcase(instrument) eq 'XRT' then default,line,0
 if yescomp ne 0 then default,line,'STOKESI'  
 default,line,195
endif

if keyword_set(instrument) then if strupcase(instrument) eq 'AIA' then aia=1
default,instrument,''

;
; in case Stokes lines entered without STOKES prefix
;
if yescomp ne 0 then if strpos(strupcase(line),'STOKES') lt 0 and strpos(strupcase(line),'DOPPLER') lt 0 and strpos(strupcase(line),'WIDTH') lt 0 then line='STOKES'+line

if strupcase(gridtype) eq 'PLANEOFSKY' then begin
 default,ngrid,512
 limb='NULL'
 rheight='NULL'
 default,typekcor,'EXTAVG'
endif else begin
; at the moment this ngrid does not affect CARRMAP data
 default,ngrid,180
 default,limb,'West'
; this deals with case of coming from model with default 1.05 set
 if keyword_set(rheight) then begin
   if abs(rheight-1.05) lt 1d-4 then rheight = 1.3
 endif
 default,rheight,1.3d0
 typekcor='CARRMAP'
 occult='NULL'
 upoccult='NULL'
endelse

; only AIA_FILTER actually affects the data (other filters applied within FOR_PLOT)
; and only AIA instrument allows this filter
;
if keyword_set(aia) then begin
  default,rfilter,'aia_rfilter' 
  default,plotlog,0
endif
;

default,eps,0
if keyword_set(eps) then nowidgmess=1 else default,nowidgmess,0

default,rotate_im,0
default,useid,''
default,noplots,0
default,redomap,0

if yescomp ne 0 or yeskcor ne 0 then default,occult,1.05d0 else default,occult,0.d0
default,upoccult,1000.d0

default,typecomp,'*'
default,wlcomp,10747

default,cgsmult,1
default,oldcomp,0
default,ucomp,0
if yescomp eq 1 and keyword_set(date) eq 1 then begin
 if anytim(date,/ccsds) gt anytim('2021-01-21',/ccsds) then ucomp=1
endif
default,pscale,0
default,sminuse,0

B0=0.d0
L0=0.d0
cmer=0.d0
RSun=1.d0

foundsavemap=0

if GET_ENVIRON('FORWARD_WORKING_DIR') ne '' then begin
     default,working_dir,GET_ENVIRON('FORWARD_WORKING_DIR')
endif else begin
     default,working_dir,''
endelse
if usewindows eq 0 then begin
 if working_dir ne '/tmp' and working_dir ne '' then working_dir='/'+strjoin(strsplit(working_dir,/extract,'/'),'/')
endif else begin
 if working_dir ne '' then begin
;
; check to see if this deals with D: (e.g.) correctly
; and also double vs single slashes
; maybe nothing is needed here?
;
        working_dir=strjoin(strsplit(working_dir,/extract,'\'),'\')
        temp=strsplit(working_dir,'/',/regex,/extract)
        working_dir=temp[0]
 endif
endelse
;
; the next line might work to replace above
;
;if working_dir ne slash+'tmp' and working_dir ne '' then working_dir=file_dirname(working_dir)+slash+file_basename(working_dir)

;---
;
; Get the data:
;
; Case 1: no filename
;

IF keyword_set(filename) EQ 0 THEN BEGIN

;
; set up min/max date range for the various telescopes
; and make sure any entered date lies in range
;

    for_setminmaxdate,instrument,date=date,typekcor=typekcor,nowidgmess=nowidgmess,ucomp=ucomp

;
; Get name of file to look for or save to.
; DATEUSE will be the form used in the file naming convention, and
; as the start date for any VSO search.  DATEUSE2 will be 30 minutes later,
; and the end date for the VSO search (only the first file found will be used).
;
    if yescomp eq 1 then begin
         if wlcomp eq 10798 then instrument='OTHERCOMP' $
         else if wlcomp eq 7894 then instrument='FE11COMP' $
         else if wlcomp eq 6374 then instrument='FE10COMP' $
         else if wlcomp eq 7062 then instrument='FE15COMP'
    endif

    if redomap eq 0 then for_namecheckfile,date,instrument,line,checkfile,dateuse,dateuse2,gridtype=gridtype,typecomp=typecomp,typekcor=typekcor,wlcomp=wlcomp,rheight=rheight,ucomp=ucomp $
    else for_namecheckfile,date,instrument,line,checkfile,dateuse,dateuse2,gridtype=gridtype,typecomp=typecomp,typekcor=typekcor,wlcomp=wlcomp,rheight=rheight,/noshortcut,ucomp=ucomp

;
; and check to see if a file already exists locally
; (in working directory if set)
;

    if n_elements(working_dir) eq 1 then if working_dir ne '' then checkfile=working_dir+slash+checkfile 

;
; if REDOMAP is not set, then check to see if there is already a save set
; of the correct resolution (NGRID) (full image only - no _SUB).
;  (eliminated _SUB from checks because not explicitly included in filename below)
; Also needs to have the consistent choice of RFILTER if AIA.
;  and of limb if CARRMAP
;  --- at the moment ngrid doesn't affect CARRMAP
;	and does not appear in its save set names (resolution will be native)
;	may later changing convection to allow pixelization of CARRMAP data
;	so leaving a check in the earlier conditionals
;
    if redomap eq 0 then begin
       if file_exist(checkfile+'_'+strtrim(string(fix(ngrid)),2)+'*.sav') then begin
	 if yescomp eq 0 then filename=findfile(checkfile+'_'+strtrim(string(fix(ngrid)),2)+'.sav') else $
	    filename=findfile(checkfile+'_'+strtrim(string(fix(ngrid)),2)+'_'+strupcase(line)+'.sav')
         if strupcase(gridtype) eq 'PLANEOFSKY' then begin
          if keyword_set(rfilter) then begin
              if strupcase(rfilter) ne 'AIA_RFILTER' and keyword_set(filename) then foundsavemap=1
	  endif else begin
            if keyword_set(filename) then foundsavemap=1
          endelse
	 endif
       endif 
       if foundsavemap ne 1 and file_exist(checkfile+'_'+strtrim(string(fix(ngrid)),2)+'_rfilter.sav') then begin
	 filename=findfile(checkfile+'_'+strtrim(string(fix(ngrid)),2)+'_rfilter.sav')
         if strupcase(gridtype) eq 'PLANEOFSKY' then begin
          if keyword_set(rfilter) then if strupcase(rfilter) eq 'AIA_RFILTER' then foundsavemap=1
	 endif
       endif 
       if foundsavemap ne 1 and file_exist(checkfile+'_'+strupcase(limb)+'.sav') then begin
         filename=findfile(checkfile+'_'+strupcase(limb)+'.sav')
         if keyword_set(rfilter) then begin
            if strupcase(rfilter) ne 'AIA_RFILTER' and strupcase(gridtype) eq 'CARRMAP' then foundsavemap=1
; remaining case of AIA_RFILTER CARRMAP is unlikely; but will force redo
	 endif
       endif 
       if foundsavemap eq 1 then usefilename=filename[0]
    endif
;
; Otherwise, look for the original FITS file
; (full resolution, full FOV)
;
    if foundsavemap eq 0 then begin
       if file_exist(checkfile+'.fits') then begin
	 filename=findfile(checkfile+'.fits')
	 usefilename=filename[0]
       endif 
    endif 

;
; If file does not exist locally, download from web -- 
; (must be online.)
;
     if exist(usefilename) eq 0 then begin
        if n_elements(working_dir) eq 1 then if working_dir ne '' then cd,working_dir,current=old_dir
	filenames=''
        use_network
;
; deal with problem where Windows machines may not have HOME system environment set -
;	causes problems with AIA_PREP
;  WARNING- messing around with $HOME is a bad idea if it is set. This should never
; do this though - and only come into affect if there is no $HOME to start with.
;  However -- if screwy things happen, contact sgibson@ucar.edu
;
	cd,current=curdir
 	makehome=0
 	testhome=get_logenv('HOME')
  	if exist(testhome) eq 0 then makehome=1
	if exist(testhome) then if testhome eq '' then makehome = 1
	if makehome then begin
	  if n_elements(working_dir) eq 1 then temp='HOME='+working_dir else $
	   temp='HOME='+curdir
          setenv,temp
	endif

        if yescomp eq 0 and strupcase(instrument) ne 'SWAP' and strupcase(instrument) ne 'KCOR' then begin
	 if strpos(strupcase(instrument),'EUVI') lt 0 and strpos(strupcase(instrument),'XRT') lt 0 then begin
;
; used this when the AIA server was broken for a bit
;          url='ht'+'tp:'+'//netdrms02'+'.nispdc.nso.'+'edu/cgi/vsoi_tabdelim'
;          setenv, 'VSO_SERVER='+url
          if line ne 0 then filelocate= vso_search(dateuse,dateuse2,instr=instrument, wave=strtrim(string(line),2)) else $
	    filelocate= vso_search(dateuse,dateuse2,instr=instrument)
         endif else begin
          if strpos(strupcase(instrument),'XRT') ge 0 then begin
           filelocate= vso_search(dateuse,dateuse2,instr=instrument,extent='fulldisk') 
	   if keyword_set(filelocate) eq 0 then filelocate= vso_search(dateuse,dateuse2,instr=instrument) 
          endif else begin 
           if strpos(strupcase(instrument),'A') gt 0 then source='STEREO_A' else source='STEREO_B'
           if line ne 0 then filelocate= vso_search(dateuse,dateuse2,detector='euvi',source=source, wave=strtrim(string(line),2)) else $
	    filelocate= vso_search(dateuse,dateuse2,source=source,detector='euvi')
          endelse
         endelse
         filelocatenew=vso_get(filelocate[0],filenames=filenames,/rice)
        endif else begin
          for_directdownload,instrument,dateuse,working_dir,filenames,line=line,nadd=nadd,typecomp=typecomp,typekcor=typekcor,wlcomp=wlcomp,nowidgmess=nowidgmess,rheight=rheight,ucomp=ucomp
        endelse
        wset,winnumsave
	if filenames eq '' then begin 
	  if yescomp eq 0 then begin
             if keyword_set(nowidgmess) then message,/info,'sorry, problem with download. Make sure instrument and wavelength are good, and that data data are available for date being requested, and that you are online.' else $
	       d=dialog(/WARNING,'sorry, problem with download. Make sure instrument and wavelength are good, and that data are available for date being requested, and that you are online.') 
          endif else begin
	     if keyword_set(nowidgmess) then message,/info,'sorry, problem with download.  CoMP data availability can be seen at calendar, e.g. http://mlso.hao.ucar.edu/mlso_data_calendar.php'  else $
	       d=dialog(/WARNING,'sorry, problem with download. CoMP data availability can be seen at calendar, e.g. http://mlso.hao.ucar.edu/mlso_data_calendar.php')
          endelse
	  usefilename=''
	endif else begin
         if strupcase(instrument) eq 'AIA' then read_sdo,filenames,header,image,/noshell
         if yescomp ne 0 then begin
           for_readcompfits,filenames,line,header,image,oldcomp=oldcomp,ucomp=ucomp,cgsmult=cgsmult,wlcomp=wlcomp,nowidgmess=nowidgmess,badfile=badfile
           if badfile eq 1 then goto,label3
         endif
         if strupcase(instrument) eq 'EIT' then read_eit,filenames,header,image
         if yescomp eq 0 and strupcase(instrument) ne 'AIA' and strupcase(instrument) ne 'EIT' then mreadfits,filenames,header,image

;
; make sure saved file is prepped/calibrated appropriately
; need to be online for this step
;
	 if strupcase(instrument) eq 'AIA' then begin
 	  index=header
	  raw=image
          if tag_exist(header,'lvl_num') then begin
	    if index.lvl_num eq 1. then aia_prep,index,raw,header,image,/do_write_fits,outfile=filenames
	  endif
         endif
	 if strpos(strupcase(instrument),'EUVI') ge 0 then begin
          secchi_prep,filenames,header,image,/rotate_on,/dn2p_off
          mwritefits,header,image,outfile=filenames
         endif
	 if strupcase(instrument) eq 'EIT' then begin
 	  index=header
	  raw=image
          eit_prep,index,header,image,data=raw,outfits=filenames
         endif

;
; if CoMP, make sure that line was found in file
;

         if yescomp ne 0 and n_elements(header) eq 1 then begin
          for file_i=0, n_elements(filenames)-1 do file_delete, filenames[file_i], /quiet
          usefilename='' 
         endif else begin
;
; update filename
;
	  for_getfileinfo,header,image,date=date,instrument=instrument,line=line,gridtype=gridtype,filename=filenames
          if yescomp eq 1 then begin
               if wlcomp eq 10798 then instrument='OTHERCOMP' $
               else if wlcomp eq 7894 then instrument='FE11COMP' $
               else if wlcomp eq 6374 then instrument='FE10COMP' $
               else if wlcomp eq 7062 then instrument='FE15COMP'
          endif

          for_namecheckfile,date,instrument,line,usefilename,typecomp=typecomp,typekcor=typekcor,wlcomp=wlcomp,gridtype=gridtype,rheight=rheight,ucomp=ucomp,/noshortcut
          usefilename=usefilename+'.fits'
 	  if yescomp ne 0 and usewindows eq 1 then usefilename=usefilename+'.gz'
;
; rename file
;
           file_move, filenames, usefilename,/overwrite
         
         endelse
        endelse

        if keyword_set(blake) and usefilename ne ''  then begin
            if file_exist('blake_'+usefilename) eq 0 then doblake,usefilename,savescale=savescale
	    usefilename='blake_'+usefilename
	endif

        if n_elements(working_dir) eq 1 and usefilename ne '' then if working_dir ne '' then usefilename=working_dir+slash+usefilename

        if n_elements(working_dir) eq 1 then if working_dir ne '' then cd,old_dir

    endif

    byfile=0
;
; Case 2: Filename entered
;
endif else begin
;
; overwrite gridtype with gridtype of data file
;   IF KCOR - otherwise should always be PLANEOFSKY
;
    gridtypeold=gridtype
    gridtypeuse=gridtype
    if strupcase(instrument) eq 'KCOR' then begin
        break_file, filename, disk_log, dir, frontfile, ext
        if strpos(strupcase(frontfile),'CARRMAP') gt 0 then gridtypeuse='CARRMAP' 
        if strpos(strupcase(frontfile),'EXTAVG') gt 0 then gridtypeuse='PLANEOFSKY'
        if strpos(strupcase(frontfile),'EXTAVG') le 0 $
         and strpos(strupcase(frontfile),'CARRMAP') le 0 then begin
         print,'unrecognized file type; assuming PLANEOFSKY'
         gridtypeuse='PLANEOFSKY'
;         stop
        endif
        gridtypeold=gridtype
        if gridtypeuse ne gridtypeold then begin
         print,'changing gridtype from ',gridtypeold,' to ',gridtypeuse
         gridtype=gridtypeuse
        endif
    endif
    byfile=0
    usefilename=filename
    if strpos(strupcase(usefilename),'.SAV') gt 0 then begin
;
; if filename is a save set, assume that the limb (if CARRMAP) and grid size and rfilter in save
; set are correct, and overwrite any inputs (AIA RFILTER and LIMB done here, NGRID done below).  
;
       if strupcase(instrument) eq 'KCOR' and strupcase(gridtype) eq 'CARRMAP' then begin
        if strpos(strupcase(frontfile),'EAST') gt 0 then limb='EAST'
        if strpos(strupcase(frontfile),'WEST') gt 0 then limb='WEST'
        if strpos(strupcase(frontfile),'EAST') le 0 and $
          strpos(strupcase(frontfile),'WEST') le 0 then begin
           print,'unrecognized file type (bad limb)'
           stop
          endif
       endif
       if strupcase(instrument) eq 'AIA' then begin
         if strpos(strupcase(usefilename),'RFILTER') gt 0 then rfilter = 'aia_rfilter' else rfilter = 'no_filter'
       endif
       foundsavemap=1
;
; If its a fits, though, look for
; a local save set of the correct NGRID, GRIDTYPE, RFILTER, LIMB, and covering full FOV.
; unless redomap is set
;
    endif else begin
; bear in mind that if opening a fits, may need to reset defaults
     if gridtypeold ne gridtypeuse then begin
      if strupcase(gridtype) eq 'CARRMAP' then begin
       if strupcase(limb) eq 'NULL' then limb='WEST'
       if strupcase(rheight) eq 'NULL' then rheight=1.30d0
       ngrid=180
       occult='NULL'
       upoccult='NULL'
       typekcor='CARRMAP'
      endif else begin
       default,ngrid,512
       limb='NULL'
       rheight='NULL'
       if yescomp ne 0 or yeskcor ne 0 then default,occult,1.05d0 else default,occult,0.d0
       default,upoccult,1000.
       typekcor='EXTAVG'
      endelse
     endif
     byfile=1
     if redomap eq 0 then begin
       break_file, filename, disk_log, dir, frontfile, ext
       checkfile=frontfile[0]
       if n_elements(working_dir) eq 1 then if working_dir ne '' then checkfile=working_dir+slash+checkfile 
;
; this is very simiilar to foundsavemap above
;  but here we have to check for _SUB
;
       if file_exist(checkfile+'_'+strtrim(string(fix(ngrid)),2)+'*.sav') then begin
	 if yescomp eq 0 then filename=findfile(checkfile+'_'+strtrim(string(fix(ngrid)),2)+'.sav') else $
	    filename=findfile(checkfile+'_'+strtrim(string(fix(ngrid)),2)+'_'+strupcase(line)+'.sav')
         if strpos(strupcase(file_basename(filename)),'_SUB') le 0 and strupcase(gridtype) eq 'PLANEOFSKY' then begin
          if keyword_set(rfilter) then begin
              if strupcase(rfilter) ne 'AIA_RFILTER' and keyword_set(filename) then foundsavemap=1
	  endif else begin
            if keyword_set(filename) then foundsavemap=1
          endelse
	 endif
       endif 
       if foundsavemap ne 1 and file_exist(checkfile+'_'+strtrim(string(fix(ngrid)),2)+'_rfilter.sav') then begin
	 filename=findfile(checkfile+'_'+strtrim(string(fix(ngrid)),2)+'_rfilter.sav')
         if strpos(strupcase(file_basename(filename)),'_SUB') le 0 and strupcase(gridtype) eq 'PLANEOFSKY' then begin
          if keyword_set(rfilter) then if strupcase(rfilter) eq 'AIA_RFILTER' then foundsavemap=1
	 endif
       endif 
       if foundsavemap ne 1 and file_exist(checkfile+'_'+strupcase(limb)+'.sav') then begin
         filename=findfile(checkfile+'_'+strupcase(limb)+'.sav')
         if strpos(strupcase(file_basename(filename)),'_SUB') le 0 and keyword_set(rfilter) then begin
            if strupcase(rfilter) ne 'AIA_RFILTER' and strupcase(gridtype) eq 'CARRMAP' then foundsavemap=1
; remaining case of AIA_RFILTER CARRMAP is unlikely; but will force redo
	 endif
       endif 
       if foundsavemap eq 1 then begin
        usefilename=filename[0]
        byfile=0
       endif
     endif

    endelse
endelse

if exist(usefilename) and file_exist(usefilename) then if usefilename ne '' then begin

 if strpos(strupcase(usefilename),'.SAV') gt 0 then begin
;
; this should never happen if REDOMAP is set
; (unless FILENAME set to a save file and redomap set, in which
; case redomap is ignored.)
;
   foundsavemap=1 
   restore,usefilename
   line=obspramsstruct.linename
   date=obspramsstruct.date
   ImageMap=QuantMap
   break_file, usefilename, disk_log, dir, filnam, ext
   ufname=filnam+ext
   mapname=ufname
   mapname=str_replace(mapname,'.sav','')
;
; note if x/y range set differently then in the map,
; this only matters as input to for_plot
; (save set is not changed, so resolution unchanged)
; however- we want to be sure that ngrid is reset to appropriately
; reflect the number of points in the range
;
   default,xxmin,double(GridPramsStruct.XRange[0])
   default,yymin,double(GridPramsStruct.YRange[0])
   default,xxmax,double(GridPramsStruct.XRange[1])
   default,yymax,double(GridPramsStruct.YRange[1])
   if is_number(xxmin) eq 0 then xxmin=double(GridPramsStruct.XRange[0])
   if is_number(xxmax) eq 0 then xxmax=double(GridPramsStruct.XRange[1])
   if is_number(yymin) eq 0 then yymin=double(GridPramsStruct.YRange[0])
   if is_number(yymax) eq 0 then yymax=double(GridPramsStruct.YRange[1])

; the only time this next will do anything is if the ngrid entered in command line
; (or default if no ngrid in command line) is different from the save set. This
; will only happen if BYFILE because if BYDATE the save set will have been found 
; based on input or default ngrid, and its internal GridPramsStruct.Ngrid 
; should match. 
;
   ngridold=ngrid
   ngrid=GridPramsStruct.NGrid
;    if ngrid ne ngridold then stop
;
; Backward compatibility
;
   if not tag_exist(GridPramsStruct,'LosOffset') then $
     GridPramsStruct=add_tag(GridPramsStruct,'NULL','LosOffset')
   if not tag_exist(GridPramsStruct,'AzEqui') then $
     GridPramsStruct=add_tag(GridPramsStruct,0,'AzEqui')
   if not tag_exist(GridPramsStruct,'DistObs') then $
     GridPramsStruct=add_tag(GridPramsStruct,215,'DistObs')
    
   if not tag_exist(LosPramsStruct,'NoStretch') then $
     LosPramsStruct=add_tag(LosPramsStruct,'NULL','NoStretch')

   if not tag_exist(GridPramsStruct,'ThPos') then begin
     xxvar=GridPramsStruct.Rpos*0.
     yyvar=GridPramsStruct.Rpos*0.
     rdim=size(Rpos)
     xdim=rdim[1]
     ydim=rdim[2]
     for i = 0,xdim-1 do begin
      xxvar[i,*] = GridPramsStruct.Xrange[0]+i*GridPramsStruct.Dx
     endfor
     for j = 0,ydim-1 do begin
      yyvar[*,j] = GridPramsStruct.Yrange[0]+j*GridPramsStruct.Dy
     endfor
     ThPos=acos(yyvar/Rpos)
     west=where(xxvar gt 0.)
     if min(west) ne -1 then ThPos[west]=2.d0*!dpi-ThPos[west]
     Phpos=xxvar*0.d0 + double(cmer*mdtor)
     GridPramsStruct=add_tag(GridPramsStruct,ThPos,'ThPos')
     GridPramsStruct=add_tag(GridPramsStruct,PhPos,'PhPos')
   endif
   if not tag_exist(ObsPramsStruct,'RotAz') then ObsPramsStruct=add_tag(ObsPramsStruct,'','RotAz') 
   if wlcomp eq 10747 or wlcomp eq 10798 then wlpol = 1 else wlpol = 0
   if yescomp ne 0 and strupcase(typecomp) ne 'DYNAMICS' and wlpol eq 1 then begin 

;
; CORMAG data will always be in RADIAL
;

    if yescomp eq 2 then ObsPramsStruct.RotAz='RADIAL'

    if strupcase(ObsPramsStruct.RotAz) ne 'RADIAL' then begin
;
; need to be careful about CoMP data,
; because it will start in E-W (Q and U will have an additional rotation of solar P angle)
; and we want to convert it to radial to be  consistent with model output (which is
; changed within for_intensint)
;
; Old save sets may not have this fixed yet.
;
; Need to change azimuth in StokesStruct even if not the line being called
;
     realdata=where(ImageMap.data ne -9999. and ImageMap.data ne -8888.)
     if min(realdata) ne -1 then begin

      for_azrot,StokesStruct.Az[realdata],StokesStruct.P[realdata],GridPramsStruct.ThPos[realdata]/mdtor,stazuse,Quse,Uuse
      StokesStruct.Az[realdata]=stazuse
      if strpos(strupcase(ObsPramsStruct.LineName),'AZ') ge 0 then imagemap.data[realdata] = stazuse

;
; now take care of Q and U if they are the line being called (or QoI or UoI)
;

      if strpos(strupcase(ObsPramsStruct.LineName),'Q') ge 0 then imagemap.data[realdata] = Quse
      if strpos(strupcase(ObsPramsStruct.LineName),'U') ge 0 then imagemap.data[realdata] = Uuse
      if strpos(strupcase(ObsPramsStruct.LineName),'QOI') ge 0 then imagemap.data[realdata] = Quse/StokesStruct.I[realdata]
      if strpos(strupcase(ObsPramsStruct.LineName),'UOI') ge 0 then imagemap.data[realdata] = Uuse/StokesStruct.I[realdata]

      ObsPramsStruct.RotAz='radial' 
     endif
    endif
   endif else ObsPramsStruct.RotAz='NULL'
   savemap=1
 endif else begin

;
; FITS file - extract information and make map
; (first filter AIA image if RFILTER keyword set)
;

  if strpos(strupcase(usefilename),'AIA') ge 0 or strupcase(instrument) eq 'AIA' then begin
   instrument='aia'
   dorfilter=0
   if keyword_set(rfilter) then if strupcase(rfilter) eq 'AIA_RFILTER' then dorfilter=1
   if dorfilter eq 1 then begin
    devname=!d.name
    use_network
    aia_rfilter,usefilename,imagenew,headernew,/idl_out,nsum=1,bin=1,/no_skip
    dsize=size(imagenew)
    if dsize[1] ne headernew.naxis1 then begin
     headernew.naxis1=dsize[1]
     headernew.naxis2=dsize[1]
     print,'unusual file resolution='+strtrim(string(dsize[1]),2)+', making header consistent'
    endif 
    set_plot,devname
   endif else begin
     read_sdo,usefilename,headernew,imagenew,/noshell
     index=headernew
     raw=imagenew
     if tag_exist(headernew,'lvl_num') then begin
	    if index.lvl_num eq 1. then aia_prep,index,raw,headernew,imagenew
     endif
   endelse
  endif else begin
     if strpos(strupcase(usefilename),'COMP') ge 0 or yescomp ne 0 then begin
       if yescomp eq 1 then instrument='COMP' else instrument='CORMAG'
       default,line,'STOKESI'
       for_readcompfits,usefilename,line,headernew,imagenew,oldcomp=oldcomp,ucomp=ucomp,StokesStruct=StokesStruct,uselinename=uselinename,occult=occult,upoccult=upoccult,typecomp=typecomp,cgsmult=cgsmult,wlcomp=wlcomp,nowidgmess=nowidgmess,badfile=badfile
       if badfile eq 1 then goto,label3
     endif
     if strpos(strupcase(usefilename),'EIT') ge 0 or strupcase(instrument) eq 'EIT' then begin
      instrument='EIT'
      read_eit,usefilename,headernew,imagenew
     endif
     if strpos(strupcase(usefilename),'COMP') lt 0 and strpos(strupcase(usefilename),'CORMAG') lt 0 and strpos(strupcase(usefilename),'EIT') lt 0 and strupcase(instrument) ne 'EIT' then begin
       mreadfits,usefilename,headernew,imagenew
       if instrument then begin
         if tag_exist(headernew,'instrume') then instrument=strcompress(headernew.instrume,/remove_all)
         if tag_exist(headernew,'object') then instrument=strcompress(headernew.object,/remove_all)
       endif
       if instrument eq 'Solar K-Corona' then instrument = 'KCOR'
;       ngrid=1024
       if strupcase(gridtype) eq 'CARRMAP' and strupcase(instrument) eq 'KCOR' then begin
	north_up_map = shift(imagenew, 0, -180)
	  imageeast = reverse(north_up_map[*, 0:359], 2)
	  imageeast = reverse(imageeast,1)
	  imagewest = north_up_map[*, 360:*]
	  imagewest = reverse(imagewest,1)
        if strupcase(limb) eq 'EAST' then imagenew=imageeast	
        if strupcase(limb) eq 'WEST' then imagenew=imagewest	
       endif
     endif
;
; check calibration
;
     if strpos(strupcase(instrument),'EUV') ge 0 then begin
       test=where(strpos(strupcase(headernew.history),'EUVI_CORRECTION') ge 0,c)
       if c lt 0 then secchi_prep,usefilename,headernew,imagenew,/rotate_on,/dn2p_off
     endif
     if strupcase(instrument) eq 'EIT' then begin
       index=headernew
       raw=imagenew
       test=where(strpos(strupcase(headernew.history),'EIT_DARK') ge 0,c)
       if c lt 0 then eit_prep,index,headernew,imagenew,data=raw,outfits=filenames
     endif
  endelse

;
; make sure only one image
;

  imagenew=imagenew[*,*,0]

  baddata=where(imagenew eq -8888.,c)
  if c gt 0 then imagenew[baddata]=0.d0
;
; if CoMP, make sure that line was found in file
;

  if yescomp ne 0 and n_elements(headernew) eq 1 then usefilename='' else begin

;
;get initial variables
;

   for_getfileinfo,headernew,imagenew,date=date,instrument=instrument,line=line,width=width,height=height,xcen=xcen,ycen=ycen,cmer=cmer,B0=B0,useid=useid,name=name,rsuninpix=rsuninpix,BUnit=BUnit,pixelsize=pixelsize,dunmult=dunmult,gridtype=gridtype,filename=usefilename


   if yescomp eq 1 then begin
         if wlcomp eq 10798 then instrument='OTHERCOMP' $
         else if wlcomp eq 7894 then instrument='FE11COMP' $
         else if wlcomp eq 6374 then instrument='FE10COMP' $
         else if wlcomp eq 7062 then instrument='FE15COMP'
   endif

;
; if rotate_im set then rotate and make a new map
; this is commented out because it was a pain to keep up
; in particular if headers were not complete 
;
;    if yescomp eq 0 then begin
;	index2map,headernew,imagenew,map_fits
;
; make sure roll_angle is 0
;
;        map_fits=rot_map(map_fits,roll_angle=0)
;
;	if rotate_im ne 0 then map_fits=rot_map(map_fits,rotate_im,/full_size)
;	imagenew=map_fits.data
;	rsuninpix=map_fits.rsun/map_fits.dx
;    endif else begin
;      IF rotate_im NE 0 THEN begin
;       imagenew=rot(imagenew,rotate_im) 
;       StokesStruct.I=rot(StokesStruct.I,rotate_im)
;       StokesStruct.centI=rot(StokesStruct.centI,rotate_im)
;       if strupcase(typecomp) ne 'DYNAMICS' then begin
;        StokesStruct.P=rot(StokesStruct.P,rotate_im)
;        if keyword_set(oldcomp) eq 0 then StokesStruct.Az=rot(StokesStruct.Az,rotate_im)-rotate_im else $
;            StokesStruct.Az=rot(StokesStruct.Az-rotate_im,rotate_im)
;       endif
;      endif
;    endelse

;
; fix e.g. units of dN/s/pix (see FOR_GETFILEINFO)
; 

   imagenew=imagenew*dunmult

   if strupcase(gridtype) eq 'CARRMAP' and strupcase(instrument) eq 'KCOR' then begin
    DSize=Size(imagenew)
    nph=DSize[1]
    nth=DSize[2]
    CType='CARRMAP'
    thunits='Deg'
    phunits='Deg'
    time=date

    Runits='Rsun'
    L0=0.
; dx
    dph=360.d0/double(nph)
; dy
    dth=180.d0/double(nth)

    th0=180.
    if cmer ge 0.d0 then ph0=cmer-360.d0 $
        else ph0=cmer

    Radii=rheight
    CarLimb=limb
    ID=useid+' r= '+$
      strtrim(string(Radii),1)+$
    ' '+CarLimb+' limb'
;
; now shift so both east and west limb line up
; thus, the Carrington longitude refers to the central meridian
; note adding a shift to the right of half the grid so that
; it will plot correctly (removed shift from for_plot)
;
    if strupcase(limb) eq 'WEST' then nshift=nph/4. + nph/2.
    if strupcase(limb) eq 'EAST' then nshift=-nph/4. + nph/2.

    for i =0,nth-1 do begin
      imagenew[*,i]=shift(imagenew[*,i],nshift)
    endfor

;
; get rid of bad points
;
    test=where(imagenew*0. ne 0.)
    if min(test) ne -1 then imagenew[test]=-8888.
    test=where(imagenew le 0.) 
    if min(test) ne -1 then imagenew[test]=-8888.

    ImageMap={data:imagenew,$
        dth:dth,dph:dph,Radii:Radii,CarLimb:CarLimb,time:time,ph0:ph0,th0:th0,$
        thunits:thunits,phunits:phunits,RUnits:Runits,ID:ID,$
        CType:CType,B0:B0,BUnit:BUnit,L0:L0,cmer:cmer,RSun:1,Name:Name}
;
; place holder for noise
;

    ImageMap=add_tag(ImageMap,0.*imagenew,'Noise')

;
; For GridPramsStruct below
;
    xcen=180.d0
    ycen=90.d0

    xxmin=0.d0
    xxmax=360.d0
    yymin=0.d0
    yymax=180.d0
    xxminorig=xxmin
    xxmaxorig=xxmax
    yyminorig=yymin
    yymaxorig=yymax

    mdtor=!dpi/180d0
    ThPos=dblarr(nph,nth)
    phpos=dblarr(nph,nth)
    for i = 0,nph-1 do begin
     for j = 0.,nth-1. do begin
       ThPos[i,j]=double(j+0.5)*dth*mdtor
       phpos[i,j]=double(i+0.5)*dph*mdtor
     endfor
    endfor
;
    Rpos=ThPos*0.+rheight

; For ObsPramsStruct below
    linenum=line
    rotaz='NULL'

;
; note that ngrid, width, height won't change for CARRMAP
; at the moment there is no way to zoom in or out
;

;  end if KCOR CARRMAP

   endif else begin

;
; find dx,xy  - pixels in units of Rsun
; will be used convert width, height to units of Rsun
;

    dx=1./rsuninpix
    dy=dx

;
; pixelsize is in units of arcseconds
; need to calculate Rsun in units of solar radii for map
; (will be slightly different from 1 because of annual variation of distance to Sun)
;

    rsun=rsuninpix*pixelsize/959.63d0

;
; xcen,ycen are in units of arcseconds - need them in units of Rsun
;

    xcen=xcen/959.63d0
    ycen=ycen/959.63d0

;
; now set up grid for map
;

    if yescomp ne 0 then begin
     tempI=StokesStruct.I
     tempcentI=StokesStruct.centI
     if wlcomp eq 10747 or wlcomp eq 10798 then wlpol = 1 else wlpol = 0
     if strupcase(typecomp) ne 'DYNAMICS' and wlpol eq 1 then begin 
      tempP=StokesStruct.P
      tempAz=StokesStruct.Az
     endif
    endif
;
; units of Rsun
;

    xxminorig=double(-1.*width/2.*dx+xcen)
    xxmaxorig=double(width/2.*dx+xcen)
    yyminorig=double(-1.*height/2.*dy+ycen)
    yymaxorig=double(height/2.*dy+ycen)
 
    default, xxmin,xxminorig
    default, xxmax,xxmaxorig
    default, yymin,yyminorig
    default, yymax,yymaxorig
    if is_number(xxmin) eq 0 then xxmin=xxminorig
    if is_number(xxmax) eq 0 then xxmax=xxmaxorig
    if is_number(yymin) eq 0 then yymin=yyminorig
    if is_number(yymax) eq 0 then yymax=yymaxorig
;
; rebin if ngrid and view implies decreased resolution
; use as near as possible to original resolution otherwise
;
; this is where ngrid will change and if passed back to for_widget_event will be changed there
; but only if bigger number (higher resolution) than native in which case will be passed back native
; if lower resolution than native, will be saved within GridPramsStruct and on the name of the save file
; that will be created at that lower resolution. 
; width and ngrid _should_ be the same but there may be some rounding issues which however should be
;  addressed in for_widget_event
;
; note ngrid scale always applied to X dimension
;
    if ngrid ne 0 then begin
     mult=dx*(double(ngrid)/double(xxmax-xxmin))
     if (mult-1.) gt 1d-4 then begin
      ngrid = (double(xxmax-xxmin)/dx)
      ngrid=fix(ngrid)
      message,/info,'ngrid bigger than original resolution, resetting to ngrid='+strtrim(string(fix(ngrid)),2) 
      mult=dx*(double(ngrid)/double(xxmax-xxmin))
      if (mult-1.) gt 1d-4 then stop
     endif
     width=width*mult
     height=height*mult
     widthold=width
     width=fix(round(width))
     heightold=height
     height=fix(round(height))
     dx=(widthold/width)*dx/mult
     dy=(heightold/height)*dy/mult
     imagenew=congrid(imagenew,width,height)
     if yescomp ne 0 then begin
      tempI=congrid(tempI,width,height)
      tempcentI=congrid(tempcentI,width,height)
      if wlcomp eq 10747 or wlcomp eq 10798 then wlpol = 1 else wlpol = 0
      if strupcase(typecomp) ne 'DYNAMICS' and wlpol eq 1 then begin 
       tempP=congrid(tempP,width,height)
       tempAz=congrid(tempAz,width,height)
      endif
     endif
    endif
;print,'in for_plotfits, after rebin, width=',width,' height=',height,' ngrid=',ngrid
 
; reduce image to sub-image
; if it is smaller in at least one dimension.  
; (add blank pixels if some dimension bigger)
; If only larger ignore inputted xxmin/max yymin/max in making map, use
; original dimensions (inputted dimensions will still be used in for_plot).
 
    if float(xxmin) gt float(xxminorig) or float(xxmax) lt float(xxmaxorig) or float(yymin) gt float(yyminorig) or float(yymax) lt float(yymaxorig) then begin

;find subset of data in pixels

     xminpix=fix(round(xxmin/dx+width/2.d0-xcen/dx))
     xmaxpix=fix(round(xxmax/dx+width/2.d0-xcen/dx)-1)
     yminpix=fix(round(yymin/dy+height/2.d0-ycen/dy))
     ymaxpix=fix(round(yymax/dy+height/2.d0-ycen/dy)-1)

     xxminreal=(-width/2.d0+xminpix)*dx +xcen
     xxmaxreal=(-width/2.d0+xmaxpix+1)*dx + xcen
     yyminreal=(-height/2.d0+yminpix)*dy + ycen
     yymaxreal=(-height/2.d0+ymaxpix+1)*dy + ycen
  
    endif else begin
     xminpix=0
     xmaxpix=width-1
     yminpix=0
     ymaxpix=height-1
     xxminreal=xxminorig
     yyminreal=yyminorig
     xxmaxreal=xxmaxorig
     yymaxreal=yymaxorig
    endelse

    sub_image= -8888. + dblarr(xmaxpix-xminpix+1,ymaxpix-yminpix+1)
    if yescomp ne 0 then begin
     sub_I= sub_image
     sub_centI= sub_image
     sub_P= sub_image
     sub_az= sub_image
    endif
    xxvar=sub_image*0.
    yyvar=sub_image*0.
 
    for ii=xminpix,xmaxpix do begin
     for jj=yminpix,ymaxpix do begin
      if ii ge 0. and ii le width-1 and jj ge 0. and jj le height-1 then begin
        sub_image[ii-xminpix,jj-yminpix]=imagenew[ii,jj]
        if yescomp ne 0 then begin
         sub_I[ii-xminpix,jj-yminpix]=tempI[ii,jj]
         sub_centI[ii-xminpix,jj-yminpix]=tempcentI[ii,jj]
         if wlcomp eq 10747 or wlcomp eq 10798 then wlpol = 1 else wlpol = 0
         if strupcase(typecomp) ne 'DYNAMICS' and wlpol eq 1 then begin
          sub_P[ii-xminpix,jj-yminpix]=tempP[ii,jj]
          sub_az[ii-xminpix,jj-yminpix]=tempaz[ii,jj]
         endif
        endif
      endif
      xxvar[ii-xminpix,jj-yminpix]=xxminreal+(ii-xminpix)*dx 
      yyvar[ii-xminpix,jj-yminpix]=yyminreal+(jj-yminpix)*dy 
     endfor
    endfor

;find center of subplot
    xc=(xxmaxreal-xxminreal)/2+xxminreal
    yc=(yymaxreal-yyminreal)/2+yyminreal
  
;
; get rid of bad points (if any)
;
    test=where(sub_image*0. ne 0.)
    if min(test) ne -1 then sub_image[test]=-8888.
    if (strupcase(line) ne 'DOPPLERVLOS' and strpos(strupcase(line),'STOKESV') lt 0 and strpos(strupcase(line),'STOKESQ') lt 0 and strpos(strupcase(line),'STOKESU') lt 0) then test=where(sub_image le 0.) 
    if min(test) ne -1 then sub_image[test]=-8888.

    if yescomp ne 0 then begin
     test=where(sub_I lt 1d-10)
     if min(test) ne -1 then sub_image[test]=-8888.
    endif
 
;
;make a map of the image 
;limb: which limb to plot, since the image is the whole disk, set to
;both so that you just use xxmin, xxmax...to set the view
;name: eventual name of the saved plot
; L0 Stonyhurst longitude
;

    L0=cmer-tim2carr(date)
    L0=L0[0]
    if is_number(line) eq 0 then linenum = '-999' else linenum=line

    if yescomp eq 0 then ID=useid else begin
     if ucomp ne 1 then ID='DATA:'+instrument+'_'+uselinename+'!c'+date $ 
      else ID='DATA:UCOMP_'+uselinename+'!c'+date 
    endelse
    if yescomp eq 1 then begin
     if strpos(strupcase(typecomp),'QUICKINVERT') ge 0 then begin
       if strpos(strupcase(typecomp),'MEDIAN_SYNOPTIC') ge 0 then nadd='MEDIAN_SYNOPTIC'
       if strpos(strupcase(typecomp),'MEDIAN_WAVES') ge 0 then nadd='MEDIAN_WAVES'
     endif
     if exist(nadd) then begin
      if nadd ne '' then ID = ID + '!c'+nadd
     endif
    endif

    if keyword_set(rfilter) then if strupcase(rfilter) eq 'AIA_RFILTER' then BUnit = 'FILTERED (Orig. units '+BUnit+')'

    ImageMap=make_map(sub_image,$
                  dx=dx,dy=dy,xc=xc,yc=yc,$
                 time=date,$
                 xunits='RSun',yunits='RSun',ID=ID,$
		 _extra={BUnit:BUnit,CType:'disk',B0:B0,L0:L0,cmer:cmer,$
                		Limb:'CMER',RSun:RSun,Name:Name,Instrument:Instrument,Line:LineNum}) 

;
; place holder for noise
;

    ImageMap=add_tag(ImageMap,0.*sub_image,'Noise')

; For GridPramsStruct below
;
;   if ngrid ne 0 then width = ngrid
    width=xmaxpix-xminpix+1
    height=ymaxpix-yminpix+1
;   print,'in plotfits, width=',width,' height =',height,' ngrid=',ngrid
    Rpos=sqrt(xxvar^2+yyvar^2)
    ThPos=acos(yyvar/Rpos)
    west=where(xxvar gt 0.)
    if min(west) ne -1 then ThPos[west]=2.d0*!dpi-ThPos[west]
    Phpos=xxvar*0.d0 + double(cmer*mdtor)

; For ObsPramsStruct below 

    if wlcomp eq 10747 or wlcomp eq 10798 then wlpol = 1 else wlpol = 0
    if yescomp eq 1 and strupcase(typecomp) ne 'DYNAMICS' and wlpol eq 1 then begin
;
; need to be careful about CoMP data,
; because it will start in E-W 
;  (Q and U could have an additional rotation of solar P angle 
;   depending on version)
; and we want to convert it to radial to be  consistent with model output (which is
; changed within for_intensint)
;
; Need to change azimuth in StokesStruct even if not the line being called
;
      realdata=where(ImageMap.data ne -9999. and ImageMap.data ne -8888.)
      if min(realdata) ne -1 then begin

       for_azrot,Sub_Az[realdata],Sub_P[realdata],ThPos[realdata]/mdtor,stazuse,Quse,Uuse
       Sub_Az[realdata]=stazuse
       if strpos(strupcase(line),'AZ') ge 0 then imagemap.data[realdata] = stazuse

;
; now take care of Q and U if they are the line being called (or QoI or UoI)
;

       if strpos(strupcase(line),'Q') ge 0 then imagemap.data[realdata] = Quse
       if strpos(strupcase(line),'U') ge 0 then imagemap.data[realdata] = Uuse
       if strpos(strupcase(line),'QOI') ge 0 then imagemap.data[realdata] = Quse/Sub_I[realdata]
       if strpos(strupcase(line),'UOI') ge 0 then imagemap.data[realdata] = Uuse/Sub_I[realdata]

      endif
      rotaz='radial' 
    endif else rotaz='NULL'
   endelse
; end not KCAR CARRMAP

   if yescomp ne 0 then IClass='CORONAL POLARIMETERS' else IClass='EUV/XRAY IMAGERS' 
   if strupcase(instrument) eq 'KCOR' then IClass='White light coronagraphs'

   break_file, usefilename, disk_log, dir, filnam, ext
   for_namecheckfile,date,instrument,line,ufname,typecomp=typecomp,typekcor=typekcor,wlcomp=wlcomp,gridtype=gridtype,rheight=rheight,ucomp=ucomp,/noshortcut
   default,mapname,ufname
   mapname=str_replace(mapname,'.sav','')
   mapname=str_replace(mapname,'.fits','')
   mapname=str_replace(mapname,'.fts','')
   mapname=str_replace(mapname,'.gz','')
   if strupcase(gridtype) eq 'CARRMAP' then mapname=mapname+'_'+strupcase(limb) $
    else mapname=mapname+'_'+strtrim(string(fix(ngrid)),2)
   if keyword_set(rfilter) then if strupcase(rfilter) eq 'AIA_RFILTER' then mapname=mapname+'_rfilter'
;
; if smaller field of view, save a new version - note this will have higher resolution than a save map of the full FOV
; (but if its just a small change because going from one date to another (and Rsun changing a bit)
; don't bother)
; if larger field of view, should save exactly as full FOV map -- the xrange and yrange will be dealt with via FOR_PLOT
;
   if float(xxmin) gt float(.99*xxminorig) or float(xxmax) lt float(.99*xxmaxorig) or float(yymin) gt float(.99*yyminorig) or float(yymax) lt float(.99*yymaxorig) then mapname=mapname+'_SUB'
   if yescomp ne 0 then mapname=mapname + '_'+strtrim(strupcase(string(line)),2)

;
; build structures needed for save map and plot
;
   if strupcase(gridtype) eq 'PLANEOFSKY' then begin 
    if strpos(mapname,'_SUB') gt 0 then GridPramsStruct={GridType:gridtype,Limb:'CMER',ngrid:width,ngy:height,Rpos:Rpos,ThPos:ThPos,Phpos:Phpos,Cmer:double(cmer*mdtor),dx:double(ImageMap.dx),dy:double(ImageMap.dy),xrange:[double(xxmin),double(xxmax)],yrange:[double(yymin),double(yymax)],xcenter:double(xcen),ycenter:double(ycen),losoffset:'NULL',phio:0.,rheight:rheight,AzEqui:0,DistObs:215} else $
    GridPramsStruct={GridType:gridtype,Limb:'CMER',ngrid:width,ngy:height,Rpos:Rpos,ThPos:ThPos,Phpos:Phpos,Cmer:double(cmer*mdtor),dx:double(ImageMap.dx),dy:double(ImageMap.dy),xrange:[double(xxminorig),double(xxmaxorig)],yrange:[double(yyminorig),double(yymaxorig)],xcenter:double(xcen),ycenter:double(ycen),losoffset:'NULL',phio:0.,rheight:rheight,AzEqui:0,DistObs:215} 
   endif else begin
     GridPramsStruct={GridType:gridtype,Limb:limb,ngrid:width,ngy:height,Rpos:Rpos,ThPos:ThPos,Phpos:Phpos,Cmer:double(cmer*mdtor),dx:double(ImageMap.dph),dy:double(ImageMap.dth),xrange:[double(xxmin),double(xxmax)],yrange:[double(yymin),double(yymax)],xcenter:double(xcen),ycenter:double(ycen),losoffset:'NULL',phio:0.,rheight:rheight,AzEqui:0,DistObs:215} 
   endelse

;
; this is empty because we do not add noise
;
   NoisePrams={DoNoise:0.d0,DoNoiseVal:'tog',Aperture:0.d0,ApertureVal:'double',Resolution:0.d0,ResolutionVal:'nodisplay',Integration:0.d0,IntegrationVal:'double',ModEffInt:0.d0,ModEffIntVal:'double',ModEffQuant:0.d0,ModEffQuantVal:'double',Efficiency:0.d0,EfficiencyVal:'double',Background:0.d0,BackgroundVal:'double',ErrTrunc:0.d0,ErrTruncVal:'double',Tscope:'NULL',TScopeVal:'nodisplay',NoRandom:0,NoRandomVal:'tog'}
   FCompPrams=for_compdefaults(CompPrams0,'dummy','dummy')
   SpecPrams=for_specdefaults(ioneq='data',instrument=instrument,line=line,IClass=IClass,working_dir=working_dir)

   ObsPramsStruct={Instrument:strupcase(instrument),LineName:strupcase(Line[0]),LineNum:linenum,Label:line,IClass:IClass,Date:date,Pos:0.d0,ObsLosLimit:0.d0,Frequency_MHz:0.d0,DoGyro:0,FCor:0,RotAz:RotAz,Wavelength_Ang:0.d0,Wavelength2_Ang:0.d0,NumIon:0,Pop2TRegime:0,SpecPrams:SpecPrams,NoisePrams:NoisePrams,FCompPrams:FCompPrams}
 
; DATA gets magmod=-1 (magnetically dependent observable), -2 (non-magnetically dependent observable)
;
   if yescomp ne 0 then magmod=fix(-1) else magmod=fix(-2)

   ModPramsStruct={name:'data',magmod:magmod,label:'data'}
   ModSolStruct={test:0.}

   LosPramsStruct={losuse:'NULL',dodisk:'NULL',bang:B0*mdtor,$
        thetao:'NULL',axisymmult:'NULL',incres:'NULL',losmin:'NULL',nostretch:'NULL',losint:'NULL',nlos:'NULL',$
        occult:occult,upoccult:upoccult}

   if yescomp ne 0 then begin
    test=where(sub_I gt 0,c)
; 
; NOTE only UComp has both StokesI and StokescentI although that will change
; But for old data, StokescentI just is StokesI/eqwidth which is a somewhat
; arbitrary line width defined in for_readcompfits. So it is passed through here.
;
    if c eq 0 then begin
      print,'data all bad'
      mapname=''
      goto,label3
    endif
    if wlcomp eq 10747 or wlcomp eq 10798 then wlpol = 1 else wlpol = 0
    if strupcase(typecomp) ne 'DYNAMICS' and wlpol eq 1 then $
; note Az is radial azimuth
     StokesStruct={CentWave:StokesStruct.CentWave,CentI:sub_centI,I:sub_I,P:sub_P,Az:sub_Az} else $
     StokesStruct={CentWave:StokesStruct.CentWave,CentI:sub_centI,I:sub_i,P:sub_I*0.,Az:sub_I*0.}
   endif else StokesStruct={Name:'empty'} 

  endelse
 endelse
; end FITS file

;
; set up ucomp tag

 if ucomp eq 1 then begin
    extratitle='!c UCOMP'
    if strupcase(line) eq 'LINEWIDTH' then begin
     default,imin,54
     default,imax,74
    endif
 endif 

 if usefilename ne '' then begin

;plot map

  Xplotrange=[double(xxmin),double(xxmax)]
  Yplotrange=[double(yymin),double(yymax)]

  default,dodisk,LosPramsStruct.dodisk
  if yescomp ne 0 then  begin
   occult=LosPramsStruct.occult
   upoccult=LosPramsStruct.upoccult
  endif else begin
   default,occult,LosPramsStruct.occult
   default,upoccult,LosPramsStruct.upoccult
  endelse

  for_plotfitsdefaults,gridtype,ngrid,line=line,typekcor=typekcor,kcor=kcor,$
    colortable=colortable,plotlog=plotlog,imax=imax,imin=imin,psplot=psplot,$
    usecolor=usecolor,axiscolor=axiscolor,docont=docont,nocontcolor=nocontcolor,noclabel=noclabel,nlev=nlev,$
    c_charsize=c_charsize,winnumber=winnumber,nwinx=nwinx,nwiny=nwiny,bgcolor=bgcolor,$
    dispexp=dispexp,dispgamma=dispgamma,rpow=rpow,nobangletitle=nobangletitle,$
    charsize=charsize,charthick=charthick,noerase=noerase,title=title,$
    whitedisk=whitedisk,nulldatacolor=nulldatacolor,sunedge=sunedge,rfilter=rfilter,$
    stklines=stklines,nstarrow=nstarrow,arrowave=arrowave,extratitle=extratitle,$
    aia=aia,$
    gif=gif,tiff=tiff,jpeg=jpeg,moreplots=moreplots

  default,units,ImageMap.BUnit
  if noplots eq 0 then begin
   if strupcase(title) ne 'ZZ' then title=Imagemap.ID+extratitle

   for_plot,ImageMap,GridPramsStruct,ObsPramsStruct=ObsPramsStruct,StokesStruct=StokesStruct,xplotrange=xplotrange,yplotrange=yplotrange,mapname=mapname,$
             dodisk=dodisk,colortable=colortable,cpos=cpos,plotlog=plotlog,imax=imax,imin=imin,psplot=psplot,eps=eps,$
             usecolor=usecolor,axiscolor=axiscolor,docont=docont,nocontcolor=nocontcolor,noclabel=noclabel,nlev=nlev,working_dir=working_dir,$
             c_charsize=c_charsize,winnumber=winnumber,nwinx=nwinx,nwiny=nwiny,units=units,bgcolor=bgcolor,$
             dispexp=dispexp,dispgamma=dispgamma,rpow=rpow,nobangletitle=nobangletitle,$
             whitedisk=whitedisk,occult=occult,upoccult=upoccult,nulldatacolor=nulldatacolor,sunedge=sunedge,rfilter=rfilter,$
             charsize=charsize,charthick=charthick,noerase=noerase,title=title,xtitle=xtitle,ytitle=ytitle,nowidgmess=nowidgmess,nodata

;
; plot stokeslines for CoMP
;

   if yescomp ne 0 and stklines eq 1 then $
    for_plotstokeslines,GridPramsStruct,LosPramsStruct,ObsPramsStruct,ModPramsStruct,occult=occult,upoccult=upoccult,$
     StokesStruct,sminuse=sminuse,pscale=pscale,nstarrow=nstarrow,stkcolor=stkcolor,stkthick=stkthick,xplotrange=xplotrange,yplotrange=yplotrange,arrowave=arrowave


;Save plots to file

   IF !d.name ne 'PS' and (gif NE 0 OR tiff NE 0 OR jpeg NE 0) THEN BEGIN
    for_save_plot,gif=gif,tiff=tiff,jpeg=jpeg,mapname=mapname,working_dir=working_dir
   ENDIF

   IF !d.name EQ 'PS' AND moreplots EQ 0 THEN BEGIN
;
;       need to get this to work
;
;               if keyword_set(eps) then pse else device,/close
                device,/close
    set_plot,'X'
   ENDIF
  endif
;  end noplots=0

  if keyword_set(savemap) and foundsavemap eq 0 then begin

    if forcemapname ne 'UNSET' then mapname=forcemapname

    if n_elements(working_dir) eq 1 then begin
     if working_dir ne '' then begin
      saveplotname=working_dir+slash+mapname 
     endif else saveplotname=mapname
    endif else saveplotname=mapname

    QuantMap=ImageMap

    for_plotdefaults,ModPramsStruct.MagMod,ModPramsStruct.Name,gridtype=GridPramsStruct.GridType,dodisk=dodisk,noerase=noerase,line=ObsPramsStruct.LineName,instrument=ObsPramsStruct.Instrument,donoise=ObsPramsStruct.NoisePrams.DoNoise,rotaz=ObsPramsStruct.RotAz,pos=ObsPramsStruct.Pos,$
        colortable=colortable,plotlog=plotlog,imax=imax,imin=imin,usecolor=usecolor,axiscolor=axiscolor,bgcolor=bgcolor,$
        docont=docont,nocontcolor=nocontcolor,noclabel=noclabel,nlev=nlev,c_charsize=c_charsize,$
        winnumber=winnumber,nwinx=nwinx,nwiny=nwiny,$
        dispexp=dispexp,dispgamma=dispgamma,rpow=rpow,nobangletitle=nobangletitle,addturbtitle=addturbtitle,$
        nulldatacolor=nulldatacolor,sunedge=sunedge,whitedisk=whitedisk,$ 
        charsize=charsize,charthick=charthick,title=title,xtitle=xtitle,ytitle=ytitle,$
        fieldlines=fieldlines,bscale=bscale,units=units,bcolor=bcolor,bthick=bthick,bminuse=bminuse,narrow=narrow,nstarrow=nstarrow,$
        pscale=pscale,stklines=stklines,stkcolor=stkcolor,stkthick=stkthick,sminuse=sminuse,$
        arrowave=arrowave,azequi=azequi,distobs=distobs,PlotInputs=PlotSave

;
; create inputs file to make save file identical in format to Model save files
;  this requires running the defaults programs because they fill the Inputs structures
;  everything should be done as defaults and not effect data
;

  GridInputs=1
  ObsInputs=1
  LosInputs=1

  for_obsdefaults,ModPramsStruct.MagMod,ModPramsStruct.Name,ngrid=ngrid,ngy=ngy,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,limb=limb,gridtype=gridtype,$
        line=line,instrument=instrument,pos=pos,frequency_MHz=frequency_MHz,dogyro=dogyro,fcor=fcor,rotaz=rotaz,wavelength_Ang=wavelength_Ang,wavelength2_Ang=wavelength2_Ang,numion=numion,labelonly=labelonly,obsloslimit=obsloslimit,ObsInputs=ObsInputs,$
        donoise=donoise,seespec=seespec,aperture=aperture,norandom=norandom,resolution=resolution,integration=integration,modeffint=modeffint,modeffquant=modeffquant,efficiency=efficiency,background=background,tscope=tscope,errtrunc=errtrunc,NoisePrams=NoisePrams,$
        xrt=xrt,eit=eit,wl=wl,iris=iris,myspect=myspect,iondens=iondens,cds=cds,eis=eis,neviii770=neviii770,neviii780=neviii780,ovi1032=ovi1032,ovi1037=ovi1037,mgix706=mgix706,lya=lya,radio=radio,faraday=faraday,aia=aia,$
        euvia=euvia,euvib=euvib,trace=trace,swap=swap,kcor=kcor,cormag=cormag,rfilter=rfilter,$
        comp=comp,fe11comp=fe11comp,si9comp=si9comp,othercomp=othercomp,si10comp=si10comp,greencomp=greencomp,swss=swss,losem=losem,colden=colden,benergy=benergy,ben_dens_int=ben_dens_int,b_int=b_int,b_dens_int=b_dens_int,b_pos_int=b_pos_int,b_pos_dens_int=b_pos_dens_int,$
        pop2losem=pop2losem,pop2colden=pop2colden,pop2abundance=pop2abundance,pop2ionfrac=pop2ionfrac,$
        pop2tregime=pop2tregime,abundance=abundance,cversion=cversion,UserSpecFiles=UserSpecFiles,$
        ioneq=ioneq,UserTResp=UserTResp,PITeamResp=PITeamResp,LWidth=LWidth,LLim=LLim,InGofNT=InGofNT,OutGofNT=OutGofNT,$
        nangleint=nangleint,collkey=collkey,isotropic=isotropic,wpar_open=wpar_open,wpar_closed=wpar_closed,aniso_open=aniso_open,$
        chromorad=chromorad,blend=blend,einsteina=einsteina,einsteinb=einsteinb,gj=gj,$
        SpecPrams=SpecPrams,FCompPrams=FCompPrams,tp2te=tp2te,for_wlmin=for_wlmin,for_wlmax=for_wlmax,$
        smalln=smalln,qnorm=qnorm,cecoeff=cecoeff,icoll=icoll,isum=isum,isplin=isplin,$
        iwatom=iwatom,iwline=iwline,vintchoice=vintchoice,iweqi=iweqi,idebug=idebug,fiwatmo=fiwatmo,seecomp=seecomp,noftran=noftran,crtn=crtn,working_dir=working_dir,azequi=azequi,distobs=distobs,nowidgmess=nowidgmess

 for_griddefaults,ModPramsStruct.Name,pos,instrument,line,$
        gridtype=gridtype,ruser=ruser,thuser=thuser,phuser=phuser,coorduser=coorduser,phio=phio,$
        xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,limb=limb,ngrid=ngrid,ngy=ngy,$
        losoffset=losoffset,rheight=rheight,GridInputs=GridInputs,azequi=azequi,distobs=distobs,nowidgmess=nowidgmess

 for_losdefaults,ModPramsStruct.Name,gridtype,rheight,limb,instrument,line,pos,azequi,$
        losuse='NULL',dodisk='NULL',axisym='NULL',bang=B0,incres='NULL',thetao='NULL',phio='NULL',$
        losmin='NULL',losint='NULL',nlos='NULL',nostretch='NULL',$
        occult=occult,upoccult=upoccult,cmer=cmer,LosInputs=LosInputs,nowidgmess=nowidgmess


;    save,QuantMap,StokesStruct,ModPramsStruct,GridPramsStruct,ObsPramsStruct,$
;       LosPramsStruct,ModSolStruct,PlotSave,filename = saveplotname+'.sav'

    save,QuantMap,StokesStruct,ModPramsStruct,GridPramsStruct,ObsPramsStruct,$
       LosPramsStruct,ModSolStruct,GridInputs,LosInputs,ObsInputs,PlotSave,filename = saveplotname+'.sav'

    cd,current=thedirectory
    print,'saved file '+saveplotname+'.sav to directory '+thedirectory
  endif

; end if usefilename
 endif else mapname=''
; end if filename exist
endif else mapname=''
;
; if opened by file save a copy with our naming convention
;
; these next lines are redundant and unnecessary and buggy
;   actually, they are needed for the special situation of opening a fits file not downloaded through this code
;   which we don't necessarily want to remove - so will make a copy with
;   FORWARD naming conventions
;   I am not sure about buggy, but will keep an eye on
;
if byfile eq 1 then begin
  for_namecheckfile,date,instrument,line,ufname,typecomp=typecomp,typekcor=typekcor,wlcomp=wlcomp,gridtype=gridtype,rheight=rheight,/noshortcut,ucomp=ucomp
  if n_elements(working_dir) eq 1 then if working_dir ne '' then ufname=working_dir+slash+ufname 
  if filename ne ufname+'.fits' then begin
   if strpos(filename,'gz') lt 0 then begin
    file_copy, filename, ufname+'.fits',/overwrite
    cd,current=thedirectory
    print,'saved file '+ufname+'.fits to directory '+thedirectory
   endif else begin
    file_copy, filename, ufname+'.fits.gz',/overwrite
    cd,current=thedirectory
    print,'saved file '+ufname+'.fits.gz to directory '+thedirectory
   endelse
  endif
endif
label3:

;
; remove file that may have been created by for_specdefaults
;

file_delete,'IONEQ',/quiet

END
