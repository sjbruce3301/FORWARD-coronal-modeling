pro for_directdownload,instrument,dateuse,working_dir,filenames,nadd=nadd,line=line,typecomp=typecomp,typekcor=typekcor,wlcomp=wlcomp,nowidgmess=nowidgmess,rheight=rheight,ucomp=ucomp

;
; Program to download fits file from web
; Works for CoMP (QuickInvert) and SWAP data
;
;
; INPUTS 
;
;  INSTRUMENT (e.g. KCOR)
;
;  DATEUSE -requested time, which will be in UT but the average file for KCOR 
;		will look for the average file for that day in HST (for now)
;
;  WORKING_DIR -- working directory, set in for_plotfits 
;	note writediruse will be this plus a slash
;
; KEYWORD INPUTS
;
;  LINE - provides information that sets up naming conventions for files
;
;  TYPECOMP - currently only QUICKINVERT allowed for download, 
;	but could be expanded in future
;
;  TYPEKCOR - currently only EXTAVG allowed, but could be expanded in future
;		and we are testing CARRMAP
;		also added EXTAVGL1.5 for earlier averages and
;		FIRSTL1 for data that only has Level1, no average
;		so grab first image of the day
;
;  WRITEDIRUSE - where to keep files (default is working directory)
;
;  WLCOMP -- CoMP wavelength -- 1074 or 1079
;
;  UCOMP- 0 means CoMP, 1 means UCoMP
;
;  NOWIDGMESS -- suppresses widget popups
;
; KEYWORD OUTPUTS
;
;  NADD passes out additional plot labels
;
; OUTPUT
;
;  FILENAMES (string array of filenames that are downloaded)
;
;
; Called by FOR_PLOTFITS
;
; Calls FOR_GETBYURL, FOR_KCOREXTAVGTIME
;
; REQUIRES SWAP PACKAGE
;
; Written by Sarah Gibson, Laurel Rachmeler 2014
;  Mike Galloy 2020
;
; Version 2.0 July 2014
;--
; Feb 2018 -- updated naming convention to be robust to changes on CoMP end (SEG)
;	also added keyword nadd to allow labelling on plots
;	and keyword line to determine naming convention
; Mar 2018 -- updated so that remove empty fits files
; Jun 2019 - used file_delete for PC compatibility
;	kept some heritage usewindows to be safe with gzip
; July 2020 - updated so that given a date can read the extended average
;		KCor file (MDG) (note, future options might include
;		choosing nearest image instead)
;	 commented HOOKs below where new subroutines could increase
;	 precision of search 
;		Also passed through working_dir from for_plotfits to define writediruse
;		Also forced gunzip in case there was a crash and the file wasnt deleted
; August 2020 -- added Carrington map capability
; August 2021 -- updated to grab first L1 image if there is no EXTAVG -- will reset TYPECOR='FIRSTL1'
;		(early data) - for EXTAVG will also adjust filename for level
;			which is carried in variable "found"
; December 2021 -- modified so typecomp could include more info, e.g., NADD
; January 2024 -- updated for naming convention ucomp, passed through ucomp keyword
;		commented out widget message
; March 2024 -- switched order of looking for UCoMP from median to mean
;	

;
; test for PC
;
usewindows=0
if strupcase(!version.os_family) eq 'WINDOWS' then usewindows=1

slash=path_sep()

writediruse=working_dir+slash

time=anytim(dateuse,/ccsds)

;
; HOOK
; Add subroutine here to determine closest HST observing date 
;  IF MLSO and TYPECOMP/TYPEKCOR set to QUICKINVERT/EXTAVG
; HOOK 
;  IF MLSO and TYPECOMP/TYPEKCOR set to something else like NEARNEXT, NEARLAST
; we could also have this know that it is seeking the closest file, not the
; averagd -- then it would figure out the full time in HST for that nearby file
; and we will also have to edit the FOR_MG codes

year=strmid(time,0,4)
month=strmid(time,5,2)
day=strmid(time,8,2)

IF strupcase(instrument) EQ 'SWAP' THEN BEGIN

   swapobj = obj_new('swap')
   swapobj -> set,filter='lv1'
   files = swapobj -> list( timerange = strmid(time,0,10)+' '+strmid(time,11,8) )

   IF files EQ '' THEN BEGIN 
      IF keyword_set(nowidgmess) THEN message,/info,'Sorry, no SWAP files found, try a different date/time.' ELSE $
         d=dialog(/WARNING,'Sorry, no SWAP files found, try a different date/time.')
      filenames=''
   ENDIF ELSE BEGIN
      swapobj -> copy, filelist = files[0]
      filenames = strmid(files[0],46,29)
   ENDELSE

   obj_destroy, swapobj

ENDIF

; download KCor extended average for a day
if (strupcase(instrument) eq 'KCOR') then begin

  ; TODO: use VSO_SEARCH to find exact time(s)
  ;start_time = string(year, month, day, format='(%"%s-%s-%sT00:00:00")')
  ;end_time   = string(year, month, day, format='(%"%s-%s-%sT23:59:59")')
  ;results    = vso_search(start_time, end_date, $
  ;                        instrument='k-cor', $
  ;                        /urls, count=n_results, /quiet)
  ; results.time.start is the format 2020-01-02T01:00:13

; Note -- the code above is the preferred simple method,
;  but we have found the connection between VSO 
;  and MLSO to be unreliable due to firewire issues 
;  below is a somewhat inelegant but more robust method.

  if strupcase(typekcor) ne 'EXTAVG' and strupcase(typekcor) ne 'CARRMAP' and strupcase(typekcor) ne 'FIRSTL1' then begin
    if keyword_set(nowidgmess) then message,/info,'only accessing daily average kcor files for now or first image of the day if average doesnt exist. You can download others from e.g. http://mlso.hao.ucar.edu/mlso_data_calendar.php?calinst=kcor and then read them in via keyword FILENAME.' else $
     d=dialog(/WARNING,'only accessing daily average kcor files or first image of the day if average doesnt exist for now. You can download others from e.g. http://mlso.hao.ucar.edu/mlso_data_calendar.php?calinst=kcor and then read them in via keyword FILENAME.')
    typekcor='EXTAVG'
  endif

; -- NOTE for KCOR  there is a chance that the actual files year/month/day is different
; than the inputted, since it looks for first available (EXTAVG or FIRSTL1) for the inputted
; date in HI time -- which is usually the same UT, but there is a small chance if observing starts
; late it pushes into the next UT day. If so, for_kcorextavgtime will overwrite year, month, date 
; but the directory needs to remember the entered year, month, date
;
  year_url=year
  month_url=month
  day_url=day

  found=0.
  if strupcase(typekcor) eq 'EXTAVG' or strupcase(typekcor) eq 'FIRSTL1' then begin
;
; finds time of extended average given year month day but ignores
; time input from FORWARD
;
   extavg_time = for_kcorextavgtime(year, month, day, typekcor, found=found) 
   if found ne 0. then begin
    hour = strmid(extavg_time, 0, 2)
    min = strmid(extavg_time, 2, 2)
    sec = strmid(extavg_time, 4, 2)
;
; if there is no EXTAVG, will grab first image, so typekcor needs to change
;
    if found eq 1. then typekcor='FIRSTL1'
   endif 
  endif else extavg_time=0L

  rspsave = 0

  if strupcase(typekcor) eq 'CARRMAP' or (extavg_time ne !null and extavg_time ne 0L) then begin

   if strupcase(typekcor) eq 'EXTAVG' and found eq 2. then filenames = string(year, month, day, hour, min, sec, $
                     format='(%"%s%s%s_%s%s%s_kcor_l2_extavg.fts.gz")')
   if strupcase(typekcor) eq 'EXTAVG' and found eq 1.5 then filenames = string(year, month, day, hour, min, sec, $
                     format='(%"%s%s%s_%s%s%s_kcor_l1.5_extavg.fts.gz")')
   if strupcase(typekcor) eq 'FIRSTL1' then filenames = string(year, month, day, hour, min, sec, $
                     format='(%"%s%s%s_%s%s%s_kcor_l1.fts.gz")')

   url_path = string(year_url, month_url, day_url, format='(%"hao/acos/%s/%s/%s")')

   if strupcase(typekcor) eq 'CARRMAP' then begin
    rheightint=strtrim(string(fix(rheight*100)),2)
    filenames = string(year, month, day, $
                     format='(%"%s%s%s.kcor.28day.synoptic.r'+rheightint+'.fts")')
    url_path = string(year_url, month_url, format='(%"hao/acos/synmaps/kcor/%s/%s")')
   endif

   urlpathuse = url_path + slash +  filenames
   print,urlpathuse

  ;print, urlpathuse, format='(%"retrieving %s...")'
   for_getbyurl, filenames, urlpathuse, $
                writediruse=writediruse, $
                rspcode=rspsave, $
                usehost='mlso.hao.ucar.edu'

   if rspsave eq 0 then begin
 ; unzip
    if (not usewindows) then begin
     spawn, 'gunzip -f ' + filenames
     filenames = file_basename(filenames, '.gz')
    endif
   endif else begin
    file_delete, filenames, /quiet, /allow_nonexistent
    filenames=''
   endelse

 endif else filenames=''

endif

IF strupcase(instrument) EQ 'COMP' THEN BEGIN

  if strpos(strupcase(wlcomp),'1074') ge 0 then usewlcomp=1074
  if strpos(strupcase(wlcomp),'1079') ge 0 then usewlcomp=1079
  if strpos(strupcase(wlcomp),'637') ge 0 then usewlcomp=637
  if strpos(strupcase(wlcomp),'706') ge 0 then usewlcomp=706
  if strpos(strupcase(wlcomp),'789') ge 0 then usewlcomp=789
  if strpos(strupcase(wlcomp),'530') ge 0 then usewlcomp=530

  if strpos(strupcase(typecomp),'QUICKINVERT') lt 0 then begin
    message,/info,'only accessing quickinvert files for now. You can download others from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+strmid(dateuse,0,4)+'.html and then read them in via keyword FILENAME.'
;    if keyword_set(nowidgmess) then message,/info,'only accessing quickinvert files for now. You can download others from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+strmid(dateuse,0,4)+'.html and then read them in via keyword FILENAME.' else $
;     d=dialog(/WARNING,'only accessing quickinvert files for now. You can download others from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+strmid(dateuse,0,4)+'.html and then read them in via keyword FILENAME.')
    typecomp='QUICKINVERT'
  endif

  if ucomp eq 0 then begin
   URLPATHUSE1 = 'hao/acos/'+year+'/'+month+'/'+day+'/'+year+month+day+'.comp.'+strtrim(string(usewlcomp),2)+'.quick_invert'
   filenames1= year+month+day+'.comp.'+strtrim(string(usewlcomp),2)+'.quick_invert'
   URLPATHUSE = URLPATHUSE1 + '.fts.gz'
   filenames= filenames1  + '.fts.gz'
  endif else begin
;
; UCoMP quickinverts have synoptic/waves/mean/median naming
;
   URLPATHUSE1 = 'hao/acos/'+year+'/'+month+'/'+day+'/'+year+month+day+'.ucomp.'+strtrim(string(usewlcomp),2)+'.l2.synoptic.mean'
   filenames1= year+month+day+'.ucomp.'+strtrim(string(usewlcomp),2)+'.l2.synoptic.mean'
   URLPATHUSE = URLPATHUSE1 + '.fts'
   filenames= filenames1  + '.fts'
  endelse

  rspsave=0

  for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspsave,usehost='mlso.hao.ucar.edu'
  if ucomp eq 1 then nadd='synoptic_mean'

  if rspsave ne  0 then begin
   for file_i=0, n_elements(filenames)-1 do file_delete, filenames[file_i], /quiet
   rspsave=0
   if ucomp eq 0 then begin
    URLPATHUSE = URLPATHUSE1 + '.median.synoptic.fts.gz'
    filenames= filenames1  + '.median.synoptic.fts.gz'
   endif else begin
    URLPATHUSE1 = 'hao/acos/'+year+'/'+month+'/'+day+'/'+year+month+day+'.ucomp.'+strtrim(string(usewlcomp),2)+'.l2.synoptic.median'
    filenames1= year+month+day+'.ucomp.'+strtrim(string(usewlcomp),2)+'.l2.synoptic.median'
    URLPATHUSE = URLPATHUSE1 + '.fts'
    filenames= filenames1  + '.fts'
   endelse
   for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspsave,usehost='mlso.hao.ucar.edu'
   if ucomp eq 0 then nadd='median_synoptic' else nadd='synoptic_median'
   trywaves=0
   if rspsave ne  0  then begin
    for file_i=0, n_elements(filenames)-1 do file_delete, filenames[file_i], /quiet
    rspsave=0
    if ucomp eq 0 then begin
     URLPATHUSE = URLPATHUSE1 + '.median.waves.fts.gz'
     filenames= filenames1  + '.median.waves.fts.gz'
    endif else begin
     URLPATHUSE1 = 'hao/acos/'+year+'/'+month+'/'+day+'/'+year+month+day+'.ucomp.'+strtrim(string(usewlcomp),2)+'.l2.waves.median'
     filenames1= year+month+day+'.ucomp.'+strtrim(string(usewlcomp),2)+'.l2.waves.median'
     URLPATHUSE = URLPATHUSE1 + '.fts'
     filenames= filenames1  + '.fts'
    endelse
    for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspsave,usehost='mlso.hao.ucar.edu'
    trywaves=1
    nadd='median_waves'
    if ucomp eq 0 then nadd='median_waves' else nadd='waves_median'
   endif 
   if rspsave eq 0 and exist(line) eq 1 and trywaves eq 0 and ucomp eq 0 then begin
;
; old comp files had Q,U in the waves, not synoptic, so need to grab that quickinvert
; if asking for polarization data
;
    if strupcase(line) ne 'STOKESI' and strupcase(line) ne 'LINEWIDTH' and strupcase(line) ne 'DOPPLERVLOS' then begin
     URLPATHUSE = URLPATHUSE1 + '.median.waves.fts.gz'
     filenames= filenames1  + '.median.waves.fts.gz'
     for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspsave,usehost='mlso.hao.ucar.edu'
     nadd='median_waves'
    endif
    if rspsave ne 0 then begin
     for file_i=0, n_elements(filenames)-1 do file_delete, filenames[file_i], /quiet
     rspsave=0
     URLPATHUSE = URLPATHUSE1 + '.median.synoptic.fts.gz'
     filenames= filenames1  + '.median.synoptic.fts.gz'
     for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspsave,usehost='mlso.hao.ucar.edu'
     nadd='median_synoptic'
    endif 
   endif
  endif 

  if rspsave eq 0 then begin
   print,'found file at ',urlpathuse

 ; unzip

   filenamesnew=filenames
   if usewindows eq 0 and ucomp ne 1 then begin
    spawn,'gunzip -f '+filenames
    filenamesnew=str_replace(filenames,'.gz','')
   endif
   filenames=filenamesnew
  endif else begin
   for file_i=0, n_elements(filenames)-1 do file_delete, filenames[file_i], /quiet
   filenames=''
  endelse

  if exist(nadd) then begin
   if nadd ne '' then typecomp='QUICKINVERT_'+strupcase(nadd)
  endif else typecomp='QUICKINVERT'
ENDIF

end
