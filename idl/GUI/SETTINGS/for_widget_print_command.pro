PRO for_widget_print_command,verbose,printresult=printresult
  
;+
;  Name:  FOR_WIDGET_PRINT_COMMAND
;
;  This program builds the print command that can be used
;	directly as input to FOR_DRIVE
;	 This is the full command -- verbose = 1 will
;	  also generate and save a short command for reproducing
;	  image "from scratch" and print in IDL window. This is done by choosing "Print Command"
;	   It's not done for DATA because that would require having the 
;	   Data save file already in hand, so not really a reproducible thing
;	if verbose set, long and short saved in GUI_COMMAND_LOG.TXT
;
; Called by FOR_WIDGET_EVENT, FOR_WIDGET_SETTINGS_EVENT
;
; External call to FOR_WIDGET_BUILD
;
;
; Written by Sarah Gibson, Blake Forland, 2013-2014
; Version 2.0 July 2014
;   	Feb 2016 fixed some extraneous print command output - SEG
;	   resolution, pop2on should be kept internal
;	Feb 2018 added exceptions for new model variables 
;	May 2019 - fixed exceptions to work with new densprof normalizations
;		I think this was just causing unnecessary extra lines on the print command
;	June 2019 -- used slash for PC compatibility
;	September 2021 -- 
;	   changed "if verbose" to "if verbose eq 1" and also
;	   removed "if verbose" from within main conditional becuase redundant
;	   also changed so that only if verbose set the gui_command_log is set
;		(unnecessary working directory clutter)
;    	   also made build from variables conditional on not DATA
;	   also testing azequi
;	October 2021 -- fixed bug where azuse was being passed into shortcommand string as integer
;		also removed azequi and from carrmap shortcommand
; 	December 2021 -- passed through distobs
;	January 2022 -- passed azequi through losdefaults
;	April 2022 -- removed tag add for odensprof etc -- this
;		was to avoid unnecessary parts of print command
;		but now I think these are in modeldefaults
;	June 2022 -- no, I think they may be needed after all
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Feb 2023 -- changed instr to instrument in call to for_plotdefaults
;-

  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
  
  slash=path_sep()

;
; get defaults for given model, gridtype, pos, instrument, line, and azequi
; unless data plot
; and only if verbose set
;
  modeluse=modops.model
  griduse=gridops.gridtype
  posuse=obsops.pos
  instuse=obsops.instrument
  lineuse=obsops.line
  azuse=gridops.azequi
  distuse=gridops.distobs
  if strupcase(modeluse) ne 'DATA' and verbose eq 1 then begin
   for_settingdefaults,SettingInputs=settingsdefaults
   usedate=settingsdefaults.date
   useworkdir=settingsdefaults.working_dir
   for_outputdefaults,OutputInputs=outputdefaults
   usenoerase=outputdefaults.noerase

   modeldefaults=1
   for_modeldefaults,modeluse,date=usedate,working_dir=useworkdir,modelinputs=modeldefaults,modpramsstruct=magtest
   if tag_exist(variables,'cuberot') eq 1 then modeldefaults=add_tag(modeldefaults,0.d0,'cuberot')
   if tag_exist(variables,'topology') eq 1 then modeldefaults=add_tag(modeldefaults,1,'topology')
; 
; I don't think these are needed because they are part of modeldefaults explicitly now?
;  had them commented but now have uncommented because was causing problems -- keep an eye on
;   -- yes, when do print command gives an error about duplicate keywords - perhaps there are 
;  some models they aren't defined for and it is the transition that is screwing up
;  Adding test for existence before adding
;
   if tag_exist(variables,'odensprof') eq 1 $
     and tag_exist(modeldefaults,'odensprof') ne 1 $
     then modeldefaults=add_tag(modeldefaults,'1.','odensprof')
   if tag_exist(variables,'oT0') eq 1 $
     and tag_exist(modeldefaults,'oT0') ne 1 $
     then modeldefaults=add_tag(modeldefaults,'1.5d6','oT0')
   if tag_exist(variables,'cdensprof') eq 1 $
     and tag_exist(modeldefaults,'cdensprof') ne 1 $
    then modeldefaults=add_tag(modeldefaults,'1.','cdensprof')
   if tag_exist(variables,'cT0') eq 1 $
     and tag_exist(modeldefaults,'cT0') ne 1 $
     then modeldefaults=add_tag(modeldefaults,'1.5d6','cT0')
   for_viewfromdata,date=usedate,gridtype=griduse,cmer=cmer,bang=bang
   if useworkdir ne settingsdefaults.working_dir then settingsdefaults.working_dir = useworkdir
   if settingsdefaults.date ne usedate then settingsdefaults.date=usedate
   magmoduse=magtest.magmod
;
; since the numerical models use a common block, this needs to be reset with original values
;
   modplace=1
   z=execute("for_modeldefaults,'"+modops.model+"'"+modops.modelstring+",date='"+settings.date+"',working_dir='"+settings.working_dir+"',ModelInputs=modplace")

   usespecon=obsops.seespecinputs.seespec
   if tag_exist(obsops.seespecinputs,'isotropic') eq 1 then useisoon=obsops.seespecinputs.isotropic else useisoon=0
   usecompon=obsops.seecompinputs.seecomp
   for_obsdefaults,magmoduse,modeluse,line=lineuse,instrument=instuse,gridtype=griduse,azequi=azuse,distobs=distuse,pos=posuse,seespec=usespecon,isotropic=useisoon,seecomp=usecompon,ObsInputs=obsdefaults,working_dir=useworkdir
;
; Pop2 will be taken care of with pop2tregime
; used for convenience with widget; should not be a line command input value
;
   obsdefaults.pop2on=obsops.pop2on
;
; Resolution will be taken care of by ngrid,ngy,xrange,yrange
; used mainly for output for plot titles; should not be a line command input value
;
   obsdefaults.donoiseinputs.resolution=obsops.donoiseinputs.resolution
   usedonoise=obsdefaults.donoiseinputs.donoise
   if obsdefaults.line ne lineuse then stop
   if obsdefaults.instrument ne instuse then stop
   if strupcase(instuse) eq 'PHYSICAL DIAGNOSTICS' then obsdefaults.pos=-1.
   for_griddefaults,modeluse,posuse,instuse,lineuse,gridtype=griduse,azequi=azuse,distobs=distuse,gridinputs=griddefaults
   userheight=griddefaults.rheight
   uselimb=griddefaults.limb
   for_losdefaults,modeluse,griduse,userheight,uselimb,instuse,lineuse,phio=0.d0,posuse,azuse,cmer=cmer,bang=bang,losinputs=losdefaults
   if uselimb ne griddefaults.limb then griddefaults.limb=uselimb
   if userheight ne griddefaults.rheight then griddefaults.rheight=userheight
   usedodisk=losdefaults.dodisk
   for_plotdefaults,magmoduse,modeluse,plotinputs=plotdefaults,gridtype=griduse,dodisk=usedodisk,noerase=usenoerase,line=lineuse,instrument=instuse,donoise=usedonoise,pos=posuse
   if usedodisk ne losdefaults.dodisk then losdefaults.dodisk=usedodisk
   if usedonoise ne obsdefaults.donoiseinputs.donoise then obsdefaults.donoiseinputs.donoise=usedonoise
   if usenoerase ne outputdefaults.noerase then outputdefaults.noerase=usenoerase

   printdefaults = "for_drive,'"+modops.model+"',"
   for_widget_build,losdefaults,printresult=printdefaults
   printdefaults=printdefaults+","
   for_widget_build,griddefaults,printresult=printdefaults
   printdefaults=printdefaults+","
   plotdefaults.winnumber = 0
   for_widget_build,plotdefaults,printresult=printdefaults
   printdefaults=printdefaults+","
   for_widget_build,modeldefaults,printresult=printdefaults
   printdefaults=printdefaults+","
   for_widget_build,outputdefaults,printresult=printdefaults
   printdefaults=printdefaults+","
   for_widget_build,obsdefaults,printresult=printdefaults
   printdefaults=printdefaults+","
   for_widget_build,settingsdefaults,printresult=printdefaults
 
  endif

  printresult = "for_drive,'"+modops.model+"',"
  
  for_widget_build,losops,printresult=printresult
  printresult=printresult+","
  
  for_widget_build,gridops,printresult=printresult
  printresult=printresult+","
  
  winnumsave=plotops.winnumber
  if verbose eq 1 then plotops.winnumber = 0

  for_widget_build,plotops,printresult=printresult
  printresult=printresult+","

  plotops.winnumber = winnumsave

  if strupcase(modeluse) ne 'DATA' then begin
   for_widget_build,variables,printresult=printresult
   printresult=printresult+","
  endif

  for_widget_build,outops,printresult=printresult
  printresult=printresult+","

  for_widget_build,obsops,printresult=printresult
  printresult=printresult+","

  nreinitsave=settings.nreinit
  reinitsave=settings.reinit
  if verbose eq 1 then settings.nreinit = 1
  if verbose eq 1 then settings.reinit = 1
  for_widget_build,settings,printresult=printresult
  settings.nreinit=nreinitsave
  settings.reinit=reinitsave

  currenttime = SYSTIME(/UTC)
  
  fname='gui_command_log.txt' 

  if settings.working_dir ne '' then begin
        fnamefull=settings.working_dir+slash+fname
  endif else fnamefull=fname

  if strupcase(modeluse) ne 'DATA' and verbose eq 1 then begin
;
; only keep differences
;
   if strupcase(griduse) eq 'PLANEOFSKY' then shortcommand="for_drive,'"+modeluse+"',instrument='"+instuse+"',line='"+lineuse+"',gridtype='"+griduse+"',azequi="+strtrim(string(azuse),2)+",distobs="+strtrim(string(distuse),2)+",pos="+strtrim(string(posuse),2) $
   else shortcommand="for_drive,'"+modeluse+"',instrument='"+instuse+"',line='"+lineuse+"',gridtype='"+griduse+"',pos="+strtrim(string(posuse),2) 
   newarray=strsplit(printresult,',',/extract)
   nitems=size(newarray)
   nitems=nitems[1]
   defarray=strsplit(printdefaults,',',/extract)
   ditems=size(defarray)
   ditems=ditems[1]
   if nitems ne ditems then stop
   beginarray=0
   addarray=''
   savearray=0
   for i = 0,nitems-1 do begin
      if strpos(strupcase(newarray[i]),'[') ge 0 then beginarray=1 
      if beginarray eq 1 then begin
        addarray=addarray+','+newarray[i]
        if (strupcase(newarray[i]) ne strupcase(defarray[i]) and strpos(strupcase(newarray[i]),'NULL') eq -1) then savearray=savearray+1
      endif else begin
        if (strupcase(newarray[i]) ne strupcase(defarray[i]) and strpos(strupcase(newarray[i]),'NULL') eq -1) then shortcommand=shortcommand+','+newarray[i]
      endelse
      if strpos(strupcase(newarray[i]),']') ge 0 then begin
        if savearray ne 0 then shortcommand=shortcommand+addarray
        beginarray=0 
	addarray=''
        savearray=0
      endif
   endfor
   print,shortcommand
  endif

;
; keep a record of the full command 
;  and short command if verbose eq 1 set
;
  
  if exist(shortcommand) then begin
    OPENW,unit,fnamefull,/get_lun
    printf,unit,currenttime
    printf,unit,printresult[0]
    printf,unit,shortcommand
    free_lun,unit
  endif
  
  
END
