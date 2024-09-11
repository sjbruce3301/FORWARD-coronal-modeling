pro for_settingdefaults,date=date,$
        readprams=readprams,readmap=readmap,usedfile=usedfile,noplots=noplots,$
        bg_input=bg_input,$
        bg_output=bg_output,$
        norunmodel=norunmodel,working_dir=working_dir,memory=memory,parallel=parallel,$
        datadump=datadump,verbose=verbose,reinit=reinit,nreinit=nreinit,noforward=noforward,$
	SettingInputs=SettingInputs

;
; this program sets defaults for the forward calculation settings.
;
;  Keywords defined below
;
;  Optional output keyword SettingInputs gathers all keyword inputs
;	useful for widget setup
;
;  Called by FOR_DRIVE, FOR_WIDGET
; 
;  Written 2013 Sarah Gibson
;  Version 2.0 July 2014
;   Started to replace usewindows 
;   but decided to keep heritage usewindows to be safe
;  Feb 2022 -- added bg_input,bg_output
;

;
;       keyword DATE (string) - date of observations
;               used in FOR_INTENSINT and FOR_POS_MAP, for example for STEREO position
;               and/or calibration time (various programs in OBSERVABLES)
;		also used to identify viewer position (FOR_VIEWFROMDATA)
;		and to choose PFSS datacube (PFSSMODPRAMS)
;		and to choose PSI datacube (MAKEMASCUBE)
;		and also TOMO datacube
;		and also STRIA datacube
;               Default empty string means unset
;		Saved in ObsPramsStruct.Date
;
default,date,''

;
;  INPUT keywords that control how the code is called and run 
;

;
;      keyword READPRAMS(string), if set uses saved set of (input) parameters
;		Default '' means unset
;

default,readprams,''

;
;      keyword READMAP(string), if set can rerun a forward calculation either identically or
;               with some changes to the way data is displayed, by setting readmap="(mapname).sav"
;               along with plotting keywords. Can also change line being plotted, but only if CoMP 
;		or POS quantity. 
;               ***SAVE FILE VIA FOR_DRIVE,/SAVEMAP,MAPNAME='myfile' ***
;               ***RESTORE FILE VIA FOR_DRIVE,READMAP='myfile.sav'
;   		***NOTE MAPNAME AND SAVEMAP ARE SET AND DOCUMENTED IN FOR_OUTPUTDEFAULTS***
;		Default '' means READMAP IS unset
;		Internal to FOR_DRIVE.

default,readmap,''


; 	keyword USEDFILE (string) - extract viewpoint from
;		fits data file
;		Used in subroutine for_viewfromdata, called by for_drive
;		

default,usedfile,''



;	
;      keyword NORUNMODEL(1 or 0) -- forces the use of existing ModSolStruct rather than
;		doing a plane-of-sky calculation.  Useful for fieldline plotting
;		on top of an integrated observable, because in the course of generating the 
;		observable a ModSolStruct for the plane of sky will be saved.
;		Also if READMAP is set (so one is drawing from a prior run)
;		and POS (so one is requesting a replot or the plotting of a 
;		different POS variable) it will not bother rerunning since
;		ModSolStruct saves all the model plasma quantities.
;               Usually should be set to 1, unless for example if using an old READMAP 
;		save file for which ModSolStruct wasnt saved in the same way.
;		Actually used in FOR_POSNOINT: if not passed as a keyword then
;		***defaults there to zero*** - this is necessary because for_posnoint
;		is called from various places throughout the tree, usually without
;		the norunmodel keyword option. FOR_PLOTFIELDLINES and FOR_DRIVE
;		are the only places where FOR_POSNOINT is called with norunmodel keyword.
;

default,norunmodel,1

;	keyword PARALLEL (0, 1, or 2) -- this enables breaking up the action within FOR_DRIVE
;		so that in a first step (when PARALLEL = 1) it gets as far as dumping the FOR_COMP
;		input files "modelcube4comp.dat", also "atom", "input", "ioneq" into WORKING_DIR
;		and then exits with no further output.  This can be done within a loop where 
;		multiple outputs are sent to multiple choices of WORKING_DIR.  At this point, 
;		FOR_COMP fortran routines may from another computer which does not need to have IDL
;		and then all the fortran output files "F77_OUTPUT" for these runs can be sent to all the directories.
;		Finally, FOR_DRIVE can be run again as a second step (with PARALLEL set = 2) where it
;		runs as usual, but does not run FOR_COMP itself but instead looks inside WORKING_DIR (or
;		a set of WORKING_DIRs) for F77_OUTPUT and carries on as usual.  Default is PARALLEL=0,
;		which makes none of these changes -- also, if FOR_DRIVE is called in a mode that does not
;		call FOR_COMP, PARALLEL is ignored.
;

default,parallel,0

;      keyword NOPLOTS (1 or 0) - suppress image plotting.
;		If USERINPUT or PARALLEL = 1 then this is forced.
;		Internal to FOR_DRIVE.
;
default,noplots,0
if parallel eq 1 then noplots=1 


;       keyword WORKING_DIR (string) -- if set, fortran executable is run from working_dir, and I/O goes into
;               working_dir.  Used in FOR_OBS_NAME (in deciding which atom/input files to use
;		and FOR_INTENSINT (in running fortran).
;		DEFAULT '' means unset
;

usewindows=0
if strupcase(!version.os_family) eq 'WINDOWS' then usewindows=1
slash=path_sep() 

if GET_ENVIRON('FORWARD_WORKING_DIR') ne '' then begin
     default,working_dir,GET_ENVIRON('FORWARD_WORKING_DIR')
endif else begin
     default,working_dir,''
endelse
if usewindows eq 0 then begin
 if working_dir ne '/tmp' and working_Dir ne '' then working_dir='/'+strjoin(strsplit(working_dir,/extract,'/'),'/')
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
; this might work instead of above (not for_plotfits also has something like thi)
;
;if working_dir ne slash+'tmp' and working_dir ne '' then working_dir=file_dirname(working_dir)+slash+file_basename(working_dir)

;       keyword    BG_INPUT,BG_OUTPUT -- a way of providing
;                 background images to subtract off
;                 in particular, if bg_output is set to a named file
;                 integrated pB,B will be saved in it
;                 conversely -- if bg_input is set it will be subtracted off
;                 from pB, B before calculating POL
;	default,unset
;	   
;
        default,bg_input,''
        default,bg_output,''

;
;       keyword MEMORY (integer) -- set to greater than zero if want to loop one of the grid dimensions,
;              to save IDL memory, if set to other than one then 
;		nmem=memory pixel arrays will be looped through a total of 
;		(ngrid*ngy)/nmem times (bites) 
;		memory = 1 goes to default (10000)
;		NOTE MEMORY = -1 will force nmem=1, so there will be a loop of ngrid steps
;		Some machines may be able to run with memory=0, so, no loops, 
;		(without IDL crashing) and this should improve run time.
;
;		Also is needed if one wants to set OBSLOSLIMIT which will change
;		NLOS and LOSMIN as a function of position in the plane of sky
;		such that LOSMIN is greater than radial height OSBLOSLIMIT
;
;		default of 10000 is something that hopefully works for most systems
;		Used in FOR_INTENS_INT
;		

default,memory,10000

; if bg_input or bg_output are set, force memory=0 so the files are complete

if keyword_set(bg_input) or keyword_set(bg_output) then begin
  print,'changing memory setting to no loops'
  memory=0
endif

;
;       keyword VERBOSE (1 or 0) -- allows comments to print during run
;		especially those pertaining to CoMP
;		used in FOR_INTENSINT and FOR_READCOMPRESULT
;

default,verbose,0

;
;       keyword DATADUMP (1 or 0) -- allows data dump of physical state data
;		note turns off memory
;		used in FOR_INTENSINT 
;

default,datadump,0

;
;       keyword REINIT (1 or 0) -- For speed in LOS integration of observables. 
;		Set reinit=1 on first time in a loop, to calculate response functions.  On subsequent times,
;                    use reinit = 0 as response functions are in common blocks.
;
;		     Allowed to be set at top level (here) to zero for example
;			if doing multiple calls to for_drive where the same
;			observable is being used.
;
;		     However, note, if for_drive run again with a different observable (even same
;		     instrument) reinit should go back to 1, because these will produce different 
;			lookup tables for the common blocks.
;
;		     Also, note that if POP2TREGIME is set (see FOR_OBSDEFAULTS)
;			reinit will be effectively always on
;
;		Used in the various *FLUX.PRO in OBSERVABLES/ as called by FOR_INTENSINT.
;

default,reinit,1

;
;       keyword NREINIT (1 or 0) -- For speed -- when one, numerical data cube is restored.  
;			After that, common block is used to access data cube.
;		Unlike REINIT - if a different observable or view or anything is used
;			but the same numerical datacube, it is still ok to have NREINIT be zero
;			when calling for_drive again.  Used in FOR_INTENSINT/FOR_POSNOINT and NUMCUBE/ADAPTCUBE.
;

default,nreinit,1

;
;	keyword NOFORWARD (1 or 0) - if set, will only set up structures but
;	not actually run for_forwardmap (which does the forward calculation)
;	Internal to FOR_DRIVE
;	Default 0 - unset
;

default,noforward,0

;
; define structure containing "settings" inputs
;  to be used in widget interface
; 

SettingInputs={Date:date,DateVal:'calendar',ReadPrams:readprams,ReadPramsVal:'searchfileifset',ReadMap:readmap,ReadMapVal:'searchfileifset',$
	UseDFile:usedfile,UseDFileVal:'nodisplay',$
;	UseDFile:usedfile,UseDFileVal:'searchfileifset',$
;	NoPlots:noplots,NoPlotsVal:'tog',$
	BG_Input:bg_input,BG_InputVal:'searchfileifset',$
	BG_Output:bg_output,BG_OutputVal:'string',$
	NoRunModel:norunmodel,NoRunModelVal:'nodisplay',Working_Dir:working_dir,Working_DirVal:'searchdirifset',Memory:memory,MemoryVal:'integer',$
	Datadump:datadump,DatadumpVal:'integer'$
,Reinit:reinit,ReinitVal:'nodisplay',NReinit:nreinit,NReinitVal:'nodisplay'}

;print,'memory=',memory
end
