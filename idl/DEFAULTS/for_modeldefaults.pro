pro for_modeldefaults,model,ModPramsStruct=ModPramsStruct,defineprams=defineprams,date=date,working_dir=working_dir,readprams=readprams,saveprams=saveprams,nreinit=nreinit,nowidgmess=nowidgmess,_extra=extra,ModelInputs=ModelInputs

;
; this program sets model and model parameters, either through defaults or as covered by _EXTRA
;                       and described and provided with defaults in the various "MODNAME/modname"prams.pro

;
; INPUTS
;
;   MODEL - Must be a string.  Choices now are 'giblow', 'liteslow','lowhund', 'mydipole',
;       'gibbaglow', 'pfssmod','numcube', 'psimas', and 'cavmorph'
;	(under development) 'adaptcube' 'stria' 'turbhy' 'syncom' 'awsom' 'cirtoy'
;
;    cavmorph - 3D streamer plus cavity
;      	in spherically symmetric background.  Density and temperature
;      	only (although also has some outputs of morphological properties)
;
;    giblow - (currently default in tree) 3D MHD model of tethered spheromak flux
;      	rope in equilibrium. Density,temperature, magnetic field, current, velocity.
;
;    liteslow - 3D MHD model of tethered spheromak flux rope not in
;      	equilibrium. Density,temperature, magnetic field, velocity.
;
;    lowhund - cylindrical flux rope model. originally in cartesian,
;      	it has been hacked to be azimuthally symmetric. Should still be
;      	near mhd equilibrium for a small rope.
;
;    gibbaglow -- dipole+octopole magnetic field with current sheets around and above 
;		closed field, with hydrostatic atmosphere different open vs closed, optional
;	velocity along open field lines
;
;    mydipole -- dipole magnetic field with hydrostatic atmosphere open vs closed, optional
;	velocity along open field lines
;
;    numcube - numerical model. Numerical data cube must be in a specific format.
;		psicube is a variety of this specific to the Predictive Science Inc
;		MAS code
;
;    adaptcube-- variation allowing for adaptive mesh numerical cubes 
;		UNDER DEVELOPMENT
;
;    stria -- global striated density model
;
;    syncom -- Moraes statistical blob flow model
;
;    turbhy -- global turbulence density model
;
;    pfssmod - Forward model a pfss calculation at a specific date.
;
;    psimas - Forward model Predictive Science MAS MHD cube for Carrington rotation associated with date
;
;    awsom - Forward model U Mich AWSOM model for Carrington rotation associated with date
;
;   cirtoy - CIR toy model (density)
;
;   croissant - forward model a croissant CME (Morgan)
;
;   tomo -- tomographic density cubes (Morgan)
;
;   keyword DEFINEPRAMS (structure name or 0), if set, allows the repeat use of a preset model
;               (output) parameter structure in its entirety.  For example, if one runs FOR_DRIVE
;               with the output keyword ModPramsStruct set (see below), this can be used
;               as the input defineprams in later runs. Internal to FOR_DRIVE.
;               Probably only needed for command-line calls to FOR_DRIVE (not widget-based).
;
;               This can alternatively be done by setting model parameters SAVEPRAMS and READPRAMS
;               which will save the input parameters to a file 
;		(argument of saveprams) and then later can use those read in from a file 
;		(argument of readprams) -- this is useful when a record of the input parameters
;               is wanted.  However, defineprams is useful when one just wants to pass the output model
;               parameter structure in from the command line without any file I/O.
; 		Another important difference is that if using READPRAMS one can 
;		change select model input parameters and the rest default to
;		READPRAMS -- but if one uses DEFINEPRAMS it will use that as
;		the ModPramsStruct output with no model parameter changes.
;
;  		If called with keyword SAVEPRAMS=1 saveprams is filled with input model parameters 
;		and one could also pass from command line (as READPRAMS) without any file I/O. Would necessitate
;		a call to modprams.pro, though. So mostly useful for widget setup - this is what keyword
;		ModelInputs forces.
;
;  keywords DATE and WORKING_DIR - used so far only for PFSSMOD and PSIMAS and AWSOM to grab data
;			testing also TOMO
;
;  keyword NRENIT - used only by NUMCUBE/ADAPTCUBE (and PSIMAS and AWSOM) to avoid re-reading datacube unnecessarily
;
;  keyword _EXTRA
;		Model-specific - see model*prams.pro
;
;
; OUTPUT: MODPRAMSSTRUCT
;
;  Optional output ModelInputs gathers all model input keywords
;	useful for widget setup
;
; Called by FOR_DRIVE, FOR_WIDGET_CALENDAR_EVENT, FOR_WIDGET_MODEL_EVENT, FOR_WIDGET
; Calls MODELNAME*PRAMS
;
; Written Sarah Gibson 2013
; Version 2.0 July 2014
;  June 1 2019 -- used slash for PC compatibility
;  August 2020 -- added hooks for CROISSANT and TOMO (ADAPTCUBE added previously)
;  November 2020 -- moved CROISSANT and TOMO into main FORWARD 
;  January 2021 -- added hooks for STRIA
;  September 2021 -- passed through NOWIDGMESS
;  March 2022 -- added hooks for TURBHY
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
; June 2023 -- added SYNCOM hooks
;  Sept 2023 -- added AWSOM hooks
;  July 2023 -- added CIRTOY hooks


slash=path_sep() 

Case 1 of
     datatype(model) eq 'UND' : model = 'giblow'
     datatype(model) eq 'STR' : begin
       
 	modeldir=concat_dir(GET_ENVIRON('FORWARD'),'MODELS')
        modelfile = modeldir+slash+'ModelList.txt'

;
; uncomment if add new models
;
;   	spawn,'ls -1 '+modeldir +'> '+modelfile

	nummodels=file_lines(modelfile)
	modelnames=strarr(nummodels-2)
	modname=''
	icount=0
	openr,unit,modelfile,/get_lun
	for im = 0,nummodels-1 do begin
	  readf,unit,modname
	  if strpos(modname,'txt') le 0 then begin
	        modelnames[icount]=modname
       	 	icount=icount+1
	  endif
	endfor
  	free_lun,unit
;       
; check for ADAPTCUBE in working directory
;

	if n_elements(working_dir) eq 1 then begin
	    checkadapt=concat_dir(working_dir,'ADAPTCUBE')
	endif else begin
  	    checkadapt='ADAPTCUBE'
	endelse

	if file_test(checkadapt) ne 0 then modelnames=[modelnames,'ADAPTCUBE']
;       
; check for STRIA and TURBHY and SYNCOM in working directory
;

	if n_elements(working_dir) eq 1 then begin
	    checkstria=concat_dir(working_dir,'STRIA')
	    checksyncom=concat_dir(working_dir,'SYNCOM')
	    checkturbhy=concat_dir(working_dir,'TURBHY')
	endif else begin
  	    checkstria='STRIA'
  	    checksyncom='SYNCOM'
  	    checkturbhy='TURBHY'
	endelse

	if file_test(checksyncom) ne 0 then modelnames=[modelnames,'SYNCOM']
	if file_test(checkstria) ne 0 then modelnames=[modelnames,'STRIA']
	if file_test(checkturbhy) ne 0 then modelnames=[modelnames,'TURBHY']

	test=where(strupcase(model) eq strupcase(modelnames))
        if strupcase(model) ne '' and strupcase(model) ne 'POINTVAL' and test lt 0 then begin
           if keyword_set(nowidgmess) then message,/info,"current models include "+strjoin(modelnames[0:n_elements(modelnames)-1],',')+". You chose "+model+" .  Will default to 'giblow'" else d=dialog(/WARNING,"current models include "+strjoin(modelnames[0:n_elements(modelnames)-1],',')+". You chose "+model+" .  Will default to 'giblow'")
           model='giblow'
	 endif
     end
     else: if keyword_set(nowidgmess) then message,/info, 'model name must be a string' else d=dialog(/WARNING, 'model name must be a string')
endcase

if model eq '' then model = 'giblow'
if strupcase(model) eq 'DATACUBE' then model = 'numcube'

;
; check to see if readprams and saveprams are set
;

savepramsset=0
if keyword_set(saveprams) then savepramsset=1

readpramsset=0
if  keyword_set(readprams) then readpramsset=1

;
; if using, keep information in keyword NREINIT to avoid unnecessary 
; rereads of numerical cube
;

addstuff=''
default,nreinit,1
if strupcase(model) eq 'NUMCUBE' or strupcase(model) eq 'ADAPTCUBE' or strupcase(model) eq 'PSIMAS' or strupcase(model) eq 'AWSOM' then addstuff=addstuff+',nreinit=nreinit'
if strupcase(model) eq 'NUMCUBE' or strupcase(model) eq 'PFSSMOD' or strupcase(model) eq 'TOMO' then addstuff=addstuff+',nowidgmess=nowidgmess'
if readpramsset eq 1 then begin
 if strpos(readprams,'.gen') lt 0 then readprams=readprams+'.genx' 
 if file_exist(readprams) eq 0 then begin
    if n_elements(working_dir) eq 1 then begin
       if working_dir ne '' then begin
         readpramsuse = working_dir+slash+readprams
       endif else readpramsuse=readprams
       if file_exist(readpramsuse) eq 0 then message,'no file :'+readpramsuse
    endif else message,'no file :'+readprams
 endif else readpramsuse=readprams
 addstuff = addstuff+',readprams=readpramsuse'
endif

;
; now get model output parameter structure ModPramsStruct
; and also, for widget, model input parameter structure
;  will be in saveprams (defineprams should be unset)
; Note -- in widget code one must be sure that ModelInputs
; is passed through as a defined quantity -- so, for example,
;  "ModelInputs=1"  then "for_modeldefaults, ModelInputs=ModelInputs"
;

if keyword_set(ModelInputs) then begin
;
; if saveprams is unset then input parameters are passed through as structure
; (requires saveprams=1)
;
	if savepramsset eq 1 then savepramsuse=saveprams else savepramsuse=1

	z=execute("call_procedure,'"+model+"prams',ModPramsStruct,date=date,working_dir=working_dir,saveprams=savepramsuse,_extra=extra"+addstuff)

;
; if saveprams was set will need to read input parameters from file
;
        case datatype(savepramsuse) of
     	'STR': begin
          if strpos(savepramsuse,'.gen') lt 0 then savepramsuse=savepramsuse+'.genx' 
          if file_exist(savepramsuse) eq 0 then begin
           if n_elements(working_dir) eq 1 then begin
            if working_dir ne '' then begin
             savepramsuse2 = working_dir+slash+savepramsuse 
             endif else savepramsuse2=savepramsuse
             if file_exist(savepramsuse2) eq 0 then message,'no file :'+savepramsuse2
           endif else message,'no file :'+savepramsuse
         endif else savepramsuse2=savepramsuse
         restgen,ModelInputs,file=savepramsuse2
        end
        'STC': ModelInputs=savepramsuse
	endcase

	if keyword_set(defineprams) then print,'cant set DefinePrams and ModelInputs at the same time'

endif else begin
  	if keyword_set(defineprams) then ModPramsStruct=defineprams $
		else begin
		 if savepramsset eq 1 then z=execute("call_procedure,'"+model+"prams',ModPramsStruct,date=date,working_dir=working_dir,saveprams=saveprams,_extra=extra"+addstuff)
		 if savepramsset eq 0 then z=execute("call_procedure,'"+model+"prams',ModPramsStruct,date=date,working_dir=working_dir,_extra=extra"+addstuff)
		endelse
endelse


if strupcase(model) eq 'NUMCUBE' or strupcase(model) eq 'ADAPTCUBE' or strupcase(model) eq 'PSIMAS' or strupcase(model) eq 'AWSOM' then begin
 if ModPramsStruct.Label ne 'Radial Field' then nreinit=0
endif
end

