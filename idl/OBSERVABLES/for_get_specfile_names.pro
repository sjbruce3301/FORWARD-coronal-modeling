function for_get_specfile_names,SpecPrams,pop2tregime=pop2tregime,LogDen=LogDen,nowidgmess=nowidgmess
;,abundance=abundance,ioneq=ioneq,LogDen=LogDen,cversion=cversion0
;+
;INPUT KEYWORDS
;	Specprams, a structure including
;		abundance - abundance assumption in calculated spectra. 
;		pop2abundance - abundance assumption in calculated spectra pop2- only used if pop2tregime set
;		ioneq - assumptions in the ionequilibrium. Not really used at this point
;		cversion - Chianti Version. Default is the latest calculated. 
;
;      pop2tregime - only set during calculation of second (coronal) population emission
;
;OUTPUTS:
;	specfilenames
;
;Keyword output:
;	specfilenames - second set of spectral files for alternate region
;
;Output Keyword
;	LogDensity of pre-saved spectra. Might not need this
;
;
; Called by FOR_CALC_AIA_RESPONSE_GRID, FOR_CALC_EIT_RESPONSE_GRID, FOR_CALC_EUVI_RESPONSE_GRID,
;  FOR_CALC_SWAP_RESPONSE_GRID, FOR_CALC_TRACE_RESPONSE_GRID,FOR_CALC_XRT_RESPONSE_GRID
; 
; Calls FOR_GET_FORWARD_DB
;
;History
;	Created 1-Mar-2013 T.A. Kucera
;	7-Apr-2013 Default abundance now suncoronal1992feldmanext 
;				(= chanti's sun_coronal_1992_feldman_ext.abund)
;			 	added cversion keyword.  TAK
;	25-Oct-2013 defaults now removed - should be set with for_specfefaults
;				Now SpecPrams an input which includes cversion, abuns, ioneq, userspecfiles
;	9-Jan-2013 Now calls for_get_forward_db. TAK
;			
; Version 2.0 July 2014
;	6-Feb-2016 added POP2 Abundance for POP2TREGIME=1 -SEG
;	20-Jun-2016 Changed wavelength range to 1 to 901 in file names. Also put in check to make sure files are in density order. TAK
;	01-Jun0-2019 used slash for PC compatibility
;	Sept-2021 passed through nowidgmess
;-


UserSpecFiles=SpecPrams.UserSpecFiles
if keyword_set(UserSpecFiles) then begin
    ;Make sure files are in density order:
	tmp=strpos(userspecfiles,"_ne")
	if min(tmp) lt 0 then message,/info,'SpecFiles must be in correct density order. Cannot determine that from these specfile names.'
	Neloc=strpos(userspecfiles,"_ne")+3
	tmp=(strmid(userspecfiles,neloc,2))[0,*]
	remchar,tmp,'_'
	Ne_srt=sort(float(tmp))
	specfiles=userspecfiles[ne_srt]
	return,SpecFiles
endif

slash=path_sep()

if keyword_set(pop2tregime) then begin
 if pop2tregime eq 1 then abundance=SpecPrams.Pop2Abundance $
    else abundance=SpecPrams.Abundance
endif else abundance=SpecPrams.Abundance

break_file,abundance,log,dir,abund_nm,ext
remchar,abund_nm,'_'
break_file,SpecPrams.IonEq,log,dir,ioneq_nm,ext
cversion=SpecPrams.CVersion


NDen=5
LogDen=findgen(NDen)+7.
FORWARD_DB=for_get_forward_db()
specfiles=concat_dir(FORWARD_DB,'cv'+SpecPrams.CVersion+$
			slash+'spectra'+slash+'spectrum_1t901A_ne'+trim(LogDen)+'_'+ioneq_nm+'_'+abund_nm+'_cv'+cversion+'.genx') 

if not file_exist(specfiles[0]) then begin
		;should the filename have been the old wavelength range?
		tmp=concat_dir(FORWARD_DB,'cv'+SpecPrams.CVersion+$
			slash+'spectra'+slash+'spectrum_1t650A_ne'+trim(LogDen)+'_'+ioneq_nm+'_'+abund_nm+'_cv'+cversion+'.genx') 
		test_exist=exist(tmp)
		if min(test_exist) eq 1 then specfiles=tmp else message,"can't find specfiles"
endif

;this replaces *all* 'wrong' slashes with 'correct' slashes					
;example: 
; IDL> print, get_environ('FORWARD')
; c:\anny\soft\ssw\packages/forward/idl
; IDL> print, file_dirname(get_environ('FORWARD'))+path_sep()+file_basename(get_environ('FORWARD'))
; c:\anny\soft\ssw\packages\forward\idl
specfiles=file_dirname(specfiles)+slash+file_basename(specfiles)


return,specfiles
end
