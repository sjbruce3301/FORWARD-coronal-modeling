function for_specdefaults,SpecPrams0,seespec=seespec,abundance=abundance,cversion=cversion,UserSpecFiles=UserSpecFiles,$
		ioneq=ioneq,UserTResp=UserTResp,PITeamResp=PITeamResp,LWidth=LWidth,LLim=LLim,InGofNT=InGofNT,OutGofNT=OutGofNT,$
		nangleint=nangleint,isotropic=isotropic,collkey=collkey,wpar_open=wpar_open,wpar_closed=wpar_closed,aniso_open=aniso_open,$
                chromorad=chromorad,blend=blend,einsteina=einsteina,einsteinb=einsteinb,gj=gj,$
		line=line,instrument=instrument,pop2tregime=pop2tregime,IClass=IClass,pop2abundance=pop2abundance,pop2ionfrac=pop2ionfrac,working_dir=working_dir
		
;+
;Name: for_specdefaults
;
;Purpose: Set defaults for spectral based calculations.
;
; SEE FOR_OBSDEFAULTS COMMENTS FOR DESCRIPTION OF KEYWORDS
;
; Calls:  FOR_CHECKABUNDANCE
;
; Called by FOR_OBSDEFAULTS, FOR_DRIVE, FOR_PLOTFITS
;
;History
;       Created 23-Sep-2013 T. Kucera
;       Modified 25-Oct-2013 TAK
;               13-Nov-2013 Bug fixes, add ARegime Keyword in header. TAK
;               17-Nov-2013 Added IClass to structure and ioneq and 
;               	abundance files now only have directories if supplied by user. 
;               	User supplied values must have a directory specified.
;               30-Dec-2013 Removed ARegime Keyword, changed LWidth default, added InGofNT and OutGofNT
;               22-Jan-2014 Removed IClass as tag, replace input Instrument with InstType (functions like
;                       IClass, but not saved in structure) SEG
;                       Also fixed some widget-interface bugs SEG
;               13-Feb-2014 Bug fixes related to ioneq called with imagers and PITeamResp
;
; Version 2.0 July 2014
;
;		11-Jan-2015 added pop2abundance and pop2ionfrac and call to routine for_checkabundance. TAK
;		17-Dec-2015 updated cversion to include 8.0.1. Routine also now checks what versions of the dbase are locally available.
;		5-Jan-2016 Checks to make sure user is using sufficiently recent version of chianti ssw packages
;		6-Feb-2016 Fixes for POP2TREGIME, also changed SEESPEC default to 1
;		22-Feb-2016 Changed simpleionfrac from IDL save file to genx. TAK
;		10-Jun-2016 Added CoMP IONEQ stuff TAK SEG
;	 	13-Aug-2016 Fixed bug where IONEQ was being written in rundir -- changed to wdir 
;			(also removed creation of IONEQ in for_intcompsetup)
;
;	 	13-Oct-2016 Fixed bug where IONEQ kept being overwritten
;		Oct 2017 - added tags for UV SPECTROPOLARIMETER calculations
;		July 2018 -- updated values of aniso and wpar for UV
;		May 2019 -- added collkey for UV
;		May 2019 -- removed aniso_closed, changed aniso_open
;			to match new model of RS in for_kinetic.pro
;	 	June 2019 -- used slash, file_copy for PC compatibility
;			forced overwrite for file_copy IONEQ
;			also changed default for coronal polarimeters IONEQ to chianti
;		4-Sept-2019  - Added option for chianti version 9.0.1. TAK
;		Sept-2019  --  updated aniso for consistency with UV models
;		Sept-2021 -- changed "inst" to "instr" because of potential for system variable confusion
;		20-Dec-2021  - Added option for chianti version 10.0.1 TAK
;		Jun-Jul-2022 -- updated wpar for case aniso_open lt 0 where wpar and aniso defined
;		Nov 2022 -- made widget display only relevant keywords for UV vs VIR/EUV
;               Dec 2022 - added NEVIII
; Jan 2023
;       restructured so abundance, LWidth (w_ang)
;       chromorad, blend, einsteina, einsteinb, and gj 
;       defaults are defined for UV/EUV here
;        (in SpecPrams not in FOR_UVMODEL.PRO).
; Feb 2023
;	changed instr to instrument
;	forced reset of abundance if its a number
; May 2023
;	changed type of collkeyval to integer to allow -1
;July 2023
;	Added option for cversion, 10.1 TAK
;Sept 2023
;	Fixed benign bug (quotation mark)
;	changed chromorad to default not forced below for UV NEVIII (only- others have occulting
;		disk so should not need to change) -- BUT
;	  check to see chromorad=0 which could be from widget
;	allowed chromorad to be displayed in widget for UV Spectropolarimeter
; Oct 2023
;	allowed check for iondens ions
;	passed through line
;	also replaced hardwired abundances and edited UVMODEL to use updated abundance files as done
;	for UV Spectromoters
; Dec 2023
;	corrected blend for NeVIII and OVI to 1/2 to account for polarization
; Feb 2024
;       added MGIX and GJ (Lande g-factor is different from other spectral lines)
; Mar 2024
;	adjusted LWidth and chromoraduse for MGIX
; May 2024
;	added default blend, einstein to IONDENS 

common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

obsstart=flag.obs

slash=path_sep() 

rundir=GET_ENVIRON('FORWARD')+slash+'FORCOMP'+slash+'run'+slash

chiondir=!xuvtop+slash+'ioneq'

if keyword_set(working_dir) then wdir=working_dir else wdir = '.'


; remove any number abundance, e.g. for UVSPECPOL or IONDENS
; (this will become obsolete, but could occur with old save files)

if is_number(abundance) then abundance=''

default,seespec,1

; 
;if input structure is supplied (can only happen from command line) we will overwrite keywords with its tags.
; If these are inconsistent with instrument type, or if there are tags missing,
; this will be taken care of below.
;

if keyword_set(SpecPrams0) gt 0 then begin
	in_tags=tag_names(SpecPrams0)
	for i=0,n_elements(in_tags)-1 do begin
		res=execute('if n_elements('+in_tags[i]+') eq 0 then '+in_tags[i]+'=SpecPrams0.'+in_tags[i])
	endfor
endif
;
							
;all possibly available options of cversion in FORWARD DB. Most recent 1st

possible_cversion=['10.1','10.0.1','9.0.1','8.0.2','7.1.3','7.0']   

;determine which of these is actually on user's computer

tmp=intarr(n_elements(possible_cversion))
;
; note -- the /dir causes problems if e.g. the astronomy library version of concat_dir is used
; so have removed this keyword call -- potentially may cause problems if running in vms, in which case
; uncomment below and make sure the main SSWIDL concat_dir with "dir" keyword is being pointed to
;for i=0,n_elements(tmp)-1 do tmp[i]=chk_dir(concat_dir((rem_blanks(break_path('$FORWARD_DB')))[0],'cv'+possible_cversion[i],/dir))
for i=0,n_elements(tmp)-1 do tmp[i]=chk_dir(concat_dir((rem_blanks(break_path('$FORWARD_DB')))[0],'cv'+possible_cversion[i]))

;cversions actually in $FORWARD_DB on user's computer

allowed_cversion=possible_cversion(where(tmp))
allowed_cversion_star=str_replace(allowed_cversion,'.','*')

;define default structure

;define abundance files

if keyword_set(PITeamResp)+keyword_set(abundance)+keyword_set(userspecfiles)+$
 			keyword_set(USERTRESP) gt 1 then $
				message,'Only one of these should be set:  Abundance, UserSpecFile, UserTResp, PITeamResp'
;		Use PITeam response (only relevant for AIA at this fault and not the default)
default,PITeamResp,0

if PITeamResp or keyword_set(UserSpecFiles) then begin
	abundance = '' 
	pop2abundance=''
	cversion=''
	allowed_abundance='nodisplay'
endif else begin
										
	if allowed_cversion[0] ne possible_cversion[0] and not exist(cversion) then begin
		message,/info,'FORWARD_DB on this computer does not contain most recent DB files for '
		message,/info, 'EUV & SXR (based on Chianti '+possible_cversion[0]+'). Using version '+allowed_cversion[0]+' instead.'
	endif
	default,cversion,allowed_cversion[0]		
        cversion=str_replace(cversion,'*','.')

	tmp=where(strlowcase(cversion) eq allowed_cversion,c)
	if c ne 1 then begin
           print,'Allowed CVersions are:' 
	   print,possible_cversion
	   print,'Available CVersions in $FORWARD_DB are: '
	   print,allowed_cversion
	   message,/info,'Using version '+allowed_cversion[0]
	  cversion=allowed_cversion[0]		
	endif
		;chianti abundance file used. 
		
		
	;determine what version of the SSW chianti package are installed on this system
	;this is distinct from the version of chianti used to calculate our data base, 
	;but the former should not be more advanced than the later
	main_chianti_version = ''
	ff = findfile(concat_dir(!xuvtop,'VERSION'))
	IF  ff[0]  NE ''  THEN BEGIN 
	   openr,unit, ff[0],/get_lun
	   readf, unit, main_chianti_version & free_lun,unit
	endif
					;The main chianti version (MCV) and forward chianti version (FCV) as arrays of integers
	mcv_arr=fix(strsplit(main_chianti_version,'.',/extract))
	fcv_arr=fix(strsplit(cversion,'.',/extract))
	i=0 & finished_test = 'ok'
	maxI=(n_elements(mcv_arr)<n_elements(fcv_arr))-1
											;if the main chianti version is lower than the forward chianti version complain
	while  finished_test eq 'ok' and i le MaxI do begin
	     case 1 of 
			  mcv_arr[i] gt fcv_arr[i]: begin
			  			finished_test = 'ok'
			  			i=MaxI +1
			  			end
			  mcv_arr[i] lt fcv_arr[i]: begin
			  			finished_test = 'problem'
			  			i=MaxI +1
			  			end
			  else: i=i+1
	      endcase
	endwhile
	if finished_test eq 'problem'  then message,'Please update to latest version of Chianti (ssw_upgrade,/chianti,/spawn) as well as the chianti database (sswdb_upgrade,/chianti,/spawn)! (If you have trouble try adding /passive_ftp to these command).'
		
        abundance=for_checkabundance(abundance,IClass,cversion=cversion,allowed_abundance=allowed_abundance)
	
        if not exist(pop2abundance) then pop2abundance=abundance $
              else pop2abundance=for_checkabundance(pop2abundance,IClass,cversion=cversion,pop2tregime=pop2tregime)

endelse

dataon=0

if keyword_set(ioneq) then if strupcase(ioneq) eq 'DATA' then begin
 dataon=1
 ioneq=''
endif

default,ioneq,''
;print,ioneq,obsstart
if keyword_set(ioneq) then begin
	case strupcase(IClass) of 
  		'UV SPECTROPOLARIMETERS': begin
  		        ioneqdir=chiondir
			break_file,ioneq,disk_log,dir,filnam,ext
			if filnam ne 'chianti' then begin
				message,/info,$
				"If modeling UV Spectropolarimeter data you can't set IONEQ. Will use chianti.ioneq. In order to change ioneq you will have to set up your own spectrum files and use the UserSpec keyword" 
				ioneq='chianti'
			endif	
  			ioneqfile=form_filename(ioneq,'.ioneq',dir=ioneqdir)
		end ;end UVSpectroPolarimeter
  		'EUV/XRAY IMAGERS': begin
  		        ioneqdir=chiondir
			break_file,ioneq,disk_log,dir,filnam,ext
			if filnam ne 'chianti' then begin
				message,/info,$
				"If modeling EUV or XRay imager data you can't set IONEQ. Will use chianti.ioneq. In order to change ioneq you will have to set up your own spectrum files and use the UserSpec keyword" 
				ioneq='chianti'
			endif	
  			ioneqfile=form_filename(ioneq,'.ioneq',dir=ioneqdir)
		end ;end EUVXrayImager
		'UV/EUV SPECTROMETERS': begin
  			ioneqdir=chiondir
			if not file_exist(form_filename(ioneq,'.ioneq',dir=chiondir)) then begin
				message,/info,$
  				"File '+form_filename(ioneq,'.ioneq')+' does not exist. Will use chianti.ioneq."
				ioneq='chianti'
                 	endif
  			ioneqfile=form_filename(ioneq,'.ioneq',dir=ioneqdir)
		end ;end UVSPEC
		'CORONAL POLARIMETERS': begin
			case 1 of
			      file_exist(form_filename(ioneq,'.ioneq',dir=chiondir)): $
					ioneqdir=chiondir
			      file_exist(form_filename(ioneq,'.ioneq',dir=wdir)): $
					ioneqdir=wdir
			      file_exist(form_filename(ioneq,'.ioneq',dir=rundir)): $
					ioneqdir=rundir
			      else: begin
;
; uncomment below if want to make default chianti
;  be careful-- user will need to have the latest fortran compiled
;
  				message,/info,'File '+form_filename(ioneq,'.ioneq')+' does not exist; using default chianti.' 
 				ioneq='chianti'
 				ioneqdir=chiondir
;
; uncomment below if want to make default cle_legacy
;  				message,/info,'File '+form_filename(ioneq,'.ioneq')+' does not exist; using default cle_legacy.' 
;  				ioneq='cle_legacy'
;  				ioneqdir=rundir
			      end
			endcase
  			ioneqfile=form_filename(ioneq,'.ioneq',dir=ioneqdir)
;                       if file_exist(form_filename('IONEQ','',dir=wdir)) eq 0 then file_copy, ioneqfile, form_filename('IONEQ','',dir=wdir) 
                        file_copy, ioneqfile, form_filename('IONEQ','',dir=wdir),/overwrite
		end ;end COMP
		else: ;for other types of instruments, don't worry about this.
	endcase ;end IClass
endif else begin
 if strupcase(IClass) eq 'CORONAL POLARIMETERS' then begin
;
; uncomment below if want to make default chianti
ioneq='chianti'
ioneqdir=chiondir
;
; uncomment below if want to make default cle_legacy
;  ioneq='cle_legacy'
;  ioneqdir=rundir

  ioneqfile=form_filename(ioneq,'.ioneq',dir=ioneqdir)

  if dataon eq 0 then begin
;       if file_exist(form_filename('IONEQ','',dir=wdir)) eq 0 then file_copy, ioneqfile, form_filename('IONEQ','',dir=wdir)  
        file_copy, ioneqfile, form_filename('IONEQ','',dir=wdir),/overwrite  
  endif

 endif
 if (strupcase(IClass) eq 'UV/EUV SPECTROMETERS' or strupcase(IClass) eq 'EUV/XRAY IMAGERS' or strupcase(IClass) eq 'UV SPECTROPOLARIMETERS') then begin
  ioneq='chianti'
  ioneqdir=chiondir
  ioneqfile=form_filename(ioneq,'.ioneq',dir=ioneqdir)
 endif
endelse

;if exist(ioneqfile) then print,'using '+ioneqfile

;pop2ionfrac will only be used if pop2tregime=2. 
;These are the ionization fractions for different elements
;Each element should be accompanied by an array of fractions 
;that add up to no more than one (checked here) and the number 
;of elements in the array should equal the atomic number (not checked)
;The first value will the the fraction of neutrals, the 2nd 
;for once ionized ions, etc. The completely ionized state does not have to be 
;listed, being implied by the other values.
;The array should be stored in a genx file and be of the form
;pop2ionfrac={H:0.7, He:[0.9,0.1]}  
;
;

default,pop2ionfrac,concat_dir(GET_ENVIRON('FORWARD'),'DEFAULTS'+slash+'simpleionfrac.genx') 

;if input check to make sure valid format
;	if datatype(pop2ionfrac) ne 'STC' then $
;				message,'POP2IONFRAC must be a structure specifying ion fractions (e.g., {H:0.7, He:[0.9,0.1],...}
;	NElem=n_tags(pop2ionfrac)
;	for i=0,NElem-1 do if total(pop2ionfrac.(i)) gt 1 then$
;				message,'Total POP2IONFRAC for '+(tag_names(pop2ionfrac))[i] +'  cannot be > 1'
	
;default - spectral files determined later using abundance and ioneq

default,UserSpecFiles,''

if keyword_set(UserTResp) then message,'UserTResp not completely implemented'
UserTResp=''

							;line width in angstroms
if strupcase(IClass) eq 'UV/EUV SPECTROMETERS' then begin
        redocase=0
	if keyword_set(LWidth) eq 0 then redocase=1
        if redocase eq 1 then begin
		case strupcase(instrument) of		;!!!I need to check these values!!!. 
							;Also, these might vary with wavelength/detector, esp. for CDS 
			'EIS': LWidth=0.1
			'CDS': LWidth=0.8  ;This really depends on detector and date.
			'IRIS': LWidth=0.2  
			else: LWidth=0.1
		endcase
	endif
					;Lower fractional limit to consider for blended lines
        if keyword_set(LLim) eq 0 then LLim = 0.01
	default,LLim,0.01
                                        ;optional input and output names for files containing G(N,T) structures
        LWidthVal='double'
        if strupcase(instrument) eq 'EIS' or strupcase(instrument) eq 'CDS' or strupcase(instrument) eq 'IRIS' then LWidthVal='nodisplay'
        LLimVal='double'
        default,InGofNT,''
        default,OutGofNT,''

endif else begin		;if not UV SPECTROMETERS these keywords won't be used -- except by UV SPECTROPOLARIMETERS -- reset later.
	LWidth=0.
	LLim=0.
        LWidthVal='nodisplay'
        LLimVal='nodisplay'
        default,InGofNT,''
        default,OutGofNT,''
endelse

if (strupcase(IClass) eq 'UV/EUV SPECTROMETERS' or strupcase(IClass) eq 'EUV/XRAY IMAGERS' or strupcase(IClass) eq 'VISIBLE SPECTROMETERS') then begin
  AbundanceVal=allowed_abundance
  CVersionVal=Allowed_CVersion_Star
  if keyword_set(pop2tregime) then begin
   if pop2tregime ne 3 then pop2abundanceVal=allowed_abundance else pop2abundanceVal='nodisplay'
  endif else pop2abundanceVal='nodisplay'
endif else begin
  AbundanceVal='nodisplay'
  CVersionVal='nodisplay'
  pop2abundanceVal='nodisplay'
endelse

cversionstar=str_replace(cversion,'.','*')

;
; UV Spectropolarimetry keywords
;

if strupcase(IClass) eq 'UV SPECTROPOLARIMETERS' or strupcase(instrument) eq 'IONDENS' then begin

 default,nangleint,100
 nangleintval='int'
 default,isotropic,1
 isotropicval='tog'
 default,collkey,0
 default,chromorad,0
 default,blend,0
 default,einsteina,0
 default,einsteinb,0
 default,gj,0
; collkeyval='tog'
 collkeyval='int'
; chromoradval='nodisplay'
 chromoradval='double'
 blendval='nodisplay'
 einsteinaval='nodisplay'
 einsteinbval='nodisplay'
 gjval='nodisplay'

;
; Lande factor GJ
; These are all lines involving the {}^2 P_{1/2,3/2} - {}^2 S_{1/2} transition. Since the atomic structure is the same, then also the Lande' g-factors of the levels, as well as the derived effective Lande' factor of the transition, are going to be the same, in the LS-coupling scheme that is assumed in FORWARD
;
; set for lines without Stokes to have something to pass through

 GJ=0.

; for each ion, determine:
; ABUNDANCE, CHROMORAD 
; EINSTEINA, EINSTEINB, GJ (Lande factor)
; LWIDTH - LINE WIDTH CHROMO (Angstrom)
; BLEND -- line blend due to transition
;  Roberto's rationale -- 
;  3/2 - 1/2 has polarizability output of 0.5
;   and 3/2 - 1/2 contributes twice as much intensity as 1/2 to 1/2
;    so contribution is 2/3 of total for intensity
;    for polarization, it is  2/3* .5 = 1/3

 if strupcase(instrument) eq 'LYA' or strpos(strupcase(line),'H1') ge 0 then begin
  LWidth=0.4065 ; Angstrom
;
; source? bemporad et al 2021 -- 1/e was .34
; also see Ishikawa et al 2017 CLASP 
;
  blend=1./3.d0
  GJ=4.d0/3.d0
  EinsteinA=62.7d7
; Einstein coefficient for photo-excitation (from Cranmer's tutorial)   
  EinsteinB=1.104d-2 ; units of cm^2 s^-1
                ; this is actually h nu / 4pi * B12
                ; AND it assumes 2p to 1s 6 to 2 
		;which is not what we want for polarization
                ; hopefully taken care of by the blend?
; chromospheric radiation total intensity (from Raymond et al. 1997)
  chromorad=5.24d15 ; units of photons cm^-2 s^-1 sr^-1 
; element abundance (relative to H) 
;  ***FOR_UVMODEL WILL NOW CALCULATE FROM LATEST CORONAL CHIANTI FILE***
;  abundance='1.'
 endif

;  Roberto's rationale -- 
;  1 - 0 has polarizability output of 1
;    there is no blend so contribution is 1 to total intensity
;    for polarization, it is 1. * 1. = 1.


 if strpos(strupcase(instrument), 'MGIX') ge 0 or strpos(strupcase(line),'MG9') ge 0 then begin
  LWidth=0.118 ; Angstrom 
;
; line width source: SOLAR CORONA ABOVE POLAR CORONAL HOLES AS SEEN BY SUMER ON SOHO (Wilhelm et al. 1998)
; 
; chromospheric radiation total intensity
;  chromoraduse= 4.703d13 ; NEVIII chromorad divided by 3.87 factor also matches with Wilhelm et al. 1998 report 
;				***DEPRECATED***
  chromoraduse= 2.26d11 ; this is observed value Curdt et al. 2001 for 1996 solar minimum corona
			; probably needs to be multiplied by 100 for solar maximum case
			; NeVIII and Lya had this level variation
;			   
;   units of photons cm^-2 s^-1 sr^-1 
  default,chromorad,chromoraduse
; maximum value
; if passed from widget could be 0
  if keyword_set(chromorad) ne 1 then chromorad=chromoraduse
;
; element abundance (relative to H) 
;  ***FOR_UVMODEL WILL NOW CALCULATE FROM LATEST CORONAL CHIANTI FILE***
;  abundance='1.'
 
  if strupcase(instrument) eq 'MGIX706' or strpos(strupcase(line),'706') ge 0 then begin
    blend=1.d0    
    GJ = 3.d0/2.d0
    EinsteinA=0.0097d7
; Einstein coefficient for photo-excitation (from CHIANTI10.2)   
    EinsteinB=5.772d-7 ; units of cm^2 s^-1
  endif
 endif

 if strpos(strupcase(instrument),'NEVIII') ge 0 or strpos(strupcase(line),'NE8') ge 0 then begin
  LWidth=0.134 ; Angstrom
;
; Doyle et al. 2000 used SUMER on-disk data and measured 
;  line-width of Ne VIII 770 from disk centre to 1 Rs 
;  around 0.1 A which is equivalent to 39 km/s
;
;  SUMER plus bandwidth integrated over line -- QS or AR
;   these are currently off-limb ***REDO for on disk
;       chromorad=1.68d12 ; QS in units of photons cm^-2 s^-1 sr^-1 
;       chromorad=7.56d12 ; AR in units of photons cm^-2 s^-1 sr^-1 

;   **Vernazza and Reeves on disk QS 
;       * note -- Raymond found values 30% higher than V&R for
;       Lya and OVI so we should check sensitivity to that*
;       chromorad=2.09d12 ; on QS disk intensities V&R value
;  chromorad=2.72d12 ; on QS disk intensities V&R value + 30%
;  replaced by
  chromoraduse=1.82d14
  default,chromorad,chromoraduse ; from SUMER data May 18 2000 (Sarro et al 2011)
; maximum value
; if passed from widget could be 0
  if keyword_set(chromorad) ne 1 then chromorad=chromoraduse
;
; element abundance (relative to H; from CHIANTI - Feldman et al. 1992 coronal abund.)
;  ***FOR_UVMODEL WILL NOW CALCULATE FROM LATEST CORONAL CHIANTI FILE***
;   abundance=10^(8.08-12)
;   abundance=string(abundance)
;
;  see discussion above for blend for lya -- 
;  for NEVIII two components are separated, look at 3/2 to 1/2 only
;  there is no blend
;  3/2 - 1/2 still has polarizability output of 0.5
;    so it is  1* .5 = 1/2

  if strupcase(instrument) eq 'NEVIII770' or strpos(strupcase(line),'770') ge 0 then begin
   blend=0.5d0
   GJ=4.d0/3.d0
   EinsteinA=57.6d7
   EinsteinB=2.711d-3
  endif
  if strupcase(instrument) eq 'NEVIII780' or strpos(strupcase(line),'780') ge 0 then begin
   blend=0.d0
   EinsteinA=55.4d7
   EinsteinB=1.342d-3
; because 2 levels instead of 4 levels
  endif
 endif

 if strpos(strupcase(instrument),'OVI') ge 0 or strpos(strupcase(line),'O6') ge 0  then begin
  LWidth=0.101 ; Angstrom
;
; parenti et al. 2000
;    1/e half width of exciting line from lower layer  = .1 A
;

; chromospheric radiation intensity (from Raymond et al. 1997)
  chromorad=1.94d13 ; units of photons cm^-2 s^-1 sr^-1 

; element abundance (relative to H; from CHIANTI - Feldman et al. 1992 coronal abund.)
; note, for IONDENS we want to straight to chianti so checkabundance as above
;  ***FOR_UVMODEL WILL NOW CALCULATE FROM LATEST CORONAL CHIANTI FILE***
;   abundance=10^(8.89-12.)
;   abundance=string(abundance)

;
; same argument as NeVIII

  if strupcase(instrument) eq 'OVI1032' or strpos(strupcase(line),'1032') ge 0 then begin
    blend=0.5d0
    GJ=4.d0/3.d0
    EinsteinA=41.6d7
    EinsteinB=3.534d-3 
  endif
  if strupcase(instrument) eq 'OVI1037' or strpos(strupcase(line),'1037') ge 0 then begin
    blend=0.d0
; updated this next
;EinsteinA=20.d7
    EinsteinA=40.9d7
    EinsteinB=1.752d-3
; approx. half, because 2 levels instead of 4 levels
  endif
 endif

;
; this was used when we were doing comparisons between
; saturated/unsaturated regimes-- a kluge -- keeping for the record
;
; can make it saturate by using 10747 EinsteinA
;if strupcase(instrument) eq 'LYA' then EinsteinA=1./14.

;
; note for now Doppler dimming values
; are same for NEVIII as OVI -- need to revisit this
; are same for MGIX as OVI -- need to revisit this

 if strpos(strupcase(instrument),'OVI') ge 0 $
    or strpos(strupcase(instrument),'NEVIII') ge 0 $
    or strpos(strupcase(instrument),'MGIX') ge 0 $
    then begin
  if obsstart eq 1 then begin
   wpar_open=8.d0
   wpar_closed=8.d0
   aniso_open=-3.d0
  endif
  default,wpar_open,8.d0
  default,wpar_closed,8.d0
  default,aniso_open,-3.d0
 endif
 if strupcase(instrument) eq 'LYA' then begin
  if obsstart eq 1 then begin
    wpar_open=2.d0
   wpar_closed=2.d0
   aniso_open=-2.d0
  endif
  default,wpar_open,2.d0
  default,wpar_closed,2.d0
  default,aniso_open,-2.d0
 endif
 default,aniso_open,1.d0
 default,wpar_open,1.d0
 default,wpar_closed,1.d0

;
;  aniso_open will be constant if set negative (default)
;	and scaled by abs(aniso_open) 
;         Ly-a default, abs(-2) as in Zhao et al 2021)
;         OVI default, abs(-3)
;       and w_par in  open field is scaled by wpar_open
;       and w_par in closed field is scaled by wpar_closed
;	(same as isotropic -- w_perp=w_par*aniso_wind)
;
;  aniso_open positive but not equal zero means 
;    analytic step-slope function for Lya
;    and model B1 for OVI (maximum anisotropy for Cranmer profile)
;  aniso_open equal zero means 
;    Cranmer model A1 for LyA
;    model B2 for OVI (minimum anisotropy)
;  note if aniso_open ge 0 so not constant,
;    choice of wpar_open and wpar_closed won't affect anything
;    w_par=w_min(T) everywhere, open and closed
;  also note aniso_wind=1 in closed field always
;

 if isotropic ne 0 then begin
  wpar_openval='double'
  wpar_closedval='double'
  aniso_openval='nodisplay'
 endif else begin 
  if aniso_open lt 0 then begin
   wpar_openval='double'
   wpar_closedval='double'
   aniso_openval='double'
  endif else begin
   wpar_openval='nodisplay'
   wpar_closedval='nodisplay'
   aniso_openval='double'
  endelse
 endelse
endif else begin
  wpar_open=1.
  wpar_closed=1.
  aniso_open=1.
  nangleint=100.
  isotropic=1
  collkey=0
  chromorad=0
  blend=0.
  einsteina=0.
  einsteinb=0.
  gj=0.
  wpar_openval='nodisplay'
  wpar_closedval='nodisplay'
  aniso_openval='nodisplay'
  nangleintval='nodisplay'
  isotropicval='nodisplay'
  collkeyval='nodisplay'
  chromoradval='nodisplay'
  blendval='nodisplay'
  einsteinaval='nodisplay'
  einsteinbval='nodisplay'
  gjval='nodisplay'
endelse

;
; since there is only one choice for lyman alpha
;  there is no point displaying aniso_open
; [no longer true]
;if strupcase(instrument) eq 'LYA' then aniso_openval='nodisplay'

if is_number(chromorad) then chromorad=double(chromorad) 
outstr={SeeSpec:SeeSpec,SeeSpecVal:'tog',Abundance:abundance,AbundanceVal:AbundanceVal,IonEq:ioneq,IonEqVal:'nodisplay',CVersion:cversionstar,CVersionVal:CVersionVal,$
		UserSpecFiles:UserSpecFiles,UserSpecFilesVal:'nodisplay',UserTResp:UserTResp,UserTRespVal:'nodisplay',PITeamResp:PITeamResp,PITeamRespVal:'nodisplay',$
		LWidth:LWidth,LWidthVal:LWidthVal,LLim:LLim,LLimVal:LLimVal,InGofNT:InGofNT,InGofNTVal:'nodisplay',OutGofNT:OutGofNT,OutGofNTVal:'nodisplay',$
		NAngleInt:NAngleInt,NAngleIntVal:NAngleIntVal,CollKey:CollKey,CollKeyVal:CollKeyVal,Isotropic:Isotropic,IsotropicVal:IsotropicVal,WPar_Open:WPar_Open,WPar_OpenVal:WPar_OpenVal,$
		WPar_Closed:WPar_Closed,WPar_ClosedVal:WPar_ClosedVal,$
		AnIso_Open:AnIso_Open,AnIso_OpenVal:AnIso_OpenVal,$
                ChromoRad:chromorad,ChromoRadVal:chromoradval,Blend:blend,BlendVal:blendval,$
		EinsteinA:einsteina,EinsteinAVal:einsteinaval,EinsteinB:einsteinb,$
		EinsteinBVal:einsteinbval,GJ:gj,GJVal:gjval,$
		Pop2Abundance:pop2abundance,Pop2AbundanceVal:pop2abundanceVal,Pop2IonFrac:pop2ionfrac,Pop2IonFracVal:'nodisplay'}
		
		
return, outstr
	
end
 
