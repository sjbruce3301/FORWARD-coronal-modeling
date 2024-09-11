function for_gofNT,lines,SpecPrams=SpecPrams,pop2tregime=pop2tregime,$
		Nobs=N_obsStr,nowidgmess=nowidgmess
;+
;Name FOR_GofNT
;	Calculate G(N,T,r) for a list of lines 
;
; Project: ISSI-Cavity
;
; Inputs
;	lines - structure or array of structures specifying line to be calculated.
;			Should include 
;				IZ (element z)
;				ION (ionization)
;				LVL1 (lower level of transition)
;				LVL2 (upper level)
;   SpecPrams, structure including
;		Abundance - abundance file to be used.
;		Pop2Abundance - abundance file to be used for second (coronal) population.
;		Ioneq - ion equilibrium assumptions
;
;  Keyword: 
;    pop2tregime-- only set if calculating emission for pop2 with pop2tregime=1
;
; Output: 3D lookup array as a funtion of 	
;	GofNT, a structure containing a 3D lookup array as a function of 	
;		LogT - Array of Log Temperature (K) values. Default 6.2
;		Density - Array of electron density values (cm^-3). Default 10^9 cm^-3
;		Radial distance from the solar surface (RSun)
;	Nobs- For Scattering calculations. I don't think this is valid at this point
;
; Called by FOR_LINE_MFLUX 
;
; Calls FOR_GET_FORWARD_DB
; 	READ_IONEQ
; Requires PACKAGE CHIANTI
;
;	History
;	Created Oct-2013 T.A. Kucera
;	Modified
;	     8-Jan-2014 Made PC compatible SEG
;        9-Jan-2014 Now calls for_get_forward_db. TAK
; 
; Version 2.0 July 2014
;	6-Feb 2017 sets POP2 Abundance for POP2TREGIME=1
;	9-Jun 2019 used slash for PC compatibility
;	Sept 2021 passed through nowidgmess
;	5-July-2023 Check for abundance file in the chianti 
;		abundance/archive if it is not in main abundance directory. TAK
;       8-Sep-2023: SEG
;               added else to check for need for archive
;
;-

;
; test for PC
;

slash=path_sep()
											;add directory (the ssw chianti database directory is default) to 
											;abundance file if needed and the .abund suffix.	
											
if keyword_set(pop2tregime) then begin
 if pop2tregime eq 1 then abundance=SpecPrams.Pop2Abundance $
    else abundance=SpecPrams.Abundance
endif else abundance=SpecPrams.Abundance


ffexist=file_exist(form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance')))          
if ffexist then $
	abund_file=form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance')) else $
	;if abundance file is not in main abundance directory, assume it is in 
	;abundance/archive
        abund_file=form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance/archive'))
read_abund,abund_file,abundances,ref

;ion equalibrium for ion fraction
										;add directory (the ssw chianti database directory is default) to 
										;ioneq file if needed and the .ioneq suffix.
ioneq_file=form_filename(SpecPrams.IonEq,'.ioneq',dir=concat_dir(!xuvtop,'ioneq'))										
read_ioneq,ioneq_file,ioneq_temp,ion_fractions,ref
cversion=SpecPrams.CVersion

NLines=n_elements(lines) 

for i=0,nLines-1 do begin	
	z2element,lines[i].iz,element,/symbol
	element=strlowcase(element)
	ion=element+'_'+trim(lines[i].ion)
	
	;file containing levels and A coeficiants for the ion:
        wgfa_file=file_dirname(!xuvtop)+slash+file_basename(!xuvtop)+slash+element+slash+ion+slash+ion+'.wgfa'
	remchar,wgfa_file,' '
	read_wgfa2,wgfa_file,lvl1,lvl2,wvl,gf,a_value,ref
	
	;which line(s) do we have?
	line_match=where(lines[i].lvl1 eq lvl1 and lines[i].lvl2 eq lvl2,c)
	if c ne 1 then begin
		if keyword_set(nowidgmess) eq 0 then d=DIALOG(/WARNING,'No '+ion+' line with levels: '+strtrim(string(lines[i].lvl1),2)+'-'+strtrim(string(lines[i].lvl2),2)+' Stopping. May need to debug.')
		message,'No '+ion+' line with levels: '+strtrim(string(lines[i].lvl1),2)+'-'+strtrim(string(lines[i].lvl2),2)+' Stopping. May need to debug.'
	endif
	
	
	;file containing the populations for the ion levels
	
	FORWARD_DB=for_get_forward_db(nowidgmess=nowidgmess)
        pop_file=file_dirname(FORWARD_DB)+slash+file_basename(FORWARD_DB)+slash+'cv'+cversion+slash+'populations'+slash+ion+'_pop_photo.genx'
	remchar,pop_file,' '
	if not file_exist(pop_file) then begin
		if keyword_set(nowidgmess) eq 0 then d=dialog(/WARNING,'Population file for '+ion+' does not exist. ('+pop_file+'). Stopping. May need to update FORWARD_DB.')
		message,'Population file for '+ion+' does not exist. ('+pop_file+'). Stopping. May need to update FORWARD_DB.'
	endif
	restgen,file=pop_file,pops
	;contains: 
	;POPS            DOUBLE    = Array[NTemp, NDen, NRad, NLevels] final one match level
	if i eq 0 then begin
		NT=n_elements(pops.Temp)
		NDen=n_elements(pops.Dens)
		NRad=n_elements(pops.radius)
					;set up output arrays
		GofNT_Arr=dblarr(NT,NDen,NRad)
		N_obs_Arr=dblarr(NT,NDen,NRad)

				;set up density array
		density3D=rebin(reform(10^pops.Dens,1,NDen),NT,NDen,NRad)
	
				;			Hydrogen to electron ratio
		H_to_electron=0.83 ;0.83 is n(H)/n_e changes units to  Intensity  per unit emission measure 
		hc = 1.986d-8 ;erg AA
	endif else begin
		if (NT ne n_elements(pops.Temp)) or (NDen ne n_elements(pops.Dens)) or $
		(NRad ne n_elements(pops.radius)) then begin
		  if keyword_set(nowidgmess) eq 0 then d=dialog(/WARNING,'Population file arrays not consistant. Stopping. May need to debug or reinstall FORWARD_DB.')
		  message,'Population file arrays not consistant. Stopping. May need to debug or reinstall FORWARD_DB.'
	        endif
	endelse
	
	;			elemental abundance
	Element_abund=(abundances(lines[i].iz-1))[0]
	
		;Resample to ionization fraction to match population arrays
	ion_frac=interpol(ion_fractions[*,lines[i].iz-1,lines[i].ion-1],ioneq_temp,pops.temp)
	ion_frac3D=rebin(ion_frac,NT,NDen,NRad)		
    level2_pop=pops.pops[*,*,*,lines[i].lvl2-1]
	GofNT_Arr = GofNT_Arr+level2_pop*ion_frac3D*Element_abund*H_to_electron*$
	(a_value[line_match])(0)*hc/(wvl[line_match])(0)/density3D/(4*!dpi)
    level1_pop=pops.pops[*,*,*,lines[i].lvl1-1]
	N_obs_arr = N_obs_arr + level1_pop*ion_frac3D*Element_abund*H_to_electron
;	and later *ne
endfor

GofNTStr={GofNT:GofNT_Arr,LogT:Pops.Temp,LogNe:Pops.Dens,Radius:Pops.Radius,lines:lines,units:'erg cm^+3 s^-1 sr-1'}

	;Based on Enrico's comments I don't think we can calculate this from 
	;the files I have (Dec. 2013) so I will comment this out for now. TAK
;N_ObsStr={Nobs:N_obs_arr,LogT:Pops.Temp,LogNe:Pops.Dens,Radius:Pops.Radius,lines:lines}

return,GofNTStr
end
