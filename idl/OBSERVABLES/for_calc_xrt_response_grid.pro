function for_calc_xrt_response_grid,band,date=date,pop2tregime=pop2tregime,SpecPrams=SpecPrams,nowidgmess=nowidgmess
;+
; Project     : ISSI - Cavities
;
; Name        : Calc_xrt_response_grid
;
; Purpose     : Produce a grid in T and Ne containing the Hinode/XRT responses
;
; Syntax      : resp=for_calc_xrt_response_grid(band,SpecPrams=SpecPrams,date=date)
;
; Inputs
;			Band - waveband
; Keyword Inputs  
;			SpecPrams - Structure including
;					Abundance, ioneq, cversion, userspecfile, PITeamResp					
;			date - date of observations
;			pop2tregime-only set if calculating emission for second population
;
; Outputs     :  structure containing response and related quantities. 
;				Also can be saved to a GENX file
;
; Called by FOR_EUV_XRAY_IMAGER
;
; Calls MAKE_XRT_WAVE_RESP
;
; REQUIRES XRT PACKAGE
;
; History     : Written, 17-Mar-2013, based on program by K. Reeves
;								T. Kucera
;				Modified
;				12-Nov-2013 Now using SpecPrams Structure. TAK
;				11-Apr-2014 Corrected listing of output units. TAK
; Version 2.0 July 2014
;               6-Feb-2016 Pass through POP2TREGIME SEG
;               1-Mar-2016 changed definition statment for outstr.response. Should not affect results. TAK
;		2021-Sept - passed through nowidgmess
;		2023-Oct -updated Rsun
;
;+

;Calculate a lookup table for XRT for a given set of spectra at different T and Ne
;use:

specfiles=for_get_specfile_names(SpecPrams,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
NDen=n_elements(specfiles)
w_resp = make_xrt_wave_resp(contam_time=date)

for i=0,NDen-1 do begin				;loop through the spectrum files
					;spectrum units: photons cm+3 sr-1 s-1 Angstroms-1
	   restgen,file=specfiles[i],spec 
	   modelname = 'Input Spectrum '+trim(i)
	   wave = spec.lambda
	   temp = 10^(spec.logt)
	   spectrum = spec.emissivity
	   if tag_exist(spec,'abund') then abund_model=strip_dirname(spec.abund) $
								  else abund_model = 'place holder'
	   if tag_exist(spec,'ioneq') then ioneq_model= strip_dirname(spec.ioneq) $
								  else ioneq_model = 'place holder'
	   dens_model =  trim(spec.density)+' cm-3'
	   data_files = specfiles[i]

		emiss_model = make_xrt_emiss_model(modelname, wave, temp, spectrum, $
						abund_model, ioneq_model,dens_model,data_files=data_files)

		xrt = make_xrt_temp_resp(w_resp, emiss_model) ;DN cm^5 s^-1 pix^-1
					;looks like this sorts out the bands
		names = xrt.confg.name
		match = where(strmatch(strupcase(names),strupcase(band)) eq 1)  
	 if i eq 0  then begin
		NTemp=n_elements(spec.LogT)
		lookup=fltarr(NTemp,NDen)
			outstr={response: fltarr(NTemp,NDen),LogT:spec.LogT,$
				LogNe:fltarr(NDen),band:band,ioneq:spec.ioneq,abund:spec.abund,$
				RUnits:'DN cm+3 s-1', version:systime(),Cal_version:xrt[match].history[0]}
		if tag_exist(spec,'chianti_version') gt 0 then outstr=add_tag(outstr,spec.chianti_version,'chianti_version')
	endif else if NTemp ne n_elements(spec.LogT) then message,'Temperature grid size changing'
	outstr.response[*,i]= xrt[match].temp_resp
	outstr.logne[i]=alog10(spec.density)
endfor ;end i look over densities

 			;Want to change units from DN cm^5 s^-1 pix^-1 to DN cm^5 /s/cm^2 = DN cm^3 s^-1
 RSun_cm=6.957d10
 RSun_arcsec=(pb0r(date,/arcsec))[2]
 pix_arcsec=1.0286
 outstr.response=outstr.response*(RSun_arcsec/RSun_cm/pix_arcsec)^2
 

return,outstr

end
 
