function for_calc_aia_response_grid,band,outfilename=outfilename,SpecPrams=SpecPrams,$
			date=date,pop2tregime=pop2tregime,nowidgmess=nowidgmess
			
;+
; Project     : ISSI - Cavities
;
; Name        : for_alc_aia_response_grid
;
; Purpose     : Produce a grid in T and Ne containing the SDO/AIA responses
;
; Syntax      : resp=for_calc_aia_response_grid(band,date=date,SpecPrams=SpecPrams)
;
; Inputs:
;				Band - wave band
;
; Inputs  keywords: 
;				OutFileName - name for an output file.
;				SpecPrams - Structure including
;					abundance, pop2abundance, ioneq, cversion, userspecfile, PITeamResp					
;				date - date of observations
;				pop2tregime - only set if calculating second population coronal emission
;
; Outputs     :  structure containing response and related quantities. 
;				Also can be saved to a GENX file
;
; Called by FOR_EUV_XRAY_IMAGER_FLUX
;
; Calls AIA_GET_RESPONSE
; REQUIRES AIA PACKAGE
;
; History     : Written, 17-Mar-2013, T. Kucera
;				Modified
;				12-Nov-2013 Updates TAK
;				11-Apr-2014 Corrected listing of output units. TAK
; Version 2.0 July 2014
;			6-Feb-2016 Pass through POP2TREGIME SEG
;			1-Mar-2016 changed definition statment for outstr.response. Should not affect results. TAK
;	Sept 2021 -- passed through nowidgmess
;	Oct 2023 -- updated Rsun
;+

AIATeam=SpecPrams.PITeamResp

;Calculate a lookup table for AIA for a given set of spectra at different T and Ne
;use:

RSun_arcsec=(pb0r(date,/arcsec))[2]
RSun_cm=6.957d+10    
RA2ArcSec=!RADeg*3600.    ;deg/rad*arcsec/deg=arcsec/rad						
arcsec2cm=	RSun_cm/RSun_arcsec   ;cm/RSun * RSun/arcsec =cm/arcsec 
sr2cmSq = (RA2Arcsec*arcsec2cm)^2  ;(arcsec/rad *cm/arcsec)^2 = cm^2/sr
pix_arcsec=0.6

case 1 of
	keyword_set(AIATeam): begin
			;earler the default response was based on a set of spectra 
							;calculated with chianti.ioneq and Sun_corona.abund
							;It is not currently possible to do this and access the best estimate of the AIA response.
			Tresp=aia_get_response(/temp,/dn,/evenorm,/chiantifix,version=version,$
									timedepend=date) ;'DN cm^3 s^-1'
			 ;'DN cm^5 s^-1 pix^-1'
			ConstDensity=1
			TTags=tag_names(TResp)
			wband=where(TTags eq 'A'+trim(str_replace(band,'A','')),c)
			if c eq 0 then begin
			   if keyword_set(nowidgmess) ne 1 then d=dialog(/WARNING,'BAND must be one of: 94, 131, 171, 193, 211, 304, 335 A. Stopping. May need to debug.')
			   message,'BAND must be one of: 94, 131, 171, 193, 211, 304, 335 A.'
			endif
			LogT_resp=Tresp.(wband).LogTE
			response0=Tresp.(wband).TResp
					;change units from DN cm^5 s^-1 pix^-1 to DN cm^5 s^-1 cm^-2
			response0=response0*(RSun_arcsec/RSun_cm/pix_arcsec)^2
			outstr={response:response0,LogT:LogT_resp}
		end	
	else: begin
	    specfiles=for_get_specfile_names(SpecPrams,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
		NDen=n_elements(specfiles)
		resp=aia_get_response(/area,/dn,/evenorm) 
		
		AIA_Bands=strmid(reform(resp.channels),1)
		NBands=n_elements(AIA_Bands)
		for j=0,NBands-1 do begin ; the wavelength bands
			for i=0,NDen-1 do begin	
								;spectrum units: photons cm+3 sr-1 s-1 Angstroms-1
				 restgen,file=specfiles[i],spec  
				 if i eq 0 and j eq 0  then begin
					NTemp=n_elements(spec.LogT)
					lookup=fltarr(NTemp,NDen,NBands)
					outstr={response:fltarr(NTemp,NDen),LogT:spec.LogT,$
							LogNe:fltarr(NDen),$
							band:band,$
							ioneq:spec.ioneq,abund:spec.abund,$
							RUnits:'dn cm+3 s-1', version:systime(),Cal_version:resp.version}
					if tag_exist(spec,'chianti_version') gt 0 then outstr=add_tag(outstr,spec.chianti_version,'chianti_version')
				 endif else if NTemp ne n_elements(spec.LogT) then message,'Temperature grid size changing'
							; Lambda is in tenths of an Angstrom, the effective area "area_a" is in cm2.
				area_resp  =reform(resp.all[j,*]) 
							;- Resample the effective area to the wavelength grid of the spectrum:
				area_s = interpol(area_resp,resp.wave,spec.lambda) 
							;- Fold spectrum and eff. area to get the number of detected
							;  photons cm+5 s-1 sr-1:
				response_grid=spec.emissivity*(area_s#replicate(1.,NTemp))  ;response for different temperatures
				dn=total(response_grid,1)/sr2cmSq   ;dn cm+5 sr-1 s-1 * str cm-1 = dn cm+5 cm-2 s-1 = dn cm+3 s-1					
				lookup[*,i,j]=dn
				outstr.logne[i]=alog10(spec.density)
			endfor ;end i look over densities
		endfor ;end j look over wavebands
		BandI=where(fix(str_replace(resp.channels,'A','')) eq fix(str_replace(band,'A','')),c)
		if c ne 1 then message,'BAND must be one of: 94, 131, 171, 193, 211, 304, 335 A'
		outstr.response=lookup[*,*,BandI]
	end
endcase

return,outstr

end
 
