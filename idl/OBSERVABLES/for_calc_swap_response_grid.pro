function for_calc_swap_response_grid,band,outfilename=outfilename,SpecPrams=SpecPrams,$
			date=date,pop2tregime=pop2tregime,nowidgmess=nowidgmess
			
;+
; Project     : ISSI - Cavities
;
; Name        : for_calc_swap_response_grid
;
; Purpose     : Produce a grid in T and Ne containing the PROBA2/SWAP responses
;
; Syntax      : resp=for_calc_swap_response_grid(band,SpecPrams=SpecPrams,date=date)
;
; Inputs:
;				Band - wave band
;
; Inputs  keywords: 
;				OutFileName - name for an output file.
;				SpecPrams - Structure including
;					Abundance, ioneq, cversion, userspecfile, PITeamResp					
;				date - date of observations
;				pop2tregime-only set during emission calculation for second coronal population
;
; Outputs     :  structure containing response and related quantities. 
;				Also can be saved to a GENX file
;
; Called by FOR_EUV_XRAY_IMAGER
; 
; Calls PB0R
;
; REQUIRES PACKAGE SWAP
;
; History     : Written, 11-Mar-2014, T. Kucera & L. Rachmeler
;				
; Version 2.0 July 2014
;	2014-Sep-22 Modification: correct problem in response function interpolation. TAK 
;                       6-Feb-2016 Pass through POP2TREGIME SEG
;			2021-Sept -pass through nowidgmess
;			2023-Oct updated Rsun
;+


;Calculate a lookup table for SWAP for a given set of spectra at different T and Ne
;use:

RSun_arcsec=(pb0r(date,/arcsec))[2]
RSun_cm=6.957d+10    
RA2ArcSec=!RADeg*3600.    ;deg/rad*arcsec/deg=arcsec/rad						
arcsec2cm=	RSun_cm/RSun_arcsec   ;cm/RSun * RSun/arcsec =cm/arcsec 
;sr2cmSq = (RA2Arcsec*arcsec2cm)^2  ;(arcsec/rad *cm/arcsec)^2 = cm^2/sr

swap_resp_file='$SSW/proba2/swap/caldb/swap_response_20120611_000000.save'
if not file_exist(swap_resp_file) then message,'You need to add SWAP to your Solar Soft tree'
restore,swap_resp_file

resp=swap_resp
pix_arcsec=swap_characteristics.angular_px_size

specfiles=for_get_specfile_names(SpecPrams,pop2tregime=pop2tregime,nowidgmess=nowidgmess)

NDen=n_elements(specfiles)
for i=0,NDen-1 do begin	
					;spectrum units: photons cm+3 sr-1 s-1 Angstroms-1
	 restgen,file=specfiles[i],spec  
	 if i eq 0 then begin
		NTemp=n_elements(spec.LogT)
		lookup=fltarr(NTemp,NDen)
		outstr={response:lookup,LogT:spec.LogT,$
				LogNe:fltarr(NDen),$
				band:174,$
				ioneq:spec.ioneq,abund:spec.abund,$
				RUnits:'dn cm+3 s-1', version:systime(),Cal_version:1.0}
		if tag_exist(spec,'chianti_version') gt 0 then outstr=add_tag(outstr,spec.chianti_version,'chianti_version')
	 endif else if NTemp ne n_elements(spec.LogT) then message,'Temperature grid size changing'
				; Lambda is in tenths of an Angstrom, the effective area "area_a" is in cm2.
	area_resp  =resp.resp
				;- Resample the effective area to the wavelength grid of the spectrum:
	area_s = interpol(area_resp,10*resp.wav,spec.lambda) 
					;interpolation results in negative values for wavelengths not in 
					; range of resp.wav so let's fix that
	area_s=area_s>0  
				;- Fold spectrum and eff. area to get the number of detected
				;  photons cm+5 s-1 sr-1:
	response_grid=spec.emissivity*(area_s#replicate(1.,NTemp))  ;response for different temperatures
	
				;cm+5 s-1 DN pix^-1 /((arcsec/sqrt(pix))*(cm/ arcsec))^2= DN cm+3 s-1 
	dn=total(response_grid,1)/(pix_arcsec*arcsec2cm)^2	
	lookup[*,i]=dn
	outstr.logne[i]=alog10(spec.density)
endfor ;end i look over densities
outstr.response=lookup

;Stop,limits(lookup)

return,outstr

end
 
