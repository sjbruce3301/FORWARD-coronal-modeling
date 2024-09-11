function for_calc_trace_response_grid,band,SpecPrams=SpecPrams,date=date,pop2tregime=pop2tregime,nowidgmess=nowidgmess
;+
; Project     : ISSI - Cavities
;
; Name        : for_calc_trace_response_grid
;
; Purpose     : Produce a grid in T and Ne containing the TRACE EUV responses
;
; Syntax      : resp=for_calc_trace_response_grid(band,SpecPrams=SpecPrams,date=date)
;
; Inputs
;			Band - waveband
; Keyword Inputs  
;				SpecPrams - Structure including
;					Abundance, ioneq, cversion, userspecfile, PITeamResp					
;				date - date of observations
;				pop2tregime-only set during emission calculation for second coronal population
;
; Outputs     : OutStr - structure containing response and related quantities. 
;				Also saved to a GENX file
;
; Called by FOR_EUV_XRAY_IMAGER
;
; Calls TRACE_EUV_RESP
;
; NEEDS TRACE PACKAGE
;
; History     : Written, 17-March-2013, T. Kucera
;				Modified
;				12-Nov-2013 Now using SpecPrams Structure. TAK
;				11-Apr-2014 Corrected listing of output units. TAK
; Version 2.0 July 2014
;			    
;                6-Feb-2016 Pass through POP2TREGIME SEG
;                1-Mar-2016 changed definition statment for outstr.response. Should not affect results. TAK
;		 2021-Sep - passed through nowidgmess
;		2023-Oct updated Rsun
;+

;Calculate a lookup table for TRACE for a given set of spectra at different T and Ne
;use:

specfiles=for_get_specfile_names(SpecPrams,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
NDen=n_elements(specfiles)

TRACE_Bands=[171,195,284]
NBands=n_elements(TRACE_Bands)
for j=0,NBands-1 do begin ; the wavelength bands
    resp=trace_euv_resp(TRACE_bands[j],lambda=lambda,/silent)  ;DN/S/PIXEL per PHOTON/CM^2/S/STR 
	for i=0,NDen-1 do begin	
		 restgen,file=specfiles[i],spec          ;spectrum units: photons cm+3 sr-1 s-1 Angstroms-1 
		 if i eq 0 and j eq 0  then begin
			NTemp=n_elements(spec.LogT)
			lookup=fltarr(NTemp,NDen,NBands)
			outstr={response:fltarr(NTemp,NDen),LogT:spec.LogT,LogNe:fltarr(NDen),bands:TRACE_bands,$
					ioneq:spec.ioneq,abund:spec.abund,RUnits:'dn cm^3 s-1', version:systime()}
			if tag_exist(spec,'chianti_version') gt 0 then $
							outstr=add_tag(outstr,spec.chianti_version,'chianti_version')
		 endif else if NTemp ne n_elements(spec.LogT) then message,'Temperature grid size changing'
					;- Resample the effective area to the wavelength grid of the spectrum:
		 if i eq 0 then area_s = interpol(resp,lambda,spec.lambda) 
					;- Fold spectrum and eff. area to get the number of detected
					;	DN/S/PIXEL per PHOTON/CM^2/S/STR * photons cm+3 sr-1 s-1 Angstroms-1
					;   DN/s/pix *cm^5* *A-1:
		response_grid=spec.emissivity*(area_s#replicate(1.,NTemp))  ;response for different temperatures
					;  total, with delta lambda = 1 A, to get DN/s/pix *cm^5
		dn=total(response_grid,1)					
		lookup[*,i,j]=dn
		outstr.logne[i]=alog10(spec.density)
	endfor ;end i look over densities
endfor ;end j look over wavebands
BandI=where(TRACE_Bands eq band,c)
if c ne 1 then message,'BAND must be one of: 171, 195, or 286 A'

pix_arcsec=0.5 
RSun_arcsec=(pb0r(date,/arcsec))[2]
RSun_cm=6.957d+10    
RA2ArcSec=!RADeg*3600.    ;deg/rad*arcsec/deg=arcsec/rad						
arcsec2cm=	RSun_cm/RSun_arcsec   ;cm/RSun * RSun/arcsec =cm/arcsec 
sr2cmSq = (RA2Arcsec*arcsec2cm)^2  ;(arcsec/rad *cm/arcsec)^2 = cm^2/sr

				;the response is in units of photons cm+5 s-1 sr-1		
				;DN/s/pix cm^5 * (arcsec/rsun*rsun/cm*pix1d/arcsec)^2
				;					= dn cm+5 cm-2 s-1 
outstr.response=lookup[*,*,BandI]*(RSun_arcsec/RSun_cm/pix_arcsec)^2  
return,outstr

end
 
