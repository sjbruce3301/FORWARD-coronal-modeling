function for_calc_eit_response_grid,band,date=date0,SpecPrams=SpecPrams,pop2tregime=pop2tregime,nowidgmess=nowidgmess
;+
; Project     : ISSI - Cavities
;
; Name        : for_calc_eit_response_grid
;
; Purpose     : Produce a grid in T and Ne containing the SOHO/EIT responses
;
; Syntax      : resp=for_calc_eit_response_grid(band,SpecPrams=SpecPrams,date=date)
;
; Inputs
;			Band - waveband
; Keyword Inputs  
;				SpecPrams - Structure including
;					Abundance, ioneq, cversion, userspecfile, PITeamResp					
;				date - date of observations
;				pop2tregime-only set during emission calculation for second coronal population
;
; Outputs:     OutStr - structure containing response and related quantities. 
;				Also saved to a GENX file
;
; Called by FOR_EUV_XRAY_IMAGER
;
; Calls EIT_FLUX, EIT_PIXSIZE
; REQUIRES EIT PACKAGE
;
; History     : Written, 17-Mar-2013, T. Kucera
;				Modified
;				12-Nov-2013 Now using SpecPrams Structure. TAK
;				11-Apr-2014 Corrected listing of output units. TAK
; Version 2.0 July 2014
;                 6-Feb-2016 Pass through POP2TREGIME SEG
;                 1-Mar-2016 changed definition statment for outstr.response. Should not affect results. TAK
;		  Sept-2021-- passed through nowidgmess
;		  Oct 2023 -- updated Rsun
;
;+

;Calculate a lookup table for EIT for a given set of spectra at different T and Ne
;use:
if not keyword_set(date0) then date='2007/08/07' else date=date0

specfiles=for_get_specfile_names(SpecPrams,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
NDen=n_elements(specfiles)

EIT_Bands=[171,195,284,304]
NBands=n_elements(EIT_Bands)
for j=0,NBands-1 do begin ; the wavelength bands    
	for i=0,NDen-1 do begin	
		 restgen,file=specfiles[i],spec          ;spectrum units: photons cm+3 sr-1 s-1 Angstroms-1
		 if i eq 0 and j eq 0  then begin
			NTemp=n_elements(spec.LogT)
			lookup=dblarr(NTemp,NDen,Nbands)
			outstr={response:dblarr(NTemp,NDen),LogT:spec.LogT,LogNe:fltarr(NDen),$
					bands:band,factor:-26,$  ;factor = log of multiplicative factor of r repsonse
					ioneq:spec.ioneq,abund:spec.abund,RUnits:'dn cm+3 s-1', version:systime()}
			if tag_exist(spec,'chianti_version') gt 0 then $
							outstr=add_tag(outstr,spec.chianti_version,'chianti_version')
		 endif else if NTemp ne n_elements(spec.LogT) then message,'Temperature grid size changing'
					;DN/s/pix*1d26 cm^5 (unit 1d26 cm^-5 dem included)
			for k=0,n_elements(spec.logt)-1 do lookup[k,i,j]=eit_flux(EIT_bands[j],10^spec.LogT[k],edens=spec.density,$
					inspec=reform(spec.emissivity[*,k]),inwave=spec.lambda,dem='unit') ;response for different temperatures			
			outstr.logne[i]=alog10(spec.density)
	endfor ;end i look over densities
endfor ;end j look over wavebands

BandI=where(EIT_bands eq fix(str_replace(band,'A')),c)
if c ne 1 then message,'BAND must be one of: 171, 195, 286, or 304 A'

RSun_arcsec=(pb0r(date,/arcsec))[2]
RSun_cm=6.957d+10    
RA2ArcSec=!RADeg*3600.    ;deg/rad*arcsec/deg=arcsec/rad						
arcsec2cm=	RSun_cm/RSun_arcsec   ;cm/RSun * RSun/arcsec =cm/arcsec 
;sr2cmSq = (RA2Arcsec*arcsec2cm)^2  ;(arcsec/rad *cm/arcsec)^2 = cm^2/sr
pix_arcsec=eit_pixsize() ;arcsec2pix
					;DN/s/pix cm^5 *pix/arcsec^2 *arcsec^2/cm^2 = DN cm^3/s
outstr.response=lookup[*,*,BandI] /(pix_arcsec^2 *arcsec2cm^2)

return,outstr
end
 
