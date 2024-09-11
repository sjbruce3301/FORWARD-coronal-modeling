function for_calc_euvi_response_grid,band,ahead=ahead,behind=behind,SpecPrams=SpecPrams,date=date0,pop2tregime=pop2tregime,nowidgmess=nowidgmess
;+
; Project     : ISSI - Cavities
;
; Name        : for_calc_euvi_response_grid
;
; Purpose     : Produce a grid in T and Ne containing the STEREO/EUVI responses
;
; Syntax      : resp=for_calc_euvi_response_grid(band,/ahead,SpecPrams=SpecPrams,date=date)
;
; Inputs:
;			Band - waveband
;
; Input Keywords:
;				Ahead- Ahead Spacecraft
;				Behind - behind spacecraft
;				SpecPrams - Structure including
;					Abundance, ioneq, cversion, userspecfile, PITeamResp					
;				date - date of observations
;				pop2tregime - only set during emission calculation for second coronal population
;
; Outputs     : OutStr - structure containing response and related quantities. 
;				Also saved to a GENX file
;
; Called by FOR_EUV_XRAY_IMAGER_FLUX
;
; Calls PB0R_STEREO
;
; REQUIRES SECCHI PACKAGE
;
; History     : Written, 17-March-2013, T. Kucera
;				Modified
;				12-Nov-2013 Updates TAK
;				11-Apr-2014 Corrected listing of output units. TAK
;				1-Mar-2016 changed definition statment for outstr.response. Should not affect results. TAK
;			   
; Version 2.0 July 2014
;                 6-Feb-2016 Pass through POP2TREGIME SEG
;                 1-Mar-2016 changed definition statment for outstr.response. Should not affect results. TAK
;		  1-Jun-2019 Removed usewindows, used slash instead for PC compatibility
;		  2021-Sept -- passed through nowidgmess
;		  2023-Oct updated Rsun
;
;+


if keyword_set(Ahead) +keyword_set(Behind) ne 1 then begin
		message,/info,'Should select EITHER Ahead or Behind. Will assume Ahead.'
		Ahead=1 & Behind=0
endif
;Calculate a lookup table for EUVI for a given set o spectra at different T and Ne

default,filter,1

specfiles=for_get_specfile_names(SpecPrams,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
NDen=n_elements(specfiles)

slash=path_sep()

if keyword_set(Ahead) then begin
    usefile=file_dirname(get_environ('EUVI_RESPONSE'))+slash+file_basename(get_environ('EUVI_RESPONSE'))+slash+'ahead_sra_001.geny'
    restgenx,file=usefile,resp
endif else begin
    usefile=file_dirname(get_environ('EUVI_RESPONSE'))+slash+file_basename(get_environ('EUVI_RESPONSE'))+slash+'behind_sra_001.geny'
    restgenx,file=usefile,resp
endelse

EUVI_Bands=reform(resp.wavelnth[0,*])
NBands=n_elements(EUVI_Bands)
for j=0,NBands-1 do begin ; the 4 wavelength bands
	for i=0,NDen-1 do begin	
		 restgen,file=specfiles[i],spec       ;spectrum units: photons cm+3 sr-1 s-1 Angstroms-1
		 if i eq 0 and j eq 0  then begin
			NTemp=n_elements(spec.LogT)
			lookup=dblarr(NTemp,NDen,NBands)
			outstr={response:dblarr(NTemp,NDen),LogT:spec.LogT,$
					LogNe:fltarr(NDen),bands:band,factor:0,$
					ioneq:spec.ioneq,abund:spec.abund,RUnits:'dn cm+3 s-1', version:systime()}
			if tag_exist(spec,'chianti_version') gt 0 then outstr=add_tag(outstr,spec.chianti_version,'chianti_version')
		 endif else if NTemp ne n_elements(spec.LogT) then message,'Temperature grid size changing'
		 
					; Lambda is in Angstrom, the effective area "area_a" is in cm^2.
		lambda_resp = resp.lambda
		area_resp  =resp.area[*,filter,j]		 
					;- Resample the effective area to the wavelength grid of the spectrum:
		area_s = interpol(area_resp,lambda_resp,spec.lambda) 
					;- Fold spectrum and eff. area to get the number of detected
					;  photons cm+5 s-1 sr-1:
		response_grid=spec.emissivity*(area_s#replicate(1.,NTemp))  ;response for different temperatures
		;photons=total(response_grid,1)
				;- To get the data number per second (DN/sec) scale with
				;  the photon-to-photoelectron conversion factor, and the camera gain:
  		h_c = 6.6262d-34 * 2.9979d8   ; h * c in Joules
 		esi = 3.65 * 1.6022d-19       ; bandgap of Si = 3.65 eV * e in Coulombs
  		el_dn = 15.0                  ; camera gain = 15 electrons/DN
  							;in the range between 150 to 350 A confac is
  							;between 1.51 and 0.64 so this is a small change 
  		confac = (h_c / (spec.lambda*1d-10*esi*el_dn))
 		dn = total(response_grid*(confac#replicate(1.,NTemp)),1)	
		lookup[*,i,j]=dn
		outstr.logne[i]=alog10(spec.density)
	endfor ;end i look over densities
endfor ;end j look over wavebands

RSun_arcsec=(pb0r_stereo(date0,/arcsec,ahead=ahead,behind=behind))[2] ;(arcsec/RSun)
RSun_cm=6.957d+10    
RA2ArcSec=!RADeg*3600.    ;deg/rad*arcsec/deg=arcsec/rad						
arcsec2cm=	RSun_cm/RSun_arcsec   ;cm/RSun * RSun/arcsec =cm/arcsec 
sr2cmSq = (RA2Arcsec*arcsec2cm)^2  ;(arcsec/rad *cm/arcsec)^2 = cm^2/sr


BandI=where(EUVI_Bands eq fix(str_replace(band,'A','')),c)
if c ne 1 then message,'BAND must be one of: 304, 171, 195, or 284 A'

;dn cm+5 sr-1 s-1 * str cm-2 = dn cm+5 cm-2 s-1 =dn cm+3 s-1
outstr.response=lookup[*,*,BandI]*(1d30/sr2cmSq)
outstr.factor=-30

return,outstr

end
 
