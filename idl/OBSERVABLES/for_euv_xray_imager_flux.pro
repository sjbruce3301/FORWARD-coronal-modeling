;**************************************************************************

function for_euv_xray_imager_flux,ObsPramsStruct,logT,losEM,logNe0,reinit=reinit,pop2tregime=pop2tregime,_extra=_extra,nowidgmess=nowidgmess
;+	
;NAME : FOR_EUV_XRAY_IMAGER_MFLUX
;
;PURPOSE : calculate the total model flux for EUV imagers
;
;CALLING SEQUENCE : flux = for_euv_xray_imager_flux(ObsPramsStruct,logT, losEM, reinit=reinit)
;
;INPUTS : 
;			ObsPramStruct
;				instr - instrument
;		  		band - waveband band 
;				Date - Date of observation being modeled - for time varying calibration
;				SpecPrams: in the SpecPrams structure:
;					abundance - default is sun_coronal_ext 
;					pop2abundance - default is sun_coronal_ext  - only used if pop2tregime=1
;					PITeamResp - use the PI team respnse. At this point is only references AIA and 
;						gives the AIA Team calibrations with  with chianti_fix				
;					userspecfile - name(s) of file containing spectrum.  Must be a genx file
;                       containing wave, emiss, units, logt.  Units should be
;                       photons/s/cm^2/A/str.
;						If this is not set the Default is a response grid based on a
;						chianti produced spectrum with sun_coronal.abund and 
;						chianti.ioneq on 22 June 2010
;					UserTResp - User supplies temprature Response. Overrides all spectral 
;						and other calibration keywords
;
;		  logT - scalar or vector array of temperature
;         losEM - line of sight emission measure. scalar or same dimentions as logT
;		  logNe - log of electron density. if not provided LogNe=9 (log cm-3) will be assumed
;		  
;		  _EXTRA - for AIA response call.
;KEYWORDS 
;           reinit - For speed. Set reinit=1 on first time in a loop, 
;                    to calculate response functions.  On subsequent times,
;                    use reinit = 0.
;			reset in FOR_INTENSINT. Note, if pop2tregime=1,
;			  should always reinit.  This should be taken
;			  care of in FOR_EUVSXRCALC.
;
;    	    pop2tregime - only used in calculation of emission for pop2, so will be set to 1
;
;OUTPUTS : flux - EUV flux in filter (given by band) in DN/cm^2/s
;
;EXAMPLE : inten = for_euv_xray_imager_mflux(ObsPramStruct,logt0,losEM)
;
; Called by FOR_EUVSXRCALC
; Calls FOR_CALC_AIA_RESPONSE_GRID, FOR_CALC_EIT_RESPONSE_GRID, FOR_CALC_EUVI_RESPONSE_GRID, 
;  FOR_CALC_SWAP_RESPONSE_GRID, FOR_CALC_TRACE_RESPONSE_GRID,FOR_CALC_XRT_RESPONSE_GRID
;
;HISTORY : Created 2-Mar-2013 based on earlier individual instrument flies.
;		 :22-Sept-2013 changed so that ObsPramsStruct relaces inputs for instr, band, 
;				date and spectral and calibration parameters
;		 12-Nov-2013 Converted calls ro respons_grid routines to use SpecPrams Structure. TAK
;		 11-Mar-2013 Now does PROBA2/SWAP
; Version 2.0 July 2014
;-							
;		Update for POP2TREGIME consistency
;                      also removed reinit=0 force at bottom
;                       since this is done within FOR_INTENSINT
;			and default setting at top
;                       since this is done within FOR_SETTINGS_DEFAULT
;
; Sept 2021 -- passed through nowidgmess

 common EUVresponse1,response0,LogT_resp,LogNe_resp,LogFact
 
instr=ObsPramsStruct.Instrument
band=ObsPramsStruct.Linename
date0=ObsPramsStruct.Date

; SpecPrams should exist - even if using old file that doesnt have it,
; for_drive or for_widget should add it.
if not tag_exist(ObsPramsStruct,'SpecPrams') then message,'SpecPrams missing; something wrong, time to debug.'
SpecPrams=ObsPramsStruct.SpecPrams
UserTResp=SpecPrams.UserTResp

if SpecPrams.PITeamResp and not (instr eq 'AIA') then begin
	message,'PITeamResp is only an option for AIA.'
endif

;default,filter,1
if is_blank(date0) then date0='2010/03/24' ;earliest calibration date for SDO

		;AIA_flux can only handel 1D temperature arrays, 
		;so here we reform logT and losEm to 1D
NT=n_elements(logT)
NEM=n_elements(losEM)
if (NEM ne NT) and (NEM ne 1) then message,'LOSEM must be a scalar or the same length as logT'
TSize=size(logT)	
NDim=TSize[0]
nx=product(TSize[1:NDim]) ;new nx for arrays made into 1-D
logT=reform(logT,nx,/overwrite)
Default,ConstDensity,0
case n_elements(LogNe0) of
		0: LogNe=replicate(9.,NT)
		1: LogNe=replicate(LogNe0,NT)
		NT:logNe=reform(logNe0,nx)
		else: message,'LogNe should have 1 element or an equal number of elements to LogT!'
endcase
if NEM eq NT then LosEM=reform(LOSEM,nx,/overwrite)


;RSun_arcsec=(pb0r(date0,/arcsec))[2]
;RSun_cm=6.957d+10    
;RA2ArcSec=!RADeg*3600.    ;deg/rad*arcsec/deg=arcsec/rad						
;arcsec2cm=	RSun_cm/RSun_arcsec   ;cm/RSun * RSun/arcsec =cm/arcsec 
;sr2cmSq = (RA2Arcsec*arcsec2cm)^2  ;(arcsec/rad *cm/arcsec)^2 = cm^2/sr

if keyword_set(pop2tregime) then extratext='POP2' else extratext=''
if reinit eq 1 then begin		
;    print,'reinitializing IMAGER '+extratext+'.....'
    if keyword_set(UserTResp) then begin  ;not realy implemented. 
		message,'UserTResp not completely implemented'
		;user temperature response
			;check to make sure this is in the right format:
			bad=0b
			if tag_exist(UserTResp,'RESPONSE') and tag_exist(UserTResp,'LogT') then begin
				RSize=size(UserTResp.response)
				TSize=n_elements(UserTResp.LogT)
				if tag_exist(UserTResp,'LOGNE') then NSize=n_elements(UserTResp.LogNe) else NSize=0
				if RSize[1] ne TSize then bad=1b
				if NSize le 1 then begin
						if RSize[0] ne 1 then bad=1b 
				endif else if RSize[2] ne NSize then bad=1b 
			endif else bad=1b
			if bad then message,'UserTResp not correctly formatted'
			resp=UserTResp
    endif else begin   								;end UserTResp
    
		Case strupcase(instr) of 
			'AIA': begin
			;check to make sure date is after the aia launch date:
			if anytim2tai(date0) lt anytim2tai('2010/03/24') then begin
				date='2010/03/24'
				message,/info,'AIA calibration not available for '+date0+'. Setting DATE to '+date+' for calibration purposes only.'
			endif else date=date0
						;the default: use spectral lookup tables
						;precalculated lookup table. not consistant with ChiantiFix 
						;in aia_get_response()	
			resp=for_calc_aia_response_grid(band,date=date,pop2tregime=pop2tregime,SpecPrams=SpecPrams,nowidgmess=nowidgmess)
				;the response is in units of photons cm+3 s-1 sr-1
			end
			'EUVIA': resp=for_calc_euvi_response_grid(band,/ahead,SpecPrams=SpecPrams,date=date0,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
			'EUVIB': resp=for_calc_euvi_response_grid(band,/behind,SpecPrams=SpecPrams,date=date0,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
			'TRACE': resp=for_calc_trace_response_grid(band,SpecPrams=SpecPrams,date=date0,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
			'EIT': resp=for_calc_eit_response_grid(band,SpecPrams=SpecPrams,date=date0,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
			'SWAP': resp=for_calc_swap_response_grid(band,SpecPrams=SpecPrams,date=date0,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
			'XRT': resp=for_calc_xrt_response_grid(band,SpecPrams=SpecPrams,date=date0,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
			else: message,'Unrecognized Instrument: '+instr
		endcase
	endelse				;end get TempResp
	response0=resp.response
						;not sure if this is good - just becasue density is constant the first time through does not mean it will always be.
	if tag_exist(resp,'LogNe') then LogNe_resp=resp.LogNe else LogNe_resp=0	
	LogT_resp=resp.logT
	if tag_exist(resp,'factor') then logfact=resp.factor else logfact=0
endif							;end reint
if n_elements(LogNe_resp) le 1 then ConstDensity=1b

										;interpolate to get response as function 
if ConstDensity then begin
										;of temperature
	x=interpol(indgen(n_elements(logt_resp)),logt_resp,LogT)
	respInt=interpolate(response0,x)    
endif else begin					;interpolate to get response as function 
									;of temperature and density
	x=interpol(indgen(n_elements(logt_resp)),logt_resp,LogT)
	y=interpol(indgen(n_elements(logNe_resp)),logNe_resp,LogNe)
	respInt=interpolate(response0,x,y)
endelse



inten=10^(alog10(respInt)+alog10(losEM)+logfact)     ;dn cm+3 s-1 * cm-5 =  dn cm-2 s-1

inten=reform(inten,TSize[1:NDim],/overwrite)
Bunits='DN/s/cm^2'

return, inten  ;should be in units of DN/cm^2/s

end

