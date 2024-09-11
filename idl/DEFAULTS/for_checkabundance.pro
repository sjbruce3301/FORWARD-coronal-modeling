;+
function for_checkabundance,abundance0,IClass,cversion=cversion,$
         allowed_abundance=allowed_abundance,pop2tregime=pop2tregime
;
;Purpose: set default abundance files
;
;Inputs
;		Abundance  - filename for Abundances. Default directory is chianti abundance database directory. 
;								Filename must be in chianti abundance file format
;		IClass - the instrument class
;
;Keyword Input
;		cversion - chianti version
;		pop2tregime - The temperature regime for the plasma population. 
;		 Not used for much at this point
;
; Outputs
;		Abundance - default set if not entered
;
;Keyword Outputs
;     Allowed_abundance
;
;  Called by FOR_SPECDEFAULTS
;
;  History
;		Created 9-Jan 9-2015, T.A. Kucera
;	Modifications
;		17-Dec-2015  Updated for Chianti 8.0.1.  TAK
;		27-Jan-2016 Bug and header fixes TAK
;		30-Aug-2019 added dbversion 9.0.1 TAK
;		20-Dec-2021 added dbversion 10.0.1 TAK
;		Jan 2023-- put in check for UV SPECTROPOLARIMETER
;		Feb 2023 -- removed that check -- 
;		  abundance will be overwritten in FOR_SPECDEFAULTS
;		  if necessary
;		  Note also change "exist(abundance0)" 
;		   to "keyword_set(abundance0)"
;		   because the overwriting is done by setting to ''
;		5-Jul-2023 added dbversion 10.1.0, 
;			expanded abundance file search to chianti archive directory TAK
;		8-Sep-2023 [SEG]
;			moved case cv10.10 outside loop of other cvs
;			also moved check for pop2 outside loops
;		13-Sep-2023 Debugged adding cversion 10.1.
;				Now checking all EUV/XRAY imager obs to make sure 
;				they are an allowed abundance, not just if pop2tregime set. TAK
;-

 if keyword_set(abundance0) then abundance=abundance0

			;sun_coronal_2021_chianti is asplund_2021 with some changes. 
			;It the chianti default
 allowed_abundance_1010=['sun_coronal_2021_chianti',$ 
			'sun_photospheric_2021_asplund.abund',$
			'sun_photospheric_2015_scott.abund']

 allowed_abundance_1001=['sun_coronal_1992_feldman_ext',$
			'sun_coronal_2012_schmelz_ext',$
			'sun_photospheric_2011_caffau']

 allowed_abundance_901=['sun_coronal_1992_feldman_ext',$
			'sun_coronal_2012_schmelz_ext',$
			'sun_photospheric_2011_caffau']

 allowed_abundance_801=['sun_coronal_1992_feldman_ext',$
			'sun_coronal_2012_schmelz_ext',$
			'sun_photospheric_2011_caffau']

 allowed_abundance_713=['sun_coronal_1992_feldman_ext',$
			'sun_coronal_2012_schmelz',$
			'sun_photospheric_2011_caffau']

 allowed_abundance_70=['sun_coronal_1992_feldman']
							;allowed_abundance is for widget selection
 if cversion eq '7.0' then allowed_abundance='nodisplay'
 if cversion eq '7.1.3' then allowed_abundance=allowed_abundance_713
 if cversion eq '8.0.1' then allowed_abundance=allowed_abundance_801
 if cversion eq '9.0.1' then allowed_abundance=allowed_abundance_901
 if cversion eq '10.0.1' then allowed_abundance=allowed_abundance_1001
 if cversion eq '10.1' then allowed_abundance=allowed_abundance_1010

 case 1 of  
	cversion eq '7.0': begin
		default,abundance,allowed_abundance_70[0]
		if abundance eq 'coronal' then abundance = allowed_abundance_70[0]
		if not(abundance eq allowed_abundance_70[0]) then begin
				message,/info,"The only abundance option for CVersion 7.0  is '"+$
						allowed_abundance_70[0]+"'."
				abundance=allowed_abundance_70[0]
				if keyword_set(pop2tregime) then message,/info,$
						'Calculation will proceed using use abundance '+abundance 
		endif
	   end   ;end cversion 7.0
	(cversion eq '7.1.3') or (cversion eq '8.0.1') or $
		(cversion eq '9.0.1') or (cversion eq '10.0.1'): begin
		default,abundance,'sun_coronal_1992_feldman_ext'
		case abundance of
						;sun_coronal_2012_schmelz (Schmelz et al 2012)
			'schmelz': begin
					   case cversion of
					        '7.1.3': abundance = 'sun_coronal_2012_schmelz'
					        '8.0.1': abundance = 'sun_coronal_2012_schmelz_ext'
					        '9.0.1': abundance = 'sun_coronal_2012_schmelz_ext'
					        '10.0.1': abundance = 'sun_coronal_2012_schmelz_ext'
					   endcase
					end
						;the chianti sun_coronal_1992_feldman_ext.abund
			'feldman': abundance = 'sun_coronal_1992_feldman_ext'
			'coronal': abundance = 'sun_coronal_1992_feldman_ext'
			'photospheric':abundance='sun_photospheric_2011_caffau'
			else:  begin				
                         ;check to make sure the file exists
              		 fexist=file_exist(form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance')))
              		 farchiveexist=file_exist(form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance/archive')))
			 if not (fexist or farchiveexist) $
			      then message,'File '+$
			      form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance'))+$
			      ' does not exist'
			 end
	  	endcase ;abundance			
	end ; end cversion 7.1.3, 8.0.1, 9.0.1, 10.0.1, 10.1
	cversion eq '10.1': begin
		default,abundance,allowed_abundance_1010[0]
		case abundance of
						;sun_coronal_2012_schmelz (Schmelz et al 2012)
			'schmelz':begin 
					abundance = 'sun_coronal_2012_schmelz_ext'
					message,'Schmelz abundance no longer recommended. This will not work for EUV/Xray imagers'  
					end
						;the chianti sun_coronal_1992_feldman_ext.abund
			'feldman': begin
				abundance = 'sun_coronal_1992_feldman_ext'
				message,'Feldman abundance no longer recommended. This will not work for EUV/Xray imagers'  
				end
			'coronal': abundance = 'sun_coronal_2021_chianti.abund'
			'photospheric':abundance='sun_photospheric_2021_asplund.abund'
			else:  begin				
                         ;check to make sure the file exists
              		 fexist=file_exist(form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance')))
              		 farchiveexist=file_exist(form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance/archive')))
			 if not (fexist or farchiveexist) $
			      then message,'File '+$
			      form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance'))+$
			      ' does not exist'
			 end
	  	endcase ;abundance			
		
	end ;end cversion 10.1
 endcase  ;end cversion case

 ;Abundance options for EUV and X-ray imagers are limited
 ;to those for which we have calculated spectral line lookup tables

 if strupcase(IClass) eq 'EUV/XRAY IMAGERS' then begin
 	 tmp=where(strlowcase(abundance) eq allowed_abundance,c)
	 if c eq 0 then begin
		message,/info,'You may only select one of the following abundances for EUV/XRAG imagers: '+$
		arr2str(allowed_abundance)
		abundance = 'sun_coronal_2021_chianti.abund'	
		message,/info,'Calculation will be continued with abundance '+abundance
	 endif 	
;	if keyword_Set(pop2tregime) then if pop2tregime ne 2 then begin
;	  tmp=where(strlowcase(abundance) eq allowed_abundance,c)
;	     if c eq 0 then begin
;		    message,/info,'You may select one of the following abundances: '+$
;		    arr2str(allowed_abundance)
;		    abundance = 'sun_coronal_2021_chianti.abund'	
;		    message,/info,'Calculation will be continued with abundance '+abundance
;	     endif
;	endif
 endif	

 return,abundance

end
