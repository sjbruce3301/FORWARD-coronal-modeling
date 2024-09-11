;+
pro for_unpacklinename,Inline,elem,ionnum,wave,chianti_ion,iz
;
; Purpose: To extract information from a line name of the form: Fe12_195.1
;
;Input 
;	InLine - Line designation of the form  Fe12_195.1
;
;Outputs
;	elem - elements (e.g., FE)
;   ionnum - ion number, (e.g., 12)
;	wavelength - line wavelength (e.g., 195.1 A)
;  chianti_ion - ion in format element_ionnum (e.g., fe_12)
;	iz - atomic number of element
;
; Called by FOR_LINESEARCH, FOR_OBSDEFAULTS
; 
;  History
;  20-Jan-2013 Written T. Kucera
;  8-Sept-2023 Added iz. TAK
;  Oct-2023 - fixed misspelling of chianti_ion in call
;
; Version 2.0 July 2014
;-

    
;if it looks like the input is just an element name 
;then set elem to that and return

    if strlen(InLine) le 2 then begin
    	elem=Inline
    	return
    endif
     
    char2=strmid(InLine,1,1)
    if valid_num(char2)  then istart=1 else istart=2
    elem=strmid(inline,0,istart)
	ion_wave_sep=strpos(InLine,'_')
	ionnum=str2number(strmid(InLine,istart,ion_wave_sep-istart))
	wave=float(repstr(strmid(InLine,ion_wave_sep+1),'_','.'))
	
	;convert ion to chianti format:
	;if isnum(strmid(ion,1,1))
	;remchar,element,'_'
	Chianti_Ion=elem+'_'+trim(ionnum)
	
	;Want atomic number
	element=['h','he','li','be','b','c','n','o','f','ne','na','mg','al',$
         'si','p','s','cl','ar','k','ca','sc','ti','v','cr','mn','fe','co','ni','cu','zn']
	tmp=where(strlowcase(elem[0]) eq element,c)
	if c eq 0 then message,'Element not on list!!'
	iz=tmp[0]+1
	
end
