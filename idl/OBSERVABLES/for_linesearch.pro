;+
; 
; Name: for_linesearch
;
; Project: ISSI-Cavity
;
; Inputs
;   InLines - string with ion and wavelength of line
;			Maybe in the future it could also be a structure contining line info
;   SpecPrams - spectral parameter structure containing, among other things
;		abundance - abundance file name
;		pop2abundance - abundance file name for Pop2 -- only used if pop2tregime keyword is set
;		LWidth - line width over which to look for lines
;		LLim - lower fractional limit to include lines for blends
;Input Keywords
;	trange - Range of Temperatures in Log Kelvin to include in search.
;       pop2tregime - will only be set if coronal (1) and second time calling emission
; Output
;	Lines - array of line structures to include in G(N,T,r) calculation
;
; Called by FOR_LINE_MFLUX
;
; Calls FOR_LINESEARCH_1TEMP (included below)
;	FOR_GET_FORWARD_DB, FOR_UNPACKLINENAME
; 	READ_ABUND ***REQUIRES CHIANTI PACKAGE***
;
; History
;	Created Oct. 28 2013, T. Kucera
;	Last Updated Jan. 20, 2013 T. Kucera
;	19-Apr-2014 Added warning for cases where abundance for an element is missing. TAK
;
; Version 2.0 July 2014
;       Added pop2abundance by passing through pop2tregime calls SEG Feb 2016
;
;	5-July-2023 Check for abundance file in the chianti 
;		abundance/archive if it is not in main abundance directory. TAK
;
;  Sept 2021 - passed through nowidgmess
;		added else to check for need for archive
;  Oct 2023 -- fixed misspelling of chianti_ion
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function for_linesearch_1temp,lfile,wrange,SpecPrams,verbose=verbose,pop2tregime=pop2tregime
;
; search for lines using a single line lookup file
;
;Inputs:
;lfile - line lookup file name
;wrange - wavelength range to consider
;trange - Temperature range to consider
;SpecPrams - spectral paramters structure
;verbose - print message about lines included

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
read_abund,abund_file,abund,ref


restgenx,file=lfile,lines
								;lines in the Wavelength Range
inrange=where(lines.lines.wvl ge wrange[0] and lines.lines.wvl le wrange[1],c)

FileWRange=[min(lines.lines.wvl),max(lines.lines.wvl)]
if  Wrange[0] lt FileWRange[0]  or WRange[1] gt FileWRange[1] then begin
	message,/info,'This routine only capable of simulating lines in range '+$
		string(FileWRange,"(F6.1,' - ',F6.1,' A')")
	return,-1
endif

if c eq 0 then begin
	if keyword_set(verbose)	then begin
			break_file,lfile,log,dir,fname,ext
			message,/info,$
			'No lines found in range '+string(wrange,"(F8.3,'- ',F8.3)") +$
			' A in '+form_filename(fname,ext)
	endif
	return,-1
endif
			
			;if there are lines with zero abundance that means there 
			;are abundances missing inthe table being used. Should warn people.
line_abund=abund[lines.lines[InRange].iz-1]
zero_abundance=where(line_abund eq 0,cz)
if cz gt 0 then begin
	missing_elements=trim(strmid(lines.lines[InRange[zero_abundance]].snote,0,2))
	nodup=rem_dup(trim(strmid(lines.lines[InRange[zero_abundance]].snote,0,2)))             
	missing_elements=missing_elements(nodup)
	message,/info,'*** There are no Abundances in the selected table for the following elements.' 
	message,/info,'    Lines intensities from these lines will be set to zero: '
	print,'                    ',missing_elements,' ***'
endif

			;Lines above a fraction of the brightest line
emiss=lines.lines[InRange].int*line_abund
tmp=where(emiss ge SpecPrams.LLim*max(emiss) and emiss gt 0,NLines)
				;if all lines have emissivity 0 then return
if NLines eq 0 then begin			
	if keyword_set(verbose)	then begin
			break_file,lfile,log,dir,fname,ext
			message,/info,$
			'No lines with emiss >0 found in range '+string(wrange,"(F8.3,', ',F8.3)") +$
			' A in '+form_filename(fname,ext)
	endif
	return,-1
endif

include=InRange[tmp]


BLines=lines.lines[include]  

if keyword_set(verbose)	then begin
	break_file,lfile,log,dir,fname,ext
		;calculate the ratio of the emissivity/max line 
		;emissivity of each line included
	MaxEmiss=max(emiss[tmp])
	If MaxEmiss gt 0 then femiss=emiss[tmp]/MaxEmiss else femiss=0

	message,/info,'Lines in range ('+form_filename(fname,ext)+'):'
	for i=0,NLines-1 do print,format="(A9,x,F10.3,' A,',x,F4.2,2x,I3,2x,I3)",$
		Blines[i].snote,Blines[i].wvl,femiss[i], Blines[i].lvl1,Blines[i].lvl2
endif

return,Blines

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function for_linesearch,InLine,Specprams,trange=trange,pop2tregime=pop2tregime,nowidgmess=nowidgmess
;
;See top of file for documentation
;
			;Range of Temperatures (Log K) to seach for lines.
default,trange,[4,8]

if datatype(InLine) eq 'stc' then begin
	;should check if it has the right tags and return
endif

			;take line string apart

for_unpacklinename,Inline,element,ionnum,lwave,chianti_ion

wrange=lwave+[-1,1.]*SpecPrams.LWidth/2
FORWARD_DB=for_get_forward_db()
linelookup_dir=concat_dir(FORWARD_DB,concat_dir('cv'+SpecPrams.CVersion,'linelookup')) 

linefiles=find_files('*.gen*',linelookup_dir)
NFiles=n_elements(linefiles)

break_file,linefiles,log,dir,fname 
FTemp=fltarr(NFiles)
for i=0,NFiles-1 do begin
	tmp=str2arr(fname[i],'_')
    whereT=where(strmid(tmp,0,1) eq 'T',c)
    if c eq 0 then begin
      if keyword_set(nowidgmess) ne 1 then d=dialog(/WARNING,'LineLookup file name problem. Possible problem in database.')
      message,'LineLookup file name problem'
    endif
    FTemp[i]=strmid(tmp[whereT],1,3)
endfor

Usefile=where(FTemp ge trange[0] and FTemp le trange[1],NFiles2)
if NFiles2 eq 0 then message,'No Line Lookup files in range '+trim(trange)
linefiles=linefiles[UseFile]

for i=0,NFiles2-1 do begin
    tmp=for_linesearch_1temp(linefiles[i],wrange,SpecPrams,pop2tregime=pop2tregime)
    if datatype(tmp) eq 'STC' then begin
    	if n_elements(lines) eq 0 then Lines=tmp else Lines=[Lines,tmp]
    endif
endfor

		;check for duplicates using ion and transition
if datatype(lines) eq 'STC' then begin
	ID=lines.snote+' '+lines.ident
	nodup=rem_dup(ID)
	lines=lines[nodup]
	NLines=n_elements(Lines)
endif else NLines=0

if NLines gt 0 then begin
		;check that original line, or at least lines 
		;from the ion are included on the list
	Ion_requested=strupcase(element)+' '+roman(ionnum)
	Ion_on_List=strupcase(lines.snote)
	tmp=where(Ion_requested eq Ion_on_List,NLines_ionmatch)
endif

;
; Might want to figure out how to escape this better in widget interface
; but maybe it will just go back to top and be ok?
;

if NLines eq 0 then begin
  if keyword_set(nowidgmess) ne 1 then d=dialog(/WARNING,'There are no lines in the range '+string(wrange,"(F8.3,'-',F8.3,' A')")) 
  message,'There are no lines in the range '+string(wrange,"(F8.3,'-',F8.3,' A')") 
  lines='Null:0'
endif

message,/info,'Including Lines (in range'+string(wrange,"(F8.3,'-',F8.3,' A):')") 
print,format="(A5,9x,A5,5x,A4,2x,A4)",'Ion','Wave','lvl1','lvl2'
for i=0,n_elements(lines)-1 do print,format="(A9,x,F10.3,' A,',x,I4,2x,I4)",$
	lines[i].snote,lines[i].wvl, lines[i].lvl1,lines[i].lvl2
if NLines_ionmatch eq 0 then begin
		bell
		tmp=xmenu_gen_but(['OK'], instructions='No lines from '+ion_requested+' in range '+$
		string(WRange,format="(F8.3,'-',F8.3, ' A')") +' Will be using lines listed in terminal window.')
endif


return,lines

end



