;**************************************************************************
;+
function for_line_mflux,ObsPramStruct,LogT,emint,dens,r3D,$
	gofnt_file=gofnt_file,reinit=reinit,pop2tregime=pop2tregime,BUnits=BUnits,nowidgmess=nowidgmess
;
; Name: FOR_LINE_MFLUX
;
; Project: ISSI-Cavity
;
; Inputs
;	Obspramstruct - contains:
;		line - string specifying EIS line to be calculated. 
;			;Format example: 'Fe10_185.2'
;		SpecPrams - parameters related to spectra		
;	LogT - Array of Log Temperature (K) values. Default 6.2
;	emint - emission measure
;	Density - Array of electron density values (cm^-3). Default 10^9 cm^-3
;	r3D - radial distance from solar surface to source
;
;Keyword Inputs
;	gofnt_file - name of GENX file with GofNT structure if default is not desired
;       reinit - For speed. Set reinit=1 on first time in a loop,
;                    to calculate response functions.  On subsequent times,
;                    use reinit = 0.
;                       reset in FOR_INTENSINT. Note, if pop2tregime=1,
;                         should always reinit.  This should be taken
;                         care of in FOR_EUVSXRCALC.

;       pop2tregime - only used if second time calling emission where pop2tregime=1
;		applied in FOR_LINESEARCH
;
;Output:
;	abundance * G(N,T,r) for the given ion, temperatures, densities, and 
;			distance from solar surface
;  	No blends from other ions currently included.
;
;Keyword Outputs:
;	BUnits - units of output. Should be erg cm^-2 s^-1 sr^-1
;
; Common Block: 
;			Line_gofnt
;
; Called by FOR_EUVSXRCALC
;
; Calls FOR_GOFNT, FOR_LINESEARCH
;
;History:
;	Created Oct. 2013 T.A.Kucera
;	Last Modified Dec. 31, 2013, Header update. T. Kucera
;
;   Version 2.0 July 2013
;		SEG Feb 2016
;		Pass through POP2TREGIME
;		fixed bug - reint instead of reinit
;		also removed reinit=0 force at bottom
;			since this is done within FOR_INTENSINT
;		and default reinit at top
;			since this is done within FOR_SETTINGDEFAULT
;	Sept 2021 passed through nowidgmess
;       March 2022 - added check for positivity so numerical cubes with "empty" space don't break things
;
;-

forward_function for_gofnt
common Line_gofnt,line0,SpecPrams0,gofnt_struct,Nobs_struct

Default,line0,''

SpecPrams=ObsPramStruct.SpecPrams
line=ObsPramStruct.LineName

usereinit=reinit
;if any of these things is true then reinit.
if reinit eq 1 || datatype(gofnt_struct) eq 'UND'  || datatype(SpecPrams0) eq 'UND' || $
	datatype(line0) eq 'UND' || (line ne line0) || $
	(SpecPrams0.Abundance ne SpecPrams.Abundance) || $
	(SpecPrams0.Pop2Abundance ne SpecPrams.Pop2Abundance) || $
	(SpecPrams0.IonEq ne SpecPrams.IonEq) || (SpecPrams0.InGofNT ne SpecPrams.InGofNT)|| $
	(SpecPrams0.CVersion ne SpecPrams.CVersion) $
		then usereinit=1

if keyword_set(pop2tregime) then extratext='POP2' else extratext=''
if usereinit eq 1 then begin
;    print,'reinitializing LINE '+extratext+'.....'
	if strlen(SpecPrams.InGofNT) gt 0 then begin
		break_file,SpecPrams.InGofNT,log,dir,fnam,ext,/last_dot
		if ext eq '' then begin
			case 1 of 
				file_exist(SpecPrams.InGofNT+'.geny'): ext='.geny'
				file_exist(SpecPrams.InGofNT+'.genx'): ext='.genx'
				else: begin
                                     if keyword_set(nowidgmess) eq 0 then d=dialog(/WARNING,'No file '+SpecPrams.InGofNT)
                                     message,'No file '+SpecPrams.InGofNT
				end
			endcase
		endif
		case ext of
			'.genx': restgen,file=SpecPrams.InGofNT,gofnt_struct
			'.geny': restgenx,file=SpecPrams.InGofNT,gofnt_struct
		endcase		
		if not (tag_exist(gofnt_struct,'LOGT') or tag_exist(gofnt_struct,'LOGNE')) then begin
				if keyword_set(nowidgmess) eq 0 then d=dialog(/WARNING,'Input G(NT) structure not right format')
				message,'Input G(NT) structure not right format'
		endif
	endif else begin
		linestruct=for_linesearch(line,SpecPrams,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
		if tag_exist(linestruct,'Null') eq 0 then gofnt_struct=for_gofnt(linestruct,SpecPrams=SpecPrams,NObs=NObs_struct,pop2tregime=pop2tregime,nowidgmess=nowidgmess) else gofnt_struct={Null:0}
	endelse
	if strlen(SpecPrams.OutGofNT) gt 0 then $
		savegenx,file=SpecPrams.OutGofNT,gofnt_struct,/overwrite
endif

if tag_exist(gofnt_struct,'Null') eq 0 then begin
 ISize=Size(LogT)
 if n_elements(dens) eq 0. then dens=1.e9 else dens=dens
 if n_elements(r3D) eq 0. then r3D=rebin([1.05],ISize[1],ISize[2]) else r3D=r3D

 x=interpol(indgen(n_elements(gofnt_struct.LogT)),gofnt_struct.LogT,LogT)
 x=0>x<(n_elements(gofnt_struct.logt)-1)
 y=interpol(indgen(n_elements(gofnt_struct.LogNe)),gofnt_struct.logNe,alog10(dens))
 y=0>y<(n_elements(gofnt_struct.logNe)-1)
 if tag_exist(gofnt_struct,'RADIUS') then begin
	z=interpol(indgen(n_elements(gofnt_struct.radius)),gofnt_struct.radius,r3D)
	z=0>z<(n_elements(gofnt_struct.radius)-1)
		; erg cm^+3 s^-1 sr-1 *cm^-5 = erg cm^-2 s^-1 sr^-1
	arg=interpolate(gofnt_struct.gofNT,x,y,z)
 endif else arg=interpolate(gofnt_struct.gofNT,x,y)
 cont=arg*0.
 pos=where(arg gt 0)
 if min(pos) ne -1 then cont[pos]=10^(alog10(arg[pos])+alog10(emint[pos])) 


 line0=line
 SpecPrams0=SpecPrams

 BUnits='erg cm^-2 s^-1 sr^-1'
 if BUnits ne strmid(gofnt_struct.units,0,strlen(BUnits)) then BUnits=gofnt_struct.units

endif else cont=0

return,cont

end


;**************************************************************************
