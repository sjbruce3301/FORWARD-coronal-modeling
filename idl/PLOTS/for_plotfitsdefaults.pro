pro for_plotfitsdefaults,gridtype,ngrid,line=line,typekcor=typekcor,kcor=kcor,$
	     colortable=colortable,plotlog=plotlog,imax=imax,imin=imin,psplot=psplot,$
             usecolor=usecolor,axiscolor=axiscolor,docont=docont,nocontcolor=nocontcolor,noclabel=noclabel,nlev=nlev,$
             c_charsize=c_charsize,winnumber=winnumber,nwinx=nwinx,nwiny=nwiny,bgcolor=bgcolor,$
             dispexp=dispexp,dispgamma=dispgamma,rpow=rpow,nobangletitle=nobangletitle,$
             charsize=charsize,charthick=charthick,noerase=noerase,title=title,$
             whitedisk=whitedisk,nulldatacolor=nulldatacolor,sunedge=sunedge,rfilter=rfilter,$
             stklines=stklines,nstarrow=nstarrow,arrowave=arrowave,extratitle=extratitle,$
	     aia=aia,$
	     gif=gif,tiff=tiff,jpeg=jpeg,moreplots=moreplots

;+
;
; program to set up defaults for plotfits
;	generally overwritten by for_drive, for_widget
; 	so really only important when for_plotfits called on its own
;
; Inputs:
;        gridtype,ngrid
;	 keywords: line,typekcor,kcor
;	 needed for conditionals below
; Outputs:
;	 (for_plot keywords -- others set elsewhere)
;	colortable,plotlog,imax,imin,psplot,usecolor,axiscolor,docont,nocontcolor,noclabel,nlev,c_charsize
;   	winnumber,nwinx,nwiny,bgcolor,dispexp,dispgamma,rpow,charsize,charthick,noerase,title
;	whitedisk,nulldatacolor,sunedge,rfilter
;	 
;        (for_plotstokeslines keywords)
;	stklines,nstarrow,arrowave
;
;	  (for_saveplot keywords
; 	gif,tiff,jpeg
;
;	 (not really used but in case
;	moreplots	
;
; Called by for_plotfits
;
; Written by Sarah Gibson 2021
;
; Sept 2021 -- expanded conditional test for STOKESQOI etc
; Jan 2022 -- added usecolor defaults for L, Q, U
; Jun 2022 -- added nobangletitle
; Feb 2024 -- commented out forced AIA docont=0
; Mar 2024 -- changed LoI minimum to -2 to be consistent with UCoMP on web


if keyword_set(line) then begin
 if strupcase(line) eq 'STOKESI' then begin
;  instrument='COMP'
;  default,usecolor,3
  default,usecolor,7777
 endif
 if strupcase(line) eq 'STOKESQ' or strupcase(line) eq 'STOKESU' or strupcase(line) eq 'STOKESL' then begin
  default,plotlog,0
  if strupcase(gridtype) eq 'PLANEOFSKY' then begin
   if strupcase(line) eq 'STOKESL' then default,imin,0 $
     else default,imin,-1
   if strupcase(line) eq 'STOKESL' then default,usecolor,0 $
     else default,usecolor,42
   default,imax,1
  endif
 endif
 if strpos(strupcase(line),'OI') ge 0 then begin
  if strupcase(gridtype) eq 'PLANEOFSKY' then begin
   if strpos(strupcase(line),'LOI') ge 0 then begin
     default,plotlog,1
;     default,imin,-2.3
     default,imin,-2.0
     default,imax,-.3
   endif else begin
     default,plotlog,0
     default,imin,-.3
     default,imax,.3
   endelse
  endif
 endif
 if strupcase(line) eq 'DOPPLERVLOS' then begin
;  instrument='COMP'
  default,usecolor,42
  default,plotlog,0
  if strupcase(gridtype) eq 'PLANEOFSKY' then begin
   default,imin,-10
   default,imax,10
  endif
 endif
 if strupcase(line) eq 'LINEWIDTH' then begin
;  instrument='COMP'
  default,usecolor,4
  default,plotlog,0
  if strupcase(gridtype) eq 'PLANEOFSKY' then begin
   default,imin,30
   default,imax,55
  endif
 endif
 if strpos(strupcase(line),'AZ') ge 0 then begin
;  instrument='COMP'
  default,usecolor,66
  default,plotlog,0
  default,imin,-90.
  default,imax,90.
 endif
endif

if keyword_set(kcor) then begin
 default,imin,'scaled to data'
 default,imax,'scaled to data'
 if strupcase(typekcor) eq 'EXTAVG' then begin
  default,dispgamma,.7
  default,dispexp,.7
  default,rfilter,'gamma_filter'
  default,plotlog,0
 endif
 if strupcase(typekcor) eq 'FIRSTL1' then begin
  default,rfilter,'no_filter'
  default,plotlog,1
 endif
endif

default,dispgamma,1.
default,dispexp,1.
default,rpow,3
default,nobangletitle,0

if strupcase(gridtype) eq 'CARRMAP' then begin
  rfilter='no_filter'
  default,nwinx,720
  default,nwiny,360
  default,plotlog,0
    if keyword_set(charsize) eq 0 then $
                if !p.charsize eq 0 then charsize=1.d0 else charsize=!p.charsize
    if keyword_set(charthick) eq 0 then $
                if !p.charthick eq 0 then charthick=1.d0 else charthick=!p.charthick
    default,c_charsize,2.*charsize/3.
endif else begin
    if keyword_set(charsize) eq 0 then $
                if !p.charsize eq 0 then charsize=1.5d0 else charsize=!p.charsize
    if keyword_set(charthick) eq 0 then $
                if !p.charthick eq 0 then charthick=1.5d0 else charthick=!p.charthick
    default,c_charsize,2.*charsize/3.
endelse

default,colortable,1
default,imax,'scaled to data'
default,imin,'scaled to data'
default,psplot,0
default,gif,0
default,tiff,0
default,jpeg,0
if keyword_set(aia) then default,usecolor,7777. else default,usecolor,0
default,rfilter,'no_filter'
if strupcase(rfilter) eq 'AIA_RFILTER' and keyword_set(aia) eq 0 then rfilter='no_filter'
default,plotlog,1

default,bgcolor,1
if bgcolor eq 1 then default,axiscolor,0 else default,axiscolor,1
;if keyword_set(aia) then docont=0. else default,docont,0
default,docont,0
default,nocontcolor,0
default,noclabel,0
default,nlev,11
default,winnumber,!d.window>0
if ngrid ne 0 then default,nwinx,ngrid else default,nwinx,750
if ngrid ne 0 then default,nwiny,ngrid else default,nwinx,750
default,whitedisk,-1

default,nulldatacolor,''
default,sunedge,1.0
default,noerase,0
default, title, ''
default, extratitle, ''
default, moreplots,0
default, stklines, 0
default, nstarrow, 50
default,arrowave,1


end
