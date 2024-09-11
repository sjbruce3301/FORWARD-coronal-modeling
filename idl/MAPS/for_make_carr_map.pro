;**********************************************************************
function for_make_carr_map,data,grid,time=time,$
			thunits=thunits,phunits=phunits,RUnits=Runits,ID=ID,$
			CType=CType,B0=B0,L0=L0,cmer=cmer,RSun=RSun,Name=Name,BUnit=BUnit
			
;+
; Name: FOR_MAKE_CARR_MAP
;
; Purpose: To make a structure for Carrington maps somewhat analogous to a MAP structure for Disk Maps
;
; Inputs:
;	DATA - data array for map
; 	GRID - grid array upon which the map is based

; Input Keywords:
;		THUNITS - Theta units (default degrees)
;		PHUNITS - phi units (default degrees)
;		RUNITS - Radius units (default RSun)
;		ID - Descriptive ID for data
;		CTYPE - type of map. (default 'CARRMAP' for carrington. other option would be  'PLANEOFSKY' or 'USERINPUT'
;		B0 - B angle (default 0)
;		L0 - L angle (default 0)
;		RSUN - solar radii in arcsec from vantage point of observation
;		
;	OutPuts: Carrington map structure
;
;
; 	Note: FORWARD should not need any of these defaults, but they are
;	left in because the code might be used for other applications
;
;  	Common Blocks: none
;
;  Called by FOR_POS_MAP
;
; 
; HISTORY
;       Written, 21-Dec-2009, TAK
;       Modified to only make map at one radial height, 2-Feb-2010, SEG
;       Changed definition of ph0 (cmer, not L0)
;       Fixed bug - dth and dph defined identically;
;               SEG August 2013
;
;  Version 2.0 July 2014
;  Sept 2018 -- changed dth/dph to nth/nph - 1 
;    to be consistent with for_get_grid
;  August 2020 -- fixed typo CARMAP
;  Sept 2021 -- defined dth/dph to work with image plot in for_plot
;-

DSize=Size(data)
nph=DSize(1)
nth=DSize(2)

;
; these defaults are left in here because this subroutine might
; be used independent of FORWARD at some point
;

default,CType,'CARRMAP'   ;should be able to guess based on range of ph
default,phunits,'Deg'
default,thunits,'Deg'  ;base on inputs
default,time,' '
default,ID,' '			;improve
default,Rsun,1.d0
default,B0,0.d0
default,Runits,'RSun'
default,Name,''
default,cmer,0.d0
; note cmer is in degrees in this subroutine
default,L0,0.d0

if cmer ge 0.d0 then ph0=cmer-360.d0 $
	else ph0=cmer

dth=180.d0/double(nth)
dph=360.d0/double(nph)

mdtor=!dpi/180d0

th0=grid.THPos[0,0]/mdtor
Radii=grid.RHeight
CarLimb=grid.Limb

;
; now shift so both east and west limb line up 
; thus, the Carrington longitude refers to the central meridian
; note adding a shift to the right of half the grid so that
; it will plot correctly (removed shift from for_plot)
;

if strupcase(CarLimb) eq 'WEST' then nshift=nph/4. + nph/2.
if strupcase(CarLimb) eq 'EAST' then nshift=-nph/4. + nph/2.
if strupcase(CarLimb) eq 'CMER' then begin
  nshift= nph/2.
  th0=180.d0
endif

for i =0,nth-1 do begin
  data(*,i)=shift(data(*,i),nshift)
endfor

carrmap={data:data,$
        dth:dth,dph:dph,Radii:Radii,CarLimb:CarLimb,time:time,ph0:ph0,th0:th0,$
        thunits:thunits,phunits:phunits,RUnits:Runits,ID:ID,$
        CType:CType,B0:B0,BUnit:BUnit,L0:L0,cmer:cmer,RSun:1,Name:Name}
			
return,carrmap
end


