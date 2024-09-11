function for_pos_map,quantity,ModPramsStruct,ObsPramsStruct,LosPramsStruct,GridPramsStruct,StokesStruct=StokesStruct,$
	extratitle

;
; Name: FOR_POS_MAP
;
;Input
;
;       ModPramsStruct -- structure containting model parameter info
;       ObsPramsStruct -- structure containting observable info
;       LosPramsStruct -- structure containting line of sight parameter info
;       GridPramsStruct -- structure containing grid info
;
;Keyword INPUTS
;
;    LINE - could be name or number, see FOR_OBS_NAME; default PB
;    EXTRATITLE -- extra information for title (e.g., parameter being varied)
;
;Output
;	Structure containing the modeled quantity and meta data. In the case of a disk map 
;			this is a MAP structure
;
; Called by FOR_FORWARDMAP
;
; Calls MAKE_MAP, FOR_MAKE_CARR_MAP, FOR_PERSONALITY
;
; Written by Jim Dove, Terry Kucera, Sarah Gibson
;     HISTORY: 
;        11-Feb-2010 GridPramsStruct now contains fields for the pixel widths
;               dx and dy, as well as the center location of the grid, xcen,
;               ycen. Thus dx and dy are no longer calculated here but are
;               determined in for_get_grid.pro. JBD
;    16-Jul-2010 removed date keyword (now in ObsPramsStruct) TAK   
;        5-Nov-2010 Now converts intensity units from DN/s/cm^2 to  DN/s/pix for 
;                       imagers which provide data in those units.  TAK
;        4-Dec-2010 RSun in output maps corrected. Was integer now float. TAK
;        10-May-2011 Output Bang now in degrees. Also had been setting L0 to the Carrington
;                       longitude in radians, but actually it should be the Stonyhurst longitude 
;                       in degrees. L0 calculatoin made using date and CMER TAK 
;        23-May-2011 Only determined L0 in Stonyhurst if date is actually set (otherwise keep just CMer)
;        27-July-2011 Redefined under-disk points as -9999 and no-data points as -8888
;       3-Nov-2011 Moved redefinition of under-disk and no-data points to a different place in code. TAK
;       Nov-2012 got rid of conversion from cgs to pixels for USERINPUT cases
;       Forced map to to have time tag equal to our date March 2013 SEG
;       29-Apr-2014 Added units for COLDEN. TAK
;       Aug-2014 Defined default BUnit for all observation types, 
;		and changed naming convention to DN/S/MAPPIX to be consistent
;		with changes in e.g. FOR_PLOTDEFAULTS. SEG
;	Aug 2023 -- added IONDENS
;
; Version 2.0 July 2014
;
;       13-Jan-2015 -  added pop2colden and pop2losem units. TAK
;	4-Feb-2017 - removed variable title because it was redundant with id
;		also changed order of Instrument/Line, Model in label
;	June-2018 - added benergy, etc
;	2020/2021 = fixed bug with CarMap
;       Sept-2021 - did not allow underdisk -9999 for USER gridtype (SEG)
;		expanded conditional on STOKESVOI
;	Sep-2021 testing azequi
;       Oct-2021 - added comment about USER default CMER
;	Dec-2021 -- passing through distobs
;	Apr-2022 -- added annotations to title and id for TURBHY
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;		removed !c between line and model
;	Nov 2022 -- added POS to title 
;	Dec 2022 -- clarified P=PB/TB
;	Oct 2023 -- updated Rsun
;       Jun 2024 -- added BPOS column variables
;-

  mdtor=!dpi/180d0

  ;Get date from ObsPramsStruct. Some versions might not have it, so check
		if not tag_exist(ObsPramsStruct,'date') then date='' else date=ObsPramsStruct.date

  if n_elements(extratitle) eq 0 then extratitle=''
    
  labelbit=ObsPramsStruct.Label
  left=strpos(ObsPramsStruct.Label,'(')
  right=strpos(ObsPramsStruct.Label,')')
  if left ne -1 and right ne -1 then begin
      labelbit=strmid(ObsPramsStruct.Label,0,left-1)
  endif

;  id=labelbit + '!c' + ModPramsStruct.Label + ' ' + extratitle 
  id=labelbit + ' ' + ModPramsStruct.Label + ' ' + extratitle 
  Name=ModPramsStruct.Name+'_'+ObsPramsStruct.Instrument+'_'+ObsPramsStruct.LineName+'_'+extratitle

  if strupcase(ModPramsStruct.Name) eq 'TURBHY' then begin
     if GridPramsStruct.AzEqui eq 0 then useun=' (Rsun)' $
           else useun=' (Degrees Elongation)'
     id=id + '!c DX=' + strtrim(string(GridPramsStruct.Dx),2) $
           + ' DY=' + strtrim(string(GridPramsStruct.Dy),2)+ useun
     Name=Name+'_'+strtrim(string(GridPramsStruct.Dx),2)+'_'+strtrim(string(GridPramsStruct.Dy),2)
  endif
  if ObsPramsStruct.Pos ge 0 and strupcase(ObsPramsStruct.Instrument) ne 'PHYSICAL DIAGNOSTICS' and strupcase(ObsPramsStruct.Instrument) ne 'IONDENS' then begin
    if ObsPramsStruct.Pos gt 0 then id=id + '!c POS Integrated Over ' + strupcase(LosPramsStruct.LosUse) $
    else id=id + '!c Integrated Over ' + strupcase(LosPramsStruct.LosUse) 
    Name=Name+'_'+strupcase(LosPramsStruct.LosUse)
    if LosPramsStruct.NoStretch ne 0 then $
	id=id+' (no stretch)'
    if strupcase(ModPramsStruct.Name) eq 'TURBHY' then begin
	if strupcase(LosPramsStruct.LosUse) ne 'TAU' then useun=' (Rsun)' $
	   else useun=' (Degrees Tau)'
        id=id + '; LOS step size=' + strtrim(string(LosPramsStruct.LosInt),2)+useun
        Name=Name+'_'+strtrim(string(LosPramsStruct.LosInt),2)
    endif
  endif
  if strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' $
        or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' $
	then Name=ModPramsStruct.Name+'_'+ObsPramsStruct.Label+'_'+extratitle

  cmer=GridPramsStruct.Cmer
;
; the alternative to this conditional should no longer occur -- USER now has default CMER=0.
;
  if is_number(cmer) then begin
   if cmer ge 2.d0*!dpi then cmer = cmer- 2.d0*!dpi
   if cmer lt 0.d0 then cmer = cmer+ 2.d0*!dpi
   cmer=cmer/mdtor ;convert to degrees
  endif
  
;
; UNITS
;
; Define native units for Model maps
;

  BUnit=''
  if strupcase(ObsPramsStruct.IClass) eq 'EUV/XRAY IMAGERS' then begin
;
; Want to convert intensity units from /cm^2 to /pix. 
; Pixels are in RSun, so we need RSun in cm:
; We only want to do this is we have an intensity, though, not, say, a polarization
; and we don't want to do it if it is user input (no real pixels)
;
   if strpos(strupcase(GridPramsStruct.GridType),'USER') lt 0 then begin
	  RSun_cm=6.957d+10 ;cm
		  	   ;DN/s/cm^2 * (cm/RSun)^2 * RSun/Xpix * RSun/YPix = DN/s/pix
	  quantity=quantity*RSun_cm^2*GridPramsStruct.dx*GridPramsStruct.dy  
	  BUnit='DN/S/MAPPIX'
;
;  this is in units relative to map pixels (MAPPIX), which is the native format for the
;  model intensities. Units can be changed to detector pixels for a given instrument (DETPIX)
;  but this is done by setting UNITS keyword and can be implemented within FOR_PLOT (changing plot, not map)
;  also can apply FOR_FIXUNITS directly to a Map and it will convert as requested and 
;  save the changed unit format information to BUnit
;

   endif else BUnit='DN/S/CM^2'
  endif

  if strupcase(ObsPramsStruct.IClass) eq 'UV/EUV SPECTROMETERS' then begin
   if strupcase(ObsPramsStruct.Instrument) eq 'IONDENS' then BUnit='CM^-3' $
    else BUnit='ERG/CM2/S/SR'
  endif

  if strupcase(ObsPramsStruct.instrument) eq 'RADIO' then if strpos(strupcase(ObsPramsStruct.linename),'VOI') lt 0 then BUnit='degrees Kelvin' else BUnit='fraction circular polarization'

  if strupcase(ObsPramsStruct.instrument) eq 'FARADAY' then if strupcase(ObsPramsStruct.linename) eq 'RM' then BUnit='radians/m^2' else BUnit='radians'

  if strpos(strupcase(ObsPramsStruct.instrument),'OMP') ge 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' $
        or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' $
	then begin
   if strpos(strupcase(ObsPramsStruct.linename),'OI') lt 0 then begin
    if strpos(strupcase(ObsPramsStruct.linename),'DOPPLERVLOS') lt 0 and strpos(strupcase(ObsPramsStruct.linename),'LINEWIDTH') lt 0 and strpos(strupcase(ObsPramsStruct.linename),'STOKESAZ') lt 0 then begin
      BUnit='ERG/CM2/S/SR'
;
; this is the native format coming out of the CLE code
; it can be changed through keyword UNITS as above
;
    endif else BUnit='KM/SEC'
    if strpos(strupcase(ObsPramsStruct.linename),'STOKESAZ') ge 0 then BUnit='DEGREES'
   endif else BUnit='FRACTION INTENSITY'
  endif

;
; again, for I, Q, U, V, this is the native format but can be changed through
; keyword UNITS and implemented in FOR_PLOT or directly via FOR_FIXUNITS
;

  if strupcase(ObsPramsStruct.instrument) eq 'WL' then BUnit='10^-8 Bsun_center'
  if strupcase(ObsPramsStruct.instrument) eq 'WL' and strupcase(ObsPramsStruct.linename) eq 'P' then BUnit='Fraction Polarized (pB/TB)'
  if strupcase(ObsPramsStruct.instrument) eq 'LOSEM' then BUnit='cm^-5'
  if strupcase(ObsPramsStruct.instrument) eq 'COLDEN' then BUnit='cm^-2'
  if strupcase(ObsPramsStruct.instrument) eq 'BENERGY' then BUnit='Gauss^2*cm'
  if strupcase(ObsPramsStruct.instrument) eq 'BEN_DENS_INT' then BUnit='Gauss^2*cm^-2'
  if strupcase(ObsPramsStruct.instrument) eq 'B_INT' then BUnit='Gauss*cm'
  if strupcase(ObsPramsStruct.instrument) eq 'B_DENS_INT' then BUnit='Gauss*cm^-2'
  if strupcase(ObsPramsStruct.instrument) eq 'B_POS_INT' then BUnit='Gauss*cm'
  if strupcase(ObsPramsStruct.instrument) eq 'B_POS_DENS_INT' then BUnit='Gauss*cm^-2'
  if strupcase(ObsPramsStruct.instrument) eq 'POP2LOSEM' then BUnit='cm^-5'
  if strupcase(ObsPramsStruct.instrument) eq 'POP2COLDEN' then BUnit='cm^-2'
  ;calculate L0, Stonyhurst Longitude. Need CMER and date
;  if date ne ' ' and date ne '' then L0=cmer-(get_stereo_lonlat(date,'Earth',system='CAR',/deg))[1]
  if date ne ' ' and date ne '' and is_number(cmer) then L0=cmer-tim2carr(date)
  if date eq ' ' or date eq '' or is_number(cmer) eq 0 then L0=0.
  
;
; now take care of physical parameters
;

   if BUnit eq '' then begin
    left=strpos(ObsPramsStruct.Label,'(')
    right=strpos(ObsPramsStruct.Label,')')
    if left ne -1 and right ne -1 then begin
      BUnit=strmid(ObsPramsStruct.Label,left+1,right-left-1)
    endif
   endif

;
; identify nulls
;

   underdisk=where(GridPramsStruct.RPos lt 1.)
  if min(underdisk) ne -1 and LosPramsStruct.DoDisk eq 0. and strupcase(GridPramsStruct.GridType) ne 'USER' $
     and strupcase(GridPramsStruct.GridType) ne 'USERINPUT' $
     then quantity[underdisk] = -9999.
;  nulldata=where(quantity eq 0. or quantity*0. ne 0.)
; I think it is dangerous to null out zero data, hopefully this
; won't cause backwards incompatibility
;
  nulldata=where(quantity*0. ne 0.)
  if min(nulldata) ne -1 then quantity[nulldata] = -8888.

    
;
; Make maps
;

  if strupcase(GridPramsStruct.GridType) eq 'PLANEOFSKY' then begin
;    Plane of Sky Map
     if GridPramsStruct.AzEqui eq 0 then begin
       quant_map=make_map(quantity,dx=GridPramsStruct.dx,dy=GridPramsStruct.dy,time=date,$
                        xunits='RSun',yunits='RSun',ID=id,xc=GridPramsStruct.xcenter,yc=GridPramsStruct.ycenter,$
                		_extra={BUnit:BUnit,CType:'disk',B0:LosPramsStruct.bang/mdtor,L0:L0,cmer:cmer,distobs:GridPramsStruct.distobs,$
                		Limb:GridPramsStruct.Limb,RSun:1.,Name:Name})    
     endif else begin
;
; in for_get_grid, dx, dy are actuall de_x,de_y already -- so all we have to do here is change the units
;
       dxdeg=GridPramsStruct.dx
       dydeg=GridPramsStruct.dy
       quant_map=make_map(quantity,dx=dxdeg,dy=dydeg,time=date,$
                        xunits='degrees',yunits='degrees',ID=id,xc=GridPramsStruct.xcenter,yc=GridPramsStruct.ycenter,$
                		_extra={BUnit:BUnit,CType:'disk',B0:LosPramsStruct.bang/mdtor,L0:L0,cmer:cmer,distobs:GridPramsStruct.distobs,$
                		Limb:GridPramsStruct.Limb,RSun:1.,Name:Name})    
     endelse
     quant_map.time=date
  endif 
  if strupcase(GridPramsStruct.GridType) eq 'CARRMAP' then begin
;    Carrington Map
      quant_map=for_make_carr_map(quantity,GridPramsStruct,time=date,$
                                  thunits='Deg',phunits='Deg',RUnits='RSun',ID=id+' r= '+$
                                  strtrim(string(GridPramsStruct.RHeight),1)+$
                                  ' '+GridPramsStruct.Limb+' limb',BUnit=BUnit,$
                                  CType='carrmap',B0=LosPramsStruct.bang/mdtor,L0=L0,cmer=cmer,RSun=1.,Name=Name+'_'$
                                  +strtrim(string(GridPramsStruct.RHeight),1)+'_'+GridPramsStruct.Limb)
  endif
  if strupcase(GridPramsStruct.GridType) eq 'USERINPUT' or strupcase(GridPramsStruct.GridType) eq 'USER' then begin
     quant_map={Data:quantity,Dx:GridPramsStruct.Dx,Dy:GridPramsStruct.Dy,Time:date,ID:id,$
		XC:GridPramsStruct.XCenter,YC:GridPramsStruct.YCenter,CType:'userinput',BUnit:BUnit,$
		B0:LosPramsStruct.bang/mdtor,L0:L0,cmer:cmer,Limb:GridPramsStruct.Limb,Rsun:1,Name:Name}
  endif

  Noise=quantity*0.d0
  if ObsPramsStruct.NoisePrams.DoNoise eq 1 then Noise=for_personality(quant_map,ObsPramsStruct,StokesStruct)

  if tag_exist(quant_map,'Noise') then quant_map.Noise=Noise else quant_map=add_tag(quant_map,Noise,'Noise')

  return,quant_map

end
