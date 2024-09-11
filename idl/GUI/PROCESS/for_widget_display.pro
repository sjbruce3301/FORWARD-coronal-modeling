PRO for_widget_display

;+
; Name: FOR_WIDGET_DISPLAY
;
;  Displays window where FORWARD plot will be
;
;  Called by FOR_WIDGET, FOR_WIDGET_EVENT
;
; Written by Blake Forland, Sarah Gibson 2013-2014
; Version 2.0 July 2014
;
; Feb 2024 -- added ucomp check
  
  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
  
;
; WIDGETS tracks IDs for the widget windows
;  DISB is display window for FORWARD plot
;
  
 if widgets.disb ne '' and outops.moreplots ne 1 and outops.noerase ne 1 then begin
     
     widget_control,widgets.disb,/DESTROY
     
 endif

 if outops.noerase eq 0 then begin
  extra=''
  if is_number(obsops.pos) then if abs(obsops.pos) eq 1 then extra='_POS'
  if obsops.Instrument eq 'NONE' then begin
     
     if outops.moreplots ne 1 or widgets.disb eq '' then $
	disbase = widget_base(yoffset=200,xoffset=370,$
                           title=settings.date+' '+modops.model+' '+obsops.line+extra,tlb_kill_request_events=1) else $
	disbase = widget_base(yoffset=200,xoffset=370,$
                           title=settings.date+' '+modops.model+' '+obsops.line+extra)
     
  endif else begin

     instuse=obsops.Instrument
     if strpos(strupcase(instuse),'COMP') ge 0  and anytim(settings.date,/ccsds) gt anytim('2021-01-21',/ccsds) then instuse='UCOMP'

     if outops.moreplots ne 1 or widgets.disb eq '' then $
         disbase = widget_base(yoffset=200,xoffset=370,$
                           title=settings.date+' '+modops.model+' '+instuse+' '+obsops.line+extra,tlb_kill_request_events=1)  else $
         disbase = widget_base(yoffset=200,xoffset=370,$
                           title=settings.date+' '+modops.model+' '+instuse+' '+obsops.line+extra) 
     
  endelse
  
  if outops.moreplots ne 1 or widgets.disb eq '' then widgets.disb=disbase
  
  draw = widget_draw(disbase, XSIZE = plotops.nwinx, YSIZE = plotops.nwiny,retain=2) 
  
  widget_control, /REALIZE, disbase 
  
  widget_control, draw, GET_VALUE = index 
  
  WSET, index 
  
  plotops.winnumber = index
  
  xmanager, 'for_widget_display', long(widgets.disb), /no_block
;  xmanager, 'for_widget_display',disbase, /no_block

 endif
if plotops.winnumber ne !d.window then begin
	message,/info,'something is wrong with window display - time to debug'
	wset,plotops.winnumber
endif
END
