PRO for_widget_plot

;+
;  Name:  FOR_WIDGET_PLOT
;
;  This program builds the menu bar for the FORWARD code plotting keywords 
;	Also, the tabs for LOS and GRIDs and OBS
;
;  Called by FOR_WIDGET, FOR_WIDGET_CALENDAR_EVENT, FOR_WIDGET_EVENT,
;       FOR_WIDGET_SETTINGS_EVENT, FOR_WIDGET_PLOT_EVENT, FOR_WIDGET_DOSTRUC
;
;  External call to FOR_WIDGET_BUILD
;
; Written by Blake Forland, Sarah Gibson 2013-2014
; Version 2.0 July 2014
;
; Sept 2021 - added hooks for CARRMAP DATA
; March 2022 - losops.losuse no longer 'NULL' for POS
; Dec 2022 - added NEVIII
; Feb 2024 - added MGIX
  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
  
;
; get rid of old widgets
;

  if widgets.plotb ne '' then begin
  
     widget_control,widgets.plotb,/DESTROY
     
  endif

  plotbase = widget_base(title='Forward Modeling Options',/column,$
			x_scroll_size=600,y_scroll_size=640,$
                         xoffset=950,yoffset=0,GROUP_LEADER=widgets.top,$
                         /BASE_ALIGN_TOP,tlb_kill_request_events=1) 
  
  plothelp=widget_base(plotbase)
  help=widget_button(plothelp,value='OPTIONS HELP',uvalue='Help')

  widgets.plotb = plotbase

  tabbase = widget_tab(plotbase,xsize=600)
  
;-------------------------------------------------------------------
; CREATING OBS TAB
;-------------------------------------------------------------------
  
;
; ROBUST issue:
; note -- if tags are added to Observables widget (e.g. obsloslimit)
; this may need editing
;

  if strupcase(gridops.gridtype) eq 'CARRMAP' then begin
;   obsops.donoiseinputsval='nodisplay'
   obsops.rfilterval='nodisplay' 
  endif 

  doobs=0
  if is_number(obsops.pos) then if obsops.pos gt -1 then doobs =1
  if is_number(obsops.obsloslimit) then if obsops.obsloslimit gt 1.0 then doobs =1
;if strupcase(modops.model) eq 'DATA' and (strupcase(obsops.instrument) eq 'AIA' or strpos(strupcase(obsops.instrument),'OMP') gt 0 or strupcase(obsops.instrument) eq 'CORMAG') then doobs =1
; 
; removed specific reference to AIA and COMP to allow RFILTER to be general
;
  if strupcase(modops.model) eq 'DATA' then doobs =1
  if strupcase(obsops.instrument) eq 'RADIO' or strupcase(obsops.instrument) eq 'FARADAY' then doobs =1
  if doobs eq 1 then begin
    for_widget_build,obsops,bar=tabbase,dotab='OBS-'+obsops.instrument+'/'+obsops.line

    if ((strpos(strupcase(obsops.instrument),'OMP') ge 0  or strupcase(obsops.instrument) eq 'CORMAG' $
       or strpos(strupcase(obsops.instrument),'OVI') ge 0 $
       or strpos(strupcase(obsops.instrument),'NEVIII') ge 0 $
       or strpos(strupcase(obsops.instrument),'MGIX') ge 0 $
       or strupcase(obsops.instrument) eq 'LYA') and strupcase(modops.model) ne 'DATA') or $
    (strupcase(obsops.SeeSpecinputsval) ne 'NODISPLAY') then for_widget_build,obsops,bar=tabbase,/dostruc
  endif


;-------------------------------------------------------------------
; CREATING LOS TAB
;-------------------------------------------------------------------
  
    if strupcase(modops.model) eq 'DATA' and strupcase(gridops.gridtype) eq 'CARRMAP' then begin 
    endif else begin
     lostitle='LOS-'+losops.losuse
;     if losops.losuse eq 'NULL' then lostitle='LOS-noint'     
     if obsops.pos gt 0 then lostitle='LOS-noint'     
     for_widget_build,losops,bar=tabbase,dotab=lostitle
    endelse

;-------------------------------------------------------------------
; CREATING GRID TAB
;-------------------------------------------------------------------
  
;
; Testing Data CARRMAP 

  if strupcase(modops.model) eq 'DATA' and strupcase(gridops.gridtype) eq 'CARRMAP' then begin 
   gruse='Cmap' 
   gridops.ngridval='nodisplay'
  endif else begin
   if strupcase(gridops.gridtype) eq 'PLANEOFSKY' then gruse='POS' else gruse='Cmap'
  endelse
  for_widget_build,gridops,bar=tabbase,dotab='FOV-'+gruse

;-------------------------------------------------------------------
; CREATING PLOTTING TAB
;-------------------------------------------------------------------
  
  for_widget_build,plotops,bar=tabbase,dotab='Plot'

  for_widget_build,plotops,bar=tabbase,/dostruc
  

  widget_control, plotbase, /realize 
  
  XMANAGER, 'for_widget_plot', plotbase, /no_block 
  
;print,'plot gui imin',plotops.imin
;print,'plot gui usecolor',plotops.usecolor
END
