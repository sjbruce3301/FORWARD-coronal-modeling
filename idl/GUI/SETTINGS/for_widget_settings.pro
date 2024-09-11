PRO for_widget_settings
  
;+
;  Name:  FOR_WIDGET_SETTINGS
;
;  This program builds the settings menu bar
;
; Called by FOR_WIDGET_EVENT
;
; Calls FOR_WIDGET_BUILD
;
; Written by Sarah Gibson, Blake Forland 2013-2014
; Version 2.0 July 2014

  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
  
;
; get rid of old widgets
;

  if widgets.setsb ne '' then begin
  
     widget_control,widgets.setsb,/DESTROY
     
  endif

;
; build widgets
;

  settingsbar = widget_base(title='Settings',column=2,yoffset=200,xoffset=400,GROUP_LEADER=widgets.top,tlb_kill_request_events=1)
  
  widgets.setsb = settingsbar
  
  for_widget_build,settings,bar=settingsbar,/close

  widget_control, settingsbar, /realize
  
  xmanager, 'for_widget_settings', settingsbar, /no_block

END
