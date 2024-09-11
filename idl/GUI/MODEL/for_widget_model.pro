PRO for_widget_model

;+ 
;  Name:  FOR_WIDGET_MODEL
;
;  This program builds the menu bar for the FORWARD code model keywords
;
;  Called by FOR_WIDGET, FOR_WIDGET_CALENDAR_EVENT, FOR_WIDGET_SETTINGS_EVENT
;
;  External call: FOR_WIDGET_BUILD
;
; Written by Sarah Gibson, Blake Forland, 2013-2014
; Version 2.0 July 2014

  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
  
;
; get rid of old widgets
;

  if widgets.modb ne '' then begin

     widget_control,widgets.modb,/DESTROY

  endif

;
; build widget
;

  toptoolbar = widget_base(title='Model '+strupcase(modops.model)+' Parameters',x_scroll_size=250,y_scroll_size=500,yoffset=230,GROUP_LEADER=widgets.top,tlb_kill_request_events=1)
  
  widgets.modb = toptoolbar
  
  modelhelp=widget_base(toptoolbar,xoffset=90)
  help=widget_button(modelhelp,value='MODEL HELP',uvalue='Help')

  toolbar = widget_base(toptoolbar,column=2,yoffset=25)
  for_widget_build,variables,bar=toolbar

  widget_control, toolbar, /realize
  
  xmanager, 'for_widget_model', toptoolbar, /no_block

END
