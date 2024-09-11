PRO for_widget_calendar

;+
;  Name:  FOR_WIDGET_CALENDAR
;
;  This program builds the calendar interface widget to choose date
;
;  Called from FOR_WIDGET_EVENT
;
; Written by Blake Forland and Sarah Gibson 2013-2014
; Version 2.0 July 2014
  
  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
  
;
; get rid of old widgets
;

  if widgets.date ne '' then begin

     widget_control,widgets.date,/DESTROY

  endif

;
; build widgets
;

  topbar = widget_base(title='Date', column=2,GROUP_LEADER=widgets.top,xoffset=500,yoffset=100,tlb_kill_request_events=1)
  
  widgets.date = topbar

  dateprams = widget_base(topbar,/column)
  
  text=widget_text(dateprams,value="Date e.g.: 2010-08-11T00:00:00")
  
  closebtn=widget_button (dateprams, uvalue='close', value='Close')
  
  pramvals = widget_base(topbar,/column)
  
  a=execute("text=WIDGET_TEXT(pramvals,value='"+settings.date+"',uvalue='date',/editable,/all_events)")

  closebtn=widget_button (pramvals, uvalue='changedate', value='Change Date')
  
  widget_control, topbar, /realize
  
  xmanager, 'for_widget_calendar', topbar, /no_block  

END
