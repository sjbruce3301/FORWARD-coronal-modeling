PRO for_widget_output

;+
;  Name:  FOR_WIDGET_OUTPUT
;
;  This program builds the output menu bar
;  (Note strings.outstring are built in FOR_WIDGET)
;
; Called by FOR_WIDGET_EVENT
;
; Written by Blake Forland, Sarah Gibson 2013-2014
; Version 2.0 July 2014
  
  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
  
;
; get rid of old widgets
;

  if widgets.outb ne '' then begin
     
     widget_control,widgets.outb,/DESTROY
    
  endif

; 
; now build widget
;

  outputbar = widget_base(title='Output',column=2,yoffset=200,xoffset=400,GROUP_LEADER=widgets.top,tlb_kill_request_events=1)
  
  widgets.outb = outputbar
  
  for_widget_build,outops,bar=outputbar,/close

  widget_control, outputbar, /realize
  
  xmanager, 'for_widget_output', outputbar, /no_block
  
END
