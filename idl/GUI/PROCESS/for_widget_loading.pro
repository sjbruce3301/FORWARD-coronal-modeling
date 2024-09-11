PRO for_widget_loading,loadstyle
  
;+
;
;  Name: FOR_WIDGET_LOADING
;
;  This program puts up warning widget to tell users to wait
;
;  Called by FOR_WIDGET, FOR_WIDGET_CALENDAR_EVENT, FOR_WIDGET_EVENT, FOR_WIDGET_SETTINGS_EVENT
;
; INPUTS
;	LOADSTYLE
;		0 = widget loading
;		1 = FORWARD code loading
;		2 = getting data
;
; EXTERNAL CALLS
;	WIDGET_CONTROL
; 
; 	History
;		Written by Blake Forland May 2013
;		revised SEG July 2013
;	Version 2.0 July 2014
;

  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
  
  loadbar = widget_base(title='', /column,xoffset=500,yoffset=100)
  
  widgets.load = loadbar
  
  prams = widget_base(loadbar,/column)
  
  if loadstyle eq 0 then begin
     
     text=widget_text(prams,value="The Widget Is Loading")
     
     text=widget_text(prams,value="Please Be Patient")
     
  endif

  if loadstyle eq 1 then begin
     
     text=widget_text(prams,value="The Forward Code Is Running!")
     
     text=widget_text(prams,value="Please Be Patient")     
     
  endif
  
  if loadstyle eq 2 then begin
     
     text=widget_text(prams,value="Accessing/processing data")
     
     text=widget_text(prams,value="Please Be Patient")     
     
  endif

  if loadstyle eq 3 then begin
     
     text=widget_text(prams,value="The data cube is loading")
     
     text=widget_text(prams,value="Please Be Patient")     
     
  endif
  widget_control, loadbar, /realize
  
;  xmanager, 'for_widget_loading', loadbar, /no_block

END
