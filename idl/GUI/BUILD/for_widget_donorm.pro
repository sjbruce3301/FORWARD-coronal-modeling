PRO for_widget_donorm,left,right,tname,name

;+ 
;  Name:  FOR_WIDGET_DONORM
;
; Deals with simple widget type input
;
; Called by FOR_WIDGET_BUILD_SIMPLE
;
; Written by Blake Forland, Sarah Gibson 2013-2014
; Version 2.0 July 2014

              x=execute("text=widget_text(left,value='"+tname+":',xsize=15)")

              x=execute("text=widget_text(right,value='"+name+"',uvalue='"+tname+"VAL',/editable,/all_events,xsize=15)")

end
