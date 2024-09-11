PRO for_widget_display_event,ev12

  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

 if strupcase(tag_names(ev12,/structure_name)) eq 'WIDGET_KILL_REQUEST' then begin

     widget_control,widgets.disb,/DESTROY   
  
     widgets.disb = ''
     
endif

END
