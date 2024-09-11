PRO for_widget_output_event,ev7
  
;
; External Calls: FOR_WIDGET_BUILD, FOR_PLOTDEFAULTS, FOR_WIDGET_PLOT
;
;
; Written by Sarah Gibson, Blake Forland 2013-2014
; Version 2.0 July 2014
;
;  Feb 2023 changed instr to instrument in call to for_plotdefaults

  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
  
 if strupcase(tag_names(ev7,/structure_name)) eq 'WIDGET_KILL_REQUEST' then begin

  d=DIALOG(/WARNING,'Please close using Close button')

 endif else begin

  widget_control, ev7.id, get_uvalue=uvalue,get_value=value
  
  if strupcase(uvalue) eq 'CLOSE' then begin
     
     widget_control,widgets.outb,/DESTROY
  
     widgets.outb = ''
     
  endif else begin
  
    for_widget_build,outops,changevalue=ev7

    if outops.noerase ne flag.noerase then begin
        c = execute("for_plotdefaults,flag.magmod,modops.model,plotinputs=plotops,gridtype=gridops.gridtype,dodisk=losops.dodisk,noerase=outops.noerase,line=obsops.line,instrument=obsops.instrument,rotaz=obsops.rotaz,donoise=obsops.DoNoiseInputs.DoNoise,pos=obsops.pos"+strings.plotstring)
        for_widget_plot
	flag.noerase=outops.noerase
    endif

  endelse

 endelse

END
