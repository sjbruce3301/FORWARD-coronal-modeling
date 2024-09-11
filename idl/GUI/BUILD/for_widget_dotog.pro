PRO for_widget_dotog,raduse,tname,pramval

;+ 
;  Name:  FOR_WIDGET_DOTOG
;
; Deal with widget input in toggle form
; 
; Called by FOR_WIDGET_BUILD_COMPLEX
;
; Written by Blake Forland, Sarah Gibson 2013-2014
; Version 2.0 July 2014

		  if pramval ge 0 then begin
	
		    x=execute("rad=widget_button(raduse,value='"+tname+"',uvalue='"+tname+"VAL')")
                    Widget_Control, rad, Set_Button=pramval

		  endif

end
