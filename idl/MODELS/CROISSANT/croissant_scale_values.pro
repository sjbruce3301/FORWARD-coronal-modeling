;+
;
; NAME:
; croissant_scale_values
;
; PURPOSE:
;
; Basic function that accepts an array of numerical values, and returns a float array
; of same size with the values scaled between 0 and 1
;
; CALLING SEQUENCE:
; out=croissant_scale_values(in)
;
; INPUTS:
;   IN: Numerical array of any type (converted to float in this function before scaling is applied)
;
; OUTPUTS:
;   OUT: Scaled array (float)
;
; OPTIONAL KEYWORD
;   None
;
; OPTIONAL INPUTS
;   None

;
; OPTIONAL OUTPUTS
;   None

;
; PROCEDURE:
; Calculates min and max of input array, and uses these to scale accordingly
;
; USE & PERMISSIONS
; Any problems/queries, or suggestions for improvements, please email Huw Morgan, hmorgan@aber.ac.uk
;
; ACKNOWLEDGMENTS:
;  This code was developed with the financial support of:
;  STFC Consolidated grant to Aberystwyth University (Morgan)
;
; MODIFICATION HISTORY:
; Created at Aberystwyth University - Huw Morgan hmorgan@aber.ac.uk
; First public availability through Solarsoft Forward package 08/2020
;
;-

function croissant_scale_values,a0

  if n_elements(a0) eq 1 then return,1
  a=float(a0)
  mina=min(a,max=maxa,/nan)
  if mina eq maxa then begin
    if mina eq 0 then return,a*0
    return,a*0+1
  endif
  return,(a-mina)/(maxa-mina)

end