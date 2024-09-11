;+
;
; NAME:
; croissant_get_path_length
;
; PURPOSE:
;
; Calculates total length of a path in 3 dimensions, given input of 
; 3 vectors listing the x,y,z coordinates respectively
;
; CALLING SEQUENCE:
; out=croissant_scale_values(x,y,z [,dx=dx,dy=dy,dz=dz,ds=ds])
;
; INPUTS:
;   X,Y,Z: Vectors giving the x,y, and z coordinates that trace a path in three dimensions. 
;           They should be all same number of elements
;
; OUTPUTS:
;   OUT: returns vector of same number of elements as x,y,z. Each element contains the total path length
;         from the first element of x,y,z.
;
; OPTIONAL KEYWORD 
;   dx,dy,dz: vector of same number of elements as x,y,z. Each element contains the x,y,z distance between
;             those neighbouring points using a finite difference. 
;
;   ds: vector of same number of elements as x,y,z. Each element contains the path distance between
;             those neighbouring points using a finite difference.
;   
; OPTIONAL INPUTS
;   None
;
; OPTIONAL OUTPUTS
;   None

;
; PROCEDURE:
; Calculates the finite distance between the x,y, and z points then sums these along the path
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

function croissant_get_path_length,x,y,z,dy=dy,dx=dx,dz=dz,ds=ds

  if  n_elements(x) ne n_elements(y) or $
      n_elements(x) ne n_elements(z) then begin
    print,'Please supply three vectors of same number of elements! (croissant_get_path_length)'
    return,-1
  endif
  
  dx=croissant_finite_difference_1d(-1,x)
  dy=croissant_finite_difference_1d(-1,y)
  dz=n_params() gt 2?croissant_finite_difference_1d(-1,z):0

  pl=total(sqrt(dx^2+dy^2+dz^2),/cumul,/nan)

  if arg_present(ds) then ds=croissant_finite_difference_1d(-1,pl)

  return,pl

end