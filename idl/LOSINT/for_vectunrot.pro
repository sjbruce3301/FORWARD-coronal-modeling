;************************************************************************

	pro for_vectunrot,Brmod,Bthmod,Bphmod,$
		Brobs,Bthobs,Bphobs,rmod,thmod,phmod,$
		phicmer,bang,lat

;
;  Name:  FOR_VECTORUNROT
;
;  undo the solar B-angle, carrington phi angle, and latitude rotations
;  for a vector field, to project it onto observer's coordinates
;
; Calls FOR_VECT_THROT
; Called by FOR_POSNOINT, FOR_FIELDCALLS
;
; Written Sarah Gibson 2012-2013
; Version 2.0 July 2014

	Brobs=Brmod
	Bthobs=Bthmod
	Bphobs=Bphmod

	robs=rmod
	thobs=thmod
	phobs=phmod

;  first adjust for theta rotation if necessary 

        if lat ne 0. then for_vect_throt,Brobs,Bthobs,Bphobs,robs,thobs,phobs,-!dpi/2.d0+lat

;  now adjust for Bangle if necessary 
;  note that the central meridian rotation is taken care of in the change
;  of the phi variable (but on its own it doesn't affect the vector B) 

        if bang ne 0. then for_vect_throt,Brobs,Bthobs,Bphobs,robs,thobs,phobs-phicmer,bang


	end
