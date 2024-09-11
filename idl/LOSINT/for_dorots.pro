;************************************************************************

	pro for_dorots,thmod,phmod,r3D,theta3D,phi3D,phicmer,bang,lat

;
;  Name:  FOR_DOROTS
;
;  do the B angle, carrington phi angle, and latitude rotations
;
; Called by FOR_INTENSINT, FOR_POSNOINT
;
; Calls FOR_THROT
;
; Written by Sarah Gibson 2012-2013
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
; Version 2.0 July 2014


	thmod=theta3D
	phmod=phi3D

;  first add Bangle if necessary - this rotates about the viewer's y axis
;  into a 3D spherical coordinate system for a model Sun

        if bang ne 0. then for_throt,r3D,thmod,phmod,-bang

; define the central meridian of the model coordinate system
; to be that of the observer
; 

        phmod = phmod+phicmer


;  now add theta rotation if necessary 
; this is a way of rotating a model structure from e.g. the equator
; and by definition it is about the model coordinate  y axis
; since model structure is centered on phi=0,theta=90
;
; Note -- this is not equivalent to the roll of a spacecraft, unless
; if Bang is zero, and Cmer=-90, and Phio=0
;
; also, it does not make much sense to apply to global models like PFFS or PSIMOD
; it is basically similar to PHIO, and a means of moving a model structure around useful
; for fitting purposes
;

        if lat ne 90. then for_throt,r3D,thmod,phmod,!dpi/2.d0-lat

;
;  make sure phi is between 0 and 2 pi
;
        test = where(phmod gt 2.d0*!dpi)
        if(min(test) ne -1) then phmod[test] = phmod[test] - 2.d0*!dpi
        test = where(phmod lt 0.)
        if(min(test) ne -1) then phmod[test] = phmod[test] + 2.d0*!dpi

	end
