;************************************************************************
     pro for_intpos,r,theta,phi,ObsPramsStruct,LosPramsStruct,GridPramsStruct,ruse,thuse,phuse,losoffuse,alfa,notdisk,nunwrap,verbose=verbose

;+
; Name: FOR_INTPOS
;
; Purpose:  	identifies points under disk gets reduced set
;			also fixes pole singularities for theta
;			note- this will exclude points where the LOS
;			 intersection with POS X=0 is < 1. There is a
;			 possibility that there could be points below
;			 the solar surface along the LOS, if either 
;			 the distobs is set very close to the Sun (instead of 1AU)
;			 and/or losuse=user,/pos and losoffset puts a point below r3d=1
;			 This will be taken care of in for_intlos.
;			 Note this can't happen
;			 for LOSUSE = 'XLOS' (parallel lines of sight).
;
; INPUTS
;       r - r in plane of the sky
;       theta - theta (polar angle, 0 - 2 pi, counterclockwise from north) in plane of the sky
;       phi - central meridian longitude
;		note these have already been unwrapped to 1D in FOR_INTENSINT
;
;	ObsPramsStruct includes information about instrument/line being analyzed
;	LosPramsStruct includes information about Occult, UpOccult
;	GridPramsStruct includes information about LOSOFFSET, POS
;
; KEYWORD INPUT
;	verbose
;
; OUTPUTS
;
;  	ruse
;	thuse
;	phuse
;		corrected POS coordinates, not counting occulted
;	losoffuse
;		line of sight offset, same dimension as ruse, thuser, phuse
;	alfa (limb)
;	notdisk - positions of unocculted
;	nunwrap - number of unocculted
;
; Called by: FOR_INTENSINT
;
; History: Written by Sarah Gibson 2015
;      Sept 2021: fixed bug where when all points below disk
;		notdisk is all points so did unnecessary calculation
;	March 2022: changed xoffset --> losoffset
;		updated notes
;	July 2022: updated to not apply notdisk to  USER
;		problem was that r=0 losoffset=1.2 is what
;			will generally come out of r=1.2,theta=90
;			for USER
;		but kept Carrington map check
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;		removed lines checking for 'NULL' value in losoffset
;		because that is now done in for_get_grid
;		Also updated treatment of losoffset to deal with 
;		  dimensions properly
;	Apr 2022 -- commented out NEVIII setting nodisk etc
;       May 2022 -- gave same capability for other UV specpol lines
;                       (OVI, LYA-- be careful esp. for LYA 
;                       -- just for tests because 
;                       generally chromo not treated in FORWARD)
;-

;
; LOSoffset should hae same dimension as r, theta, phi
       losoffset=GridPramsStruct.LOSoffset


; both limbs will be represented in the plane of the sky:
;  the angle phi is the central meridian angle
;
; alfa +1 for West (right) limb
; alfa -1 for East (left) limb
;
;
; make sure that theta is not too close to the poles or alfa will blow up
;
       test=where(abs(theta) lt 1d-5)
       if min(test) ne -1 then theta[test]=1d-5
       test=where(abs(theta-(!dpi)) lt 1d-5)
       if min(test) ne -1 then begin
                thetafix=theta[test]
                test2=where(thetafix gt (!dpi))
                if min(test2) ne -1 then thetafix[test2]=(!dpi)+1d-5
                test3=where(thetafix le (!dpi))
                if min(test3) ne -1 then thetafix[test3]=(!dpi)-1d-5
                theta[test]=thetafix
       endif
       test=where(abs(theta-2.d0*(!dpi)) lt 1d-5)
       if min(test) ne -1 then theta[test]=2.d0*1d-5
;
;
;  unlike for plane of sky variable, LOS integrand is directional
;  so it makes sense to check that a point is not occulted even for 
;  Carrington maps (at least for LIMB=CMER case this could happen)
;
;  so, establish points not under solar disk (or occulters)
;  unless set to negative (dashed line, no data removed)
;
       odid=0
       udid=0
       ddid=0
       if is_number(LosPramsStruct.Occult) then if LosPramsStruct.Occult ge 1. then odid=1
       if is_number(LosPramsStruct.UpOccult) then if LosPramsStruct.UpOccult gt 1. and LosPramsStruct.UpOccult lt 1000.d0 then udid=1
       if is_number(LosPramsStruct.DoDisk) then if LosPramsStruct.DoDisk ne 0. and strpos(strupcase(ObsPramsStruct.Instrument),'OMP') lt 0 $
            and strpos(strupcase(ObsPramsStruct.Instrument),'CORMAG') lt 0 and $
;            (strupcase(ObsPramsStruct.IClass) ne 'UV SPECTROPOLARIMETERS') and $
;            strupcase(ObsPramsStruct.Instrument) ne 'LYA' and $
;            strpos(strupcase(ObsPramsStruct.Instrument),'OVI') lt 0 and $
;            strpos(strupcase(ObsPramsStruct.Instrument),'NEVIII') lt 0 and $
;            strpos(strupcase(ObsPramsStruct.Instrument),'MGIX') lt 0 and $
            strupcase(ObsPramsStruct.Instrument) ne 'WL' and $
            strupcase(ObsPramsStruct.Instrument) ne 'KCOR' then ddid=1
       if odid eq 1 and udid eq 1 and ddid eq 0 then notdisk=where(r ge LosPramsStruct.Occult and r le LosPramsStruct.UpOccult)
       if odid eq 1 and udid eq 0 and ddid eq 0 then notdisk=where(r ge LosPramsStruct.Occult)
       if odid eq 0 and udid eq 1 and ddid eq 0 then notdisk=where(r ge 1. and r le LosPramsStruct.UpOccult)
       if odid eq 0 and udid eq 0 and ddid eq 0 then notdisk=where(r ge 1.)
       if odid eq 0 and udid eq 1 and ddid eq 1 then notdisk=where(r le LosPramsStruct.UpOccult)
       if odid eq 0 and udid eq 0 and ddid eq 1 then notdisk=-2


       if strupcase(GridPramsStruct.GridType) eq 'USER' then begin
         notdisk=-3
;
; so notdisk=-1 happens if not USER and no data with rpos within limits set by occult,upoccult,dodisk
;  notdisk=-2 happens if not USER and dodisk set and upoccult not
;  notdisk=-3 is USER 
;
       endif else begin
        if min(notdisk) ge 0  then begin
         nunwrap=size(notdisk)
         nunwrap=nunwrap[1]
;
;  don't bother doing calculation below disk
;
         ruse=r[notdisk]
         thuse=theta[notdisk]
         phuse=phi[notdisk]
         losoffuse=losoffset[notdisk]
        endif 
;
; check to be sure there are any points worth calculating at all
;
        if min(notdisk) eq -1 then begin
         nunwrap=0
         ruse=0.d0
         thuse=0.d0
         phuse=0.d0
         losoffuse=0.d0
        endif
       endelse
;
; and keep all points if dodisk is set (forcing occult=0) and upoccult not set
;	notdisk=-2
;  or USER (notdisk=-3)
;

       if min(notdisk) le -2 then begin
         nunwrap=size(r)
         nunwrap=nunwrap[1]
         ruse=r
         thuse=theta
         phuse=phi
	 losoffuse=losoffset
       endif

       if verbose ne 0 then $
                print,'for_intensint: number of non-disk grid points: nunwrap = ',nunwrap

       if nunwrap ne 0 then alfa = -sin(thuse)/abs(sin(thuse)) else alfa=0.d0  ;theta  is polar angle 0 - 2*pi - note counterclockwise!

end
