pro for_azrot,azimuth,linear,theta,stazuse,Quse,Uuse

;
; this program takes azimuth, Q, and U  data from CoMP FITS file
; and rotates to coordinate system where azimuth is measured relative
; to local vertical (i.e, radial)
;
;  Called by FOR_PLOTFITS
;
;   INPUTS:  
;
;	AZIMUTH: CoMP E-W coordinate frame Azimuth data
;
;	LINEAR: Magnitude of linear polarization vector (e.g, L)
;
;	THETA (degrees)
;		polar angle in plane of sky associated with azimuth data
;
;   OUTPUTS:
;
;	STAZUSE:  Azimuth rotated to radial frame
;	QUSE: Stokes Q in radial frame
;	UUSE: Stokes U in radial frame
;
;
;  Written by Sarah Gibson 
;  FORWARD Version 2.0 August 2014
;

;
; first rotate AZ from E-W to radial
;

      thew=theta+90.d0
      testsouth=where(theta gt 90.d0 and theta le 270.d0)
      if min(testsouth) ge 0. then thew[testsouth]=thew[testsouth]-180.d0
      testquad=where(theta gt 270.d0)
      if min(testquad) ge 0. then thew[testquad]=thew[testquad]-360.

      stazuse=azimuth-thew
      test=where(stazuse lt 0.)
      if min(test) ge 0 then stazuse[test]=stazuse[test]+180.d0

;
; now take care of Q and U 
;
      mdtor=!dpi/180d0
      Quse = linear*cos(stazuse*mdtor*2.d0)
      Uuse = linear*sin(stazuse*mdtor*2.d0)

end
