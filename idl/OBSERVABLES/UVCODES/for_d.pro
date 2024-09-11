;------------------------------------------------------------------------
;
;                               FOR_D.pro
;
;------------------------------------------------------------------------
;
; Purpose:
;
; This function computes Doppler/temperature dimming factor
;
; Called by FOR_UV_STOKES
;
; CALLING SEQUENCE:
;   for_d(theta_prime, chi_prime, angles, widths)
;
; INPUTS:
;   angles: structure containing
;           theta = polar angle of the scattered photon direction [rad]
;           chi = azimuthal angle of the scattered photon direction [rad]
;           thetaB = polar angle of vector magnetic field (the wind velocity vector has the same spherical coordinates ) [rad]
;           chiB = azimuthal angle of vector magnetic field (the wind velocity vector has the same spherical coordinates ) [rad]
;
;   theta_prime = polar angle of the incident photon direction [rad]
;   chi_prime = azimuthal angle of the incident photon direction [rad]
;   
;   widths: structure containng:
;           vwind = solar wind speed [km/s]
;           w_d = 1/e most probable speed equivalent to the chromospheric line-width [km/s]
;           w_par = 1/e most probable speed parallel to the B field vector of the emitting coronal ion [km/s]
;           w_perp = 1/e most probable speed perpendicular to the B field vector of the emitting coronal ion [km/s]
;
; OUTPUTS:
;  Doppler/temperature dimming factor
;
; SIDE EFFECTS:
;   None.
;
; RESTRICTIONS:
;   None.
;
; PROCEDURE:
;   Straightforward.
;
; CALLED BY:
;       FOR_UV_STOKES
;
; CALLS:
;       None.
;
; MODIFICATION HISTORY:
;   vers. 0.  S. Fineschi, 15 October 2017.
;
function for_d, theta_prime, chi_prime, angles, widths

thetaB=angles.thetaB
chiB=angles.chiB
vwind=widths.vwind
w_d=widths.w_d
w_par=widths.w_par
w_perp=widths.w_perp

; introducing shorter notations for the trigonometric functions of the scattering geometry

C2=cos(theta_prime)
S2=sin(theta_prime)

CB=cos(thetaB)
SB=sin(thetaB)

cB2=cos(chi_prime-chiB)
sB2=sin(chi_prime-chiB)


W=sqrt( 1. +$
  (w_perp/w_d)^2 * ( (S2*sB2)^2+(SB*C2-CB*S2*cB2)^2 ) +$
  (w_par /w_d)^2 * (S2*SB*cB2+CB*C2)^2 )

D=1./(sqrt(!pi)*W)*$
  exp( -( (vwind/w_d)*(S2*SB*cB2+C2*CB)/W )^2 )	
;print, 
return, D
end
;-------------------------------------------------------------------------
