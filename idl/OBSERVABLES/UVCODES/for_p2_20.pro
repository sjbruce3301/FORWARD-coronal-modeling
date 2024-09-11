;------------------------------------------------------------------------
;
;                               FOR_P2_20.pro
;
;-------------------------------------------------------------------------
;
; PURPOSE:
;  This function computes the term of rank 2 of the element "20"
;  of the scattering phase-matrix, P^(2)_20, in any arbitrary frame of reference.
;  See Eqs. 15 and 18 in Landi Degl'Innocenti, A&A 192 (374-379) (1988)
;
; CALLING SEQUENCE:
;   for_P2_20(for_p2_20, theta_prime, chi_prime, angles, gamma)
;
; INPUTS:
;   angles: structure containing 
;           theta = polar angle of the scattered photon direction [rad]
;           chi = azimuthal angle of the scattered photon direction [rad]
;           thetaB = polar angle of vector magnetic field [rad]
;           chiB = azimuthal angle of vector magnetic field [rad]
;   
;   theta_prime = polar angle of the incident photon direction [rad]
;   chi_prime = azimuthal angle of the incident photon direction [rad]
;   
;   gamma:  Hanle effect parameter => gamma = 0.88 * B * g(J')/A(J',J) 
;                                     where B is in gauss and A(J',J) is 1e.7 s units
;                                     See Eq. 16 in Landi Degl'Innocenti, Solar Phys. 102 (1-20) (1985)
;          
;
; OUTPUTS:
;  term of rank 2 of the element "20" of the scattering phase-matrix, P^(2)_20,
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
;
; CALLED BY:
;       FOR_UV_STOKES
;
; CALLS: 
;       None.
;
; MODIFICATION HISTORY:
;   vers. 0.  S. Fineschi, R. Susino, 12 October 2017.
;	*double-checked against eqn 15 switching 02 --> 20 Dec 2023
;	changed integer multipliers to floats
;
;
function for_p2_20, theta_prime, chi_prime, angles, gamma

; introducing shorter notations for the trigonometric functions of the scattering geometry

theta=angles.theta
chi=angles.chi
thetaB=angles.thetaB
chiB=angles.chiB

C1=cos(theta)
S1=sin(theta)

C2=cos(theta_prime)
S2=sin(theta_prime)

c=cos(chi-chi_prime)
s=sin(chi-chi_prime)

CB=cos(thetaB)
SB=sin(thetaB)
cB1=cos(chi-chiB)
sB1=sin(chi-chiB)
cB2=cos(chi_prime-chiB)
sB2=sin(chi_prime-chiB)

;Hanle effect parameters
cI=1./sqrt(1.+gamma^2)
sI=gamma/sqrt(1.+gamma^2)
cII=1./sqrt(1.+4.*gamma^2)
SII=2.*gamma/sqrt(1.+4.*gamma^2)


P2_20=(3./4.)*(-2.*(C2*S1-S2*C1*c)*S2*s $
  - 2.*cI*sI*(C2*CB+S2*SB*cB2)*((C2*SB-S2*CB*cB2)*(S1*CB*cB1-C1*SB*(cB1^2-sB1^2))-(S1*(CB^2-SB^2)-2*C1*CB*SB*cB1)*S2*sB2*sB1) $
  - 2.*sI^2*(C2*CB+S2*SB*cB2)*((C2*SB-S2*CB*cB2)*(S1*(CB^2-SB^2)-2.*C1*CB*SB*cB1)*sB1+(S1*CB*cB1-C1*SB*(cB1^2-sB1^2))*S2*sB2) $
  - cII*sII*(((C2*SB-S2*CB*cB2)^2-S2^2*sB2^2)*(S1*SB*cB1+C1*CB*(cB1^2-sB1^2))-2.*(C2*SB-S2*CB*cB2)*(S1*CB*SB+C1*(1.+CB^2)*cB1)*S2*sB2*sB1) $
;  + sII^2*(((C2*SB-S2*CB*cB2)^2-S2^2*sB2^2)*(S1*CB*SB+C1*(1.+CB^2)*cB1)*sB1+2.*(C2*SB-S2*CB*cB2)*(S1*SB*cB1+C1*CB*(cB1^2-sB1^2))*S2*sB2))
  - sII^2*(((C2*SB-S2*CB*cB2)^2-S2^2*sB2^2)*(S1*CB*SB+C1*(1.+CB^2)*cB1)*sB1+2.*(C2*SB-S2*CB*cB2)*(S1*SB*cB1+C1*CB*(cB1^2-sB1^2))*S2*sB2))
;
;print, 'P2_20(',chi2,')= ', P2_20

return, P2_20
end
;-------------------------------------------------------------------------
