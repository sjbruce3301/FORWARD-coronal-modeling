pro for_uv_stokes,r3D,thetaB,chiB,bigtheta,bigchi,Bmag,Vwind,w_par,aniso,nangleint,w_d,blend,einsteinA,gJ,StokesUV_I,StokesUV_Q,StokesUV_U

;------------------------------------------------------------------------
;
; Modified from pol_dim1.pro
;	Silvano Fineschi
;                              for_uv_stokes.pro
;------------------------------------------------------------------------
; Purpose:
;
; This routine computes the integrals of the Stoke's parameters over the 
; azimuthal and polar angles of the propagation vector of the chromospheric 
; radiation coming from the spherical cap of the chromospere limited by a 
; cone centered on the scattering point.
;
; Called by FOR_UVMODEL
; Calls FOR_D, FOR_P2_00, FOR_P2_10, FOR_P2_20
;------------------------------------------------------------------------
;
; Inputs: 
; 	All are points in this version
;
;	R3D,	radial (z') heliocentric height [Rsun]
;
;	(B vector spherical coordinated in the local solar vertical frame)
;	THETAB, CHIB: polar and azimuthal angles of magnetic vector [rad]
;	BIGTHETA: polar angle between local solar vertical and the line of sight [rad]
;	BIGCHI: angle between the plane containing the
;               line of sight and plane perpendicular to the plane of the sky
;	BMAG: magnitude of magnetic vector [gauss]
;	
; VWIND -- magnitude of solar wind speed (direction assumed to be along field) [km/s]
; W_PAR -- 1/e Gaussian width for velocity distribution of the coronal ion parallel the magnetic field [km/s]
; ANISO -- parameter describing anisotropy of coronal velocity distribution: 
;          1/e Gaussian width for velocity distribution of the coronal ion perpendicular to the magnetic field 
;	      W_PERP=W_PAR*ANISO; [1 = isotropic distribution]
;  NANGLEINT -- how many steps to take in solid angle integration
;
; W_D   -- 1/e Gaussian width for velocity distribution of the transition region ion [km/s]
; BLEND - line blending factor due to transition
; EINSTEINA: Einstein coefficient for spontaneous emission [1/s] 
; GJ: Landè factor for the atomic transition
;
; Outputs: 
;
; STOKESUV_I,STOKESUV_Q,STOKESUV_U

;Hanle effect parameter => gamma = 0.88 * B * g(J')/A(J',J)                                  
; where B is in gauss and A(J',J) is 1e.7 s units
; See Eq. 16 in Landi Degl'Innocenti, Solar Phys. 102 (1-20) (1985)

gamma = 0.88 * Bmag * gJ/(einsteinA/1.e7)

; Geometrical parameters 

angles={theta:bigtheta,chi:bigchi,thetaB:thetaB,chiB:chiB}

; Doppler dimming parameters

widths={vwind:vwind,w_d:w_d,w_perp:aniso*w_par,w_par:w_par}

; polar angle subtending the solar disk

theta_limb=asin(1./R3D) ; polar angle limit of the solid angle subtending the Sun from the heliocentric height "R3D" [rad]  


;-------------------------------------------------------------------------
; 	                       Main program
;-------------------------------------------------------------------------
 
; Integration over the spherical coordinates of the incoming radiation

; The discrete angular steps dtheta and dchi are defined for a reasonable integration over the spherical coordinates of the incoming radiation
nsteps=nangleint
dtheta_prime=theta_limb/nsteps
dchi_prime=2.*!dpi/nsteps

;creating the angular vectors
theta_prime=dindgen(nsteps)*dtheta_prime
chi_prime=dindgen(nsteps)*dchi_prime

; Converting the angular vectors in 2D matrices in the 2D space of dtheta and dchi
theta_prime_2D=theta_prime # replicate(1.,nsteps)
chi_prime_2D=replicate(1.,nsteps) # chi_prime


; 2D Doppler dimming matrix 
D_2D = for_d(theta_prime_2D, chi_prime_2D, angles, widths) * sin(theta_prime_2D)*dtheta_prime*dchi_prime/(4.*!pi)

; 2D Stokes parameters matrix 	
StokesUV_I_2D=(1.+blend*for_p2_00(theta_prime_2D, chi_prime_2d, angles, gamma)) * D_2D
StokesUV_Q_2D=blend*for_p2_10(theta_prime_2D, chi_prime_2d, angles, gamma) * D_2D
StokesUV_U_2D=blend*for_p2_20(theta_prime_2D, chi_prime_2d, angles, gamma) * D_2D

;------------------------------------------------------------------------
;	                     Results Output
;------------------------------------------------------------------------

StokesUV_I=total(StokesUV_I_2D)
StokesUV_Q=total(StokesUV_Q_2D)
StokesUV_U=total(StokesUV_U_2D)


end
