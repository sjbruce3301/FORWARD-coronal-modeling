;
; Programs written/modified from Fortran by Stephen White
;
; Version 2.0 July 2014
;---------------------------------------------------------------------------

; gyrores uses cold plasma parameters for x,o modes

pro for_coldpl,mode,theta,freq,fgyro,fplasma,th,nind,tee,qa,fact,vgz
;+
; NAME:
;     COLDPL
;
; PURPOSE:
;     Determines all the parameters for the x and o modes in the cold
;     plasma approximation
;
; CATEGORY:
;       Mathematical Functions, General
;
; CALLING SEQUENCE:
;       COLDPL, mode, theta, frequency, gyro_frequency, plasma_frequency,
;               temperature, refractive_index, tee, qa, factor, group_velocity_z
;
; INPUTS:
;     Mode               'X' or 'O'
;     Theta              angle between B and k
;     Frequency          wave frequency in Hz
;     Gyro_Frequency     gyro frequency in Hz
;     Plasma_Frequency   plasma frequency in Hz
;     Temperature        Plasma temperature in K
;
; KEYWORD PARAMETERS:
;       None
;
; OUTPUT PARAMETERS:
;     Refractive_index   
;     Tee                Melrose T transverse polarization vector parameter
;     Qa                 Melrose K longitudinal polarization parameter
;     Factor             n*d(n*f)/df, n=index, f=frequency
;     Group_velocity_z   z-component of group velocity
;
;                        tder=f/T*dT/df & nder=1/n*dn/dtheta are also calculated
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; PROCEDURE:
;       Evaluate formulae from Plasma Astrophysics in double precision
;
;
; Called by FOR_RADIOMODEL
;
; MODIFICATION HISTORY
;       Converted Fortran program
;-


      if ((mode eq 'X') or (mode eq 'x')) then im=0 else im=1
      isig=[-1,1]
      thet=double(theta)
      Y=double(fgyro/freq)                                                      
      X1=double(fplasma*fplasma/fgyro/fgyro)
      X=double(X1*Y*Y)
      VT=SQRT(TH/6.d9)
      IF (ABS(COS(THET)) GT 1.d-10) then GOTO,L101
      IF (ISIG(IM) EQ 1) then GOTO,L400
      TEE=0.                                                            
      GOTO,L102 
L101:
DELTA=SQRT(((Y*SIN(THET))^4.d0)/4.d0+(1.d0-X)*(1.d0-X)*((Y*COS(THET))^2.d0))
      TEE=(-((Y*SIN(THET))^2.d0)/2.d0-ISIG(IM)*DELTA)/Y/(1.d0-X)/COS(THET)    
L102:
QA=X*Y*SIN(THET)*(1.d0+Y*TEE*COS(THET))/(1.d0-X-Y*Y+X*Y*COS(THET)*Y*COS(THET))
NIND2=1.d0-X*(1.d0-X)*(1.d0+Y*TEE*COS(THET))/(1.d0-X-Y*Y+X*Y*Y*COS(THET)*COS(THET))                                  
      IF (NIND2 LT 0.d0) then goto,L200
      NIND=SQRT(NIND2)                                                  
      TDER=(1.d0+X)/(1.d0-X)*(1.d0-TEE*TEE)/(1.d0+TEE*TEE)                      
      FACT=1.d0+X*Y*TEE*COS(THET)/2.d0/((TEE-Y*COS(THET))^2.d0)*(1.d0+TDER)
      NDER=QA*TEE/(1.d0+TEE*TEE)                                          
      VGZ=(COS(THET)+NDER*SIN(THET))/FACT*NIND/VT                       
      GOTO,L300
L400:
      NIND=SQRT(1.d0-X)                                                   
      TEE=1.d20                                                         
      QA=X*Y/(1.d0-X)                                                     
      FACT=1.d0                                                           
      VGZ=0.d0                                                            
      GOTO,L300                                                         
L200:
      NIND=-1.d0                                                          
L300:
;      WN=OMEG*NIND*VT                                                   
      RETURN                                                            
      END                                                               

