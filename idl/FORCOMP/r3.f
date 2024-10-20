C
C PURPOSE: COMPUTE RADIATIVE RATES R(3) FOR EQN (20) OF 
C          CASINI, R. & JUDGE, P. G., 1999. AP J 522, 524-539
C          THE COMPLETE COEFFICIENT NEEDED FOR EQ. (20) IS 
C          BUILT BY SUMMING OUTSIDE OF THIS ROUTINE IN ROUTINE
C          SE0_BUILD
C
C INPUTS:
C       
C OUTPUTS:
C       R3COEFF
C COMMON:
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
	SUBROUTINE R3(KR,IJ,IJ1,K,K1,IQ,R3COEFF)
C  THIS SUBROUTINE CALCULATES THE RADIATIVE RATES R(3)  (IJ1<IJ)
C
C  THE RADIATION FIELD IS ASSUMED TO BE CYLINDRICALLY
C  SYMMETRIC IN THE REFERENCE FRAME IN WHICH THE S.E.
C  EQUATIONS ARE EXPRESSED.
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'CSE'
	INCLUDE 'SGNM'
	INCLUDE 'CINTS'
	RJ=AJ(IJ)
	RJ1=AJ(IJ1)
	RK=FLOAT(K)
	RK1=FLOAT(K1)
	Q=FLOAT(IQ)
	R3TMP=ZERO
C Note - this next line from 6jul22 version-- should it be kept?
	DO K2=0,2,2 !PGJ TEST
	  RK2=FLOAT(K2)
	  R3TMP=SQRT(TWO*RK2+ONE)
     /	  *FUN3J(RK,RK1,RK2,Q,-Q,ZERO)
     /	  *FUN9J(ONE,RJ,RJ1,ONE,RJ,RJ1,RK2,RK,RK1)
     /        *RADJ(K2,KR)+R3TMP
	END DO
	R3COEFF=SG(K1-IQ)*SQRT(THREE*(TWO*RK+ONE)*(TWO*RK1+ONE))
     /	  *(TWO*RJ1+ONE)*ECOEFF(IJ1,IJ)*R3TMP
	RETURN
	END
