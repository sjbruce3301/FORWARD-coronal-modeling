C
C PURPOSE: COMPUTE RADIATIVE RATES R(4) FOR EQN (20) OF 
C          CASINI, R. & JUDGE, P. G., 1999. AP J 522, 524-539
C          THE COMPLETE COEFFICIENT NEEDED FOR EQ. (20) IS 
C          BUILT BY SUMMING OUTSIDE OF THIS ROUTINE IN ROUTINE
C          SE0_BUILD
C
C INPUTS:
C       
C OUTPUTS:
C       R4COEFF
C COMMON:
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
	SUBROUTINE R4(IJ,IJ1,K,R4COEFF)
C  THIS SUBROUTINE CALCULATES THE RADIATIVE RATES R(4)  (IJ1 > IJ)
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'CSE'
	INCLUDE 'SGNM'
	INCLUDE 'CINTS'
	RJ=AJ(IJ)
	RJ1=AJ(IJ1)
	RK=FLOAT(K)
	R4COEFF=SG(INT(RJ+RJ1)+K+1)*(TWO*RJ1+ONE)*ECOEFF(IJ1,IJ)
     /	  *FUN6J(RJ,RJ,RK,RJ1,RJ1,ONE)
	RETURN
	END
