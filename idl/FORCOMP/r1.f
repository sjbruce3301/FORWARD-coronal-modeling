C
C PURPOSE: COMPUTE RADIATIVE RATES R(1) FOR EQN (20) OF 
C          CASINI, R. & JUDGE, P. G., 1999. AP J 522, 524-539
C          R1 IS INITIALIZED TO ZERO AND COMPUTED.
C INPUTS:
C       IJ
C OUTPUTS:
C       R1COEFF
C COMMON:
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
	SUBROUTINE R1(IJ,R1COEFF)
C
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'CSE'
	INCLUDE 'CINTS'
C
	R1COEFF=ZERO
C
	DO IJ1=1,IJ-1
	  R1COEFF=ECOEFF(IJ,IJ1)+R1COEFF
	END DO
C
	RETURN
	END
