C
C PURPOSE: COMPUTE COLLISION RATES C(5) FOR EQN (20) OF 
C          CASINI, R. & JUDGE, P. G., 1999. AP J 522, 524-539
C          THE COMPLETE COEFFICIENT NEEDED FOR EQ. (20) IS 
C          BUILT BY SUMMING OUTSIDE OF THIS ROUTINE IN ROUTINE
C          SE0_BUILD
C INPUTS:
C          IJ       INDEX OF LEVEL.  
C          IJ1      INDEX OF UPPER LEVEL
C          K        MULTIPOLAR COEFFICIENT
C OUTPUTS:
C          C5COEFF  
C COMMON:
C          CSE
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
	SUBROUTINE C5(IJ,IJ1,K,C5COEFF)
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'CSE'
	INCLUDE 'CINPUT'
	INCLUDE 'CLU'
	INCLUDE 'CINTS'
	RJ=AJ(IJ)
	RJ1=AJ(IJ1)
	C5COEFF=SQRT((TWO*RJ1+ONE)/(TWO*RJ+ONE))*CS(IJ,IJ1,K)
	IF(IDEBUG.NE.0)WRITE(lout,100)IJ,IJ1,K,C5COEFF
 100	format('C5(L,U,K)',3(I4,1x),1p,e9.2,0p)
	RETURN
	END