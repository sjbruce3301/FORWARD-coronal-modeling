C
C PURPOSE: COMPUTES INELASTIC COLLISION RATES 
C          FROM THE 
C INPUTS:
C          IJ       INDEX OF UPPER LEVEL 
C          IJ1      INDEX OF LOWER LEVEL
C          K        MULTIPOLAR COEFFICIENT
C
C OUTPUTS:
C          CI       INELASTIC RATE FROM MULTIPOLAR COMPONENT K
C COMMON:
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
	FUNCTION CI(IJ,IJ1,K)
C  THIS FUNCTION CALCULATES THE INELASTIC COLLISIONAL RATES 
C  C_I(IJ,IJ1,K) (FOR IJ1 < IJ)
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'CSE'
	INCLUDE 'SGNM'
	INCLUDE 'CINTS'
C
	RJ=AJ(IJ)
	RJ1=AJ(IJ1)
	RK=FLOAT(K)
	JP=NINT(RJ+RJ1)
	JM=NINT(ABS(RJ-RJ1))
	CITMP=ZERO
	DO K1=JM,JP
	  RK1=FLOAT(K1)
     	  CITMP=SG(K1)*FUN6J(RJ,RJ,RK,RJ1,RJ1,RK1)*GAMMAI(IJ,IJ1,K1)
     /	  +CITMP
	END DO
	CI=SG(JP-K)*SQRT((TWO*RJ+ONE)*(TWO*RJ1+ONE))*CITMP
	RETURN
	END