C
C PURPOSE: CALCULATES AND STORES FACTORIALS AND SIGNUM FOR 3NJ PROGRAMS
C
C INPUTS:
C
C OUTPUTS:
C
C COMMON:
C       SGNM, FACT
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
	SUBROUTINE FCTSG
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'SGNM'
	INCLUDE 'CINTS'
	INCLUDE 'FACT'
	FL(0)=ZERO
	SG(0)=ONE
	DO I=1,MSGN
	  I1=I-1
	  FL(I)=FL(I1)+LOG(FLOAT(I))
	  SG(I)=-SG(I1)
	  SG(-I)=SG(I)
	END DO
	RETURN
	END
