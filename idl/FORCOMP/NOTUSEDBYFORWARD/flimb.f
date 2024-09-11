C
C PURPOSE: CALCULATES RADIATION FIELD INTENSITY IN THE DIRECTION THETA=ARCOS(X)
C
C INPUTS:
C       KR  TRANSITION INDEX
C	X   COS THETA
C       XM  COS THETAM
C           YMU IS MU= COS ZETA IN EQUATION 29 OF CASINI & JUDGE 1999
C OUTPUTS:
C       FLIMB
C COMMON:
C
C RESTRICTIONS:
C       
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
	FUNCTION FLIMB(KR,X,XM)
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'CATOM'
	INCLUDE 'CCONST'
	INCLUDE 'CINTS'
	YMU=SQRT((X+XM)*(X-XM)/(ONE-XM*XM))
C
C  LAMBDA IS IN ANGSTROMS
C  XNU IS ANGULAR FREQUENCY OF TRANSITION
C
	WL=ALAMB(KR)
	WM=WL/1.E4
	XNU=1.E8*(TWO*PI)*(CC/WL)
C
C XT IS DISK CENTER RADIATION TEMPERATURE FOR I (ALLEN 1973, SEC 80)
C
	XT=6050.
C
C ALLEN'S (1973) LIMB DARKENING (P 170, TOP)
C
	CALL UV(WM,U,V)
	FLIMB=PLANCK(XNU,XT)*(ONE-U-V+(U+V*YMU)*YMU)
	RETURN
	END
