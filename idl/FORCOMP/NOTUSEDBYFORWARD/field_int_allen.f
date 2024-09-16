C
C PURPOSE: CALCULATES THE S-FRAME IRRED. TENSOR COMPONENTS OF AVG RADN FIELD
C
C  THIS SUBROUTINE CALCULATES THE IRREDUCIBLE TENSOR COMPONENTS
C  OF THE AVERAGE RADIATION FIELD FROM THE PHOTOSPHERE IN THE
C  S FRAME, GIVEN THE LIMB-DARKENING FUNCTION, AND ASSUMING
C  CYLINDRICAL SYMMETRY OF THE RADIATION FIELD. 
C
C  H IS THE HEIGHT OF THE OBSERVED POINT OVER THE LIMB, EXPRESSED
C  IN SOLAR RADII
C
C INPUTS:
C       KR    INDEX OF THE TRANSITION
C       (FLIMB EXTERNAL  FUNCTION THAT HAS TO BE DEFINED)
C
C OUTPUTS:
C       RJ0   K=0 TENSOR COMPONENT OF THE RADIATION FIELD
C       RJ1   K=1 TENSOR COMPONENT OF THE RADIATION FIELD
C                 (THIS IS ZERO IN CYLINDRICAL SYMMETRY)
C       RJ2   K=2 TENSOR COMPONENT OF THE RADIATION FIELD
C             SEE EQUATIONS 22 AND 23 OF 
C             CASINI, R. & JUDGE, P. G., 1999. AP J 522, 524-539
C
C COMMON:
C       CORON
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
	SUBROUTINE FIELD_INT(KR,RJ0,RJ1,RJ2)
C
C
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'CATOM'
	INCLUDE 'CORON'
	INCLUDE 'CCONST'
	INCLUDE 'CINTS'
C
	SM=ONE/(H+ONE)
	SM2=SM*SM
	CM2=ONE-SM2
	CM=SQRT(CM2)
C
	RLN=ZERO
	IF (CM2.NE.ZERO) RLN=LOG((ONE+SM)/CM)
C
	A1=ONE-CM
	A2=.5*(ONE-RLN*CM2/SM)-A1
	A3=(CM*(THREE-CM2)-TWO)/(THREE*SM2)
C
	B1=CM*SM2
	B2=.125*(THREE*SM2+RLN*CM2*(THREE*SM2+ONE)/SM-ONE)-B1
	B3=((CM2*(20.-9.*CM2)-15.)*CM+FOUR)/(15.*SM2)
C
	WL=ALAMB(KR)
	WM=WL/1.E4
	CALL UV(WM,U,V)
	XNU=1.E8*(TWO*PI)*(CC/WL)
C
C NOW ADD THE ABSOLUTE INTENSITY FACTOR PINT
C XT IS DISK CENTER RADIATION TEMPERATURE FOR I (ALLEN 1973, SEC 80)
C
	XT=6050.
	PINT=PLANCK(XNU,XT)
C
C	PRINT *,'NO LIMB DARKENING'
C	U=ZERO
C	V=ZERO
C
	RJ0=.5*(A1+A2*U+A3*V)*PINT
	RJ1=ZERO
	RJ2=.25*(B1+B2*U+B3*V)/SQRT(TWO)*PINT
	RETURN
	END