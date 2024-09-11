C
C PURPOSE: CALCULATES LINEAR INTERPOLATION
C
C INPUTS:
C       X, Y, N, X1,NNN (NNN NOT NEEDED, USED FOR COMPATIBILITY WITH SPLIN)
C OUTPUTS:
C       Y1 INTERPOLATED VALUE
C COMMON:
C
C RESTRICTIONS:
C       
C COMMENTS: JANUARY 11, 2006 P. JUDGE
C 
	FUNCTION ALIN(T,X,Y,N,NNN)
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	DIMENSION X(N),Y(N)
C       
	IF (T.LT.X(1)) THEN
	   ALIN=Y(1)
	   RETURN
	END IF
C       
	IF (T.GT.X(N)) THEN
	   ALIN=Y(N)
	   RETURN
	END IF
	I=0
 1	I=I+1
	I1=I+1
	IF ((T.GE.X(I)).AND.(T.LT.X(I1))) THEN
	   DL=(T-X(I))/(X(I1)-X(I))
	   ALIN=Y(I)+(Y(I1)-Y(I))*DL
	ELSE
	   GOTO 1
	END IF
	RETURN
	END
