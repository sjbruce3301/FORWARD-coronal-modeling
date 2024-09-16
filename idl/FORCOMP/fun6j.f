C
C PURPOSE: CALCULATES 6J VALUE
C
C INPUTS:
C       RJ11,RJ12,RJ13  REAL J QUANTUM NUMBERS IN 1ST ROW OF 6J
C       RJ21,RJ22,RJ23  REAL J QUANTUM NUMBERS IN 2ND ROW OF 6J
C
C OUTPUTS:
C       FUN6J
C COMMON:
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
	FUNCTION FUN6J(RJ11,RJ12,RJ13,RJ21,RJ22,RJ23)
C   CALCULATES 6J	VALUE, EACH RJ IS REAL
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'SGNM'
	INCLUDE 'CINTS'
	INCLUDE 'FACT'
C
	IJ1=NINT(RJ11+RJ11)
	IJ2=NINT(RJ12+RJ12)
	IJ3=NINT(RJ13+RJ13)
	IJ4=NINT(RJ21+RJ21)
	IJ5=NINT(RJ22+RJ22)
	IJ6=NINT(RJ23+RJ23)
	IJM1=(IJ1+IJ2+IJ3)/2
	IJM2=(IJ1+IJ5+IJ6)/2
	IJM3=(IJ4+IJ2+IJ6)/2
	IJM4=(IJ4+IJ5+IJ3)/2
	IJM=IJM1
	IF(IJM2.GT.IJM) IJM=IJM2
	IF(IJM3.GT.IJM) IJM=IJM3
	IF(IJM4.GT.IJM) IJM=IJM4
	IJX1=(IJ1+IJ2+IJ4+IJ5)/2
	IJX2=(IJ2+IJ3+IJ5+IJ6)/2
	IJX3=(IJ3+IJ1+IJ6+IJ4)/2
	IJX=IJX1
	IF(IJX2.LT.IJX) IJX=IJX2
	IF(IJX3.LT.IJX) IJX=IJX3
	FUN6J=ZERO
	IF(IJM.LE.IJX) THEN
	  TERM=ZERO
	  TERM1=FN2(IJ1,IJ2,IJ3)+FN2(IJ1,IJ5,IJ6)
     /	  +FN2(IJ4,IJ2,IJ6)+FN2(IJ4,IJ5,IJ3)
	  DO I=IJM,IJX
	    TERM2=FL(I+1)-FL(I-IJM1)-FL(I-IJM2)-FL(I-IJM3)
     /	    -FL(I-IJM4)-FL(IJX1-I)-FL(IJX2-I)-FL(IJX3-I)
	    TERM=SG(I)*EXP(TERM1+TERM2)
	    FUN6J=FUN6J+TERM
	  END DO
	END IF
	RETURN
	END