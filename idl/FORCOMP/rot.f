C
C PURPOSE: ROTATES VECTOR COMPONENTS
C
C INPUTS:
C       U   VECTOR COMPONENT
C       V   ORTHOGONAL VECTOR COMPONENT
C       TH  ANGLE TO BE ROTATED
C OUTPUTS:
C       U
C       V
C COMMON: NONE
C
C COMMENTS: AUGUST 24, 2000, P. JUDGE
C
C NOTES:    
C 
      SUBROUTINE ROT(U,V,TH)
      INCLUDE 'PREC'
      C=COS(TH)
      S=SIN(TH)
      U1=U*C-V*S
      V1=U*S+V*C
      U=U1
      V=V1
      RETURN
      END
