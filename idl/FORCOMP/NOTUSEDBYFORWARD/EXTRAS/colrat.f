C
C PURPOSE: INITIALIZE COLLISIONAL RATES C AND 
C
C INPUTS:
C
C OUTPUTS:
C
C COMMON:
C
C COMMENTS: October 6, 1999, P. JUDGE
C 
      subroutine colrat

c  Chooses collisional routine. In FIXRAD fixed rates are added into C. 
c  Collision part is therefore stored to enable separation in routine WRATE

      include 'PREC'
      include 'PARAM'
      include 'CINPUT'
      include 'CATOM'
      include 'CLU'

c  initialize collisional rates to zero

      do j=1,nk
        do i=1,nk
          c(i,j)=.0d0
	  end do
	end do

      if (icoll.eq.1) call gencol

c      write(lout,*) 'COLLISIONAL RATES'
c      write(lout,1) (i,i=1,nk)

1	format(3x,6(2x,4x,i6))

      return
      end
