C
C PURPOSE: MAIN PROGRAM TO COMPUTE EMISSION COEFFS OF M1 CORONAL LINES
C          INTEGRATED ALONG THE LOS, FOR A SET OF LOS'S IN AN 
C          ASSIGNED GRID OF CARTESIAN COORDINATES.
C
C INPUTS:
C
C OUTPUTS:
C
C COMMON:
C
C COMMENTS: AUGUST 9, 2000 P. JUDGE
C 
      PROGRAM FORCOMP
C
C  MAIN PROGRAM
C
C  CALCULATES THE EMISSION COEFFICIENTS OF FORBIDDEN CORONAL
C  LINES, INTEGRATED ALONG THE LOS, FOR A SET OF LOS'S IN AN 
C  ASSIGNED GRID OF CARTESIAN COORDINATES.
C
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      INCLUDE 'CGRID'
      INCLUDE 'CCONST'
      INCLUDE 'CBCNST'
      INCLUDE 'CDOVE'
      common /udove/ ldove
      real*8 tempry(9)
      integer nlos, nunwrap
      integer nverbose
c
C      CALL CPTIME('TOTAL ',0,0,5)
C
      CALL STARTD
c read the grid from for_intensint.pro IDL output.
      irecd=0
      call open(idove,'modelcube4comp.dat',irecd,'OLD')
      irecw=0
      call open(ldove,'F77OUTPUT',irecw,'NEW')
      read(idove) nlos,nunwrap,nverbose
      write(ldove) nlos
      write(ldove) nunwrap
      if (nverbose .eq. 1) write(*,*)
     *   'forcomp: reading data cube: nlos nunwrap= ',nlos,nunwrap
C      print*,'munwrap = ',munwrap
      if(nlos .gt. mtau) call stop('nlos .gt. mtau')
      if(nunwrap .gt. munwrap) call stop('nunwrap .gt. munwrap')
      ngx=nlos
C
C  j-LOOP OVER THE DIFFERENT LINES OF SIGHT
C
      do j=1,nunwrap
         if (nverbose .eq. 1) then
          if(mod(j-1,100) .eq. 0) WRITE(*,*)'DOING LOS INDEX=',J,'/',NUNWRAP,'...'
	 endif
         do i=1,nlos
            read(idove), tempry
            r3d(i)=real(tempry(1))
            theta3d(i) =real(tempry(2))
            phi3d(i) =real(tempry(3)) 
            dens3d(i) =real(tempry(4))
            temp3d(i) =real(tempry(5))
            br3d(i) =real(tempry(6))
            bth3d(i) =real(tempry(7))
            bph3d(i) =real(tempry(8))
            vel3d(i) =real(tempry(9))
C            if (nverbose .eq. 1) write(*,*)'r= ',r3d
         end do
 3654    format(1x,9(f8.5,1x))
         CALL M1SYNTHD
         CALL OUTU
      END DO
C      CALL CPTIME('TOTAL ',0,1,5)
      call close(idove)
      call close(ldove)
      CALL STOP('NORMAL END')
      END
      

