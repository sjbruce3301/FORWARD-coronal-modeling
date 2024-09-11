C
C PURPOSE: Calculates the rotation matrix	d[rJ,rM1,rM](thet)
c       for -180 < thet < 180
C
C INPUTS:
C
C OUTPUTS:
C
C COMMON:
C
C COMMENTS: October 6, 1999, P. JUDGE
C 
	function rdmat(rJ,rM1,rM,thet)
	include 'PREC'
	include 'PARAM'
	include 'CCONST'
	include 'SGNM'
	include 'FACT'

	Th=thet*PI/.18d3

	cc=dcos(.5d0*Th)
	ss=dsin(.5d0*Th)

	ifs=idnint(rJ+rM)
	ifd=idnint(rJ-rM)
	ifs1=idnint(rJ+rM1)
	ifd1=idnint(rJ-rM1)
	imd=idnint(rM1-rM)
	imdf2=idnint(rJ+rJ)-imd

	imax=min0(ifs,ifd1)
	imin=max0(0,-imd)

	tmp=.0d0

	do i=imin,imax

	  i2=i+i

	  k1=imdf2-i2
	  k2=imd+i2

	  ecc=.0d0
	  ess=.0d0

	  if(dabs(thet).ne..18d3) then
	    ecc=cc**k1
	  else
	    if(k1.eq.0) ecc=.1d1
	  end if

	  if(dabs(thet).ne..0d0) then
	    ess=ss**k2
	  else
	    if(k2.eq.0) ess=.1d1
	  end if

	  k=imd+i

	  tmp=sg(k)*dexp(-fl(k)-fl(ifs-i)-fl(ifd1-i)-fl(i))*ecc*ess+tmp

	end do

	rdmat=dexp(.5d0*(fl(ifs)+fl(ifd)+fl(ifs1)+fl(ifd1)))*tmp

	return
	end
