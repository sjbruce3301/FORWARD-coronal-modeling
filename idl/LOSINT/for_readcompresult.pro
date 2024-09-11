pro for_readcompresult,stokesout,stokeslout,wavelengths,velocities,centwave,verbose=verbose,uselinenum=uselinenum,linewidth=linewidth,lchisq=lchisq,vintchoice=vintchoice

;
; read the fortran output of forcomp and return Stokes parameters
;
;  INPUTS
;
;   From F77OUTPUT file (output of FORCOMP.F and OUTU.F code): 
;     NI = number of LOS positions (not used in this code since CLE integrates along LOS)
;     NGRID= number of POS positions
;        NI and NGRID originate in FOR_INTENTSINT as NLOS and NUNWRAP
;	 Arise from FORWARD keyword inputs (e.g., NLOS and NGRID)
;     NLINES = number of lines (e.g., Fe13 generally has two, 10747 10898)
;     NQ = [nlines] -- number of points in wavelength for each line
;      (note, this may be different for each line, so MQ is set as max dimension of lines)
;     DLAMBDA=[mq,nlines] - distance from line center in wavelength
;     ILAMBDA=[nlines] - central (rest) wavelength
;	  NLINES, NQ, DLAMBDA, ILAMBDA set in ATOM files in FORCOMP/run
;     IWLINE whether or not to output full line profile (STOKESLOUT, below)
;	  FORWARD keyword input, passed through FORTRAN codes
;     STOKES=[6,ngrid,nlines]-stokes vectors plus V magnetogram approx and central intensity
;       for each POS position and line
;     STOKESL=[5,ngrid,nlines,mq] stokes vectors plus V magnetogram approx
;       for each POS position and line and wavelength
;     CENNUMARR=[ngrid,nlines] - wavelength position of central intensity
;	for each POS position and line
;     WEIGHTS=[ngrid,nlines,mq] integration weights 
;       for each POS position and line and wavelength
;
;   KEYWORDS:
;
;   VERBOSE print various information
;
;   USELINENUM 
;    set in FOR_INTCOMPCLOSE
;    default is zero, which will give first line within range
;     set by FOR_WLMIN, FOR_WLMAX (set in FOR_COMPDEFAULT)
;     with Fe13_107898 it is set to 1,
;     so e.g., 0=10747, 1=10797 for Fe13
;    Right now other lines (Fe11,Fe14, Si9, Si10) default USELINENUM to zero,
;     so in order to access lines other than the first ones listed
;     in the FORCOMP/run/atom* files, need to narrow the wavelength
;     range using FOR_WLMIN, FOR_WLMAX
;
;  VINTCHOICE -- how to represent Stokes V
;	set in FOR_COMPDEFAULTS (carried through in ObsPramsStruct.FCompPrams)
;		0: "classic"- use output of FORCOMP; here sign on V is changed at 
;			rest wavelength (ilambda). This gives an unsigned integral _if_
;			Stokes V is antisymmetric about the rest wavelength, which it won't be 
;			if e.g. there is velocity. BETTER NOT TO USE.
;		1: "classic_check"same as 0, but calculated in this routine instead of FORCOMP -- should give
;			same result as 0 (for debugging purposes). BETTER NOT TO USE.
;		2: "upgraded_classic" -- uses Icent instead of rest wavelength, calculated as 1
;			this is better for dealing with velocity (Icent is shifted) -- but,
;			still could be weirdness arising from LOS integral - that is, the zero
;			crossing of (integrated) Stokes V might not be at the maximum of (integrated) Stokes I
;		3: "unsigned_integral" -- thus, integral is done over |V|, with a sign assigned
;			based on the dominating sign to the left of ilambda
;				***DEFAULT***
;		4: "peak-to-peak/eff" -- this is the max(V) - min(V), multiplied by sign based on
;			dominating sign to left of ilambda as in choice 2
;			In order to keep the units straight and avoid too many other changes,
;			this will be also be multiplied by I/Icent - effective line width
;			so that V/I will actually be V(peak to peak)/)Icent 
;		5:"peak-to-peak/lw" -- same as #3, but instead of multiplying by I/Icent 
;			multiplies by line width (calculated below via gaussian fit)
;	
;
;  OUTPUTS
;
; -STOKESOUT = [6,ngrid,0] -stokes vectors plus V magnetogram approx and central intensity
;	        for each position and for one chosen line i.e. keyword USELINENUM
; -STOKESLOUT = [5,ngrid,nq,0]
; 		stokes vectors + V magnetogram approx for each position, wavelength resolved
;		only passed back if IWLINE ne 0
;		also done just for one line USELINENUM
; -WAVELENGTHS [nq] 
; -VELOCITIES [ngrid]
; -CENTWAVE = [ngrid] central wavelength 
; -LINEWIDTH = [ngrid]
; -LCHISQ = [ngrid] accuracy of velocity Gaussian fit
;
; Called by FOR_INTCOMPCLOSE
;
; Written by Jim Dove, Laurel Rachmeler and Sarah Gibson
; Version 2.0 July 2014
;  Edited for clarity, fixed minor discrepancies Dec. 2016 SG
;  Added new calculations for Stokes V. May 2017 SEG
;  Converted to double; fit gaussian to del_lambda rather than wavelengths July 2017 SEG
;  Added some comments about the gaussian fitting -- March 2022 SEG
;  Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;  

;Compile_Opt defint32

       openr,lu,/get,/f77_un,'F77OUTPUT'

       ni=315 & ngrid=2500L         ; these are the arbitrary default values 
;
; (define ngrid as a long integer since it can get big)
;
;
       readu,lu,ni ; (not used here because CLE integrates along LOS)
       readu,lu,ngrid
       if verbose eq 1 then print,'number of gridpoints = ',ngrid
;
; from here on outputs are from OUTU.F
; which is called by FORCOMP.F in a loop of length NGRID (NUNWRAP)
; however the first set of material is only outputted in the
; first iteration of the NGRID loop, i.e. info about number of lines 
; and wavelengths that should not change from grid point to grid point
;
       nlines=0
       readu,lu, nlines         
;
; number of lines which have been outputted by forcomp
; this is KOUNT in OUTU.F
;
       if verbose eq 1 then print,'nlines = ',nlines
       kr=0 & lambda=1.0 & nq=0 & iq=0
       mq=100
;
; for now make mq big enough
; (later will reduce arrays to size of USELINENUM line
;
       dlambda=dblarr(mq,nlines)
       ilambda=dblarr(nlines)
;
; read lines and wavelengths calculated
;
       
       for i=0,nlines-1 do begin 
          readu,lu,kr
          readu,lu,lambda
          readu,lu,iq
          nq=[nq,iq]
          dl=fltarr(iq)
        ;  print,kr,lambda,iq,form='("Line ",i3," Lambda=",f8.2," Nlambda=",i3/"delta lambda:")'
          readu,lu,dl
         ; print,dl,form='(10f9.2)'
          dlambda[0:iq-1,i]=double(dl)
          ilambda[i]=double(lambda)
       endfor

       nq=nq[1:*]
       mq=max(nq)
       dlambda=dlambda[0:mq-1,*]
;
; see if the full profiles were written to the file
;
       iwline=0
       readu,lu,iwline
       
       stokes=dblarr(6,ngrid,nlines)
       cennumarr=intarr(ngrid,nlines)
       
       if(iwline ne 0) then begin 
          if verbose eq 1 then $
             print,'Full line profiles saved, ',$
                   strtrim(string(nq[uselinenum]),2),' wavelength positions'

          stokesl=dblarr(5,ngrid,nlines,mq)
       endif else begin 
          if verbose eq 1 then print,'Only wavelength-Integrated line data are given (iwline=0)'
       endelse
;
; read the output file from forcomp to fill up Stokes vectors
; from here on information is outputted for every step of the NGRID loop
;
       st=fltarr(5)
       if iwline ne 0 then weights=fltarr(ngrid,nlines,mq)
       
       for ii=0L,ngrid-1 do begin    ; loop over each grid point
          for kr=0,nlines-1 do begin ; loop over each line
             if(iwline ne 0) then begin ; wavelength information is given
                sta=fltarr(nq[kr])
                for im=0, 4 do begin 
                   readu,lu,sta
                   stokesl[im,ii,kr,0:nq[kr]-1]=double(sta)
                endfor
                www=fltarr(nq[kr])
                readu,lu,www
	        weights[ii,kr,0:nq[kr]-1]=double(www)
             endif
             readu,lu,st
             cennum=0
 	     readu,lu,stint,cennum
             stokes[0:4,ii,kr]=double(st)
             stokes[5,ii,kr]=stint
             cennumarr[ii,kr]=cennum-1
          endfor
       endfor
       free_lun,lu
       
       stokesout = stokes[*,*,uselinenum] 
       stokeslout=0.
       wavelengths=0.
       velocities=0.
       restwave=ilambda[uselinenum]       ;central wavelength (angstrom)
       if verbose eq 1 then print,'Rest wavelength=',restwave
;
; note this is positive first then negative
; this means that "wavelengths" will be decreasing with increasing index
;
       del_lambda=dlambda[0:nq[uselinenum]-1L,uselinenum] ;distance from line center (angstrom)
       wavelengths=del_lambda+restwave    ;wavelengths (angstrom)
       centwave=dblarr(ngrid)+restwave
       linewidth=dblarr(ngrid)
       icent=dblarr(ngrid)
       lchisq=dblarr(ngrid)
       for ii = 0L,ngrid-1 do if cennumarr[ii,uselinenum] gt 0 then centwave[ii]=wavelengths[cennumarr[ii,uselinenum]]
       centwaveuse=centwave
       if(iwline ne 0) then begin
         stokeslout=dblarr(5,ngrid,nq[uselinenum])
         stokeslout[*,*,*] = stokesl[*,*,uselinenum,0:nq[uselinenum]-1] 
         velgrid=1.*del_lambda*3.e5/restwave       ;dopper velocity (km/s)
;
; not doing this any more, not sure if it ever did what I wanted
;   	 if abs(velgrid[0]+velgrid(nq[uselinenum]-1L)) lt 1e-2 then velzero=1 
         velocities=dblarr(ngrid)
	 for ii = 0L,ngrid-1 do begin
           velocities[ii]=total(velgrid*stokeslout[0,ii,*]*weights[ii,uselinenum,0:nq[uselinenum]-1L])
	   velocities[ii]=velocities[ii]/total(stokeslout[0,ii,*]*weights[ii,uselinenum,0:nq[uselinenum]-1L])
;
; change central wavelength if shifted by velocity
;
; fit a Gaussian to I
;
	   y=dblarr(nq[uselinenum])
	   for i = 0L,nq[uselinenum]-1L do y[i]=stokeslout[0,ii,i]
           dontrun=0
	   if min(y) eq 0. and max(y) eq 0. then dontrun=1
           if dontrun eq 0 then begin
	     fit=gaussfit(del_lambda,y,a,nterms=3,sigma=sigma,chisq=chisq)
	     a[1]=a[1]+restwave
	     if (a[1] lt wavelengths[0] and a[1] gt wavelengths[nq[uselinenum]-1L]) and (a[0] gt 1d-8) then begin
	      centwaveuse[ii]=a[1]
              icent[ii]=a[0]
              linewidth[ii]=2.*sqrt(2.*alog(2))*a[2]
              lchisq[ii]=chisq
	     endif else begin
;
; warning -- in coronal holes sometimes double peak, not
; well defined.  This will eliminate the worst cases
; but others the Gaussian will try to fit with dubious results
; will plan to test for double peaks more explicitly later
;
; this will miss many double peaks, just the ones so bad that they
; are clearly outside the wavelength range
;  However, double peaks tend to make central intensity very small anyway
;
	      centwaveuse[ii]=restwave
              icent[ii]=0.
;
; note, this next line identifies all stokes info passed by CLE as null data
;   this elminates points where gaussian is shifted outside the window
;   which probably CoMP would too (check this)
;   if we make the check for double peak more stringent to include
;   double peaks within the wavelength window, it may make sense to not
;   zero out the I, Q, U -- because integral intensity in the window might be 
;   worth saving (CoMP might do it) -- but DO need to eliminate V, Vmag, linewidth
;   (unless come up with sensible fit for V and linewidth to one or both peak)
; 
; 
	      stokesout[*,ii]=-8888.
;              linewidth[ii]=wavelengths[0]-wavelengths[nq[uselinenum]-1L]
              linewidth[ii]=-8888.
;              lchisq[ii]=1.d0
              lchisq[ii]=-8888.
	      print,'no gaussian fit -- identifying point as null data'
	     endelse
;
; diagnosing double peak - commenting when not testing
;	     imx=2e-4
;	     imx=.05
; 	     if ii eq 0 then plot,wavelengths,y,yr=[0,imx] else oplot,wavelengths,y
; 	     if ii lt 10 then stop
           endif
;
	 endfor
;
; this will overwrite central intensity which was calculated by looking
; for first maximum within OUTU.F, also central wavelength
;
         stokesout[5,*]=icent
	 centwaveold=centwave
         centwave=centwaveuse
;
; fix Stokes V -- currently CLE (forcomp) outputs it as integral with sign changed
;		at rest wavelength, which is wrong for non-zero velocity
;  NOTE -- for double peak, V  profile could be calculated separately for foreground vs background
;		or a better job could be done to really get out some sort of average field
;            Vmag profile makes no sense at all
;

	case fix(vintchoice) of 
;	  "classic" -- unchanged from cle
		0:	
; 	  "classic_check" -- should be the same as classic
		1: begin
                   for ii=0L,ngrid-1 do begin    ; loop over each grid point
		    sign=-1*del_lambda/abs(del_lambda)
		    if stokesout[3,ii] ne 0. then begin
			stokesout[3,ii]=$
				total(stokeslout[3,ii,*]*sign*weights[ii,uselinenum,0:nq[uselinenum]-1L])
		        stokesout[4,ii]=$
				total(stokeslout[4,ii,*]*sign*weights[ii,uselinenum,0:nq[uselinenum]-1L])
		    endif
		   endfor
		end
; 	  "upgraded_classic" -- use shifted rather than rest wavelength
		2: begin
                   for ii=0L,ngrid-1 do begin    ; loop over each grid point
		    sign=-1*(wavelengths[*]-centwave[ii])
		    sign = fix(sign gt 0.) - fix(sign lt 0.)
		    if stokesout[3,ii] ne 0. then begin
			stokesout[3,ii]=$
				total(stokeslout[3,ii,*]*sign*weights[ii,uselinenum,0:nq[uselinenum]-1L])
		        stokesout[4,ii]=$
				total(stokeslout[4,ii,*]*sign*weights[ii,uselinenum,0:nq[uselinenum]-1L])
		    endif
		   endfor
		end
;	  "unsigned_integral" -- use sign determined from signal to the right of center
		3: begin
                   for ii=0L,ngrid-1 do begin    ; loop over each grid point
		    if stokesout[3,ii] ne 0. then begin
		        signarr = -1*stokeslout[3,ii,*]/abs(stokeslout[3,ii,*])
			testbad=where(signarr*0. ne 0.)
			if min(testbad) ne -1 then signarr[testbad]=0.
			sign = total(signarr[0,0,0:cennumarr[ii,uselinenum]])
		        sign = fix(sign gt 0.) - fix(sign lt 0.)
			stokesout[3,ii]=$
				sign*total(abs(stokeslout[3,ii,*])*weights[ii,uselinenum,0:nq[uselinenum]-1L])
		        stokesout[4,ii]=$
				sign*total(abs(stokeslout[4,ii,*])*weights[ii,uselinenum,0:nq[uselinenum]-1L])
		    endif
		   endfor
		end
;	  "peak-to-peak/eff" Vmax-Vmin, multiplied by effective line width
		4: begin
                   for ii=0L,ngrid-1 do begin    ; loop over each grid point
		    if stokesout[3,ii] ne 0. then begin
		        signarr = -1*stokeslout[3,ii,*]/abs(stokeslout[3,ii,*])
			sign = total(signarr[0,0,0:cennumarr[ii,uselinenum]])
		        sign = fix(sign gt 0.) - fix(sign lt 0.)
			eff=stokesout[0,ii]/stokesout[5,ii]
			stokesout[3,ii]=$
				sign*eff*(max(stokeslout[3,ii,*])-min(stokeslout[3,ii,*]))
		        stokesout[4,ii]=$
				sign*eff*(max(stokeslout[4,ii,*])-min(stokeslout[4,ii,*]))
		    endif
		   endfor
		end
;	  "peak-to-peak/lw" Vmax-Vmin, multiplied by gaussian line width
		5: begin
                   for ii=0L,ngrid-1 do begin    ; loop over each grid point
		    if stokesout[3,ii] ne 0. then begin
		        signarr = -1*stokeslout[3,ii,*]/abs(stokeslout[3,ii,*])
			sign = total(signarr[0,0,0:cennumarr[ii,uselinenum]])
		        sign = fix(sign gt 0.) - fix(sign lt 0.)
			stokesout[3,ii]=$
				sign*linewidth[ii]*(max(stokeslout[3,ii,*])-min(stokeslout[3,ii,*]))
		        stokesout[4,ii]=$
				sign*linewidth[ii]*(max(stokeslout[4,ii,*])-min(stokeslout[4,ii,*]))
		    endif
		   endfor
		end
	endcase
;
; check for interesting things to do with Stokes V vs Stokes I
;
	if verbose eq 1 then begin
	  goodpoints=where(velocities*0. eq 0.)
	  print,'rest wavelength:',restwave
	  print,'departure from rest of I center wavelength (from CLE - first maximum)',max(abs(centwaveold[goodpoints]-restwave))
	  print,'departure from rest of I center wavelength (from gaussian fit)',max(abs(centwave[goodpoints]-restwave))
; note -- there will be a departure for zero velocity for CLE because the cennum wavelength passed through
;  	for an array of 100 will be on one side or the other of the actual maximum.  My gauss fit should fix this.
	  vcross=centwave*0.
          for ii=0L,ngrid-1 do begin    ; loop over each grid point
           ivcross = get_interpolation_index(stokeslout[3,ii,cennumarr[ii,uselinenum]-5.>0.:cennumarr[ii,uselinenum]+5.<nq[uselinenum]-1],0.)
	   if ivcross ne 0 and max(abs(stokeslout[3,ii,*])) gt 1d-8 then begin
            vcross[ii]=interpolate(wavelengths[cennumarr[ii,uselinenum]-5.>0.:cennumarr[ii,uselinenum]+5.<nq[uselinenum]-1],ivcross)
 	   endif else vcross[ii]=restwave
	  endfor
;
; note -- this does not work perfectly - it still sometimes finds zeros at the end points
;
	  print,'departure from rest of Stokes V crossing wavelength',max(abs(vcross-restwave))
   	endif
       endif

;print,'nlines= '+strtrim(string(nlines),2)+', centwaves= '+strtrim(string(ilambda),2)+', uselinenum= '+strtrim(string(uselinenum),2)

    end
