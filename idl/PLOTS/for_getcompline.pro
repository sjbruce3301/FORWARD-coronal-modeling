pro for_getcompline,filename,line,header,image,uselinename=uselinename,oldcomp=oldcomp,typecomp=typecomp,baddata=baddata,nowidgmess=nowidgmess

;
; Extract image and header of particular line from CoMP fits file
;   taking into account what type of file it is (QUICKINVERT, POLARIZATION, DYNAMICS)
;
; Inputs:
;
;   FILENAME - fits file with CoMP data
;
;   LINE - quantity being extracted: I, Q, U, V, L, LoI, VoI, Az, Vlos (Doppler velocity), linewidth
;               note, must be consistent with filename type (see below)
;
; Outputs:
;
;   HEADER, IMAGE
;   USELINENAME
;
; Keywords:
;
;   TYPECOMP: type of file, QUICKINVERT, POLARIZATION, DYNAMICS - this can also
;       be determined from filename. Must be established or program stops.
;
;   OLDCOMP: allows access of data in old (2005) CoMP quickinvert files.
;               keyword, or can name the file to include it and be noted.
;

;
; QUICKINVERT: OLDCOMP
;	1) I, 2) Q, 3) U, 4) V, 5) LoI, 6) Az
;
; QUICKINVERT: RECENT/NEW
;	1) I (at center wavewength), 2) Q (center), 3) U (center), 4) L (center), 
; ** note new UCoMP will give these all summed; FOR_READCOMPFITS will check to see 
;	if CoMP is reprocessed to match and in that case, FOR_GETUCOMPLINE will be run instead of this **
;	5) Az, 6) (newer files) Doppler Vlos, 
;	7) (newer files) linewidth, 8) (newer files) radial azimuth; 
;	9) (rarely for analytics) uncorrected Doppler Vlos
;
; QUICKINVERT: CORMAG
;	1) I, 2) Q, 3) U, 4) L, 5) Az, 6) I_KCOR, 7) L_KCOR
;
; DYNAMICS:
;	1) I (peak intensity (gaussian fit)), 2) Enhanced I (center), 3) Vlos, 4) Line Width
;
; POLARIZATION:
;	1) summed I  [.3,.7,.3], 2) Enhanced I (summed), 3) summed Q, 4) summed U, 5) summed L
;	6) Azimuth, 7) Radial Azimuth
; * this is a change-- I used to be peak
;
;NOTE THERE HAVE BEEN CHANGES OVER TIME in order for Quickinvert
;	I have replaced reading by extension number with reading by extension name to deal with this
;
; Called by FOR_READCOMPFITS 
;
; Calls FOR_DATECOMPFITS
;	PB0R
;
; Written by Sarah Gibson 2014
; 
; Version 2.0 July 2014
;
;	July 2017: changed readfits to fits_read in order to read by extension
;		name, not number
;
;	October 2017: fixed bug where pB0r was passed an undefined variable
;	September 2021: added check on LOI as well as AZ
;
;   December 2021 -- commented out stops because
;     early data may have Q, U not in extensions
;    also generalized QUICKINVERT so it can contain info about type of fits file
;   January 2022-- set image etc. = -1 so wouldn't crash since no stops
;  March 2023 -- put in check for 0 in denominator for atan
;  August 2023 -- updated for UCoMP/Comp rationalization 
;--

;
; added this next line to enable fits_read
;   July 2017

fits_open,filename,fcb

mdtor=!dpi/180d0
type=strupcase(typecomp)

for_datecompfits,filename,dateuse,timeuse,ucomp=0

if timeuse ne '' then date=dateuse+'_'+timeuse else date=dateuse
yearuse=strmid(dateuse,0,4)

;
; this probably won't happen but just in case

if strupcase(line) eq 'AZ' then line='STOKESAZ'
if strupcase(line) eq 'LOI' then line='STOKESLOI'

CASE strupcase(line) OF
   'STOKESI':   BEGIN
       image=READFITS(filename,header,exten_no=1)
       baddata=where(image*0. ne 0. or image le 1d-2 or image gt 10000. or image eq 1./0. )
       uselinename='Intensity (I)'
   END
   'STOKESQ':   BEGIN
;
; informational note-- this assumes P angle polarization rotation is accounted for in Q,U
;   CURRENTLY THIS IS WRONG- will be fixed in UCOMP reprocessing
;   Not a problem for FORWARD though, because for_plotfits calls 
;   for_azrot.pro which only uses Azimuth in establishing Q, U to plot

       if strpos(strupcase(type),'QUICKINVERT') ge 0 then begin
;        image=READFITS(filename,header,exten_no=2)
	 fits_read,fcb,image,header,extname='Q',message=message,/no_abort
; this should not happen, but in case
	 if message ne '' then begin
	  print,'extension name in CoMP file is invalid'
	  print,'you asked for Q, but these are the options:'
	  print,fcb.extname
          image=-1
;	  stop 
	 endif
       endif 
       if strupcase(type) eq 'POLARIZATION' then begin
        image=READFITS(filename,header,exten_no=3)
       endif 
       if strupcase(type) eq 'DYNAMICS' then begin
         if keyword_set(nowidgmess) then message,/info,'NOTE! Dynamics type file does not have linear polarization.  Download polarization or quickinvert file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.' else $
         d=dialog(/WARNING,'NOTE! Dynamics type file does not have linear polarization. Download polarization or quickinvert file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.')
         image=-1
         header=''
       endif
       baddata=where(image*0. ne 0.)
       uselinename='Linear Polarization (Q)'
       if strupcase(type) eq 'POLARIZATION' then uselinename='Summed Linear Polarization (Q)'
   END
   'STOKESU':   BEGIN
       if strpos(strupcase(type),'QUICKINVERT') ge 0 then begin
;        image=READFITS(filename,header,exten_no=3)
	fits_read,fcb,image,header,extname='U',message=message,/no_abort
; this should not happen, but in case
	 if message ne '' then begin
	  print,'extension name in CoMP file is invalid'
	  print,'you asked for U, but these are the options:'
	  print,fcb.extname
          image=-1
;	  stop 
	 endif
       endif 
       if strupcase(type) eq 'POLARIZATION' then begin
        image=READFITS(filename,header,exten_no=4)
       endif 
       if strupcase(type) eq 'DYNAMICS' then begin
         if keyword_set(nowidgmess) then message,/info,'NOTE! Dynamics type file does not have linear polarization.  Download polarization or quickinvert file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.' else $
         d=dialog(/WARNING,'NOTE! Dynamics type file does not have linear polarization. Download polarization or quickinvert file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.')
         image=-1
         header=''
       endif
       baddata=where(image*0. ne 0.)
       uselinename='Linear Polarization (U)'
       if strupcase(type) eq 'POLARIZATION' then uselinename='Summed Linear Polarization (U)'
   END

   'STOKESL':    BEGIN
        if keyword_set(oldcomp) then begin
         i=READFITS(filename,header,exten_no=1)
         loi=READFITS(filename,header,exten_no=5)
         image=loi*i
        endif else begin
         if strpos(strupcase(type),'QUICKINVERT') ge 0  then begin
;           image=READFITS(filename,header,exten_no=4)
	   fits_read,fcb,image,header,extname='Linear Polarization',message=message,/no_abort
	   if message ne '' then begin
 		print,'something is fishy about reading CoMP linear polarization'
	        print,'you asked for Linear Polarization, but these are the options:'
	        print,fcb.extname
		image=-1
;		stop
           endif
          endif 
          if strupcase(type) eq 'POLARIZATION' then begin
           image=READFITS(filename,header,exten_no=5)
          endif
          if strupcase(type) eq 'DYNAMICS' then begin
            if keyword_set(nowidgmess) then message,/info,'NOTE! Dynamics type file does not have linear polarization.  Download polarization or quickinvert file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.' else $
            d=dialog(/WARNING,'NOTE! Dynamics type file does not have linear polarization. Download polarization or quickinvert file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.')
           image=-1
           header=''
          endif
        endelse
        baddata=where(image*0. ne 0. or image le 0.)
        uselinename='Linear Polarization (L)'
       if strupcase(type) eq 'POLARIZATION' then uselinename='Summed Linear Polarization (L)'
   END

   'STOKESLOI':  BEGIN
; LOI will only be sent to this code for oldcomp
        if keyword_set(oldcomp) then begin
         image=READFITS(filename,header,exten_no=5)
         baddata=where(image le 0. or image*0. ne 0. or image gt 1.)
         uselinename='Linear Polarization Fraction (L/I)'
        endif else begin
         print,'this should not happen except for oldcomp'
         stop
        endelse
   END

   'STOKESV':    BEGIN
       if keyword_set(oldcomp) then begin
         image=READFITS(filename,header,exten_no=4)
       endif else begin
         if keyword_set(nowidgmess) then message,/info,'NOTE! File does not have circular polarization.' else $
         d=dialog(/WARNING,'NOTE! File does not have circular polarization.')
         image=-1
         header=''
       endelse
       baddata=where(image*0. ne 0.)
       uselinename='Circular Polarization (V)'
   END

   'STOKESAZ': BEGIN
        if keyword_set(oldcomp) then begin
          azimuth=READFITS(filename,header,exten_no=6)
          p_angle=-25.59
          image=azimuth-p_angle+45.
;
; this may not appropriately deal with lt 0 and gt 180
; but not changing for historic reasons
;
        endif else begin
         if strpos(strupcase(type),'QUICKINVERT') ge 0 then begin
;
; note-- this assumes P angle polarization rotation is accounted for in Azimuth
;  	currently this is CORRECT
; 
;          azimuth=READFITS(filename,header,exten_no=5)
	  fits_read,fcb,azimuth,header,extname='Azimuth',message=message,/no_abort
; backward compatibility
;  as above, replacing with a stop because should not happen
;
;          if max(azimuth) lt 0 then azimuth=READFITS(filename,header,exten_no=3)
	  if message ne '' then begin
 		print,'something is fishy about reading CoMP Azimuth'
	        print,'you asked for Azimuth, but these are the options:'
	        print,fcb.extname
;		stop
		azimuth=-1
          endif
          image=azimuth
         endif
         if strupcase(type) eq 'POLARIZATION' then begin
           Q=READFITS(filename,header,exten_no=3)
           U=READFITS(filename,header,exten_no=4)
	   alpha = 0.5*atan(u,q)
           test=where(q eq 0. and u eq 0.) 
           if min(test) ne -1 then alpha[test] =sqrt(-1.)
           image = alpha/mdtor
;           image = (alpha+!dpi/2.)/mdtor
; we used the line above for a while, but then there was a change
; so for a while i used the same formula Steve uses for Quickinvert
; (lines below)
; which assumes P angle polarization rotation is NOT accounted for in Polarization Q, U
; but this is also no longer correct.
;           pangle=pb0r(date)
;           pangle=pangle[0]
;           image=(alpha+!dpi/4.)/mdtor-pangle 
;
; for consistency with other files, need to get between 0 and 180
;
           image=image mod 180.
           bad=where(image lt 0.,count)
           if count gt 0 then image[bad]=image[bad]+180. 
         endif
         if strupcase(type) eq 'DYNAMICS' then begin
           if keyword_set(nowidgmess) then message,/info,'NOTE! Dynamics type file does not have linear polarization.  Download polarization or quickinvert file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.' else $
           d=dialog(/WARNING,'NOTE! Dynamics type file does not have linear polarization. Download polarization or quickinvert file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.')
           image=-1
           header=''
         endif
        endelse
        baddata=where(image*0. ne 0.)
        uselinename='Azimuth'
    END
    
    'DOPPLERVLOS':  BEGIN
       if strpos(strupcase(type),'QUICKINVERT') ge 0 then begin
;        image=readfits(filename,header,exten_no=6)
	fits_read,fcb,image,header,extname='Doppler Velocity',message=message,/no_abort
;        if max(image) le 0. then begin
;
; image = -1 was response to fail of readfits
; fits_read is a little different, so, using message to indicate fail
;
	if message ne '' then begin
         if keyword_set(nowidgmess) then message,/info,'No Doppler velocity in '+strtrim(filename,2)+'.  Download dynamics file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.' else $
         d=dialog(/WARNING,'No Doppler velocity in '+strtrim(filename,2)+'.  Download dynamics file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.')
         image=-1
         header=''
        endif
       endif
       if type eq 'DYNAMICS' then begin
        image=readfits(filename,header,exten_no=3)
       endif
       if strupcase(type) eq 'POLARIZATION' then begin
         if keyword_set(nowidgmess) then message,/info,'NOTE! Polarization type file does not have Doppler velocity.  Download dynamics file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.' else $
         d=dialog(/WARNING,'NOTE! Polarization type file does not have Doppler velocity.  Download dynamics file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.')
         image=-1
         header=''
       endif
       baddata=where(image*0. ne 0.)
       if strpos(strupcase(type),'QUICKINVERT') lt 0 then uselinename='Doppler Velocity' else uselinename='Doppler Velocity (partly corrected)'
     END

    'LINEWIDTH':  BEGIN
       if strpos(strupcase(type),'QUICKINVERT') ge 0  then begin
;        image=readfits(filename,header,exten_no=7)
	fits_read,fcb,image,header,extname='Line Width',message=message,/no_abort
	if message ne '' then begin
         if keyword_set(nowidgmess) then message,/info,'No Line Width in'+strtrim(filename,2)+'.  Download dynamics file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.' else $
         d=dialog(/WARNING,'No Line Width in '+strtrim(filename,2)+'.  Download dynamics file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.')
         image=-1
         header=''
        endif
       endif
       if type eq 'DYNAMICS' then begin
        image=readfits(filename,header,exten_no=4)
       endif
       if strupcase(type) eq 'POLARIZATION' then begin
         if keyword_set(nowidgmess) then message,/info,'NOTE! Polarization type file does not have line width.  Download quickinvert or dynamics file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.' else $
         d=dialog(/WARNING,'NOTE! Polarization type file does not have line width.  Download quickinvert or dynamics file from e.g. http://mlso.hao.ucar.edu/mlso_data_COMP_'+yearuse+'.html.')
         image=-1
         header=''
       endif
       baddata=where(image*0. ne 0.)
       uselinename='Line Width'
     END

    'STOKESI_KCOR':  BEGIN
       image=readfits(filename,header,exten_no=6)
       baddata=where(image*0. ne 0.)
       uselinename='CORMAG K Corona Brightness'
     END
    'STOKESL_KCOR':  BEGIN
       image=readfits(filename,header,exten_no=7)
       baddata=where(image*0. ne 0.)
       uselinename='CORMAG K Corona Polarized Brightness'
     END

ENDCASE

fits_close,fcb

end
