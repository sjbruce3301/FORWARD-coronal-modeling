pro for_getucompline,filename,line,wlpol,header,image,uselinename=uselinename,baddata=baddata,nowidgmess=nowidgmess

;
; Extract image and header of particular line from UCoMP fits file
;
; Inputs:
;
;   FILENAME - fits file with UCoMP data
;
;    NOTE on UCOMP: all level 2 UCoMP data has the same format, though some 
;    wave regions (1074 and 1079) have polarization data and the rest dont. 
;    This means what used to be called dynamics and polarization files 
;    are combined into a file with a name like:
;
;    YYYYMMDD.HHMMSS.ucomp.WWWW.l2.fts
;
;    And quick inverts are named:
;
;    YYYYMMDD.ucomp.WWWW.l2.{synoptic,waves}.{mean,median}.fts
;
;   LINE - quantity being extracted: I, Q, U, V, L, LoI, VoI, Az, Vlos (Doppler velocity), linewidth
;               note, must be consistent with filename type (see below)
;		and also wavelength - wlpol = 1 includes polarization
;   WLPOL -- if 10747 or 10798, WLPOL=1
;
; *note, there is no TYPECOMP needed because both L2 and QUICKINVERT (L2_AVE) are the same in UCOMP
; *what matters is WLPOL
;
; Outputs:
;
;   HEADER, IMAGE
;   USELINENAME
;
; Keywords:
;
;
; QUICKINVERT and L2 WLPOL=0:
;	1) I center wavelength
;       2) Enhanced I center wavelength
;	3) I (peak intensity - gaussian fit) 
;       4) Vlos 
;       5) Line Width (FWHM)
;       6) Noise mask
;
; same for WLPOL=1 (10747, 10798, so, including polarization)-- it is the same first six, and then
;
;	7) weighted average I (e.g. 3 point summed [.5,.5,.5]
;       8) Q (summed)
;       9) U (summed)
;       10) L (summed)
;	11) Az
;       12) Radial Az
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
;  August 2023 -- updated for UCoMP/Comp rationalization 
;  January 2024 -- further ucomp updates
;	passed through wlpol
;  July 2024 -- added option for CENTI
;--

;
; added this next line to enable fits_read
;   July 2017

fits_open,filename,fcb

mdtor=!dpi/180d0

for_datecompfits,filename,dateuse,timeuse,ucomp=1

if timeuse ne '' then date=dateuse+'_'+timeuse else date=dateuse
yearuse=strmid(dateuse,0,4)

;
; this probably won't happen but just in case

if strupcase(line) eq 'AZ' then line='STOKESAZ'
if strupcase(line) eq 'LOI' then line='STOKESLOI'

CASE strupcase(line) OF
   'STOKESI':   BEGIN
; 
; for WLPOL = 1 make summed intensity default
; for consistency with Q, U 
; with central intensity in centI
; otherwise go with line center intensity in both Stokes I and centI
;
       if wlpol eq 1 then begin
          image=READFITS(filename,header,exten_no=7) 
          uselinename='Weighted Average Intensity (I)'
       endif else begin
          image=READFITS(filename,header,exten_no=1)  
          uselinename='Intensity (I)'
       endelse
       baddata=where(image*0. ne 0. or image le 1d-2 or image gt 10000. or image eq 1./0. )
   END
   'STOKESCENTI':   BEGIN
       image=READFITS(filename,header,exten_no=1)  
       uselinename='Center Intensity (I)'
       baddata=where(image*0. ne 0. or image le 1d-2 or image gt 10000. or image eq 1./0. )
   END
   'STOKESQ':   BEGIN
       if wlpol eq 1 then begin
          image=READFITS(filename,header,exten_no=8) 
          uselinename='Weighted Average Q'
       endif else begin
	   print,'you asked for Q, but '+filename+' has no polarization data'
           image=-1
;	  stop 
       endelse
       baddata=where(image*0. ne 0.)
   END
   'STOKESU':   BEGIN
       if wlpol eq 1 then begin
          image=READFITS(filename,header,exten_no=9) 
          uselinename='Weighted Average U'
       endif else begin
	   print,'you asked for U, but '+filename+' has no polarization data'
           image=-1
;	  stop 
       endelse
       baddata=where(image*0. ne 0.)
   END
   'STOKESL':    BEGIN
       if wlpol eq 1 then begin
          image=READFITS(filename,header,exten_no=10) 
          uselinename='Weighted Average L'
       endif else begin
	   print,'you asked for L, but '+filename+' has no polarization data'
           image=-1
;	  stop 
       endelse
       baddata=where(image*0. ne 0.)
   END
   'STOKESV':    BEGIN
       if keyword_set(nowidgmess) then message,/info,'NOTE! File does not have circular polarization.' else $
       d=dialog(/WARNING,'NOTE! File does not have circular polarization.')
       image=-1
       header=''
       baddata=where(image*0. ne 0.)
       uselinename='Circular Polarization (V)'
   END
   'STOKESAZ': BEGIN
       if wlpol eq 1 then begin
          image=READFITS(filename,header,exten_no=11) 
          uselinename='Weighted Average Azimuth'
       endif else begin
	   print,'you asked for Azimuth, but '+filename+' has no polarization data'
           image=-1
;	  stop 
       endelse
       baddata=where(image*0. ne 0.)
    END
    'DOPPLERVLOS':  BEGIN
      image=READFITS(filename,header,exten_no=4)  
      uselinename='Doppler Velocity'
      baddata=where(image*0. ne 0.)
     END

    'LINEWIDTH':  BEGIN
      image=READFITS(filename,header,exten_no=5)  
      uselinename='Line Width'
      baddata=where(image*0. ne 0.)
     END

ENDCASE

fits_close,fcb

end
