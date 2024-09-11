pro for_datecompfits,filename,dateuse,timeuse,ucomp=ucomp

;
; Program to establish date/time from CoMP data fits file
;
; Called by FOR_READCOMPFITS, FOR_GETCOMPLINE
;
; Written by Sarah Gibson
;
; Version 2.0 July 2014
;   February 2023 -- UCoMP update
;   August 2023 -- more UCoMP update
;-- 


ext_hdr = headfits(filename,ext=1)
hdr = headfits(filename)

dateuse = sxpar(ext_hdr,'DATE-OBS')
if keyword_set(dateuse) eq 0 then $
  dateuse = sxpar(hdr,'DATE-OBS')
if keyword_set(dateuse) eq 0  then stop

if keyword_set(ucomp) eq 0 then begin
 timeuse = sxpar(ext_hdr,'TIME-OBS')
 if keyword_set(timeuse) eq 0 then  $
  timeuse = sxpar(hdr,'TIME-OBS')
 if keyword_set(timeuse) eq 0 then begin
   timeuse_hour = strmid(sxpar(ext_hdr,'FLATFILE'),9,2,/reverse) 
   timeuse_min = strmid(sxpar(ext_hdr,'FLATFILE'),7,2,/reverse)
   timeuse_sec = strmid(sxpar(ext_hdr,'FLATFILE'),5,2,/reverse) 
;
; add 10 hours to UT 
; by inspection, none of the early data where this happens
; have times past 14 UT where this would be a problem
;
   timeuse_hour=strtrim(string(timeuse_hour+10),2)
   timeuse = timeuse_hour+':'+timeuse_min+':'+timeuse_sec
 endif
endif else timeuse=''

end
