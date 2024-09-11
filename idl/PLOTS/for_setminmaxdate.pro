pro for_setminmaxdate,instrument,date=date,typekcor=typekcor,nowidgmess=nowidgmess,ucomp=ucomp

;
; this program sets a minimum and maximum date depending on 
; telescope being sought after, and checks if the DATE being requested
; is within this range. If not, it overwrites DATE with one where there
; is data.  
;
; Called by FOR_PLOTFITS
;
; Written by Sarah Gibson 2014
; Version 2.0 July 2014
;
; 2020-07-01 added KCOR
; Sept 2021 -- passed through nowidgmess
; Jan-Feb 2024 -- updated for ucomp
;		made change date for > max the usual change not the max
;---

; 
; set default datemax to now (00:00 UT)
;

nowtime=systime(/utc)
year=strmid(nowtime,20,4)
month=strmid(nowtime,4,3)
day=strmid(nowtime,8,2)
t=strmid(nowtime,10,9)

datemax=anytim(strtrim(day,2)+'-'+strtrim(month,2)+'-'+strtrim(year,2),/ccsds)

if strupcase(instrument) eq 'AIA' then datemin=anytim('2010-05-13',/ccsds)

if strupcase(instrument) eq 'XRT' then datemin=anytim('2006-10-23',/ccsds)

if strupcase(instrument) eq 'SWAP' then datemin=anytim('2010-04-01',/ccsds)

if strpos(strupcase(instrument),'COMP') ge 0 then begin
 if keyword_set(ucomp) eq 1 then begin
  datemin=anytim('2021-01-21',/ccsds)
 endif else begin
  datemin=anytim('2011-05-04',/ccsds)
  datemax=anytim('2018-04-07',/ccsds)
 endelse
endif

if strpos(strupcase(instrument),'EUVI') ge 0  then datemin=anytim('2007-01-01T00:03',/ccsds)

if strpos(strupcase(instrument),'KCOR') ge 0  then datemin=anytim('2013-09-30T00:00',/ccsds)

if strupcase(instrument) eq 'EIT' then begin
 datemin=anytim('1997-07-01T12:00:00',/ccsds)
 datemax=anytim('2008-05-26T12:00:00',/ccsds)
endif

if exist(datemin) then begin
  if strpos(strupcase(instrument),'COMP') ge 0 then begin
   if keyword_set(ucomp) ne 1 then datechange = '2014-02-20' $
     else datechange='2022-09-21'
  endif
  if strupcase(instrument) eq 'KCOR' then begin
    datechange='2020-04-30'
    if strupcase(typekcor) eq 'CARRMAP' then datechange='2020-09-10'
  endif
  default,datechange,datemin
  default,date,datechange
  if date eq '' then date=datechange
endif else begin
 default,date,datemax
 if date eq '' then date=datemax
endelse

;
; this could be uncommented if we want to force the min/max- 
; otherwise it will just return without doing anything but
; giving an error message

if exist(datemin) then if anytim(date) lt anytim(datemin) then begin
 if keyword_set(nowidgmess) then message,/info,'date out of range, changing to: '+datechange else d=dialog(/WARNING,'date out of range, changing to: '+datechange)
 date=datechange
endif

if exist(datemax) then if anytim(date) gt anytim(datemax)  then begin
 if keyword_set(nowidgmess) then message,/info,'date out of range, changing to: '+datemax else d=dialog(/WARNING,'date out of range, changing to: '+datemax)
 date=datechange
endif

end
