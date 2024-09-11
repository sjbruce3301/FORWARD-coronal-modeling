function for_kcorextavgtime, year, month, day, typekcor, found=found

;+
; Find the time for the KCor extavg file time, as a string, e.g., '191715'.
;
; Inputs:
;
;   year : in, required, type=string
;     year of queried date 
;   month : in, required, type=string
;     month of queried date 
;   day : in, required, type=string
;     day of queried date 
;	**NOTE -- Assumes MLSO observing day in HST
;		but returns time in UT
;		also checks to see if firstl1 or average image 
;		is early next UT day, in which case overwrites year,month,day
;
;  typekcor -- EXTAVG or FIRSTL1
;		if FIRSTL1 will return found=2
;
; :Keywords:
;   found : out, optional, type=boolean
;     set to a named variable to retrieve whether a time was found
;
; :Returns:
;   string of time (six characteric)
;
; CALLED BY FOR_DIRECTDOWNLOAD
;
; CALLS FOR_MG_URL_DIRLISTING
;
;
; Written by Mike Galloy, May 2020
;
; August 2021 -- updated to grab first L1 image if there is no EXTAVG
;			and to identify level
;			(happens for early data -- will return found=2)
;			and also to overwrite year, month, day if the first
;			or EXTAVG image occurs on next calendar date
;-

  compile_opt strictarr

  found = 0.

  url = string(year, month, day, format='(%"http://download.hao.ucar.edu/%s/%s/%s/")')

  listing = for_mg_url_dirlisting(url)
  if listing eq !null then return,!null

  n_extavg = 0L

  if strupcase(typekcor) eq 'EXTAVG' then $
   extavg_indices = where(strmatch(listing.name, '*_extavg_cropped.gif'), $
                         n_extavg, /null)
  
  if (n_extavg eq 0L) then begin
;
; FIRSTL1 (either because requested, or because EXTAVG doesn't exist)
;  however - I ran into problems where sometimes there were gifs
;  but no fits -- so I left in option of using the nth image 
;  not actual first  - but reprocessing should make this unnecessary
;
   nimage=0
   found = 1.
   print,'LEVEL 1'
   times = strmid((listing.name)[nimage], 9, 6)
   daycheck = strmid((listing.name)[nimage], 6, 2)
   monthcheck = strmid((listing.name)[nimage], 4, 2)
   yearcheck = strmid((listing.name)[nimage], 0, 4)
  endif else begin
;
; now for EXTAVG -- need to figure out if L1.5 or L2
;
   level=strmid((listing.name)[extavg_indices],22,3)
   case 1 of 
       level[0] eq '1.5': found = 1.5
       level[0] eq '2_e': found = 2.
       else: stop
   endcase
;
   print,'LEVEL ',found
;
   times = strmid((listing.name)[extavg_indices], 9, 6)
   daycheck = strmid((listing.name)[extavg_indices], 6, 2)
   monthcheck = strmid((listing.name)[extavg_indices], 4, 2)
   yearcheck = strmid((listing.name)[extavg_indices], 0, 4)
  endelse
  if daycheck[0] ne day then begin
    print,'WARNING -- image is actually on next UT calendar day
    print,'entered date: ', year,' ',month,' ',day
    print,'first available for that Hawaii observing day: ',yearcheck,' ',monthcheck,' ',daycheck
    day=daycheck[0]
    month=monthcheck[0]
    year=yearcheck[0]
  endif

  return, times[0]

end

