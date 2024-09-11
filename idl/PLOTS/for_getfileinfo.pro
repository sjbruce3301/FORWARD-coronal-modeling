pro for_getfileinfo,header,image,date=date,instrument=instrument,line=line,width=width,height=height,xcen=xcen,ycen=ycen,cmer=cmer,B0=B0,useid=useid,name=name,rsuninpix=rsuninpix,bunit=bunit,nadd=nadd,pixelsize=pixelsize,dunmult=dunmult,gridtype=gridtype,filename=filename

;
; Program to dal with all the differences in FITS headers 
; in extracting information for FORWARD maps
;
;
; this has been tested for AIA, EIT, and EUVI, XRT, SWAP, also COMP/UCOMP
;
; Called by FOR_PLOTFITS
;
; Written by Sarah Gibson 2014
;
; Version 2.0 July 2014
;--
; Feb 2018 -- added keyword nadd
; 2019 -- updated options for rsun for kcor
; 2020- 2021- updated KCOR Carrington map; passed through rheight
;		updated label on Bunit for KCOR (including Level)
;		made sure width/height were integer not long
; Aug 2023 - Jan 2024 UCoMP updates
; Feb 2024 -- removed stop, changed ucomp date to 1-21-21
; Jun 2024 -- updated KCor units description
; Jul 2024 -- removed keyword STOKESSTRUCT because not used

default,nadd,''

if keyword_set(filename) then if keyword_set(instrument) eq 0 then instrument = ''

dsize=size(image)
headertype=size(header,/type)

;
; special case KCOR Carrington maps
;  (need to generalize this if we do more than KCOR)
;

if strupcase(gridtype) eq 'CARRMAP' then begin
;
; replace these once header is updated
;
 instrument='KCOR'
 Bunit='Bsun_mean'
;
 if exist(date) then dateold = date
 date=anytim(anytim(header.date_d$end),/ccsds) 
 default,dateold,date
 if anytim(date,/ccsds) ne anytim(dateold,/ccsds) then begin
  print,'old date',dateold
  print,'date in fits file',date
 endif
;
; pixel size in degrees longitude
 height=fix(header.naxis1)
 width=fix(header.naxis2)
 pixelsize=360./height
 line='pB'
 spacecraft = 'Earth'
 coords = pb0r(date, /arcsec, /earth)
 rsuninarc=coords[2]
 cmer=coords[0]
 B0=coords[1]
 dunmult=1.d0
;
; I don't think xcen,ycen will be used and will be different
; for East/West anyway
; ditto for rsuninpix
; 
endif else begin

 if headertype eq 8 then begin
  if tag_exist(header,'instrume') then instrument=strcompress(header.instrume,/remove_all) else $
  instrument=strcompress(header.object,/remove_all) 
 endif else begin
  instrument=strcompress(FXPAR(header,'instrume'),/remove_all)
 endelse

 if instrument eq 'Solar K-Corona' then instrument='KCOR'
 if instrument eq 'SolarK-Corona' then instrument='KCOR'
 if instrument eq 'COSMOK-Coronagraph' then instrument='KCOR'
 if strupcase(instrument) eq 'DALSA' then instrument='MK4'

 chop = strpos(instrument,'_')
 if chop ne -1 then instrument=strmid(instrument,0,chop)

 if headertype eq 8 then date=header.date_obs else date=FXPAR(header,'date-obs')

;
; pixel size in arcseconds
;
 if headertype eq 8 then pixelsize=header.cdelt1 else pixelsize=FXPAR(header,'cdelt1')
 if pixelsize eq 0 then begin
  if headertype eq 8 then pixelsize=1./header.arcpix else pixelsize=1./FXPAR(header,'arcpix')
 endif

;
; exposure time
; whether this is needed depends on data units (below)
;

 if strupcase(instrument) eq 'AIA' or strupcase(instrument) eq 'EIT' or strupcase(instrument) eq 'XRT' then if headertype eq 8 then exptime=header.exptime else exptime=FXPAR(header,'exptime')

;
; data units: I want these in units dN/s/detpix (detector pixel size)
;
; AIA - PIXLUNIT - 'DN/pixel'
;	I will assume AIA data are not binned, so pixel=detector pixel
;	if normalized by exposure time, this will be reflected in
;	keyword EXPTIME so dividing by this keyword works no matter what
; EUVI - BUNIT - 'dN/s/CCDPIX'
;	nothing needs to be done -- already in the units I want
; EIT - BUNIT - counts / pixel 
;	needs to be normalized by exposure time
;	I will assume pixel is binned pixel, so need to multiply
;	by binning factor, determined from ratio of pixelsize and
;	(known) detector pixel size
; SWAP - BUNIT - 'dN/s/pixel'
;	I will assume pixel is detector pixel so nothing needs to be done
; XRT - unit = dN/pixel where pixel is the binned pixel
;	so -- divide by binning factor squared to get dN/detpix
;	if normalized by exposure time, it is NOT reflected in exposure time
;	keyword so you need to search comments (as below)
;
; KCOR - units - depends on level. 
;	**NOTE UNITS OF SUN DISK AVERAGE BRIGHTNESS, NOT SUN CENTRAL LIKE FORWARD MODEL**
;   The old version is level 1 (filenames contain 'l1'. 
;   The new version is level 1.5 and the filenames (and header) reflect that change (contain 'l1.5'.
; The level 1 calibrated data were stored as 16 bit integers in intensity units of 1e-6 Bsun_mean. 
; The 'final' calibration [level 1.5] are stored as floats in units of Bsun_mean; 
;   i.e. the data are 1e-06 smaller than before. 
; Typical low corona pB values range from 1e-06 down to 1.e-09.
;  **
;
 
 dunmult=1.d0
 if strupcase(instrument) eq 'AIA' or strupcase(instrument) eq 'EIT' then dunmult=dunmult/exptime 

 if strupcase(instrument) eq 'XRT' then begin

;
; divide by exposure time if necessary to get dN/detpix/s
;
  if total(strmatch(header.history, '*XRT_RENORMALIZE*')) eq 0 then dunmult = dunmult/exptime

;
; divide by binning factor to get dN/detpix
;

  dunmult=dunmult/header.chip_sum/header.chip_sum

 endif 

 if strupcase(instrument) eq 'EIT' then begin

;
; divide by binning factor to get dN/s/detpix
;

  eit_detpix=2.62900
  binmult=eit_detpix/pixelsize
  dunmult=dunmult/binmult/binmult

 endif

 if strpos(strupcase(instrument),'SECCHI') ge 0 then begin
     spacecraft=header.OBSRVTRY
     if strpos(strupcase(spacecraft),'B') gt 0 then instrument=strtrim(header.DETECTOR,2)+'B' else instrument=strtrim(header.DETECTOR,2)+'A'

;
; Rsun in pixels
;
     rsuninarc=header.rsun
     rsuninpix=rsuninarc/pixelsize
 endif 

 if strpos(strupcase(instrument),'XRT') ge 0 then line='0' else if strpos(strupcase(instrument),'KCOR') ge 0 or strpos(strupcase(instrument),'MK') ge 0 then line='pB' else begin
  if headertype eq 8 then line=strcompress(string(header.wavelnth),/remove_all) else line=strcompress(string(FXPAR(header,'wavelnth')),/remove_all)
  chop = strpos(line,'_')
  if chop ne -1 then line=strmid(line,0,chop)
 endelse

 if headertype eq 8 then width=fix(header.naxis1) else width=fix(FXPAR(header,'naxis1'))
 if headertype eq 8 then height=fix(header.naxis2) else height=fix(FXPAR(header,'naxis2'))
 if fix(dsize[1]) ne width then stop

 if strupcase(instrument) ne 'EIT' then begin
   if strupcase(instrument) eq 'XRT' or strupcase(instrument) eq 'SWAP' then begin
       spacecraft = 'Earth'
       coords = pb0r(date, /arcsec, /earth)
       rsuninarc=coords[2]
       cmer=coords[0]
       B0=coords[1]
   endif else begin
      if headertype eq 8 then begin
         if tag_exist(header,'crln_obs') then cmer=header.crln_obs  else cmer=header.solar_l0
         if tag_exist(header,'crlt_obs') then B0=header.crlt_obs  else B0=header.solar_b0
      endif else begin
         cmer=FXPAR(header,'crln-obs')
         B0=FXPAR(header,'crlt-obs')
      endelse
   endelse
;
; xcen, ycen units of arcseconds
;
   if strupcase(instrument) ne 'SWAP' and strupcase(instrument) ne 'KCOR' and strpos(strupcase(instrument),'MK') lt 0 then begin
    if headertype eq 8 then XCen=header.xcen else ycen=FXPAR(header,'ycen')
    if headertype eq 8 then YCen=header.ycen else xcen=FXPAR(header,'xcen')
   endif 
 endif else begin
    cmer=tim2carr(date)
    B0=header.solar_B0
    rsuninpix=header.solar_r
 endelse

 if strupcase(instrument) eq 'EIT' or strupcase(instrument) eq 'SWAP' then begin
    xcenpix=header.crpix1
    ycenpix=header.crpix2
;
; lower left corner pixel is (1,1)
;
    xcen=pixelsize*((width+1.)/2.-xcenpix)
    ycen=pixelsize*((height+1.)/2.-ycenpix)
 endif

 if strupcase(instrument) eq 'KCOR' or strpos(strupcase(instrument),'MK') ge 0 then begin
    xcen=header.crval1
    ycen=header.crval2
 endif

 if strupcase(instrument) eq 'AIA' then rsuninpix=header.r_sun 
;
;   if strupcase(instrument) eq 'AIA' then rsuninpix=header.rsun_obs/pixelsize 
; these should be the same if calibrated
;
;   
 if headertype eq 8 then $
  if tag_exist(header,'rsunpix') then rsuninpix=header.rsunpix 
 if headertype ne 8 then $
    if FXPAR(header,'rsunpix') ne 0 then rsuninpix=FXPAR(header,'rsunpix')

 if exist(rsuninpix) eq 0 then begin
   if headertype eq 8 then $
     if tag_exist(header,'rsun') then rsuninarc=header.rsun
   if headertype ne 8 then $
    if FXPAR(header,'rsun') ne 0 then rsuninarc=FXPAR(header,'rsun')
    if exist(rsuninarc) then rsuninpix=rsuninarc/pixelsize else stop
 endif

 if headertype eq 8 then $
  if tag_exist(header,'bunit') then Bunit=strcompress(header.bunit,/remove_all)
 if headertype eq 7 then $
  if exist(FXPAR(header,'bunit')) then Bunit=strcompress(FXPAR(header,'bunit'),/remove_all)
 default,Bunit,'DN/s/pix'

 if strupcase(Bunit) eq 'DN/S/PIX' then Bunit='DN/S/DETPIX' else print,'Bunit= ',Bunit

 if strupcase(instrument) eq 'KCOR' or strpos(strupcase(instrument),'MK') ge 0 then begin
  if strupcase(instrument) eq 'KCOR' then begin
;
; update KCOR - changes to naming of rsun, also units
;   
   if tag_exist(header,'r_sun') then rsuninpix=header.r_sun else begin
    rsuninpix=header.rsun/pixelsize
   endelse
   Bunit='Bsun_mean'
   if header.level eq 'L1' then Bunit='1d-6 Bsun_mean Level 1'
   if header.level eq 'L1.5' then Bunit='Bsun_mean Level 1.5'
  endif else Bunit='Bsun_mean'
 endif
;
; for now will replace MK* with KCOR -- need to fix throughout code
; later
;

 if strpos(strupcase(instrument),'MK') ge 0 then instrument='KCOR'
endelse

if strpos(strupcase(instrument),'COMP') ge 0 then begin
 datecheck=anytim(date,/ccsds)
 dateucomp=anytim('2021-01-21',/ccsds)
 if datecheck lt dateucomp then begin
  instrumentuse=instrument
 endif else begin
  instrumentuse='UCOMP'
 endelse
endif else instrumentuse=instrument

useid=instrumentuse+'_'+line+'_'+date
useid=str_replace(useid,' ','_')
useid='DATA:'+useid
if nadd ne '' then useid=useid+'_'+nadd
name=instrumentuse+line
name=name+nadd
;print,name
;print,useid

end

