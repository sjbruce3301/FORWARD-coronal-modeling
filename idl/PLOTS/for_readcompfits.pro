pro for_readcompfits,filename,line,headernew,image,badfile=badfile,uselinename=uselinename,typecomp=typecomp,cgsmult=cgsmult,oldcomp=oldcomp,ucomp=ucomp,StokesStruct=StokesStruct,fileout=fileout,occult=occult,upoccult=upoccult,wlcomp=wlcomp,nowidgmess=nowidgmess

;
; extract header and image from CoMP filename for given line 
;

; Inputs:
;
;   FILENAME - fits file with CoMP data
;
;   LINE - quantity being extracted: I, Q, U, V, L, LoI, VoI, Az, Doppler (los velocity),Linewidth
;		note, must be consistent with filename type (see below)
;
; Outputs:
;
;   HEADER, IMAGE
;   BADFILE  - if line not found
;   USELINENAME
;
;   FILEOUT: output fits file with header and image specific to LINE
;
;   STOKESSTRUCT: structure with I, P (a.k.a. L) and az (radial Azimuth)
;	used for making stokes lines on plots
;	**NOTE THAT somewhat different quantities (but equivalent) are stored for model, see FOR_INTCOMPCLOSE)
;
;
; Keywords:
;
;   TYPECOMP: type of file, QUICKINVERT, POLARIZATION, DYNAMICS - this can also
;	be determined from filename. Must be established or program stops.
;           *note, UCoMP data will have combined dynamics/polarization typecomp=L1
;           * and will also vary depending on WLCOMP-- only 10747/10798 have polarization
;
;   CGSMULT: convert data from PPM to ERG/CM2/S/SR
;
;   OLDCOMP: allows access of data in old (2005) CoMP quickinvert files.
;		keyword, or can name the file to include it and be noted.
;
;   UCOMP: 
; 	if set to 1, files can be accessed using the same syntax etc as in UCOMP
;	i.e., by callling FOR_GETUCOMPLINE instead of FOR_GETCOMPLINE
;
;	but UCOMP=0 will force a call of the legacy program, FOR_GETCOMPLINE
;	and so should work with old CoMP files people may have
;
;	default 0 has been set in FOR_PLOTFITS
;        but will be changed below to 1 
;         if the filename/pipeline software tag "VERSION" warrants it; 
;	   (or maybe it will be in the name?)
;
;   WLCOMP: CoMP wavelengths, 10747 and 10798
;		CORMAG 5303
;
;  Calls:  FOR_GETCOMPLINE, FOR_GETUCOMPLINE, FOR_DATECOMPFITS, FOR_SUN_FLUX
;
; Called by FOR_PLOTFITS
;
; Written by Sarah Gibson 2014
; Version 2.0 July 2014
;
; Sept 2021 - 	expanded conditional test for STOKESQOI/UOI etc
; Jan 2022 -- added badfile designation if line not found
;		updated pixel arcsec default 4.5 --> 4.35 and put in warning if RSUN not found
; Feb 2023 -- UCoMP update
; Aug 2023 -- UCoMP more update; also update polarization files interpretation for CoMP
;		also commented out bad data from negative intensity
; Jan 2024 -- UCoMP updates
; Feb 2024 -- got rid of upoccult default set because was causing problems when going between CoMP and UCoMP
; May 2024 -- added test for occult as number
; July 2024 -- added comments about STokesStruct
;		and clarified treatment of StokesI vs StokescentI
;---

if strpos(strupcase(file_basename(filename)),'OLDCOMP') gt 0 then oldcomp=1

;
; first figure out if it is UCoMP


if strpos(strupcase(file_basename(filename)),'UCOMP') gt 0 then begin
 ucomp=1
;
; NOTE this is where it would be good also add this tag
; if it is a reprocessed COMP file
endif

;
; now check to see what type of file it is: 
; 	polarization, dynamics, or quickinvert
;  IF COMP
;

if ucomp eq 0 then begin
 case 1 of 
  strpos(strupcase(file_basename(filename)),'QUICKINVERT') ge 0:  typecomp='quickinvert'
  strpos(strupcase(file_basename(filename)),'QUICK_INVERT') ge 0:  typecomp='quickinvert'
  strpos(strupcase(file_basename(filename)),'POLARIZATION') ge 0: typecomp='polarization'
  strpos(strupcase(file_basename(filename)),'DYNAMICS') ge 0: typecomp='dynamics'
  strpos(strupcase(file_basename(filename)),'QUICKINVERT') lt 0 and strpos(strupcase(file_basename(filename)),'QUICK_INVERT') lt 0 and strpos(strupcase(file_basename(filename)),'POLARIZATION') lt 0 and strpos(strupcase(file_basename(filename)),'DYNAMICS') lt 0: begin 
    if keyword_set(nowidgmess) then message,"file type must be indicated via keyword TYPE or as part of file name; valid options are 'quickinvert', 'polarization', and 'dynamics.'" else $
    d=dialog(/WARNING,"file type must be indicated via keyword TYPE or as part of file name; valid options are 'quickinvert', 'polarization', and 'dynamics.'")
   end
 endcase
endif else begin
;
; ucomp only has two types of Level 2 data--  a quickinvert type
; which is averaged, and then time specific data 
; Both have the equivalent of the CoMP dynamics data in them
; and then if 1074 or 1079 also include the polarization data
; (the latter will have 12 extensions; the former 6)
; see for_getucompline.pro
;
 case 1 of
   strpos(strupcase(file_basename(filename)),'QUICKINVERT') ge 0: typecomp='quickinvert'
; if we are looking at original filename
   strpos(strupcase(file_basename(filename)),'UCOMP') eq 9: typecomp='quickinvert'
   strpos(strupcase(file_basename(filename)),'UCOMP') eq 16: typecomp='l2'
   strpos(strupcase(file_basename(filename)),'L2') ne -1: typecomp='l2'
   strpos(strupcase(file_basename(filename)),'QUICKINVERT') ne -1: typecomp='quickinvert'
   else : begin
    print,'Unknown UCoMP file type'
    stop
   end
 end 
endelse


;
; now figure out what wavelength
;

if strpos(strupcase(file_basename(filename)),'637') ge 0 then wlcomp=6374
if strpos(strupcase(file_basename(filename)),'706') ge 0 then wlcomp=7062
if strpos(strupcase(file_basename(filename)),'789') ge 0 then wlcomp=7894
if strpos(strupcase(file_basename(filename)),'1074') ge 0 then wlcomp=10747
if strpos(strupcase(file_basename(filename)),'1079') ge 0 then wlcomp=10798
if strpos(strupcase(file_basename(filename)),'5303') ge 0 then wlcomp=5303

if wlcomp eq 10747 or wlcomp eq 10798 then wlpol = 1 else wlpol = 0

;
; read image and header for main line
; making sure that the requested line is consistent with file type
;

; this next was needed historically but now
; both COMP and UCOMP now calculated simple summed ([.5,.5,.5]: UCOMP; [.3,.7,.3]: COMP)
; for ALL I, Q, U
;
;if strupcase(typecomp) eq 'POLARIZATION' then begin
; if strpos(strupcase(line),'QOI') ge 0 then begin
;   if keyword_set(nowidgmess) then message,/info,'WARNING! Polarization files sum linear polarization but intensity is peak intensity, so not good to take ratio. Changing line to STOKESQ' else $
;     d=dialog(/WARNING, 'WARNING! Polarization files sum linear polarization but intensity is peak intensity, so not good to take ratio. Changing line to STOKESQ')
;   line='STOKESQ'
; endif
; if strpos(strupcase(line),'UOI') ge 0 then begin
;   if keyword_set(nowidgmess) then message,/info,'WARNING! Polarization files sum linear polarization but intensity is peak intensity, so not good to take ratio. Changing line to STOKESU' else $
;     d=dialog(/WARNING, 'WARNING! Polarization files sum linear polarization but intensity is peak intensity, so not good to take ratio. Changing line to STOKESU')
;   line='STOKESU'
; endif
;endif

if strpos(strupcase(line),'OI') le 0 then begin

 if ucomp eq 0 then $
  for_getcompline,filename,line,header,image,uselinename=uselinename,oldcomp=oldcomp,typecomp=typecomp,baddata=baddata,nowidgmess=nowidgmess $
 else $
  for_getucompline,filename,line,wlpol,header,image,uselinename=uselinename,baddata=baddata,nowidgmess=nowidgmess 

endif else begin
 if strpos(strupcase(line),'LOI') ge 0 and keyword_set(oldcomp) then $
  for_getcompline,filename,line,header,image,uselinename=uselinename,oldcomp=oldcomp,typecomp=typecomp,baddata=baddata,nowidgmess=nowidgmess  $
 else begin
  useline='STOKESI'
  if ucomp eq 0 then $
   for_getcompline,filename,useline,header,StokesI,uselinename=uselinename,oldcomp=oldcomp,typecomp=typecomp,baddata=baddata,nowidgmess=nowidgmess $
  else $
   for_getucompline,filename,useline,wlpol,header,StokesI,uselinename=uselinename,baddata=baddata,nowidgmess=nowidgmess 
  if strpos(strupcase(line),'VOI') ge 0 then begin
    useline='STOKESV'
    uselinename='Circular Polarization Fraction (V/I)'
  endif
  if strpos(strupcase(line),'QOI') ge 0 then begin
    useline='STOKESQ'
    uselinename='Linear Polarization Fraction (Q/I)'
  endif
  if strpos(strupcase(line),'UOI') ge 0 then begin
    useline='STOKESU'
    uselinename='Linear Polarization Fraction (U/I)'
  endif
  if strpos(strupcase(line),'LOI') ge 0 then begin
    useline='STOKESL'
    uselinename='Linear Polarization Fraction (L/I)'
  endif
  if ucomp eq 0 then $
   for_getcompline,filename,useline,header,image,oldcomp=oldcomp,typecomp=typecomp,baddata=baddata,nowidgmess=nowidgmess $
   else $
   for_getucompline,filename,useline,wlpol,header,image,baddata=baddata,nowidgmess=nowidgmess 
  test=where(StokesI ge 1.d0,c)
  testbad=where(StokesI lt 1.d0,d)
  if c gt 0 then image[test]=image[test]/StokesI[test]
; removed this line because it was throwing away data
; from the first quantity and not others
; in StokesStruct
; if i want to include it, I should also put it 
; in the StokesStruct calls below
;  if d gt 0 then image[testbad]=-8888.
 endelse
endelse

if n_elements(image) ne 1 then begin

 uselinename=strtrim(string(wlcomp),2)+' '+uselinename

 if n_elements(header) ne 1 then begin

  if min(baddata) ne -1 then image[baddata]=-8888.

;
; use information about wavelength
;

  case wlcomp of 
   10747: begin
    centwave=10747.
;
; WARNING!!! these are the generic line width,
; and not necesssarily the right thing to use
; 
    eqwidth=1.4
    end
   10798: begin
    centwave = 10798.
    eqwidth=1.4
    end
   5303: begin
    centwave = 5303.
    eqwidth=1.
    end
   6374: begin
    centwave = 6374.
    eqwidth=1.
    end
   7062: begin
    centwave = 7062.
    eqwidth=1.
    end
   7894: begin
    centwave = 7894.
    eqwidth=1.
    end
   else: begin
     eqwidth=1.
     print,'for wavelength',wlcomp,' assuming equivalent width=1 Angstrom which may not be right'
;     stop
    end
  end

; 
; Now build the StokesStruct
; unless reading from dynamics file
; if ucomp, can pass through both center I and I integrated
; and wont need to use the eqwidth kluge
;

  if strupcase(typecomp) ne 'DYNAMICS' and wlpol eq 1 then begin 

   if ucomp eq 0 then $
    for_getcompline,filename,'STOKESI',headernotuse,StokesI,oldcomp=oldcomp,typecomp=typecomp,baddata=baddata,nowidgmess=nowidgmess $
    else $
    for_getucompline,filename,'STOKESI',wlpol,headernotuse,StokesI,baddata=baddata,nowidgmess=nowidgmess
   if min(baddata) ne -1 then StokesI[baddata]=-8888.

   if ucomp eq 0 then $
    StokescentI=StokesI/eqwidth $
    else $
    for_getucompline,filename,'STOKESCENTI',wlpol,headernotuse,StokescentI,baddata=baddata,nowidgmess=nowidgmess
   if min(baddata) ne -1 then StokescentI[baddata]=-8888.

   if ucomp eq 0 then $
    for_getcompline,filename,'STOKESL',headernotuse,StokesL,oldcomp=oldcomp,typecomp=typecomp,baddata=baddata,nowidgmess=nowidgmess $
    else $
    for_getucompline,filename,'STOKESL',wlpol,headernotuse,StokesL,baddata=baddata,nowidgmess=nowidgmess
   if min(baddata) ne -1 then StokesL[baddata]=-8888.
 
   if ucomp eq 0 then $
    for_getcompline,filename,'STOKESAZ',headernotuse,StokesAZ,oldcomp=oldcomp,typecomp=typecomp,baddata=baddata,nowidgmess=nowidgmess $
    else $
    for_getucompline,filename,'STOKESAZ',wlpol,headernotuse,StokesAZ,baddata=baddata,nowidgmess=nowidgmess
    if min(baddata) ne -1 then StokesAZ[baddata]=-8888.
;

   StokesStruct={CentWave:centwave,CentI:StokescentI,I:StokesI,P:StokesL,AZ:StokesAZ}
   StokesStruct.CentI[baddata]=-8888.

  endif else begin
   if ucomp eq 0 then for_getcompline,filename,'STOKESI',headernotuse,StokesI,oldcomp=oldcomp,typecomp=typecomp,baddata=baddata,nowidgmess=nowidgmess else $
     for_getucompline,filename,'STOKESI',wlpol,headernotuse,StokesI,baddata=baddata,nowidgmess=nowidgmess
   StokesStruct={CentWave:centwave,CentI:StokesI/eqwidth,I:StokesI}
  endelse

;calculate the date

  for_datecompfits,filename,dateuse,timeuse,ucomp=ucomp
  if timeuse ne '' then date=dateuse+'_'+timeuse else date=dateuse
 
  hdr = headfits(filename)
  ext_hdr = headfits(filename,ext=1)
;
; convert to physical units
;

  if cgsmult and (strupcase(line) eq 'STOKESI' or strupcase(line) eq 'STOKESL' or strupcase(line) eq 'STOKESV' or strupcase(line) eq 'STOKESQ' or strupcase(line) eq 'STOKESU') and (wlcomp ne 5303 and wlcomp ne 6374 and wlcomp ne 7062) then begin

   lambda=centwave
   dlambda=eqwidth
   Bsun_notele=1d7*for_sun_flux(lambda,dlambda,x2,y2,x1,y1,expTime=1.,Aperture=sqrt((4.d0/!dpi)),Resolution=1.,Efficiency=1.,quiet=1,/power)
   cgsmult=1d-6*Bsun_notele*4.25d10

   gooddata=where(image ne -8888.,c)
   if c gt 0 then image[gooddata]=image[gooddata]*cgsmult
   StokesStruct.I[gooddata]=StokesStruct.I[gooddata]*cgsmult

   if strupcase(typecomp) ne 'DYNAMICS' then StokesStruct.P[gooddata]=StokesStruct.P[gooddata]*cgsmult
   
   bunit='ERG/CM2/S/SR'

  endif else bunit='PPM'
 
; Unless it is Cormag
; if wlcomp eq 5303 then bunit='DN'
; **but wait, now we have UCoMP 5303, at least some!
  if wlcomp eq 5303 then bunit='PPM'

  if strupcase(line) eq 'DOPPLERVLOS' then bunit='KM/SEC'
  if strpos(strupcase(line),'OI') gt 0 then bunit='fraction intensity'
  if strpos(strupcase(line),'AZ') ge 0  then bunit='degrees'
  if strupcase(line) eq 'LINEWIDTH' then bunit='KM/SEC'

  Cmer=tim2carr(date)
  B0=SXPAR(hdr,'SOLAR_B0')
;
; this will be put into UCoMP soon -- if not it will pass back something wrong
; i.e. 0 but not totally nuts for B angle

  if sxpar(hdr,'CDELT1') ne 0 then arcsec_per_pix = sxpar(hdr,'CDELT1') $
    else arcsec_per_pix = sxpar(ext_hdr,'CDELT1') 
  if arcsec_per_pix eq 0 then arcsec_per_pix=4.35

  if sxpar(hdr,'RSUN') ne 0 then rsuninpix=sxpar(hdr,'RSUN')/arcsec_per_pix else rsuninpix=sxpar(hdr,'CRRADIUS') 
  if rsuninpix eq 0 then begin
   if sxpar(hdr,'RSUN_OBS') ne 0 then rsuninpix=sxpar(hdr,'RSUN_OBS')/arcsec_per_pix 
   if rsuninpix eq 0 then begin
    print,'RSUN missing from COMP header'
    rsuninpix=955./arcsec_per_pix
   endif
  endif

  up_o_value=sxpar(hdr,'FRADIUS')
  if up_o_value eq 0 then up_o_value=(sxpar(hdr,'FCRAD1')+sxpar(hdr,'FCRAD2'))/2.

; can over occult if you really want to
;   add_o=3
  add_o=0.

  if ucomp eq 0 then begin
   o_value= sxpar(hdr,'ORADIUS')+add_o
   if o_value eq 0 then o_value=((sxpar(hdr,'OCRAD1')+sxpar(hdr,'OCRAD2'))/2.)+add_o
  endif else begin
;
; interpolation effect leads to additional masking needed
; UCOMP - may change this - play around to find usable data
;
   o_value= sxpar(hdr,'RADIUS')+add_o
   if o_value eq 0 then o_value=((sxpar(hdr,'RADIUS0')+sxpar(hdr,'RADIUS1'))/2.)+add_o
  endelse
  if o_value eq 0 then o_value=rsuninpix
  if up_o_value eq 0 then up_o_value=1000.d0
 

; now create modified header in structure format
;

  if is_number(occult) eq 0 then occult=o_value/rsuninpix
  default,occult,o_value/rsuninpix
  occult=occult>o_value/rsuninpix
; got rid of default because going between comp and ucomp remembering comp
;  default,upoccult,up_o_value/rsuninpix
;  if up_o_value/rsuninpix ne 0. then upoccult=upoccult<up_o_value/rsuninpix

  headernew=hdr

  if wlcomp eq 10747 then sxaddpar,headernew,'instrume','COMP' $
    else if wlcomp eq 10798 then sxaddpar,headernew,'instrume','OTHERCOMP' $
    else if wlcomp eq 7894 then sxaddpar,headernew,'instrume','FE11COMP' $
    else if wlcomp eq 5303 then sxaddpar,headernew,'instrume','CORMAG' $
    else if wlcomp eq 6374 then sxaddpar,headernew,'instrume','FE10COMP' $
    else if wlcomp eq 7062 then sxaddpar,headernew,'instrume','FE15COMP'

  sxaddpar,headernew,'wavelnth',line
  sxaddpar,headernew,'date-obs',date
  sxaddpar,headernew,'crln-obs',Cmer[0]
  sxaddpar,headernew,'crlt-obs',B0
  sxaddpar,headernew,'xcen',0.d0
  sxaddpar,headernew,'ycen',0.d0
  sxaddpar,headernew,'rsunpix',rsuninpix
  sxaddpar,headernew,'arcpix',arcsec_per_pix
  sxaddpar,headernew,'Bunit',bunit

  if SXPAR(headernew,'naxis1') eq 0 then begin
   if SXPAR(header,'naxis1') ne 0 then begin
     sxaddpar,headernew,'naxis1',SXPAR(header,'naxis1')
     sxaddpar,headernew,'naxis2',SXPAR(header,'naxis2')
   endif else begin
     sxaddpar,headernew,'naxis1',SXPAR(ext_hdr,'naxis1')
     sxaddpar,headernew,'naxis2',SXPAR(ext_hdr,'naxis2')
   endelse
   if SXPAR(headernew,'naxis1') eq 0 then stop
  endif 

;
; write to output fits file if requested
;

  if keyword_set(fileout) then mwritefits,headernew,image,outfile=fileout
 
 endif else headernew=header
 badfile=0
endif else badfile=1
end
