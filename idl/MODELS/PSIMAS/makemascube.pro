pro makemascube,date=date,working_dir=working_dir,mheat=mheat,local=local,sequence=sequence,choosehigh=choosehigh,cubename=cubename,resstring=resstring

;
; PRO MAKEMASCUBE
;
; Converts Predictive Science Inc. (PSI) MAS
; model cubes into form readable by FORWARD 
;
; will save a cube e.g. '2120_HMI2_cube_MAS.dat'
; 	to working_dir
;
; which can be accessed e.g.
;
; for_drive,'numcube',cubename='2120_HMI2_cube_MAS.dat',line='Bx'
;
; or via for_widget -- the date will be carried along.
;
;  Keyword inputs
;
;  DATE -> will establish Carrington rotation and central meridian
;
;  WORKING_DIR --> file I/O will occur here
;
;  LOCAL -- use a local cube
;
;  SEQUENCE -- MAS identifier
;
;  CHOOSEHIGH -- choose high res model if available
;
;  MHEAT - MAS heating model and magnetic boundary
;
;     HMI1:  
;hmi_mast_std_0101 (Heating model 1): The 'standard' heating model from Lionello et al. 2009. 
;  - This model typically gives hotter QS temperatures, typically between 1.4-1.8 MK.
;  - Coronal holes are wider/more open.
;  - Active regions aren't as visible due to the properties of the magnetic maps and heating model at this resolution.
;
;    HMI2:
;hmi_mast_std_0201 (Heating model 2): used for most recent eclipse predictions.
;  - This model typically gives cooler QS temperatures, typically between 1.2-1.5 MK.
;  - The coronal base density is slightly higher everywhere.
;  - Coronal holes are smaller and slightly more dense than heating model 1.
;  - Active regions are heated more and are hotter/more visible.
;
;    HMI3:
;hmi_masp_std_0201
;	Old, polytropic simulation, not recommended
;
;    GONG2:
;also available as gong_mast_std_0201
;
;  OUTPUT
;	Cubename= filename of cube with forward-friendly model
;
; CALLED BY NUMCUBEPRAMS
; CALLS MAKEMYCUBE
;
; Written Cooper Downs, Modified Sarah Gibson
; Version 2.0 July 2014
;
;  Feb 2019 -- fixed bug where Boltzmann's constant was slightly off (SEG)
;  May 2019 -- added sequence keyword 
; June 2019 used slash, file_delete for PC compatibility
; July 2020 -- made writediruse have a slash but filenames not
;	for compatibility with for_directdownload
; Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
; June 2023 -- bug fix - misname for MDI1
;	    -- removed undefine(rspcode) because redundant with tempvar 
;		was causing a bug because applied to MDI1 undefining 
; Sept 2023 -- added "choosehigh" option which checks first for high
;		before medium
;		updated comments, fixed minor issues

slash=path_sep()

;
; absolute earliest KP rotation 1625
;
dateminkpo='1975-03-01T00:00:00'

; 
; a good default
;
datedefault='2012-01-04'

; this is needed because forward will pass in '' which will result in garbage CROT
if date eq '' then date=datedefault
default,date,datedefault

default,mheat,'HMI2'
default,sequence,'002'

crotminkpo=1625
crotminmdi=1911
crotminmdimed=2000
crotmaxmdi=2104
crotminhmi=2097

crot=fix(TIM2CARR(date,/dc))

if strpos(strupcase(mheat),'HM') ge 0 then if crot lt crotminhmi then begin
 mheat='MDI2'
endif
if strpos(strupcase(mheat),'MD') ge 0 then if crot lt crotminmdimed then begin
 mheat='MDI1'
endif
if strpos(strupcase(mheat),'MD') ge 0 then if crot lt crotminmdi then begin
 mheat='KP1'
endif
if strpos(strupcase(mheat),'KP') ge 0 then if crot lt crotminkpo then date=dateminkpo 
if strpos(strupcase(mheat),'MD') ge 0 then if crot gt crotmaxmdi then begin
  mheat='HMI2'
endif

case mheat of
  'HMI1': mheatuse='hmi_mast_mas_std_0101' 
  'HMI2': mheatuse='hmi_mast_mas_std_0201' 
  'HMI3': mheatuse='hmi_masp_mas_std_0201' 
  'MDI1': mheatuse='mdi_mas_mas_std_0101' 
  'MDI2': mheatuse='mdi_mas_mas_std_0201' 
  'KP1': mheatuse='kpo_mas_mas_std_0101' 
  'KP2': mheatuse='kpo_mas_mas_std_0201' 
  'MW1': mheatuse='mwo_mas_mas_std_0101' 
  'GONG1': mheatuse='gong_mas_mas_std_0101' 
  'GONG2': mheatuse='gong_mast_mas_std_0201' 
  'SOLIS1': mheatuse='solis_mas_mas_std_0101' 
  'SOLIS2': mheatuse='solis_mas_mas_std_0201' 
  'WTD':mheatuse='wave-turbulence-driven'
endcase

;datename=anytim(date,/ccsds)
;datename=str_replace(datename,':','')
;datename=str_replace(datename,'-','')
;datename=str_replace(datename,'_','')

;
; in case date changed
;
crot=fix(TIM2CARR(date,/dc))
crotname=strtrim(string(crot),2)
crotname=crotname[0]

if keyword_set(working_dir) then writediruse=working_dir+slash else writediruse='.'+slash

cubename = writediruse+crotname+'_'+mheat+'_cube_MAS'
if keyword_set(local) then cubename = writediruse+crotname+'_'+mheat+'_local_cube_MAS'
if keyword_set(resstring) then $
 if resstring eq 'eclipse' then cubename=cubename+'_eclipse_'+date

;print,'crot=',crotname,'date=',date
if file_exist(cubename+'.dat') eq 0 then begin

 if keyword_set(local) eq 0 then begin
;
; downloading e.g.
; http://www.predsci.com/data/runs/cr2116-medium/hmi_mast_mas_std_0101/corona.zip
;
; note recent ones are high not medium!

  if keyword_set(choosehigh) then begin
   resstring='high' 
   resstring2='medium' 
  endif else begin
   resstring='medium'
   resstring2='high'
  endelse

  resused=resstring
  URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/'+mheatuse+'/corona.zip'
  URLPATHUSE=URLPATHUSE[0]
  filenames='corona.zip' 
  for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'

; if does not find 
; first check to see if high or medium depending where it checked first res

  if exist(rspcode) eq 1 and crot ge crotminhmi then begin
;   undefine,rspcode
   URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring2+'/'+mheatuse+'/corona.zip'
   URLPATHUSE=URLPATHUSE[0]
   filenames='corona.zip' 
   tempvar=size(temporary(rspcode))
   print,'checking other resolution'
   for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
   resused=resstring2

; if still not find, check to see if another HMI is available
   if exist(rspcode) eq 1 then begin
     resused=resstring
;    undefine,rspcode
    if strupcase(mheat) ne 'HMI2' then begin
     URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/hmi_mast_mas_std_0201/corona.zip'
     URLPATHUSE=URLPATHUSE[0]
     tempvar=size(temporary(rspcode))
     filenames='corona.zip' 
     print,'checking HMI2'
     for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
     if exist(rspcode) eq 0 then mheat='HMI2'
    endif
    if exist(rspcode) eq 1 and strupcase(mheat) ne 'HMI1' then begin
;     undefine,rspcode
     URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/hmi_mast_mas_std_0101/corona.zip'
     URLPATHUSE=URLPATHUSE[0]
     tempvar=size(temporary(rspcode))
     filenames='corona.zip' 
     print,'checking HMI1'
     for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
     if exist(rspcode) eq 0 then mheat='HMI1'
    endif
    if exist(rspcode) eq 1 then begin
;     undefine,rspcode
     URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/hmi_masp_mas_std_0201/corona.zip'
     URLPATHUSE=URLPATHUSE[0]
     tempvar=size(temporary(rspcode))
     filenames='corona.zip' 
     print,'checking HMI3'
     for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
     if exist(rspcode) eq 0 then mheat='HMI3'
    endif
   endif
  endif

; if earlier, check to see if another MDI is available
  if exist(rspcode) eq 1 and crot le crotmaxmdi then begin
;   undefine,rspcode
   if crot ge crotminmdimed then if strupcase(mheat) ne 'MDI2' then begin
    URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/mdi_mas_mas_std_0201/corona.zip'
    URLPATHUSE=URLPATHUSE[0]
    tempvar=size(temporary(rspcode))
    filenames='corona.zip' 
    print,'checking MDI2'
    for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
    if exist(rspcode) eq 0 then mheat='MDI2'
   endif
   if exist(rspcode) eq 1 and strupcase(mheat) ne 'MDI1' then begin
;    undefine,rspcode
    URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/mdi_mas_mas_std_0101/corona.zip'
    URLPATHUSE=URLPATHUSE[0]
    tempvar=size(temporary(rspcode))
    filenames='corona.zip' 
    print,'checking MDI1'
    for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
    if exist(rspcode) eq 0 then mheat='MDI1'
   endif
  endif

 ;--- now trap for the other medium resolution polytropic runs
 ; if necessary
  if exist(rspcode) eq  1 then begin
;    undefine,rspcode
    URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/kpo_mas_mas_std_0201/corona.zip'
    URLPATHUSE=URLPATHUSE[0]
    tempvar=size(temporary(rspcode))
    print,'checking KP2'
    for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
    if exist(rspcode) eq 0 then mheat='KP2' else begin
      URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/mwo_mas_mas_std_0201/corona.zip'
      URLPATHUSE=URLPATHUSE[0]
      tempvar=size(temporary(rspcode))
      print,'checking MW2'
      for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
      if exist(rspcode) eq 0 then mheat='MW2' else begin
       URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/gong_mast_mas_std_0201/corona.zip'
       URLPATHUSE=URLPATHUSE[0]
       tempvar=size(temporary(rspcode))
       print,'checking GONG2'
       for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
       if exist(rspcode) eq 0 then mheat='GONG2' else begin
           URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/solis_mas_mas_std_0201/corona.zip'
           URLPATHUSE=URLPATHUSE[0]
           print,'checking SOLIS2'
           tempvar=size(temporary(rspcode))
           for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
           if exist(rspcode) eq 0 then mheat='SOLIS2'
       endelse
      endelse
    endelse
  endif

 ;--- find any low res polytropic run for this date
  if exist(rspcode) eq  1 then begin
;   undefine,rspcode
   URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/kpo_mas_mas_std_0101/corona.zip'
   URLPATHUSE=URLPATHUSE[0]
   tempvar=size(temporary(rspcode))
   print,'checking KP1'
   for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
   if exist(rspcode) eq 0 then mheat='KP1' else begin
     URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/mwo_mas_mas_std_0101/corona.zip'
     URLPATHUSE=URLPATHUSE[0]
     tempvar=size(temporary(rspcode))
     print,'checking MW1'
     for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
     if exist(rspcode) eq 0 then mheat='MW1' else begin
        URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/gong_mas_mas_std_0101/corona.zip'
        URLPATHUSE=URLPATHUSE[0]
        tempvar=size(temporary(rspcode))
        print,'checking GONG1'
        for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
        if exist(rspcode) eq 0 then mheat='GONG1' else begin
          URLPATHUSE='data/runs/cr'+strtrim(string(crot),2)+'-'+resstring+'/solis_mas_mas_std_0101/corona.zip'
          URLPATHUSE=URLPATHUSE[0]
          tempvar=size(temporary(rspcode))
          print,'checking SOLIS1'
          for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,rspcode=rspcode,usehost='www.predsci.com'
          if exist(rspcode) eq 0 then mheat='SOLIS1' else mheat=''
        endelse
     endelse
   endelse
  endif
 endif else begin
  resused=resstring
 endelse

 if exist(rspcode) eq  0 or keyword_set(local) then begin

 ; unzip

  cd,writediruse,current=old_dir
  spawn,'unzip corona.zip'
  cd,old_dir 

  MasDataDir = writediruse+'corona'

  ps_read_mas_run, MasDataDir, mas, sequence=sequence

;help, mas

;---- now convert to FORWARD datacube style/units
  r = mas.r
  th = mas.t
  ph = mas.p
  br = mas.br
  bth = mas.bt
  bph = mas.bp
  dens = mas.rho
  temp = mas.temp*1d6
  pres = 2*1.3807d-16*dens*temp
  vr=mas.vr
  vth=mas.vt
  vph=mas.vp

; put ph between -pi and pi limits
; output so far will have 181 values of ph,
; from 0 to 2*pi. The problem with doing the next commented out
; lines was that it gave double value 0 and was not symmetric
; better to hard code the shift to avoid this
;  however, be careful with local files which may have different resolution
;   and/or may not have odd number of points
;

  testlow=where(ph lt -!dpi)
;  if min(testlow) ne -1 then ph[testlow]=ph[testlow]+2.d0*!dpi

  testhigh=where(ph gt !dpi)
;  if min(testhigh) ne -1 then ph[testhigh]=ph[testhigh]-2.d0*!dpi

;
; put in a test here-- below it assumes that the input was 0-2pi
;
  if min(testlow) ne -1 then begin
    print,'some phi values less than zero?'
    stop
  endif

  if min(testhigh) ne -1 then begin
    print,'reordering cube so that runs -pi to pi'
    sizeph=size(ph)
   sizeph=sizeph[1]
   isodd=0
   if fix((sizeph-1)/2.)  eq (sizeph-1)/2. then isodd=1
   if isodd eq 1 then begin
    nnsp=1
    nnhm=(sizeph-1)/2
    nnh=(sizeph-1)/2
    nnhp=(sizeph-1)/2 + 1
    nnfm=sizeph-2
    nnf=sizeph-1
   endif else begin
    nnsp=0
    nnhm=sizeph/2 -1
    nnh=sizeph/2
    nnhp=sizeph/2
    nnfm=sizeph-1
    nnf=sizeph-1
   endelse
   
   phold=ph
   ph[0:nnhm]=phold[nnh:nnf]-2.d0*!dpi
   ph[nnhp:nnfm]=phold[nnsp:nnh-1]
   ph[nnf]=phold[nnhp-1]
 
   densold=dens
   dens[*,*,0:nnhm]=densold[*,*,nnh:nnf]
   dens[*,*,nnhp:nnfm]=densold[*,*,nnsp:nnh-1]
   dens[*,*,nnf]=densold[*,*,nnhp-1]
 
   tempold=temp
   temp[*,*,0:nnhm]=tempold[*,*,nnh:nnf]
   temp[*,*,nnhp:nnfm]=tempold[*,*,nnsp:nnh-1]
   temp[*,*,nnf]=tempold[*,*,nnhp-1]
 
   presold=pres
   pres[*,*,0:nnhm]=presold[*,*,nnh:nnf]
   pres[*,*,nnhp:nnfm]=presold[*,*,nnsp:nnh-1]
   pres[*,*,nnf]=presold[*,*,nnhp-1]
 
   brold=br
   br[*,*,0:nnhm]=brold[*,*,nnh:nnf]
   br[*,*,nnhp:nnfm]=brold[*,*,nnsp:nnh-1]
   br[*,*,nnf]=brold[*,*,nnhp-1]
 
   bthold=bth
   bth[*,*,0:nnhm]=bthold[*,*,nnh:nnf]
   bth[*,*,nnhp:nnfm]=bthold[*,*,nnsp:nnh-1]
   bth[*,*,nnf]=bthold[*,*,nnhp-1]
 
   bphold=bph
   bph[*,*,0:nnhm]=bphold[*,*,nnh:nnf]
   bph[*,*,nnhp:nnfm]=bphold[*,*,nnsp:nnh-1]
   bph[*,*,nnf]=bphold[*,*,nnhp-1]
 
   vrold=vr
   vr[*,*,0:nnhm]=vrold[*,*,nnh:nnf]
   vr[*,*,nnhp:nnfm]=vrold[*,*,nnsp:nnh-1]
   vr[*,*,nnf]=vrold[*,*,nnhp-1]

   vthold=vth
   vth[*,*,0:nnhm]=vthold[*,*,nnh:nnf]
   vth[*,*,nnhp:nnfm]=vthold[*,*,nnsp:nnh-1]
   vth[*,*,nnf]=vthold[*,*,nnhp-1]

   vphold=vph
   vph[*,*,0:nnhm]=vphold[*,*,nnh:nnf]
   vph[*,*,nnhp:nnfm]=vphold[*,*,nnsp:nnh-1]
   vph[*,*,nnf]=vphold[*,*,nnhp-1]
 
  endif

;---- save the data in FORWARD format as an IDL save file
  RawFile=writediruse+'data_forward.dat'
  save, r, th, ph, br, bth, bph, dens, temp, pres, vr, vth, vph, FileName=RawFile

  if resused eq 'high' then begin
   case mheat of
   'HMI1':   mdescr = 'hmi_thermo_high_h1'
   'HMI2':   mdescr = 'hmi_thermo_high_h2'
   'HMI3':   mdescr = 'hmi_polytropic_high_h3'
     else:   mdescr = 'something else'
   endcase
;
; no other high res should be there
;
  endif else begin
   case mheat of
   'HMI1':   mdescr = 'hmi_thermo_med_h1'
   'HMI2':   mdescr = 'hmi_thermo_med_h2'
   'HMI3':   mdescr = 'hmi_polytropic_h3'
   'MDI1':   mdescr = 'mdi_polytropic_low'
   'MDI2':   mdescr = 'mdi_polytropic_med'
   'KP1':    mdescr = 'kpo_polytropic_low'
   'KP2':    mdescr = 'kpo_polytropic_med'
   'MW1':    mdescr = 'mwo_polytropic_low'
   'GONG1':  mdescr = 'gong_polytropic_low'
   'GONG2':  mdescr = 'gong_polytropic_med'
   'SOLIS1': mdescr = 'solis_polytropic_low'
   'SOLIS2': mdescr = 'solis_polytropic_med'
   'WTD':    mdescr =  'wave-turbulence-driven'
   endcase
   if resused eq 'eclipse' then mdescr=mdescr+'-'+resused+'-'+date
  endelse

;---- now load it up into forward
  ModelName=strtrim(string(crot),2)+'_'+strupcase(mdescr)+'.dat'
  ModelName=ModelName[0]
  CubeDir = writediruse

  make_my_cube, simname=RawFile, OutFile=cubename, OutDir=CubeDir, /rsun, /global, modelname=ModelName,mheat=mheat

  file_delete, writediruse+'corona.zip', writediruse+'data_forward.dat', writediruse+'corona', /quiet, /recursive

 endif else begin 
   cubename=''
   print, ''
   print, '### makemascube.pro: COULD NOT FIND THE MAS DATA FOR DOWNLOAD!'
   print, '### makemascube.pro: The date requested is either too old, too new'
   print, '### makemascube.pro: or the specific model requested does not exist'
   print, ''
 endelse
endif else print,'using existing cube',cubename+'.dat'
end
