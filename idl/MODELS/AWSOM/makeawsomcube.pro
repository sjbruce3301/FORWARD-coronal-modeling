pro makeawsomcube,date=date,working_dir=working_dir,local=local,cubename=cubename,filenamein=filenamein

;
; PRO MAKEAWSoMCUBE
;
; Converts SWMF AWSoM 'slg' 'idl_real4' saved file
; into model output into form readable by FORWARD 
;
; will save a cube e.g. 'cube_AWSoM.dat'
; 	to working_dir
;  
;
;  Keyword inputs
;  
;     DATE -> will establish Carrington rotation and central meridian
;      this code will call subroutine DOWNLOADAWSOM which will
;	obtain the appropriate FILENAMEIN: slg_*.out to read in
;         default: '2011-03-01T00:00:00'
;  
;     LOCAL -- use local file in the directory
;	(note, in general, FORWARD looks for local files
;	 already existing with expected naming convention before 
;	 downloading from web)
;
;     FILENAMEIN-- used instead of date/download as AWSOM input file
;
;     WORKING_DIR --> file I/O will occur here
; 
;  OUTPUT Keyword
;       CUBENAME= filename of cube with forward-friendly model
;		(even if unset, the filename will be saved to WORKING_DIR
;
; CALLED BY NUMCUBEPRAMS
; CALLS MAKEMYCUBE, DOWNLOADAWSOM

;  
; Written by Judit Szente 
; Version 0.0 Sept 2023
;
; March 2024 - fixed bug in definition of Br
; 		added FILENAMEIN

  slash=path_sep()

; check to see if filenamin is set

  if exist(filenamein) then begin
    awsom_cr=strmid(filenamein,0,4)
    date=anytim2utc(CARr2ex(awsom_cr),/ccsds)
    print,'using AWSOM file,',filenamein
    print,'CROT=',awsom_cr
    print,date
  endif else begin
   
   
; 
; a good default
;
   datedefault='2011-03-01'

; this is needed because forward will pass in '' which will result in garbage CROT
   if date eq '' then date=datedefault
   default,date,datedefault

;
; min date AWSOM has data for
;
;   default,datemin,'2007-01-01'

; 
; convert to carrington rotation
;

;awsom_cr = strtrim(fix(TIM2CARR(date,/dc)),2)
  awsom_cr = fix(TIM2CARR(date,/dc))

 endelse
;
; make cube name
;

  crotname=strtrim(string(awsom_cr),2)
  crotname=crotname[0]

  if keyword_set(working_dir) then writediruse=working_dir+slash else writediruse='.'+slash

  cubename = writediruse+crotname+'_cube_AWSOM'
  if keyword_set(local) then cubename = writediruse+crotname+'_local_cube_AWSOM'


  if file_exist(cubename+'.dat') eq 0 then begin

     if keyword_set(local) eq 0 then $
; downloading
; get filenamein
        downloadawsom,crotname,writediruse,filenamein,rspcode=rspcode

     if exist(rspcode) eq 0 or keyword_set(local) then begin

        
;HEADER READER - real4
        unit = 10
        close,unit
        openr,unit,writediruse+filenamein
        lenhead=1L
        readu,unit,lenhead
        head=bytarr(lenhead+4)
        len=1L
        readu,unit,head,len

        if lenhead ne 79 and lenhead ne 500 or len ne 20 then begin
           print,'need to have real4 type of filein'
           retall
        endif
        close,unit

        openr,unit,writediruse+filenamein,/f77_unf  

        lenstr = lenhead
        headline=''
        for i=1, lenstr do headline=headline+' '
        it     = 1L
        awsom_ndim   = 1L
        neqpar = 0L
        eqpar  = 0.0
        awsom_nvar     = 1L
        varname=''
        for i=1, lenstr do varname=varname+' '

        awsom_time=float(1)
        readu,unit,headline
        readu,unit,it,awsom_time,awsom_ndim,neqpar,awsom_nvar
        gencoord=(awsom_ndim lt 0)
        awsom_ndim=abs(awsom_ndim)
        awsom_nx=lonarr(awsom_ndim)
        readu,unit,awsom_nx
        if neqpar gt 0 then begin
           eqpar=fltarr(neqpar)
           readu,unit,eqpar
        endif
        readu,unit,varname
        
;DATA READER
        nr=awsom_nx(0)
        nlon=awsom_nx(1)
        nlat=awsom_nx(2)
        awsom_grid=fltarr(nr,nlon,nlat,awsom_ndim)
        awsom_data=fltarr(nr,nlon,nlat,awsom_nvar)
        awsom_line=fltarr(nr,nlon,nlat)
        readu,unit,awsom_grid
        for iw=0,awsom_nvar-1 do begin
           readu,unit,awsom_line
           awsom_data(*,*,*,iw)=awsom_line
        endfor
        close,unit
        
        awsom_varname = strsplit(varname,' ',/extract)
        awsom_varname = awsom_varname[0:awsom_nvar+awsom_ndim-1]
; cartesian to spherical and unit conversion
; scalars
        r = 10^(reform(awsom_grid[*,0,0,0])) ;log grid in Rs
        lon = awsom_grid[*,*,*,1]*!pi/180.   ;deg to RAD
        lat = awsom_grid[*,*,*,2]*!pi/180.   ;deg to RAD
        density = awsom_data[*,*,*,-awsom_ndim + $
                             where(awsom_varname eq 'rho',/null)]/1.67262192e-24
                                ;g/cm3 to # NOT CONSIDERING He in plasma
        etemp = awsom_data[*,*,*,-awsom_ndim+where(awsom_varname eq 'te',/null)] ;K
        pe = $
           awsom_data[*,*,*,-awsom_ndim+where(awsom_varname eq 'pe',/null)] ;dyne/cm^2

; reindexing phi from 0->2pi  
; phi should have values (-pi LE phi LE pi)
        nlonhalf = nlon/2+(nlon mod 2)
        phi_index = shift(indgen(nlon),nlonhalf)
        phi = reform(lon[0,*,0]) ; 0 -> 2pi
        phi = phi(phi_index)
        phi(0:nlonhalf-1) = phi(0:nlonhalf-1) - 2*!pi

; reindexing theta from -pi/2 -> pi/2 where N pole is pi/2    
; theta are (0 LE theta LE pi, zero is the north pole )
        theta_index = reverse(indgen(nlat))  
        theta = -reform(reverse(lat[0,0,*],3))+ !pi/2. ; 0 -> pi

; vectors
        bx = awsom_data[*,*,*,-awsom_ndim+where(awsom_varname eq 'bx',/null)] ;G
        by = awsom_data[*,*,*,-awsom_ndim+where(awsom_varname eq 'by',/null)] ;G 
        bz = awsom_data[*,*,*,-awsom_ndim+where(awsom_varname eq 'bz',/null)] ;G

        vx = awsom_data[*,*,*,-awsom_ndim+where(awsom_varname eq 'ux',/null)] ;km/s 
        vy = awsom_data[*,*,*,-awsom_ndim+where(awsom_varname eq 'uy',/null)] ;km/s 
        vz = awsom_data[*,*,*,-awsom_ndim+where(awsom_varname eq 'uz',/null)] ;km/s 

;(NR, NTH, NPH)
        th = theta
        ph = phi
        br = fltarr(nr,nlat,nlon)
        bth = fltarr(nr,nlat,nlon)
        bph = fltarr(nr,nlat,nlon)
        vr = fltarr(nr,nlat,nlon)
        vth = fltarr(nr,nlat,nlon)
        vph = fltarr(nr,nlat,nlon)
        dens = fltarr(nr,nlat,nlon)
        temp = fltarr(nr,nlat,nlon)
        pres = fltarr(nr,nlat,nlon)

        for n = 0,nlat-1 do begin
           for m = 0,nlon-1 do begin
              for i = 0,nr-1 do begin
                 k = phi_index(m)
                 j = theta_index(n)
                 br[i,j,k] = sin(theta[j])*cos(phi[k])*bx[i,m,n]+$
                             sin(theta[j])*sin(phi[k])*by[i,m,n]+$
                             cos(theta[j])*bz[i,m,n]
                 bth[i,j,k] = cos(theta[j])*cos(phi[k])*bx[i,m,n]+$
                              cos(theta[j])*sin(phi[k])*by[i,m,n]-$
                              sin(theta[j])*bz[i,m,n]
                 bph[i,j,k] = -sin(phi[k])*bx[i,m,n]+cos(phi[k])*by[i,m,n]
                 vr[i,j,k] = sin(theta[j])*cos(phi[k])*vx[i,m,n]+$
                             sin(theta[j])*sin(phi[k])*vy[i,m,n]+$
                             cos(theta[j])*vz[i,m,n]
                 vth[i,j,k] = cos(theta[j])*cos(phi[k])*vx[i,m,n]+$
                              cos(theta[j])*sin(phi[k])*vy[i,m,n]-$
                              sin(theta[j])*vz[i,m,n]
                 vph[i,j,k] = -sin(phi[k])*vx[i,m,n]+$
                              cos(phi[k])*vy[i,m,n]
                 dens[i,j,k] = density[i,m,n] 
                 temp[i,j,k] = etemp[i,m,n]
                 pres[i,j,k] = pe[i,m,n]
              end
           end
        end  

; savefile for make_my_cube
        SaveFileName=writediruse+'awsom_data_forward_TEMP.dat'
        save, r, th, ph, br, bth, bph, dens, temp, pres, vr, vth, vph, FileName=SaveFileName
        
;---- now load it up into forward
        ModelName=strtrim(string(awsom_cr),2)+'.dat'
        ModelName=ModelName[0]
        CubeDir = writediruse


; make cube
        make_my_cube, simname=SaveFileName, OutFile=cubename, OutDir=CubeDir, /rsun, /global, modelname=ModelName

        file_delete, SaveFileName,/quiet, /recursive
        file_delete, writediruse+filenamein,/quiet, /recursive

     endif else begin
        cubename=''
        print, '' 
        print, '### makeawsomcube.pro: COULD NOT FIND THE AWSOM DATA FOR DOWNLOAD!'
        print, ''
     endelse

  endif
  
end
