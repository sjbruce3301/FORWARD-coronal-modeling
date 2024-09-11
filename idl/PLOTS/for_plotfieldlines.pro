;**********************************************************************
pro for_plotfieldlines,GridPramsStruct,LosPramsStruct,$
	ObsPramsStruct,ModPramsStruct,ModSolStruct,norunmodel=norunmodel,$
 	Bscale=Bscale,narrow=narrow,nreinit=nreinit,bcolor=bcolor,occult=occult,upoccult=upoccult,$
	bthick=bthick,bminuse=bminuse,xplotrange=xplotrange,yplotrange=yplotrange,nowidgmess=nowidgmess,working_dir=working_dir

;+
;Name: FOR_PLOTFIELDLINES
;
;Purpose: To overplot plane-of-sky Bfield vectors onto plot 
;
;Input 
;
;       KEYWORDS
;
;       BSCALE - The scale factor for plotting the fieldline vector onto
;                the plane of sky
;               if set to zero, does fixed llength lines, if less than zero,
;               scales to absolute value (but fixed length), if greater than zero
;               then field line length reflects POS strength, scaled by Bscale
;       XPLOTRANGE/YPLOTRANGE - Range of map to plot 
;
;	NARROW - spacing of vectors
;
;	BCOLOR - color of vectors (default red)
;	BTHICK - thickness of vectors (default 1)
;
;       BMINUSE -- minimum magnitude of plane of sky field to plot vector for
;
;	NREINIT -- allows numerical models to use common block to access data cube
;
;  calls FOR_POSNOINT to calculate By, Bz (plane of sky directions-- called Bx, By below)
;
; Common Blocks: none
;
; Called by FOR_DRIVE
;
; Written by Sarah Gibson 2010-2014
;  Modified for narrow, ngrid keyword and default value changes Mar 8 2010 SEG
;  Modified to make negative Bscale a multiplier of Bscale=0 constant length
; Modified to be able to make subplot using xplotrange yplotrange input
; Modified for better Bscale defaults May 2013 SEG
; Modified so never plot on disk
; 
; Version 2.0 July 2014
;
;  June 2019 - bug fix -- added if min(test) ne -1 before defs of Bmax, Bmed
;  Sept 2021 -- passed through nowidgmess
;  April 2022 -- passed through working_dir
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;--

  useoccult=occult > 1

  dx=GridPramsStruct.dx
  dy=GridPramsStruct.dy

  temp=ObsPramsStruct.LineName

;
; determine By, Bz (plane of sky)
; since it is a 2d x-y plot, will rename them Bx, By

  r=GridPramsStruct.Rpos
  theta=GridPramsStruct.THpos
  phi=GridPramsStruct.PHpos

  ObsPramsStruct.LineName='By'
  for_posnoint,quantBy,r,theta,phi,$        
	LosPramsStruct,ObsPramsStruct,ModPramsStruct,GridPramsStruct,ModSolStruct,norunmodel=norunmodel,nreinit=nreinit,nowidgmess=nowidgmess,working_dir=working_dir
  Bxar= quantBy

  ObsPramsStruct.LineName='Bz'
  for_posnoint,quantBz,r,theta,phi,$        
	LosPramsStruct,ObsPramsStruct,ModPramsStruct,GridPramsStruct,ModSolStruct,norunmodel=norunmodel,nreinit=nreinit,working_dir=working_dir
;	LosPramsStruct,ObsPramsStruct,ModPramsStruct,GridPramsStruct,ModSolStruct,norunmodel=1,nreinit=nreinit
; commented this out -- should not be needed because for_Settingdefaults will set
; norunmodel and so default,0 in for_posnoint should not have an effect.
; on the other hand, forcing norunmodel = 1 does not allow user to force a rerun,
; which could be necessary for old save sets.
;

  Byar= quantBz

  Bmax=0.d0
  test=where(bxar ne -9999. and bxar ne -8888. and byar ne -9999. and bxar ne -8888.)
  if min(test) ne -1 then Bmax = max(sqrt(Bxar[test]*Bxar[test]+Byar[test]*Byar[test]))
  if min(test) ne -1 then Bmed = median(sqrt(Bxar[test]*Bxar[test]+Byar[test]*Byar[test]))
  if abs(Bmed) lt 1d-8 then Bmed=Bmax
;
;  MAKE PLOTS
;

  Bscalemult=(yplotrange[1]-yplotrange[0])/(100.*(Bmed))
  if datatype(Bscale) eq 'STR' then $
	if strupcase(Bscale) eq 'SCALED TO DATA' then Bscale=1.d0
  if Bscale gt 0.d0 then Bscale=Bscalemult*Bscale


; note number of points to skip varies with grid size, thus ngrid/nsarrow is 
; number of points to skip.  Also put in x,y difference
; if grid not squre


   nsize=size(quantBz)
   nxpts=nsize[1]
   nypts=nsize[2]
   

   xsubplot=(xplotrange[1]-xplotrange[0])/(GridPramsStruct.xrange[1]-GridPramsStruct.xrange[0])
   ysubplot=(yplotrange[1]-yplotrange[0])/(GridPramsStruct.yrange[1]-GridPramsStruct.yrange[0])

   nskipx=fix(nxpts*xsubplot/narrow)
   nskipy=fix(nypts*ysubplot/narrow)

; nskipx=fix(nxpts/narrow)
; nskipy=fix(nypts/narrow)
 
   xar = GridPramsStruct.xrange[0] + findgen(nxpts)*dx
   yar = GridPramsStruct.yrange[0] + findgen(nypts)*dy
   xcount=0
   for i=0,nxpts-1 do begin
      x = xar[i]
      ycount=0
      for j=0,nypts-1 do begin
         y = yar[j]
         if xcount eq nskipx and ycount eq nskipy and x^2+y^2 gt useoccult*useoccult and sqrt(Bxar[i,j]^2 + Byar[i,j]^2) gt bminuse then begin
	    if Bscale gt 0. then begin
              Xpoint = [x-Bscale*Bxar[i,j],x+Bscale*Bxar[i,j]]
              Ypoint = [y-Bscale*Byar[i,j],y+Bscale*Byar[i,j]]
	    endif
	    if Bscale le 0. then begin
	      bhyp = sqrt(Bxar[i,j]^2+Byar[i,j]^2)
	      if abs(bhyp) lt 1d-8 then bhyp = 1.
	      range=max([(xplotrange[1]-xplotrange[0]),(yplotrange[1]-yplotrange[0])])
 	      if Bscale eq 0. then Bnorm=range*0.02/bhyp
 	      if Bscale lt 0. then Bnorm=-range*Bscale*0.02/bhyp
              Xpoint = [x-Bnorm*Bxar[i,j],x+Bnorm*Bxar[i,j]]
              Ypoint = [y-Bnorm*Byar[i,j],y+Bnorm*Byar[i,j]]
	    endif
 	    if exist(Xpoint) and exist(Ypoint) then if Xpoint[0] ge xplotrange[0] and Xpoint[0] le xplotrange[1] and Xpoint[1] ge xplotrange[0] and Xpoint[1] le xplotrange[1] and Ypoint[0] ge yplotrange[0] and Ypoint[0] le yplotrange[1] and Ypoint[1] ge yplotrange[0] and Ypoint[1] le yplotrange[1] and sqrt(Xpoint[0]^2+Ypoint[0]^2) gt useoccult and sqrt(Xpoint[1]^2+Ypoint[1]^2) gt useoccult then begin 
 	     upuse=1
             if upoccult gt 0 then $
		if sqrt(Xpoint[0]^2+Ypoint[0]^2) ge upoccult or sqrt(Xpoint[1]^2+Ypoint[1]^2) ge upoccult then upuse=0
             if upuse eq 1 then arrow,Xpoint[0],Ypoint[0],Xpoint[1],Ypoint[1],/data,hsize=-.1,color=bcolor,thick=bthick,hthick=bthick*2
;	     if upuse eq 1 then print,xpoint[0],xpoint[1],ypoint[0],ypoint[1],xplotrange[0],xplotrange[1],yplotrange[0],yplotrange[1]
            endif 
         endif 
         if ycount ge nskipy then ycount=0 else ycount = ycount + 1
      endfor
      if xcount ge nskipx then xcount = 0 else xcount = xcount + 1
   endfor
   
ObsPramsStruct.LineName=temp

end
