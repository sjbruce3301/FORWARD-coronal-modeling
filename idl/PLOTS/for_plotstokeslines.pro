;**********************************************************************
pro for_plotstokeslines,GridPramsStruct,LosPramsStruct,$
	ObsPramsStruct,ModPramsStruct,StokesStruct,xplotrange=xplotrange,yplotrange=yplotrange,$
 	Pscale=Pscale,nstarrow=nstarrow,stkcolor=stkcolor,stkthick=stkthick,sminuse=sminuse,$
        occult=occult,upoccult=upoccult,arrowave=arrowave

;+
;Name: FOR_PLOTSTOKESLINES
;
;Purpose: To overplot plane-of-sky polarization vectors (as calculated using
;stokes parameters) onto pre-existing plot 
;
;       Input: Grid parameter structure, Line of sight parameter
;              structure, observation parameter structure, model parameter
;              structure, stokes structure. 
;
;       KEYWORDS
;
;       PSCALE - The scale factor for plotting the fieldline vector onto
;                the plane of sky
;		if set to zero, does fixed llength lines, if less than zero,
;		scales to absolute value (but fixed length), if greater than zero
;		then field line length reflects POS strength, scaled by Pscale
;       XPLOTRANGE/YPLOTRANGE - Range of map to plot 
;
;	NARROW - spacing of vectors
;	
;	STKCOLOR - color of vectors (default green)
;	STKTHICK - thickness of vectors (default 1.5)
;	SMINUSE -- minimum magnitude of plane of sky to plot vector
;                  for
;
;	ARROWAVE  arrowave will average over skipped data in
; 		creating arrow length and direction
; 		default is  no averaging
; 		(most important for data)
;
;  uses Stokes Structure to calculate Py Pz (polarization vector components)
;
; Common Blocks: none
;
; Called by FOR_DRIVE, FOR_PLOTFITS
;
; Calls FOR_CHANGEREF
;
; Written by Jim Dove, Sarah Gibson 2010-2014
; Version 2.0 July 2014
;
; Sept 2021 -- revised commented out conditional OI statement (no real change)
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;  March 2023 -- put in check for 0 in denominator for atan
;---

  mdtor=!dpi/180d0

  useoccult = occult > 1

  dx=GridPramsStruct.dx
  dy=GridPramsStruct.dy

  temp=ObsPramsStruct.LineName

;
; determine Py, Pz (plane of sky)
; since it is a 2d x-y plot, will rename them Px, Py

  compplot=0
;
; check to see if data
;

  if tag_exist(StokesStruct,'az') then compplot = 1

  IF compplot EQ 1 THEN BEGIN

     p=StokesStruct.p
     test=where(p eq -8888.,c)
     if c gt 0 then p[test]=0.d0

     az=StokesStruct.az
     test=where(az eq -8888.,c)
     if c gt 0 then az[test]=0.d0

;
; data have been shifted to radial reference frame
;
; alpha is the azimuth defined relative to the z-axis N-S
; (see e.g. program QU2POLVEC)
;

     thns=GridPramsStruct.ThPos/mdtor
     testwest=where(GridPramsStruct.ThPos/mdtor gt 180.d0)
     if min(testwest) ge 0. then thns[testwest]=thns[testwest]-180.
     alpha=az+thns
     alpha = alpha mod 180.
     alpha=alpha - 90.

     px=p*cos(alpha*mdtor)
     py=p*sin(alpha*mdtor)

  ENDIF ELSE BEGIN

      Q = StokesStruct.Q
      U = StokesStruct.U
     
      p = sqrt(Q*Q+U*U)

;
; calculate in N-S frame
;

      for_changeref,GridPramsStruct.ThPos,Q,U,Qprime,Uprime,type=2

      alpha=0.5*atan(Uprime,Qprime)-!dpi/2
      test=where(Qprime eq 0. and Uprime eq 0.) 
      if min(test) ne -1 then alpha[test]=sqrt(-1.)

      px=p*cos(alpha)
      py=p*sin(alpha)
      
      
  ENDELSE
  
;
;  Determine P/I if needed (P = polarization vector magnitude)
;  NOTE I AM NOW MAKING LENGTH OF VECTOR ALWAYS REFLECT L/I NOT L
;
;  if strpos(strupcase(temp), 'OI') ge 0 then begin
      I = StokesStruct.I
      test=where(I eq -8888. or I eq 0.,c)
      if c gt 0 then I[test]=1.d0
      p = p/I
      px = px/I
      py = py/I
      if c gt 0 then begin
        p[test]=0.d0
        px[test]=0.d0
        py[test]=0.d0
      endif
      test2=where(p ge 1.,d)
      if d gt 0 then begin
        p[test2]=0.d0
        px[test2]=0.d0
        py[test2]=0.d0
      endif
;  endif

  pmaxmag = max(p)
  pmedmag = median(p)
  if pmedmag eq 0 then pmedmag = pmaxmag

;
;  MAKE PLOTS
;

;
; there is a big range of lengths in stokes vectors
; this next statement should mean that the biggest line is 1/10
; of the image size.  This is important, because if lines are too big
; s.t. they extend out of the image or above UPOCCULT, they will not be
; drawn.
;
  pscalemult = (yplotrange[1]-yplotrange[0])/(10.*pmaxmag)
  if datatype(pscale) eq 'STR' then $
	if strupcase(pscale) eq 'SCALED TO DATA' then pscale = 1.d0
  if pscale gt 0.d0 then pscale=pscalemult*pscale

; note number of points to skip varies with grid size, thus ngrid/nstarrow is 
; number of points to skip.  Also put in x,y difference
; if grid not squre
; arrowave will average over skipped data in
; creating arrow length and direction
; default is  no averaging
; (most important for data)
;

   nsize=size(p)
   nxpts=nsize[1]
   nypts=nsize[2]

   xsubplot=(xplotrange[1]-xplotrange[0])/(GridPramsStruct.xrange[1]-GridPramsStruct.xrange[0])
   ysubplot=(yplotrange[1]-yplotrange[0])/(GridPramsStruct.yrange[1]-GridPramsStruct.yrange[0])

   nskipx=fix(nxpts*xsubplot/nstarrow)
   nskipy=fix(nypts*ysubplot/nstarrow)

;print,'nskipx=',nskipx,', nskipy=',nskipy
 
   xar = GridPramsStruct.xrange[0] + findgen(nxpts)*dx
   yar = GridPramsStruct.yrange[0] + findgen(nypts)*dy

   pxuse=fltarr(nypts)
   pyuse=fltarr(nypts)
   xcount=0
   xx=xar[0]
   underoccult=fltarr(nypts) 
   for i=0,nxpts-1 do begin
      x = xar[i]
      ycount=0
      yy=yar[0]
      jj=0
      for j=0,nypts-1 do begin
         y = yar[j]
	 pxuse[j]=pxuse[j]+px[i,j]
	 pyuse[j]=pyuse[j]+py[i,j]
	 if px[i,j] eq 0 or py[i,j] eq 0 then underoccult[j]=1
         if xcount eq nskipx and ycount eq nskipy then begin
            if arrowave eq 0 then begin
	     pxusept=px[i,j] 
	     pyusept=py[i,j]
 	    endif else begin
	     pxusept=total(pxuse[jj:j])/(nskipx+1)/(nskipy+1)
	     pyusept=total(pyuse[jj:j])/(nskipx+1)/(nskipy+1)
             test=where(underoccult[jj:j] eq 1)
	     if min(test) ne -1 then begin
	      pxusept=0.
	      pyusept=0.
	     endif
	    endelse
	    if nskipx gt 0 and arrowave eq 1 then xmid=xx+(x-xx)/2. else xmid=x
	    if nskipy gt 0 and arrowave eq 1 then ymid=yy+(y-yy)/2. else ymid=y
	    if Pscale gt 0. then begin
              Xpoint = [xmid-Pscale*pxusept,xmid+Pscale*pxusept]
              Ypoint = [ymid-Pscale*pyusept,ymid+Pscale*pyusept]
	    endif
	    if Pscale le 0. then begin
	      phyp=sqrt(pxusept^2+pyusept^2)
	      if phyp eq 0. then phyp = 1.
              range=max([(xplotrange[1]-xplotrange[0]),(yplotrange[1]-yplotrange[0])])
 	      if Pscale eq 0. then Pnorm=range*0.05/phyp
 	      if Pscale lt 0. then Pnorm=-range*Pscale*0.05/phyp
              Xpoint = [xmid-Pnorm*pxusept,xmid+Pnorm*pxusept]
              Ypoint = [ymid-Pnorm*pyusept,ymid+Pnorm*pyusept]
	    endif
	    if exist(Xpoint) and exist(Ypoint) then if Xpoint[0] ge xplotrange[0] and Xpoint[0] le xplotrange[1] and Xpoint[1] ge xplotrange[0] and Xpoint[1] le xplotrange[1] and Ypoint[0] ge yplotrange[0] and Ypoint[0] le yplotrange[1] and Ypoint[1] ge yplotrange[0] and Ypoint[1] le yplotrange[1] and sqrt(Xpoint[0]^2+Ypoint[0]^2) gt useoccult and sqrt(Xpoint[1]^2+Ypoint[1]^2) gt useoccult then begin 
             upuse=1
             if upoccult gt 0 then $
                if sqrt(Xpoint[0]^2+Ypoint[0]^2) ge upoccult or sqrt(Xpoint[1]^2+Ypoint[1]^2) ge upoccult then upuse=0
	     if upuse eq 1 and sqrt(pxusept^2+pyusept^2) gt sminuse then $
		oplot,Xpoint,Ypoint,linestyle=0,thick=stkthick,color=stkcolor
            endif
         endif 
         if ycount eq nskipy then begin
	    ycount=0 
            yy=y
	    jj=j
         endif else ycount = ycount + 1
      endfor
      if xcount eq nskipx then begin
	    xcount = 0 
            xx=x
            pxuse=fltarr(nypts)
            pyuse=fltarr(nypts)
            underoccult=fltarr(nypts) 
      endif else xcount = xcount + 1
   endfor
end

