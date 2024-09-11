pro for_viewfromdata,usedfile=usedfile,$
	date=date,gridtype=gridtype,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,$
        ngrid=ngrid,ngy=ngy,cmer=cmer,bang=bang,aia=aia,line=line,instrument=instrument,euvia=euvia,euvib=euvib,ViewInputs=ViewInputs

;
; Set up viewer coordinate from date and/or keyword input
;
; Called by FOR_DRIVE, FOR_WIDGET_EVENT, FOR_WIDGET_CALENDAR_EVENT, FOR_WIDGET_MODEL_EVENT, FOR_WIDGET
;
; Calls FOR_PLOTFITS, GET_STEREO_LONLAT, TIM2CARR, PB0R
; REQUIRES PACKAGE SECCHI
;
; 
; Written by Sarah Gibson 2012-2013
; Version 2.0 July 2014
;  
; 1-Mar-2016 Added option for setting cmer by date if cmer='null' (USER) SEG
; 3-Sep-2021 -- defined gridtypeuse
;  	(its needed in USEDFILE because the file chosen defines the gridtype
;       and need to check for contradiction -- although note this hasn't really been tested for 
;	DATA CARRMAP)
; October 2021 -- added comment about CMER no longer being non-number

  if n_elements(date) eq 1 then begin
   if date ne '' then begin
        spacecraft='Earth'
        if keyword_set(euvia) then spacecraft='A'
        if keyword_set(euvib) then spacecraft='B'
        if keyword_set(instrument) then begin
	  if strupcase(instrument) eq 'EUVIA' then spacecraft='A'
	  if strupcase(instrument) eq 'EUVIB' then spacecraft='B'
	  if strupcase(instrument) eq 'AIA' then aia=1
        endif
	if strupcase(spacecraft) ne 'A' and strupcase(spacecraft) ne 'B' then begin
   	    if n_elements(cmer) eq 0 then begin
		cmer=tim2carr(date)
		cmer=cmer[0]
	    endif 
            if is_number(cmer) eq 0 then begin
;
; this will be the case if cmer unset
;  but should no longer happen (USER goes to default cmer=0)
;
		cmer=tim2carr(date)
		cmer=cmer[0]
            endif
   	    if n_elements(bang) eq 0 then begin
		bang=pb0r(date)
		bang=bang[1]
	    endif
	endif else begin
   	    if strupcase(spacecraft) eq 'A' then apos=get_stereo_lonlat(date,spacecraft,system='CAR',/deg,/ahead)
   	    if strupcase(spacecraft) eq 'B' then bpos=get_stereo_lonlat(date,spacecraft,system='CAR',/deg,/behind)
   	    if n_elements(cmer) eq 0 and strupcase(spacecraft) eq 'A' then cmer=apos[1]
   	    if n_elements(cmer) eq 0 and strupcase(spacecraft) eq 'B' then cmer=bpos[1]
   	    if is_number(cmer) eq 0 and strupcase(spacecraft) eq 'A' then cmer=apos[1]
   	    if is_number(cmer) eq 0 and strupcase(spacecraft) eq 'B' then cmer=bpos[1]
   	    if n_elements(bang) eq 0 and strupcase(spacecraft) eq 'A' then bang=apos[2]
   	    if n_elements(bang) eq 0 and strupcase(spacecraft) eq 'B' then bang=bpos[2]
	endelse
        ViewInputs={cmer:cmer,bang:bang}
   endif
  endif

  if n_elements(usedfile) eq 1 then begin
   if usedfile ne '' and usedfile ne ' ' then begin

     ngriduse=0
     if n_elements(ngrid) eq 0 then begin
      ngriduse=1
      ngrid=0
     endif
     if n_elements(gridtype) eq 0 then gridtypeuse='PLANEOFSKY' else gridtypeuse = gridtype
     if strpos(strupcase(usedfile),'AIA') ge 0 then aia=1
     for_plotfits,filename=usedfile,ImageMap=ImageMap,$
              ngrid=ngrid,gridtype=gridtypeuse,xxmax=xxmax,xxmin=xxmin,yymax=yymax,yymin=yymin,$
              aia=aia,/noplots
     if strupcase(gridtype) ne strupcase(gridtypeuse) then print,'WARNING -- inputted gridtype inconsistent with data file'
     gridtype=gridtypeuse
     Map=ImageMap
     DSize=size(Map.data)
     if ngriduse eq 1 then begin
	ngrid=DSize[1]
        ngy=DSize[2]
     endif
     xxmin=[min(get_map_xp(map))]
     xxmin=xxmin[0]
     xxmax=[max(get_map_xp(map))]
     xxmax=xxmax[0]
     yymin=[min(get_map_yp(map))]
     yymin=yymin[0]
     yymax=[max(get_map_yp(map))]
     yymax=yymax[0]
     if keyword_set(date) eq 0 then begin
	date=Map.time
   	if n_elements(cmer) eq 0 then cmer=Map.cmer
   	if is_number(cmer) eq 0 then cmer=Map.cmer
   	if n_elements(bang) eq 0 then bang=Map.B0
     endif else begin
;	if date ne Map.time then date=''
     endelse
     if keyword_set(line) eq 0 then line=double(Map.Line)
     if keyword_set(instrument) eq 0 then instrument=Map.Instrument
     ViewInputs={xxmin:xxmin,xxmax:xxmax,yymin:yymin,yymax:yymax,date:date,cmer:cmer,bang:bang,ngrid:ngrid,ngy:ngy,line:line,instrument:instrument}
   endif
  endif

end
