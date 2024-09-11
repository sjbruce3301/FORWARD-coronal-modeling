pro for_namecheckfile,date,instrument,line,name,dateuse,dateuse2,gridtype=gridtype,typecomp=typecomp,typekcor=typekcor,wlcomp=wlcomp,rheight=rheight,noshortcut=noshortcut,ucomp=ucomp

;
; this program puts information entered either via keywords or 
; extracted from a FITS header into standard form. In particular,
; NAME is the name of the saved FITS and SAV files, and DATEUSE
; is the form of the date used in calls to VSO
;
; INPUTS: DATE, INSTRUMENT, LINE
;
; OUTPUTS: NAME, DATEUSE, DATEUSE2 (endtime for VSO search)
;
; KEYWORD: 
;	   GRIDTYPE - CARRMAP or PLANEOFSKY
;	   TYPECOMP - for instrument COMP, choices are QUICKINVERT, DYNAMICS, and POLARIZATION
;            *note, UCoMP data will have combined dynamics/polarization typecomp=L1
;            * and will also vary depending on WLCOMP-- only 10747/10798 have polarization
;	   TYPEKCOR - right now only EXTAVG but we may add e.g. NEAREST or NEARNEXT and NEARBEFORE
;		note -- earlier data may not have EXTAVG in which case will grab first image of day
;		and reset TYPEKCOR=FIRSTL1
;	   WLCOMP - comp wavelength for naming
;	   RHEIGHT -- Carrington map radial height for naming
;	   NOSHORTCUT -- if redomap set in for_plotfits, won't allow wild card 
;		(forces search for closest to exact time)
;	   UCOMP -- check if UCOMP file
;	   
;
; Called by FOR_PLOTFITS
;
; Written by Sarah Gibson 2014
; 
; Version 2.0 July 2014
;
; July 2020 -- set up KCOR so that like COMP it doesnt look for time in file name for average file
;	passed through typekcor with default EXTAVG
;  2021 -- passed through gridtype and allowed for CARRMAP
;	revised conditional assigining name so removed unnecessary * for CARRMAP
;  Jan 2022 -- fixed bug where QUICKINVERT name check was ge 0 instead of lt 0
;  Aug 2023 -- added ucomp check
;--

     dateuse=anytim(date,/ccsds)
;
;Construct NAME based on instrument, observation time, wavelenth, & version tag.
;
     if strupcase(gridtype) eq 'CARRMAP' then begin
	name1 = strupcase(instrument) + '_' + strmid(dateuse,0,4) + strmid(dateuse,5,2) + strmid(dateuse,8,2)+'_'+strtrim(string(fix(rheight*100)),2)
	name2 = ''
     endif else begin
        name1 = strupcase(instrument) + '_' + strmid(dateuse,0,4) + strmid(dateuse,5,2) + strmid(dateuse,8,2) 
        name2 = '_'+strmid(dateuse,11,2) + strmid(dateuse,14,2) 
     endelse
  
     if strpos(strupcase(instrument),'COMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
        or strupcase(instrument) eq 'KCOR' then begin
           if keyword_set(noshortcut) or (strpos(strupcase(typecomp),'QUICKINVERT') lt 0 and strupcase(typecomp) ne 'FIRSTL1') or (strupcase(typekcor) ne 'EXTAVG') then name=name1+name2 else name=name1+'*'
	   if strupcase(instrument) eq 'KCOR' then begin
            name=name+ '_' + strtrim(strupcase(typekcor),2) 
           endif else begin
            name=name+ '_' + strtrim(strupcase(typecomp),2) + '_' + strtrim(wlcomp,2)
            if keyword_set(ucomp) then name='U_'+name
           endelse
     endif else begin
           if keyword_set(noshortcut) or (strupcase(instrument) ne 'EIT' and strupcase(instrument) ne 'SWAP') then name=name1+name2 else name=name1+'*'
           name=name+'_' + strtrim(string(line),2)
     endelse

;
; now end time for VSO
;
     if strupcase(instrument) ne 'XRT' and strupcase(instrument) ne 'EIT' then dateuse2=anytim(date)+60.*30. else dateuse2=anytim(date)+24.*60.*60.
     if strupcase(instrument) eq 'SWAP' then dateuse2=anytim(date)+60.*60.
     dateuse2=anytim(dateuse2,/ccsds)

print,name
end
