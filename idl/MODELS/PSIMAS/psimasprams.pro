PRO PSIMASPRAMS,outarray,date=date,working_dir=working_dir,cubename=cubename,$
                   cuberot=cuberot, magmod=magmod,$
                   bout=bout, bnorm=bnorm,nreinit=nreinit,$
	           hydro=hydro,densprof=densprof,te=te,velimpose=velimpose,deltar=deltar,$
                   coldpts=coldpts,dthres=dthres,p2fill=p2fill,$
		   mheat=mheat,choosehigh=choosehigh,nofield=nofield,$
		   saveprams=saveprams,readprams=readprams
;+
;
;Name: 
;   PSIMASPRAMS
;
;Purpose:
;          To set up the structure containing the information needed
;          to run a MAS data cube interpolation. This procedure is to  be
;          called by the driver routine with keyword inputs that
;	   with resulting structure OUTARRAY assigned to ModPramsStruct 
;	   (with ModPramsStruct.name='psimas') 
;
;   	ESSENTIALLY JUST CALLS NUMCUBEPRAMS, BUT WITH GETMAS FLAG SET
;		IF CUBENAME AND READPRAMS NOT SET 
;		If GETMAS not set, acts exactly like NUMCUBEPRAMS
;
; Written Sarah Gibson
; Version 2.0 July 2014
;
;  June 2019 -- removed flduse keyword
;  Feb 2022 -- renamed B0 Bnorm to avoid conflict with solar B angle
;       Sept 2023 -- passed through choosehigh keyword for PSI
;			commented out naming at the end because now dealt with in NUMCUBEPRAMS
;	changed velimpose default zero to small number
;
;  Oct 2023 -- changed getmas=-1 so that if cube exits can still create appropriate AWSOM parameter file 
; Dec 2023 -- added nofield parameter


getmas=-1

if keyword_set(cubename) eq 0 and keyword_set(readprams) eq 0 then getmas=1

;
; make zeroed out velocity the default
; otherwise comp line profiles end up with double peaks 
; this is something we are looking into
; used small value because 0 should be treated as unset
;

default,velimpose,1d-10
NUMCUBEPRAMS,outarray,date=date,working_dir=working_dir,cubename=cubename,$
                   cuberot=cuberot, magmod=magmod,nofield=nofield,$
                   bout=bout, bnorm=bnorm,nreinit=nreinit,$
		   coldpts=coldpts,dthres=dthres,p2fill=p2fill,$
	           hydro=hydro,densprof=densprof,te=te,velimpose=velimpose,deltar=deltar,$
		   saveprams=saveprams,readprams=readprams,getmas=getmas,mheat=mheat,choosehigh=choosehigh

;outarray.name='psimas'
;if datatype(saveprams) eq 'STC' then saveprams.name='psimas'


END
