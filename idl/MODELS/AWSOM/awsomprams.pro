PRO AWSOMPRAMS,outarray,date=date,working_dir=working_dir,cubename=cubename,$
                   cuberot=cuberot, magmod=magmod,$
                   bout=bout, bnorm=bnorm,nreinit=nreinit,$
	           hydro=hydro,densprof=densprof,te=te,velimpose=velimpose,deltar=deltar,$
                   coldpts=coldpts,dthres=dthres,p2fill=p2fill,$
		   nofield=nofield,$
		   saveprams=saveprams,readprams=readprams
;+
;
;Name: 
;   AWSOMPRAMS
;
;Purpose:
;          To set up the structure containing the information needed
;          to run an AWSOM data cube interpolation. This procedure is to  be
;          called by the driver routine with keyword inputs that
;	   with resulting structure OUTARRAY assigned to ModPramsStruct 
;	   (with ModPramsStruct.name='awsom') 
;
;   	ESSENTIALLY JUST CALLS NUMCUBEPRAMS, BUT WITH GETAWSOM FLAG SET
;               IF CUBENAME AND READPRAMS NOT SET 
;               If GETAWSOM not set, acts exactly like NUMCUBEPRAMS
;
; Written Sarah Gibson and Judit Szente
;  Sept 2023
;
;  Oct 2023 -- changed getawsom=-1 so that if cube exits can still create appropriate AWSOM parameter file 
;  Nov 2023 -- changed velimpose default to tiny-- this will zero
;  out model velocities as default but can be reinstated by setting
;  velimpose=0
;  Dec 2023 -- added nofield parameter

getawsom=-1

if keyword_set(cubename) eq 0 and keyword_set(readprams) eq 0 then getawsom=1

;
; make zeroed out velocity the default
; otherwise comp line profiles end up with double peaks 
; this is something we are looking into
;

;default,velimpose,0.
default,velimpose,1e-10

NUMCUBEPRAMS,outarray,date=date,working_dir=working_dir,cubename=cubename,$
                   cuberot=cuberot,magmod=magmod,nofield=nofield,$
                   bout=bout, bnorm=bnorm,nreinit=nreinit,$
		   coldpts=coldpts,dthres=dthres,p2fill=p2fill,$
	           hydro=hydro,densprof=densprof,te=te,velimpose=velimpose,deltar=deltar,$
		   saveprams=saveprams,readprams=readprams,getawsom=getawsom

END
