
function for_get_forward_db,nowidgmess=nowidgmess
;+
;Name: for_get_forward_db
; 
; Purpose: Checks to see if enviroment variable FORWARD_DB is defined and the directory exists.
;
;  Project: ISSI-Cavity
;
; Inputs: None
;
; Outputs: directory name
;
; Called by FOR_GET_SPECFILE_NAMES, FOR_GOFNT
;
; History
;	Created 9-Jan-2014 T. Kucera
;	Removing final '\' or '/' on directory names for PCs  14-Mar-2014
;
; Version 2.0 July 2014
;
; Sept 2021 -- passed through nowidgmess
;-
   
        dirloc=GET_ENVIRON('FORWARD_DB')
						;check if the last character is '\'. If it is remove before checking for existence. This is needed for PCs'
	lc=strlastchar(dirloc)
	if (lc eq '\') or (lc eq '/')  then dirloc=strmid(dirloc,0,strlen(dirloc)-1)
	
	if dirloc eq '' or dir_exist(dirloc) ne 1 then begin
		if keyword_set(nowidgmess) ne 1 then d=dialog(/WARNING,'In order to run this portion of FORWARD the FORWARD_DB files must be downloaded and the system variable FORWARD_DB must be set.  See http://www.hao.ucar.edu/FORWARD/. Stopping.')
		stop
	endif 
	
	return,dirloc
end
