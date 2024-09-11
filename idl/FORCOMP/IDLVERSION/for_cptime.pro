;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Document name: for_cptime.pro
; Created by:    Phil &, HAO/NCAR, Boulder CO USA, September 29, 1995
;
; Last Modified: Fri Oct 13 15:44:27 1995 by judge (Phil &) on pika
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
PRO for_cptime, text,istart,ipri,level,open=open,CLOSE=CLOSE,help=help
;+
; PROJECT:
;       HAOS-DIPER
;
; NAME:
;       for_cptime
;
; PURPOSE: 
;       the elapsed time from one call to another
;       
; EXPLANATION:
;       
; CALLING SEQUENCE: 
;       for_cptime, 
;
; INPUTS:
;       
; OPTIONAL INPUTS: 
;       None.
;
; OUTPUTS:
;       None.
;
; OPTIONAL OUTPUTS:
;       None.
;
; KEYWORD PARAMETERS: 
;       /help.  Will call doc_library and list header, and return
;
; CALLS:
;       None.
;
; CALLED BY: FOR_SE0_BUILD
;
; COMMON BLOCKS:
;       None.
;
; RESTRICTIONS: 
;       None.
;
; SIDE EFFECTS:
;       None.
;
; CATEGORY:
;       
; PREVIOUS HISTORY:
;       Written September 29, 1995, by Phil &, HAO/NCAR, Boulder CO USA
;
; MODIFICATION HISTORY:
;       
; VERSION:
;       Version 1, September 29, 1995
;-
;
@clu
COMMON ctime,btime
IF(N_ELEMENTS(help) GT 0) THEN BEGIN
   doc_library,'ctime'
   RETURN
ENDIF
;
;
;  gives the cpu time from one call to another
;  it can be activated on up to 7 different levels [0-6].
;  the meanings of istart and ipri:
;  istart =  0   zero clock on exit
;  ipri .gt. 1   write time, ipri-1 blank lines before and
;                after printout
;
; Opening, closing
;
IF(N_ELEMENTS(open)) NE 0 THEN BEGIN
   OPENW,ltime,/get,'ctime'
   RETURN
ENDIF
;
IF(N_ELEMENTS(CLOSE)) NE 0 THEN BEGIN
   free_lun,ltime
   RETURN
ENDIF
;
IF(N_ELEMENTS(ltime) EQ 0) THEN BEGIN
   OPENW,ltime,/get,'ctime'
ENDIF 
res = fstat(ltime)
IF(NOT res.open) THEN openw,ltime,/get,'ctime'
;
;
IF(N_ELEMENTS(btime) EQ 0) THEN btime = DINDGEN(6)
;
a=systime(1)
cpt=a-btime(level)
IF(istart EQ 0) THEN btime(level)=a
IF(ipri GT 0) THEN BEGIN
   FOR i=1,ipri-1 DO PRINTF,ltime,form = '(1x)'
; 
   s1 = STRING(7-level,form = '(i2)')
   s2 = STRING(level+2,form = '(i2)')
   fmt = '(' + s1 + 'x,a12,' +s2 +'x,'+"'cpu=',"+'f9.3'+",' s')"
   PRINTF,ltime,form = fmt, text+'            ',cpt
   FOR i=1,ipri-1 DO PRINTF,ltime,form = '(1x)'
ENDIF

RETURN
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end of 'ctime.pro'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
