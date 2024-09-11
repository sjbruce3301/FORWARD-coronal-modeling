pro for_write_compinput,FCompPrams

;
; write out input files for FORCOMP program
;
; Called by FOR_INTCOMPSETUP
;
; Input FCompPrams - structure with comp parameter settings
;
; Written Sarah Gibson 2014
; Version 2.0 July 2014

openw,lun,'INPUT',/get_lun

;printf,lun,format='("TP2TE=",F3.1,",WLMIN=",F6.0,",WLMAX=",F6.0,",SMALLN=",E6.0,",QNORM=",F5.2,",")',FCompPrams.TP2TE,FCompPrams.for_WLMin,FCompPrams.for_WLMax,FCompPrams.SmallN,FCompPrams.QNorm
;printf,lun,format='("CECOEF=",E6.0,",ISUM=",I0,",ICOLL=",I0,",ISPLIN=",I0,",")',FCompPrams.CECOEFF,FCompPrams.ISum,FCompPrams.IColl,FCompPrams.ISplin

;printf,lun,format='("TP2TE=",F3.1,",WLMIN=",F6.0,",WLMAX=",F6.0,",SMALLN=",F6.0,",QNORM=",F5.2,",")',FCompPrams.TP2TE,FCompPrams.for_WLMin,FCompPrams.for_WLMax,FCompPrams.SmallN,FCompPrams.QNorm
;printf,lun,format='("CECOEF=",F6.0,",ISUM=",I0,",ICOLL=",I0,",ISPLIN=",I0,",")',FCompPrams.CECOEFF,FCompPrams.ISum,FCompPrams.IColl,FCompPrams.ISplin
;printf,lun,format='("IWATOM=",I0,",IWLINE=",I0,",IWEQI=",I0,",IDEBUG=",I0,",IWATMO=",I0,",")',FCompPrams.IWAtom,FCompPrams.IWLine,FCompPrams.IWEqi,FCompPrams.IDebug,FCompPrams.FIWatmo
;printf,lun,format='("CRTN=",A5)',FCompPrams.CRTn

printf,lun,'TP2TE=',strtrim(string(FCompPrams.TP2TE),2),',WLMIN=',strtrim(string(FCompPrams.for_WLMIN),2),',WLMAX=',strtrim(String(FCompPrams.for_WLMAX),2),',SMALLN=',strtrim(string(FCompPrams.SmallN),2),',QNORM=',strtrim(string(FCompPrams.QNorm),2),','
printf,lun,'CECOEF=',strtrim(string(FCompPrams.CECOEFF),2),',ISUM=',strtrim(string(FCompPrams.ISum),2),',ICOLL=',strtrim(string(FCompPrams.IColl),2),',ISPLIN=',strtrim(string(FCompPrams.ISplin),2),','
printf,lun,'IWATOM=',strtrim(string(FCompPrams.IWAtom),2),',IWLINE=',strtrim(string(FCompPrams.IWLine),2),',IWEQI=',strtrim(string(FCompPRams.IWEqi),2),',IDEBUG=',strtrim(string(FCompPrams.IDebug),2),',IWATMO=',strtrim(string(FCompPrams.FIWatmo),2),','
printf,lun,'CRTN=',FCompPrams.CRTn

free_lun,lun

end
