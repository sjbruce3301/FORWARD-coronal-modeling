function for_detpix_lookup,instrument

;+
; Project     : ISSI - Cavities
;
; Name        : for_detpix_lookup
;
; Purpose     : Returns the detector pixel size
;
; Syntax      : for_detpix_lookup,instrument
;
; Inputs:
;			instrument instrument name
;
; Outputs     : the detector pixel size in arcsec
;
; History     : Written, 22-Aug-2014, T. Kucera
;+

Case strupcase(instrument) of
	'AIA': 		pixsize = 0.6
	'EUVIA':	pixsize = 1.58777
	'EUVIB':    pixsize = 1.59000
	'EIT': 		pixsize = 2.62900   ;eit_pixsize()
	'SWAP': 	pixsize = 3.17   ;from '$SSW/proba2/swap/caldb/swap_response_20120611_000000.save'
	'TRACE': 	pixsize = 0.5 
	'XRT': 		pixsize = 1.0286
     else: begin
        		message,/info,$
        		   'No pixel size listed for Instrument '+instrument+'. Will use pixsize= 1 arcsec.'
     			pixsize = 1.
		end
endcase

return, pixsize

end
