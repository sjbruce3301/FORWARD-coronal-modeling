function for_convl,lmbda ; converts vacuum lmbda to air (>2000 a)
;+
;   convl(lmbda)
;
;            converts vacuum lmbda to air (>2000 a).
;
;  CALLED BY FORCOMP
;-
;  88-04-25  mats carlsson
;
array=size(lmbda)
if array(0) eq 0 then begin
   if (lmbda lt 2000.d0) then $
      lambda_ut = lmbda $
   else $
      lambda_ut = lmbda/(1.0d0+2.735182d-4+131.4182d0/lmbda/lmbda+ $
                          2.76249d8/lmbda/lmbda/lmbda/lmbda)
   return,lambda_ut
endif else begin
   lambda_ut=lmbda
   if(max(lmbda) gt 2000.d0) then begin
      iw=where (lmbda gt 2000.d0)
      lambda_ut(iw) = lmbda(iw)/(1.0d0+2.735182d-4+ $
                                  131.4182/lmbda(iw)/lmbda(iw)+ $
                                  2.76249d8/lmbda(iw)/lmbda(iw)/lmbda(iw)/lmbda(iw))
   endif
   return,lambda_ut
endelse
end

;*******************************************************************************

