; 
pro for_freql
;
;  GIVES  FREQUENCY QUADRATURE POINTS AND CORRESPONDING WEIGHTS.
;  1.d0- OR 2.d0-SIDED CASES.  THE POINTS AND WEIGHTS ARE BASED ON
;  THE TRAPEZOIDAL RULE APPLIED TO A MAPPING FUNCTION  THAT MAPS
;  EQUIDISTANT  POINTS IN  X TO  EQUIDISTANT POINTS IN  Q  IN THE
;  CORE (Q  LT  Q0), AND EQUIDISTANT POINTS IN LOG(Q) IN THE WINGS
;  (Q  GT  Q0).
;
;  NQ     : TOTAL NUMBER OF QUADRATURE POINTS                (IN)
;  QMAX   : MAXIMUM VALUE OF FREQUENCY (DOPPLER UNITS)       (IN)
;  IND    : =1 FOR 1.d0-SIDED AND =2 FOR 2.d0-SIDED CASES      (IN)
;  Q0     : TRANSITION FROM CORE TO WING (DOPPLER UNITS)     (IN)
;  Q      : QUADRATURE POINTS                               (OUT)
;  WQ     : QUADRATURE WEIGHTS                              (OUT)
;
;
; CALLED BY FOR_ATOM
;

@include.icle.atom
;
ind=2.d0  ; two sided case only
half=0.5d0
al10=alog(10.d0)

for kr=0,atom.nline-1 do begin 
;
   if(trn[kr].qmax eq trn[kr].q0) then begin 
      dq=ind*trn[kr].qmax/(trn[kr].nq-1)
      trn[kr].q(0)=-trn[kr].qmax*(ind-1.d0)
      for j=1, trn[kr].nq-1 do begin 
         trn[kr].q(j)=trn[kr].q(j-1)+dq
         trn[kr].wq(j)=dq*2./ind
      endfor
      trn[kr].wq(0)=0.5*dq*2./ind
      trn[kr].wq(trn[kr].nq-1)=0.5*dq*2./ind
   endif  else begin 
      a=10.d0^(trn[kr].q0+.5d0)
      mx= half > trn[kr].qmax-trn[kr].q0-half
      xmax=alog10(a*mx)
;
      dx=2.*xmax/(trn[kr].nq-1)
      for j=0,trn[kr].nq-1 do begin 
         x=-xmax+(j)*dx
         x10=10.^x
         trn[kr].q(j)=x+(x10-1./x10)/a
         trn[kr].wq(j)=dx*(1.+(x10+1./x10)*al10/a)
      endfor
;
;  These next three lines are inside an ELSE in the original FREQL.F
;  and so the logic is different. I have commented them out for consistency.
; SHOULD DOUBLE CHECK WHICH IS RIGHT!
;
;      for j=1,trn[kr].nq-2 do trn[kr].wq(j)=0.5*(trn[kr].q(j+1)-trn[kr].q(j-1))
;      trn[kr].wq(0)=0.5*(trn[kr].q(1)-trn[kr].q(0))
;      trn[kr].wq(trn[kr].nq-1)=0.5*(trn[kr].q(trn[kr].nq-1)-trn[kr].q(trn[kr].nq-2))
   endelse
;
endfor
return
end
