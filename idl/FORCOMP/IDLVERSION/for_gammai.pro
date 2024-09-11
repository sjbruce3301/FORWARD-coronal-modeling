function for_gammai, ij,ij1,k
;
; purpose: compute inelastic multipolar collisional rate
;
; inputs:
;       ij    upper level index
;       ij1   lower level index
;       k     multipolar index
; outputs:
;       gammai
; restrictions:  the total collisional rates c(i,j) are assumed
;       to be the result of 1 multipole component.  this may not be 
;       consistent with the input data. see the readme file with this program. 
;
; CALLS FOR_FUN3J
; CALLED BY FOR_CI
;       BUG FIX JUNE 2019 AA1 --> AA in CASSUME


@include.icle.input
@include.icle.atom
@include.icle.cse
@include.icle.thermal
gammai=0.d0
if(input.icoll  eq  0 or ij1 ge ij) then return,gammai
;
if(ttype(ij1,ij)  eq  1  and  k  eq  1) then gammai=c(ij1,ij)
if(ttype(ij1,ij)  eq  2  and  k  eq  2) then gammai=c(ij1,ij)
;
;  cmm formalism
;
; this is the equation  after eq (a4.2) of landi's book, for the diagonal case
; (n=n', n''=n''')
;'
aa=cse.aj(ij) 
aa1=cse.aj(ij1)
fk=double(k)
;
; here we assume for spin-changing and strong coupling cases
; that c(ij1,ij,n1,n) = c(ij1,ij)/(2*ij1+1)
; this is flagged for levels ij1 and ij when ttype(ij1,ij) < 0
;
cassume=0.d0
if(ttype(ij1,ij)  le  0  and  input.icoll  ne  2) then cassume=c(ij1,ij)/(2.d0*aa+1.d0)
;
for  an=-aa,aa do begin
   for  an1=-aa1,aa1 do begin 
      for  p=-fk,fk do begin 
         n=fix(an)
         n1=fix(an1)
         f3=for_fun3j(aa,aa1,fk,-an,an1,p)
         gammai+=(cmm(ij1,ij,n1,n)+cassume) *f3*f3 * (2.d0*fk+1.d0)/(2.d0*aa1+1.d0)
      endfor 
   endfor 
endfor 
;print,ij,ij1,gammai
;print,ttype(ij1,ij),k
return,gammai
end

