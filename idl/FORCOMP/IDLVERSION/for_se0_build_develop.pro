pro for_se0_build_develop,ndim
;
;  this subroutine builds the coefficient matrix of the statistical
;  equilibrium equations in the b-frame, once the b-direction in the
;  s-frame, and the height over the limb of the scatterer are given.
;  a cylindrically symmetric, unpolarized radiation field is assumed.
;  quantum coherences are neglected.
;
;  set the radiation fields in B frame 
;
;
; CALLS FOR_FIELDB, FOR_R1,FOR_R2, FOR_R3, FOR_R4, FOR_R5, FOR_R6, FOR_CE, FOR_C2, FOR_C3, FOR_C5, FOR_C6
; CALLED BY FOR_M1SYNTH
;
@include.icle
@include.icle.input
@include.icle.atom
@include.icle.cse
@include.icle.corona

;for ij=0,atom.nk-1 do begin ; levels
;  for ij1=0,atom.nk-1 do begin 
; enumerate radiatively permitted transitions
;
;    kr = krad(ij,ij1)
;    if kr ne 0 then for_fieldb,kr
;  endfor
;endfor

for kr=0,atom.nline-1  do for_fieldb,kr  ; get cse.radj, radiative tensor components

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  now get the SE equations and store in cse.scoeff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
cse.scoeff*=0.

dbg=input.idebug ne 0
;dbg=1

dk=1 

i=-1
count=-1
for ij=0,atom.nk-1 do begin ; levels
   r1coeff=0.
   for_r1,ij,r1coeff  
   c1tmp0=for_ce(ij,0)
   for_c2,ij,c2coeff
   for_c6,atom.nk-1,ij,c6coeff
   for k=0,round(2.d0*cse.aj(ij)),dk do begin ; multipole order
;  
;       row number
;  
      i+=1 
      cse.weight(i)=sqrt(2.d0*cse.aj(ij)+1.d0) * (k eq 0)  
      c1tmpk=for_ce(ij,k)
      c1coeff=c1tmp0-c1tmpk
      i1=-1
      for ij1=0,atom.nk-1 do begin 
         r2coeff=0.
         r3coeff=0.
         r4coeff=0.
         r5coeff=0.
         r6coeff=0.
         kr=krad(ij,ij1)
         for_r4,ij,ij1,k,r4coeff
         if (ij1 lt ij) then for_c3,ij,ij1,k,c3coeff
         if (ij1 gt ij) then for_c5,ij,ij1,k,c5coeff
         for k1=0,round(2.d0*cse.aj(ij1)),dk  do begin 
            leqk=(k1 eq k)
            count +=1
;  
;       column number
;  
            i1+=1 
            if (ij1 eq ij) then begin 
               for_r2,ij,k,k1,0,r2coeff
               for_r6,atom.nk,ij,k,k1,0,r6coeff
               cse.scoeff(i,i1)-=r2coeff+r6coeff
               if (leqk) then cse.scoeff(i,i1)-=r1coeff+c1coeff+c2coeff+c6coeff
            endif
            if (ij1 lt ij) then begin
             if (kr ge 0) then begin ; connected by a radiative transition
               for_r3,kr,ij,ij1,k,k1,0,r3coeff
               cse.scoeff(i,i1)+=r3coeff
             endif
             if (leqk) then cse.scoeff(i,i1)+=c3coeff
            endif
            if (ij1 gt ij) then begin 
             if (kr ge 0) then begin ; connected by a radiative transition
                  for_r5,kr,ij,ij1,k,k1,0,r5coeff
                  cse.scoeff(i,i1)+=r5coeff
             endif
             if (leqk) then cse.scoeff(i,i1)+=r4coeff + c5coeff ; spontaneous decay into level from above
            endif

         endfor ; sum k1=0,2*aj
      endfor  ; sum i1=0..nk-1
   endfor ; sum k=0,2*aj
endfor  ; sum i=0..nk-1

finished:

if (i ne i1) then message, 'se_build: scoeff is not square'

ndim=i+1

if(dbg) then begin
   print,'SE matrix'

   print,cse.scoeff(0:ndim-1,0:ndim-1)
   a=cse.scoeff(0:ndim-1,0:ndim-1)
   
   LA_SVD, A, W, U, V , status=status
   print,'Condition number = ',max(w)/min(w),form='(a,e10.3)'
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	call for_cptime('for_se0:r&c',0,1,1)
;	call for_cptime('for_se0_build',0,1,1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
return
end
