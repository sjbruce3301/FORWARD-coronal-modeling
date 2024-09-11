;
; template to read the fortran output of CLE modified for Jim Dove.
;
; 
openr,lu,/get,/f77_un,'F77OUTPUT'
;openr,lu,/get,/f77_un,'F77colls'
ni=315 & nj=2500L  ; these are the 
Nx = fix(sqrt(nj))
readu,lu,ni
readu,lu,nj
print,'arrays are of size (nlos,nunwrap)',ni,nj
;
nlines=0
readu,lu, nlines   ; number of lines which have been outputted by CLE
print,'nlines = ',nlines

kr=0 & lambda=1.0 & nq=0 & iq=0
mq=100
dlambda=fltarr(mq,nlines)
;
; read lines and wavelengths calculated
;
print
for i=0,nlines-1 do begin 
   readu,lu,kr
   readu,lu,lambda
   readu,lu,iq
   nq=[nq,iq]
   dl=fltarr(iq)
   print,kr,lambda,iq,form='("Line ",i3," Lambda=",f8.2," Nlambda=",i3/"delta lambda:")'
   readu,lu,dl
   print,dl,form='(10f9.2)'
   dlambda(0:iq-1,i)=dl
endfor
nq=nq(1:*)
mq=max(nq)
dlambda=dlambda(0:mq-1,*)
;
; see if the full profiles were written to the file
;
iwline=0
readu,lu,iwline
stokes=fltarr(5,nj,nlines)
if(iwline ne 0) then begin 
   print,'Full line profiles are given'
   stokesl=fltarr(5,nj,nlines,mq)
endif else begin 
   print,'Only wavelength-Integrated line data are given'
endelse
;
; read the output Stokes vectors
;
st=fltarr(5)
for ii=0L,nj-1 do begin 
   for kr=0,nlines-1 do begin 
      if(iwline ne 0) then begin ; wavelength information is given
         sta=fltarr(nq(kr))
         for im=0, 4 do begin 
            readu,lu,sta
            stokesl(im,ii,kr,0:nq(kr)-1)=sta
         endfor
      endif
      readu,lu,st
      stokes(*,ii,kr)=st
   endfor
endfor
free_lun,lu
;
;
;  reform the array to the (y,z) plane
;
sout=fltarr(5,Nx,Nx,nlines)
if(iwline ne 0) then soutl=fltarr(5,Nx,Nx,nlines,mq)
!p.multi=[0,2,2]
for is=0,4 do begin  ; loop over stokes component
   for kr=0,nlines-1 do begin ; loop over line 
      s=reform(stokes(is,*,kr))
      sout(is,*,*,kr)=reform(s,[Nx,Nx],/over)
      if(iwline ne 0) then begin 
         for iq=0,nq(kr)-1 do begin  ; loop over wavelength
            s=reform(stokesl(is,*,kr,iq))
            soutl(is,*,*,kr,iq)=reform(s,[Nx,Nx],/over)
         endfor
      endif
   endfor
endfor
;
stokes=sout
if(iwline ne 0) then stokesl=soutl
print,'iwline = ',iwline
print,'output is in variables stokes'
print,'  stokes=stokes(is,iy,iz,iline) where'
print,'    I:is=0, Q:is=1, U:is=2, V:is=3, W:is=4'
print,'    W= Stokes V but with zero alignment'
print
if(iwline ne 0) then begin 
   print,'output is also in variables stokesl'
   print,'  stokesl=stokes(is,iy,iz,iline,iwavelength)'
endif

;
; some example plotting....
;

i=reform(stokes(0,*,*,0))
q=reform(stokes(1,*,*,0))
u=reform(stokes(2,*,*,0))
v=reform(stokes(3,*,*,0))
valt=reform(stokes(4,*,*,0))

qu2polvec,q,u,px,py
p=sqrt(px*px+py*py)

plot_image,i,title='I'
ifixed=i ;> 1.e-20
plot_image,p/ifixed,title='P/I and direction P'
velovect,px,py,/over
plot_image,v/ifixed,title='V/I'
;write_png,'second.png',tvrd()
;
end
