pro for_atom
;+
;
; NAME:
;       for_atom
;
; PURPOSE: Reads, stores atomic data in ASCII (CLE) format from file
;

; EXPLANATION:
;       Stores atomic data in structures
;   
;       Atomic parameters are stored in structure atom as follows:
;       ATOMID : string containing atomic ID (usually just 'HE', 'FE', 'O')
;       ABUND  : real containing abundance relative to hydrogen (Log scale, H=12)
;       AWGT   : atomic mass in atomic units (12C=12.0)
;       NK     : number of levels, continuum levels included
;       NRAD   : number of radiative transitions
;       NLINE  : number of bound-bound transitions
;       NCONT  : number of bound-frconst.ee transitions i detail
;       LABEL  : e.g., ''N IV 1S2 2S2 1SE 0'' 
;       G      : statistical weight of level
;       EV     : energy in ev  (input in cm-1)
;       F      : for lines   : *absorption* oscillator strength
;                for continua: cross-section at limit
;                the wavelength dependence of the cross-section is
;                assumed to be A=A0*(NY0/NY)**3 if not given explicitly
;       A      : einstein A coefficient
;       B      :          B
;       
;       IRAD(K): lower level for radiative transition k
;       JRAD(K): upper             "
;       ALAMB  : vacuum wavelength in angstrom
;                in printout routines it is printed as either vacuum or air
;                depending on the value (.lt.2000 vacuum, .gt.2000 air)
;       HNY4P  : h*ny/4pi, ny in units of a typical doppler width
;       
; CALLING SEQUENCE: 
;       atom
;
; CALLS FOR_COLRD, FOR_FREQL, FOR_CSTRIP
; CALLED BY FOR_ICLESTART
;
;-
@include.icle
@include.icle.input
@include.icle.atom
@include.icle.thermal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; define the basic atomic structures
atom = {atom,atomid:'',abnd:0.d0,awgt:0.d0,nk:0l,nline:0l,nrad:0l}
;   
; structure 'levels':
;   
lvl = {lvl,ev:0.d0,label:'',ion:0,g:0.d0,coupling:'',glande:0.d0,$
       n:0, eff:0.d0, smalll:0, active:0, tsp1:0, bigl:0, $
       parity:'', tjp1:0, term:intarr(3), orb:intarr(3), meta:0, ref:''}
;
; structure 'trn':   
;
; this next line overwrites mq as set in for_param_icle and messes things up
;mq = 50            ; maximum number of allowed wavelengths for each transition
;so I have added this
mq=param.mq

trn={trn, irad:0, jrad:0, alamb:0.d0,f:0.d0, $
     qmax:0.d0, q0:0.d0, nq:0, q:dblarr(mq), wq:dblarr(mq), frq:dblarr(mq), $
     ga:0.d0, gw:0.d0, gq:0.d0, a:0.d0, bij:0.d0, bji:0.d0, type:'', multnum:0,$
     phi:dblarr(mq),  phip:dblarr(mq),   emiss:dblarr(5, mq),  emerge:dblarr(5,mq)}
;   
; structure 'col' 
;
mcol = 20         ; maximum number of data temperatures for collisional tables
; y2 is the spline 2nd deriv
col={col,nt:0,$
     temp: dblarr(mcol), $
     key: '' , $
     ihi: 0, ilo:0, $
     lab: '' ,$
     type:0,$
     mhi:0,$
     mlo:0,$
     data: dblarr(mcol),y2: dblarr(mcol),$
     ref:''}
;
;  useful constant combinations
;
hce=const.hh*const.cc/const.ee*1.d8
hc2=2.*const.hh*const.cc *1.d24
hck=const.hh*const.cc/const.bk*1.d8
ek=const.ee/const.bk
hny4p=const.hh*const.cc/input.qnorm/4.d0/const.pi*1.d-5
;
str=''
for_cstrip,'ATOM'
openr,lu,/get,'dums.dat'
readf,lu,str
atom.atomid=getwrd(str)
xx=dblarr(2)
readf,lu,xx
atom.abnd=xx[0]
atom.awgt=xx[1]*const.uu
nn=lonarr(4)
readf,lu,nn
atom.nk=nn[0]
atom.nline=nn[1]
;atom.ncont=nn[2]
;atom.nrfix=nn[3]
atom.nrad=nn[1]+nn[2]
;
;  krad gives the transition corresponding to certain levels
;
krad=lonarr(atom.nk,atom.nk)-1
ttype=krad*0;
;  while reading, check that just one ion is present
;
jmx=0
;
; now read the data
;
lvl = replicate(lvl,atom.nk)
str=''
for i=0,atom.nk-1 do begin
   readf,lu, str
;
; strip out the stuff between ' and '
;
   lab=getwrd(str,delim="'",1)
   lvl(i).label=lab
   str=str_replace(str_replace(str,"'",""),lab,'') 
   lvl(i).ev = double(getwrd(str,0))*const.hh*const.cc/const.ee
   lvl(i).g = double(getwrd(str,1))
   lvl(i).ion = long(getwrd(str,2))
   lvl(i).glande = double(getwrd(str,/last))
endfor
;print,lvl.ev
;
; remove, e.g., 'c iii' from labels, if present, and other "trash"
for ii = 0,atom.nk-1 do begin 
   if(strupcase(getwrd(lvl(ii).label) ) eq strupcase(atom.atomid)) then $
      lvl(ii).label = getwrd(lvl(ii).label,2,50)
endfor
;
;  transitions
;
;
;  bound-bound transitions in detail
;  calculate lambda, a and b
;  if qmax or q0.lt.0 frequency points in doppler units are read
;
kt=0
trn = replicate(trn(0),atom.nrad)
;
if (atom.nline gt 0) then begin
   j = 0 &  i = 0 &  f2 = 0.d &  nq2 = 0 &  qmax2 = 0.d 
   q02 = 0.d0 &  io2 = 0 &  ga2 = 0.d &  gw2 = 0.d &  gq2 = 0.d
  for kr=0l,atom.nline-1 do begin
    readf,lu, j,i,f2,nq2,qmax2,q02,io2,ga2,gw2,gq2
    if(nq2 gt mq) then message,/info,'input nq2='+strn(nq2)+$
       '> mq='+strn(mq)+'   too many wavelengths'
    dn = i &  up = j
    if(lvl(j-1).ev lt lvl(i-1).ev) then begin 
       dn = j &  up = i
    endif
; subtract 1 from indices.
    i=dn-1
    j=up-1
    krad(i,j)=kr
    krad(j,i)=krad(i,j)
;
    trn(kr).f=f2
    trn(kr).nq=nq2 
    trn(kr).qmax=qmax2
    trn(kr).q0=q02
    trn(kr).ga=ga2   
    trn(kr).gw=gw2
    trn(kr).gq=gq2
    if(trn(kr).qmax lt 0.0) or (trn(kr).q0 lt 0.0) then begin
      dum=dblarr(nq2)
      readf,lu, dum
      trn(kr).q=dum
    endif
    trn(kr).irad=i
    trn(kr).jrad=j
    trn(kr).alamb=hce/(lvl(j).ev-lvl(i).ev)

;
;  for dipole transitions  
;
;    trn(kr).a=trn(kr).f*6.6702e15*lvl(i).g/(lvl(j).g*trn(kr).alamb*trn(kr).alamb)
; changed this to be the same as fortran

    trn(kr).a=trn(kr).f*6.671e15*lvl(i).g/(lvl(j).g*trn(kr).alamb*trn(kr).alamb)
    trn(kr).bji=(trn(kr).alamb)*(trn(kr).alamb)*(trn(kr).alamb)*trn(kr).a/hc2
    trn(kr).bij=(lvl(j).g/lvl(i).g)*trn(kr).bji


; fill ttype array (type of atomic transition

          ttype(i,j)=0l
;
;  use quantum numbers to identify the type of transition
;  trtype(i,j)=+1 : e1
;  trtype(i,j)=+2 : e2
;  trtype(i,j)=-1 : m1
;  trtype(i,j)=+3 : e1/spin changing
;  trtype(i,j)=-3 : e1 forbidden/spin changing
;  trtype(i,j)= 0 : otherwise
;
;  spin allowed?
          sallow=lvl(i).tsp1 eq lvl[j].tsp1
; parity change?
          pallow=lvl(i).parity ne lvl[j].parity
;  e1:
          rji=0.5d0*(lvl[i].g-1.d0)
          rjj=0.5d0*(lvl[j].g-1.d0)
          isumm=round(rji+rjj)
          idiff=abs(round(rji-rjj))
          if(sallow  and  pallow  and  isumm ne 0  and  idiff le 1) then ttype(i,j)=1l
;  e2:
          if(sallow and ( not  pallow) and isumm ge 2  and idiff le 2) then ttype(i,j)=2l
;  m1:
          if(sallow  and  ( not  pallow)  and  isumm ne 0 and  idiff le 1) then ttype(i,j)=-1l
;  e1 but spin changing
          if(( not  sallow)  and  pallow  and  isumm ne 0 and  idiff le 1) then ttype(i,j)=3l
;  e1-forbidden but spin changing
          if(( not  sallow)  and  ( not  pallow)  and  isumm ne 0 and  idiff le 2) then ttype(i,j)=-3l
          ttype(j,i)=ttype(i,j)
  endfor
endif
;
; get frequencies in lines, quadrature points and weights
;
for_freql
;
;  get quantum numbers
;
;qn,ok = ok
;
;  get collisional data
;
for_colrd,lu
;
free_lun,lu
;
;  for the thermal common block
;
pop=dblarr(atom.nk) & pstar=pop
c=dblarr(atom.nk,atom.nk)
cmm=dblarr(atom.nk,atom.nk,2*param.mjmax+1,2*param.mjmax+1)

return
end

