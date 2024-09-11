pro for_rinput
;
;  reads an input file consisting of variable names and values
;  the variable names and values must occur in pairs:
;  name, value, name, value, etc.
;  ierr is initialized to 0.
;  if a variable in the list is not give a value in the input file,
;  a warning is issued and ierr is set to 2.
;  if a variable in the input file is not in the list
;  a warning is issued and ierr is set to 1.
;  if ierr is 2 after the read operation, stop is called
;
;  list of local variables:
;  mavar  number of real-type variables in the variable list
;  mivar  number of integer type variables in the list
;  mcvar  number of character type variables in the list
;  vname  array containing the variable names
;  cvalue character array where the variable values are put at input
;  indv   = 1 for variable names
;         =-1 for variable values
;
;  if the input list is changed, changes have to be made three places:
;  parameter (mavar=,mivar=)
;  data vname/.../
;  read(ldumi,*)
;
;  the order of variables must be the same in data vname and in
;  read(ldumi,*).
;
;  in addition the output list should be changed in winput
;  and relevant common blocks should be changed
;
;  
; CALLED BY FOR_ICLESTART
;
@include.icle.input
MAVAR=6 
MIVAR=8 
MCVAR=1
CVALUE=strarr(MAVAR+MIVAR+MCVAR)
text=''
VNAME=['TP2TE ','WLMIN ','WLMAX ','SMALLN','QNORM ','CECOEF', $
     'ISUM  ','ICOLL ','ISPLIN', $
     'IWATOM','IWLINE','IWEQI ','IDEBUG', $
     'IWATMO','CRTN  ']
vala=dblarr(mavar)
vali=lonarr(mivar)
valc=strarr(mcvar)
done=bytarr(mavar+mcvar+mivar)
;
VDESCR=['RATIO OF PROTON TO ELECTRON TEMPERATURES     ', $
               'SHORTEST WAVELENGTH OUTPUT TO FILE           ', $
               'LONGEST  WAVELENGTH OUTPUT TO FILE           ',$
               'CALCN IS SKIPPED FOR ELECT. DENS. < THIS     ',$
               'NORMALIZATION FOR FREQUENCIES [KM/S]         ',$
               'RATE COEFFICIENT FOR ELASTIC COLLISIONS       ',$
               'LEVEL INDEX TO USE FOR CONSERVATION EQN.     ',$
               'INCLUDE COLLISIONS                           ',$
               'INTERPOLATION: 2=LINEAR, 3....N= SPLINE      ',$
               'OUTPUT ATOMIC PARAMETERS TO FILE             ',$
               'OUTPUT FREQUENCY DEPENDENT LINE DATA TO FILE ',$
               'OUTPUT IONIZATION EQUILIBRIUM DATA           ',$
               'OUTPUT DEBUGGING INFO.                       ',$
               'OUTPUT CORONAL DATA TO FILE  ''ATMOS''       ',$
               'NAME OF ROUTINE SUPPLYING MAGNETIC DATA      ']
;
IERR=0
INDV=-1
openr,lu,/get,'INPUT'
str=''
tstr=str
while not eof(lu) do begin 
   readf,lu,str
   tstr+=str
endwhile
free_lun,lu

tstr= str_replace(tstr,'=',' ')
tstr= str_replace(tstr,',',' ')

for i=0,mavar-1 do begin 
   p=strpos(tstr,strcompress(vname[i],/remove_all))
   if(p ge 0) then begin  ; found the keyword
      sub=strmid(tstr,p)
      vv=getwrd(sub,1)
      vala(i)=double(vv)
;      print,vname[i],' ',vv
;      tstr=str_replace(tstr,vname[i],'')
;      tstr=str_replace(tstr,vv,'')
      done[i]=1b
   endif
endfor

for i=mavar,mavar+mivar-1 do begin 
   p=strpos(tstr,strcompress(vname[i],/remove_all))
   if(p ge 0) then begin  ; found the keyword
      sub=strmid(tstr,p)
      vv=getwrd(sub,1)
      vali(i-mavar)=long(vv)
;      print,vname[i],' ',vv
;      tstr=str_replace(tstr,vname[i]+' '+vv,'')
      done[i]=1b
   endif
endfor


for i=mavar+mivar,mavar+mivar+mcvar-1 do begin 
   p=strpos(tstr,strcompress(vname[i],/remove_all))
   if(p ge 0) then begin  ; found the keyword
      sub=strmid(tstr,p)
      vv=getwrd(sub,1)
      valc(i-mavar-mivar)=vv
      print,vname[i],' ',vv
      done[i]=1b
;      tstr=str_replace(tstr,vname[i]+' '+vv,'')
   endif
endfor

input={TP2TE:vala[0],WLMIN:vala[1],WLMAX:vala[2],SMALLN:vala[3],QNORM:vala[4],CECOEF:vala[5],$
      ISUM:vali[0],ICOLL:vali[1],ISPLIN:vali[2],$
      IWATOM:vali[3], IWLINE:vali[4], IWEQI:vali[5], IDEBUG:vali[6], IWATMO:vali[7],$
      CRTN:valc[0]}


RETURN
END
