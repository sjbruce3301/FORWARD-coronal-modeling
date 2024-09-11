pro for_colrd,lu
;
;
; purpose: store collisional data for use with for_colcal
;          and initialize collisional data to 0.d0
; inputs:
;     (read from file 'atom')
; outputs:
;     keywords and data are stored in common block  coll
; common:
;     catom coll catmos catmo2 cconst cinput clu
; restrictions:  only bound-bound collisions are treated. 
;     elastic collisions are not yet included but can 
;     easily be set using the diagonal comp1.d0nts
;
;
; CALLED BY FOR_ATOM
;

@include.icle
@include.icle.atom
;
tgrid=dblarr(param.mtgrid)
cgrid=tgrid
key=''
;     
;     assume initially that data are given independent of temperature:
;     i.e. that ntemp is originally set = 1
;     
ncol=0
ntemp=1
ctmp=col[0]  ; use first one.
;     
;     READ THE KEYWORD 'KEY' AND ASSOCIATED PARAMETERS
;     
str=''
readf,lu,str
;print,lu,str
while not eof(lu) do begin 
again:
   READf,lu,KEY
   NCOL+=1
   key=strcompress(key,/remove_all)
   case key of 
      '': begin 
         NCOL=NCOL-1
         GOTO, again
      END
      'END': begin 
         NCOL=NCOL-1
         goto, zero_and_exit
      end
      'TEMP': begin 
         NCOL=NCOL-1
         str=''
         readf,lu,str
         ntemp=fix(getwrd(str))
         wloop:
         last=getwrd(str,nwords=nwords)
         if((nwords-1) lt ntemp) then begin 
            str1=''
            readf,lu,str1
            str+=str1
            goto,wloop
         endif
         for it=0,ntemp-1 do begin 
            tgrid(it)=double(getwrd(str,it+1))
         endfor
;         print,tgrid[0:ntemp-1]
         if(ntemp  gt  param.mtgrid) then  message,' colrd: work arrays (tgrid) too small'
         tgrid=tgrid[0:ntemp-1]
      end
      'OHM': begin 
         ctmp.key=key
;         print,key
         il=0l & ih=0l
         cgrid=fltarr(ntemp)
         readf,lu,il,ih,cgrid
         ctmp.ihi=ih > il
         ctmp.ilo=il < ih
         ctmp.nt =ntemp
         ctmp.temp(0:ntemp-1)=tgrid
         ctmp.data(0:ntemp-1)=cgrid
         col=[col,ctmp]
      end
      'CPMM': begin 
         ctmp.key=key
;         print,key
         il=0l & ih=0l & ml =0l & mh=0l
         cgrid=fltarr(ntemp)
         readf,lu,il,ih,ml,mh,cgrid
         ctmp.ihi=ih
         ctmp.ilo=il
         ctmp.mhi=ml
         ctmp.mlo=mh
         ctmp.nt =ntemp
         ctmp.temp(0:ntemp-1)=tgrid
         ctmp.data(0:ntemp-1)=cgrid
         col=[col,ctmp]
      end
   endcase
   GOTO,again
endwhile
zero_and_exit:
col=col[1:*]
col.ihi-=1
col.ilo-=1
;
;  store splines
;
n=n_elements(col.ihi)
for ii=0,n-1 do begin 
   nt=col[ii].nt
   t=alog10(col[ii].temp[0:nt-1])
   y=col[ii].data[0:nt-1]
   col[ii].y2[0:nt-1]=spl_init(t,y,/double) ; store spline, allowing linear extrapolation at ends (2nd deriv=0)
endfor
;
return
END
