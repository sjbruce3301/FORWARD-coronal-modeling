;**********************************************************************
pro getbound,r,wa,va,fmax

common pramsstream,thcs,phcs,alpha,m,awidth,bwidth,wmult

; first check the plus/minus pi limits
; only a problem for streamers not parallel
; to equator

fmax1=0
if m ne 0. then begin
 thmax=-1.d0*!dpi*m*sqrt(m^2+1.d0)*wa^2/(va^2+m^2*wa^2)
 phmax=m*thmax + sqrt(m^2+1.d0)*!dpi
 fmax1=exp(-(thmax/wa)^2-(phmax/va)^2)
endif

; now check north pole limit

Go= -thcs - (alpha - asin(sin(alpha)/r))
Go=Go/sqrt(1+m*m)
thmax=Go*(((1.+m*m)*wa*wa)/(wa*wa+m*m*va*va))
phmax=Go*m*(((1.+m*m)*va*va)/(wa*wa+m*m*va*va))
fmax2=exp(-(thmax/wa)^2-(phmax/va)^2)

; now check south pole limit

Go= !dpi -thcs - (alpha - asin(sin(alpha)/r))
Go=Go/sqrt(1+m*m)
thmax=Go*(((1.+m*m)*wa*wa)/(wa*wa+m*m*va*va))
phmax=Go*m*(((1.+m*m)*va*va)/(wa*wa+m*m*va*va))
fmax3=exp(-(thmax/wa)^2-(phmax/va)^2)

; check for which is biggest

fmax=fmax1
fmax=fmax > fmax2
fmax=fmax > fmax3

end
;**********************************************************************
