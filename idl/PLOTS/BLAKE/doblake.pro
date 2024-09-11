pro doblake,filename,savescale=savescale

;
; use radial gradient filter designed by Blake Forland
;
; Called by FOR_PLOTFITS if keyword "blake" used
;
; Calls EXPFIT, MPFITFUN
;
; Written by Blake Forland
;
; Version 2.0 July 2014
;--

read_sdo,filename,index,raw

h=index
data=raw
if index.lvl_num eq 1. then aia_prep,index,raw,h,data

wavelength=h.wavelnth
aia_lct, wave=wavelength,/load

if n_elements(savescale) eq 0 then begin

  x=((findgen(h.naxis1)-h.crpix1)*h.cdelt1)#(fltarr(h.naxis1)+1)
  y=((fltarr(h.naxis2)+1)#(findgen(h.naxis2)-h.crpix2)*h.cdelt2)
  r=sqrt(x^2+y^2)

  sr=sort(r)
  sx=r[sr]
  sim=data[sr]
  lminp=min(where(sx ge h.rsun_obs))
  dminp=min(where(sx lt h.rsun_obs))
  maxp=n_elements(data)-1
  lx1=sx[lminp:maxp]
  ly1=sim[lminp:maxp]
  dx1=sx[dminp:maxp]
  dy1=sim[dminp:maxp]
  err=1
  start=[495516,50]
  lfit=mpfitfun('expfit',lx1,ly1,err,start)
  dfit=mpfitfun('expfit',dx1,dy1,err,start)
  save,filename=filename+'.sav',lfit,dfit,r

endif else restore,savescale

limb=bytscl(data/expfit(r,lfit),min=0,max=4)
disk=bytscl(data/expfit(r,dfit),min=0,max=5)
;limb=unsharp_mask(limb,amount=5)

image=(disk*(r le h.rsun_obs-1))+(limb*(r ge h.rsun_obs-1))

mwritefits,h,image,outfile='blake_'+filename

end

