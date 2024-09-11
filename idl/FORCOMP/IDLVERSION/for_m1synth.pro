pro for_m1synth,line,r3D,theta3D,phi3D,Br,Bth,Bph,Dens,Temperature,Velocity
;
; integrates along the los emission coefficients for M1 emission lines.
;
; inputs: line, e.g.  'fe13'
;             r3d  are arrays along LOS specifying geometry and
;             magnetic and thermal structure
;
;  CALLS FOR_COLCAL, FOR_CORONA, FOR_EMISSION, FOR_SOLVE, FOR_SE0_BUILD, FOR_SE0_COPY, FOR_T0TENS, FOR_TRAP
;


@include.icle
@include.icle.input
@include.icle.atom
@include.icle.corona
@include.icle.cse
@include.icle.emerge
;
;
; to be returned, emerge structure
; initialize quantities to be integrated and the emergent intensity to
; zero
;
emerge.emerge=0.d0
emerge.emiss=0.d0
;
;  main loop over the los coordinate
;
ngx=n_elements(r3d)
for i=0,ngx-1 do begin 
;
; skip if small densities
;
   ed=dens[i]
   if(ed lt input.smalln) then goto, skip 
   gx=r3d[i]*sin(theta3d[i])*cos(phi3d[i])
   gy= r3d[i]*sin(theta3d[i])*sin(phi3d[i])
   gz= r3d[i]*cos(theta3d[i])
;       
;  get some coronal data 
;
   for_corona,r3D[i],theta3D[i],phi3D[i],Br[i],Bth[i],Bph[i],Dens[i],Temperature[i],Velocity[i]
;
;       ignore those locations where line of sight has small ion fractions 
;
   if(totn  lt  10.^(atom.abnd-12.)*input.smalln) then goto, skip
;
;
;  skip if los is on the disk
;
   rrr=gz*gz+gy*gy
   if(rrr  le  1.d0*1.001) then goto, skip
;
;  assign common block variables used by icle
;
; I commented out the next two lines because they were overwriting
; the common block variables - no difference for temp/temperature,
; but factor of qnorm for vel/velocities
;
;   temp=temperature[i]
;   vel=velocity[i]
;
;
   case i of 
;
      0: gxstep = (r3d[i+1]*sin(theta3d[i+1])*cos(phi3d[i+1])-gx)
;
      (ngx-1): gxstep = (r3d[i-1]*sin(theta3d[i-1])*cos(phi3d[i-1])-gx)
;
      else: gxstep = (r3d[i+1]*sin(theta3d[i+1])*cos(phi3d[i+1]) $
                      -  r3d[i-1]*sin(theta3d[i-1])*cos(phi3d[i-1]))
   endcase
;
;  weights for LOS integration, in physical units
;
   wtx=0.5d0*const.rsuncm*abs(gxstep)
   for_profil
   c=0.d0
   cmm=0.d0
   if(input.icoll ne 0) then begin 
      for_ltepop
      for_colcal
   endif
;
   for_se0_build_develop,ndim
;   for_se0_build,ndim
   for_solve,ndim,sol

   if input.idebug ne 0 then print,'PAY ATTENTION *****',temp,ed

   for_se0_copy,ndim,sol
;       
;       solve for emergent stokes profiles
;       
   for_t0tens,t0
;       
   for kr=0,atom.nline-1 do begin 
      if ((trn[kr].alamb ge input.wlmin) and $ 
          (trn[kr].alamb le input.wlmax)) then for_emission,kr,t0
   endfor
   for_trap,wtx
skip:
endfor
;stop
return
end

