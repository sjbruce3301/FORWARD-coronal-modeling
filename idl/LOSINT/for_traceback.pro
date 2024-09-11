;*********************************************************************
 	pro for_traceback,rmod,thmod,phmod,Brmod,Bthmod,Bphmod,Brphot1,Brphot2,Bthphot1,Bthphot2,Bphphot1,Bphphot2,linelengths,open,ModPramsStruct,nreinit=nreinit

;+
; Name: FOR_TRACEBACK
;
; Purpose: traces field lines from point rmod,thmod,phmod and returns magnetic vector
; 		at photosphere for open field lines, in model frame of reference
;		Currently set up just for PFSS, using DeRosa's PFSS_TRACE_FIELD code
;
; INPUTS
;
;	rmod - radial position of point from which field line will be drawn 
;	thmod - theta (colatitude) of point 
;	phmod - phi (longitude) of point
;
;	Brmod - radial component of field at point from which field line will be drawn 
;	Bthmod - theta component
;	Bphmod - phi component
; 	(note, these are not used, except as bug check)
;
;       ModPramsStruct - Structure containing name and model parameters
;
; Keyword inputs
;
;       NREINIT -- avoid loading numerical datacubes more than once
;		(not used yet; in future for numerical simulations may be)
;
;	USEINTERP -- interpolate footpoints to r=1 - default,1
;		This is actually not an input now.
;		The danger of making it an input is that it may get confusing
;		as to whether a file had this set or not.  Thefore, I will hard
;		wire it below = 1, and it can be changed as needed for debugging.
;		Due caution should be taken if this is done.
;
; Outputs 
;
;  	Brphot1/2,Bthphot1/2,Bphphot1/2 -- magnetic field at photospheric footpoints
;	linelengths- field line lengths
;       open -- set to 1 if field line through point is open; to 0 if closed
;
; Common block -- links to PFSS_DATA_BLOCK in PFSS package
;                (br,bth,bph) = on input, (r,theta,phi) components of field
;			3D CUBE in PFSSFILE
;                (str,stth,stph) = on input, contains an n-vector (where
;                                  n=number of field lines) of starting
;                                  coordinates for each field line
;			RMOD, THMOD, PHMOD above
;                (ptr,ptth,ptph) = on output, contains a (n,stepmax)-array of
;                                  field line coordinates
;                nstep = on output, an n-vector containing the number of
;                        points comprising each field line
;
; Calls: pfss_trace_field (DeRosa PFSS package)
;
; Called by: FOR_POSNOINT
;
; Written by Sarah Gibson
;
; HISTORY:
;	 Written Nov 26 2016
;	  May 2017 -- Revised to use save both footpoints and line length
;		also to use outfield instead of interpolating end points
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Dec 2022 -- added quiet to call to deRosa code
;-


@pfss_data_block

CoordSize=size(rmod)
nx=CoordSize[1]
NDim=CoordSize[0]
nunwrapall=product(CoordSize[1:NDim]) ; map input onto 1-D arrays of dimension nunwrapall
nunwrapall=long(nunwrapall)

;
; start points and B values
;

str=rmod
stth=thmod
stph=phmod
Bstr=Brmod
Bstth=Bthmod
Bstph=Bphmod

str=reform(str,nunwrapall,/overwrite)
stth=reform(stth,nunwrapall,/overwrite)
stph=reform(stph,nunwrapall,/overwrite)

Bstr=reform(Bstr,nunwrapall,/overwrite)
Bstth=reform(Bstth,nunwrapall,/overwrite)
Bstph=reform(Bstph,nunwrapall,/overwrite)

pfss_trace_field,kind,stbr,stbth,stbph,outfield=outfield,linelengths=linelengths,/quiet

;          where kind = on output, contains kind of fieldline:
;                      -1=line starting point out of bounds
;                       0=error of some sort?
;                       1=maximum step limit reached
;                       2=line intersects inner and outer boundaries, 
;                       3=both endpoints of line lie on inner boundary, 
;                       4=both endpoints of line lie on outer boundary,
;                       5=line intersects inner and side boundaries,
;                       6=line intersects outer and side boundaries,
;                       7=both endpoints of line lie on side boundary/ies


; linelengths = an array of dimension [nunwrapall] containing the lengths of
;                  the fieldlines that were traced (in whatever units the
;                  radius variable is in)
; outfield = an array of dimension [nunwrapall,3,3], containing the
;            three components of the vector field at both
;            endpoints and the starting point for each line.
;            The first dimension indexes the fieldlines, the
;            second dimension indexes the points of interest
;            (endpoint #1, starting point, endpoint #2), and
;            the third dimension indexes the coordinate
;            (r,theta,phi).

;
; now figure out open vs closed from "kind"
;

openlines=where(kind eq 2)
closedlines=where(kind eq 3)
surfacepole=where(kind eq 5)
openpole=where(kind eq 6)
weird=where(kind ne -1 and kind ne 2 and kind ne 3 and kind ne 6 and kind ne 5)

if min(openpole) ne -1 then begin
 nw=size(openpole)
 nw=nw[1]
 print,nw,' open field lines close to poles..'
endif

if min(surfacepole) ne -1 then begin
 nw=size(surfacepole)
 nw=nw[1]
 print,nw,' undetermined field lines close to poles..'
endif

if min(weird) ne -1 then begin
 nw=size(weird)
 nw=nw[1]
 for i = 0,nw-1 do begin
  if kind[weird[i]] eq 1 then print,'maximum step limit reached for field line'
  if kind[weird[i]] eq 4 or kind[weird[i]] eq 7 then print,'disconnected line'
  if kind[weird[i]] eq 8 then print,'null'
 endfor
endif

open=dblarr(nunwrapall)+2
open[openlines]=1
open[openpole]=1
open[closedlines]=0

;
; now the field at the footpoints
;

rphot1=str*0.0
thphot1=str*0.0
phphot1=str*0.0
rphot2=str*0.0
thphot2=str*0.0
phphot2=str*0.0

;
; I have noticed that the footpoints are a little below the
; photosphere, so have tried to see if I can improve things by
; interpolating to exactly r=1.
;

;
; hard wiring here to avoid confusion
;

useinterp=1

if useinterp eq 1 then begin
 for i = 0,nunwrapall-1 do begin
;
; find the true photosphere and interpolate values there
; we want to only use relevant segments near feet
;
; footpoint 1 (both closed and open field lines)
;
  if kind[i] eq 2 or kind[i] eq 3 then begin
   testabove=where(ptr[*,i] ge 1.)
   if min(testabove) ne -1 then begin
    flopruse=ptr[0:testabove[0],i]
    floptuse=ptth[0:testabove[0],i]
    floppuse=ptph[0:testabove[0],i]
   endif else stop
   zero=where(flopruse eq 0.)
   if min(zero) ne -1 then stop
   iphoto=get_interpolation_index(flopruse,1.)
   rphot1[i]=interpolate(flopruse,iphoto)
   thphot1[i]=interpolate(floptuse,iphoto)
   phphot1[i]=interpolate(floppuse,iphoto)
  endif
;
; footpoint 2 (just closed field)
;
  if kind[i] eq 3 then begin
   testabove=where(ptr[0:nstep[i]-1,i] ge 1.)
   if min(testabove) ne -1 then begin
    nel=size(testabove)
    nel=nel[1]
    flopruse=ptr[testabove[nel-1]:nstep[i]-1,i]
    floptuse=ptth[testabove[nel-1]:nstep[i]-1,i]
    floppuse=ptph[testabove[nel-1]:nstep[i]-1,i]
   endif else stop
   zero=where(flopruse eq 0.)
   if min(zero) ne -1 then stop
   iphoto=get_interpolation_index(reverse(flopruse),1.)
   rphot2[i]=interpolate(reverse(flopruse),iphoto)
   thphot2[i]=interpolate(reverse(floptuse),iphoto)
   phphot2[i]=interpolate(reverse(floppuse),iphoto)
  endif
 endfor
 pfssmod,rphot1,thphot1,phphot1,ModPramsStruct,ModSolStruct
 brphot1=ModSolStruct.Br
 bthphot1=ModSolStruct.Bth
 bphphot1=ModSolStruct.Bph
 test1=where(rphot1 eq 0.)
 if min(test1) ne -1 then begin
   brphot1[test1] = 0.
   bthphot1[test1] = 0.
   bphphot1[test1] = 0.
 endif
 pfssmod,rphot2,thphot2,phphot2,ModPramsStruct,ModSolStruct
 brphot2=ModSolStruct.Br
 bthphot2=ModSolStruct.Bth
 bphphot2=ModSolStruct.Bph
 test2=where(rphot2 eq 0.)
 if min(test2) ne -1 then begin
   brphot2[test2] = 0.
   bthphot2[test2] = 0.
   bphphot2[test2] = 0.
 endif

;
; for debugging -- these should be almost exactly the same
;	(roundoff error only)

 testbr=(stbr-Bstr)
 testbth=(stbth-Bstth)
 testbph=(stbph-Bstph)

endif else begin
 brphot1=outfield[*,0,0]
 bthphot1=outfield[*,0,1]
 bphphot1=outfield[*,0,2]
 brphot2=outfield[*,2,0]
 bthphot2=outfield[*,2,1]
 bphphot2=outfield[*,2,2]
endelse

;
; end of interpolation section


Brphot1=reform(brphot1,CoordSize[1:NDim],/overwrite)
Bthphot1=reform(bthphot1,CoordSize[1:NDim],/overwrite)
Bphphot1=reform(bphphot1,CoordSize[1:NDim],/overwrite)
Brphot2=reform(brphot2,CoordSize[1:NDim],/overwrite)
Bthphot2=reform(bthphot2,CoordSize[1:NDim],/overwrite)
Bphphot2=reform(bphphot2,CoordSize[1:NDim],/overwrite)

open=reform(open,CoordSize[1:NDim],/overwrite)
linelengths=reform(linelengths,CoordSize[1:NDim],/overwrite)

end

;************************************************************************


