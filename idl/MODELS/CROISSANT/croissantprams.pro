PRO CROISSANTPRAMS,outarray,date=date,working_dir=working_dir,$
                orientation=orientation0, width=width0,angular_extent=angular_extent0, $
                htwidthfact=htwidthfact0, legsqueeze=legsqueeze0,edgewidth=edgewidth0, $
                twist=twist0, heightprof=heightprof0, mass=mass0, $
                nloop=nloop0, cme_distance=cme_distance0, hydro=hydro0,$
                odensprof=odensprof,oT0=oT0,$
                saveprams=saveprams,readprams=readprams

;+
;
;Name: CROISSANTPRAMS
;
;Purpose: To create structure containing parameters that control the croissant CME model;
; To be called by driver routine and resulting structure will be named
; ModPramsStruct (with ModPramsStruct.name='croissant')
;
; The Croissant CME model is a large tube with
; footpoints at the Sun. In this model, the density is peaked at the boundaries
; of the flux tube, decreasing with distance from the boundaries. The tube is
; narrow near the Sun and widest at the CME apex. The tube can be centered at any longitude,
; latitude, can be twisted around the vertical symmetry axis, and manipulated in several
; other ways.
;
; This module allows users to specify various parameters that control the geometry and total mass
; of the Croissant-shaped flux tube, described below.
;
; This implementation of the Croissant model is a simple, general model for large CMEs, and is
; similar to the model developed by Thernisien et al, see
; https://iopscience.iop.org/article/10.1088/0067-0049/194/2/33/meta
; and references within. See also
; https://iopscience.iop.org/article/10.1088/0004-637X/813/1/35/meta
; https://www.aanda.org/articles/aa/abs/2017/03/aa29516-16/aa29516-16.html
; for brief descriptions of similar models. Please cite the above if you use this module for
; published work.
;
; Called by FOR_MODELDEFAULTS
;
;Keyword Inputs:
;
;Several parameters to control the distribution of CME points
;
;  ORIENTATION: orientation of croissant around its central vertical axis of symmetry. For a CME centered above the
;               equator, at the limb, orientation=90 will result in a CME seen face-on. orientation=0 will be a CME
;               seen edge-on.
;                       Units=degrees
;                       DEFAULT  0
;
;  WIDTH:
;                       Unitless factor controlling the width of the tube at the CME apex. This is multiplied by the distance
;                             of the CME from the Sun in order to replicate self-similar expansion. To be specific, this width is the radius
;                             of the tube from the central axis of the tube at the CME apex. The tube then narrows from this maximum width
;                             from the apex down to the footpoints (see htwidthfact parameter below).
;     Units=unitless
;                       DEFAULT 0.4
;
;  ANGULAR_EXTENT:
;     The range of the Croissant from one leg to the other at its widest point. For a CME viewed face-on, this
;     corresponds approximately to the position angle extent of the CME
;      Units=unitless. This parameter is multiplied by the distance
;           of the CME from the Sun in order to replicate self-similar expansion.
;                       DEFAULT  3.0
;
;  HTWIDTHFACT: A factor that controls how the tube narrows from the maximum radius at the CME apex
;                       Units=unitless
;                       DEFAULT  0.3
;
;  HEIGHTPROF: In calculating the CME density, we need a profile that reduces the density of the legs at smaller distances
;               to the Sun  (else legs are overwhelmingly bright, contrary to observations)
;                       Units=unitless
;                       DEFAULT 1.0
;
;  LEGSSQUEEZE: A factor that controls the distance between the CME footpoints. Rarely used
;                       Units= unitless
;                       DEFAULT 0.2
;
;  TWIST: twist of main CME around central vertical axis of symmetry. Very interesting CME shapes can arise from
;         quite small twists.
;                       Units=number of full rotations from footpoints to CME apex
;                       DEFAULT 0.2
;
;  MASS: Total mass of the CME. If only a portion of the CME is within the image field of view, then this mass
;           is reduced accordingly. The density is calculated based on this mass. Approximate only.
;                       Units=grams
;                       DEFAULT 3.d16
;
;  NLOOP: 	number of points defining croissant axis
;			DEFAULT 100
;
;  CME_DISTANCE: The heliocentric distance of the CME apex
;		  - the top of the central axis of the croissant, not the CME front
;                       Units=Solar radii
;                       DEFAULT 3.15
;
;  EDGEWIDTH: The width of the CME boundary 
;		- multiples rloop/3.15 -- 
;			where rloop is set by cme_distance and angular_extent and legssqueeze 
;			DEFAULT 0.02
;
; HYDRO, ODENSPROF, OT0:
;  how to handle the plasma throughout the corona. 
;	The CME density is added to this 'background' density.
;	and the croissant is given the same temperature
;	DEFAULT HYDRO 0
;		no plasma outside the croissant
;
;       FOR ODENSPROF, OT0 DEFAULTS SEE NUMCUBE/FOR_HYDRODEFAULTS
;
; BOOKKEEPING
;
; SAVEPRAMS - if keyword set to a string, write parameters to filename
;                       (which is the keyword value saveprams)
;                       or if set to 1, then replace with pramarray
;
; READPRAMS - if keyword set, read in parameters from filename
;                       (which is the keyword value filename)
;                       or if a structure than read directly

;                       NOTE, flagged keywords will overwritten
;
; ModPramsStruct Outputs:
;
;               As above, plus
;
;               NAME -- croissant-- so that procedure can be called in intensint
;               LABEL -- Croissant CME -- for plot label
;               MAGMOD -- 0 -- meaning it is NOT a magnetized model
;		ODENSPROF -- where odensprof may be converted to a vector by FOR_HYDRODEFAULTS
;		OT0 -- where OT0 may be changed in for_hydrodefaults
;
;Output: outarray - structure containing keyword output model parameters
;
;Common Blocks: None
;
; Author and history:
;      Written by Huw Morgan Aug-Sept 2020
;       edited for FORWARD code consistency (SEG) Sept. 2020
;	Feb 2022 -- cleaned up hydro etc
;		passed through 6-D vector for odensprof
;		  (for all hydro >1)
;		also change Te, Densprof to OT0, ODENSPROF
;       Dec 2022 -- added EDGEWIDTH parameter to allow thicker shell
;		changed default to .05
;-

slash=path_sep()

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

;
; set parameter defaults
;

if keyword_set(readprams) then begin
  ; read parameter file (a structure file or structure)
  case datatype(readprams) of
    'STR': restgen,inarray,file=readprams
    'STC': inarray=readprams
    else: message, 'must provide a named readprams file or a structure'
  endcase
  t=tag_names(inarray)
  for i=0,n_elements(t)-1 do void=execute(t[i]+'_rd=inarray.(i)')
endif

;Parameters
;If keyword set for a given parameter, then this is used.
;If keyword not specified, then use value from readparams if set. If readparams not set,
;uses the default values as listed in these following statements

orientation=n_elements(orientation0) eq 0?(n_elements(orientation_rd) eq 0?0.0:orientation_rd):orientation0

width=n_elements(width0) eq 0?(n_elements(width_rd) eq 0?0.4:width_rd):width0

angular_extent=n_elements(angular_extent0) eq 0?(n_elements(angular_extent_rd) eq 0?3.0:angular_extent_rd):angular_extent0

htwidthfact=n_elements(htwidthfact0) eq 0?(n_elements(htwidthfact_rd) eq 0?0.3:htwidthfact_rd):htwidthfact0
legsqueeze=n_elements(legsqueeze0) eq 0?(n_elements(legsqueeze_rd) eq 0?0.2:legsqueeze_rd):legsqueeze0

twist=n_elements(twist0) eq 0?(n_elements(twist_rd) eq 0?0.2:twist_rd):twist0

heightprof=n_elements(heightprof0) eq 0?(n_elements(heightprof_rd) eq 0?1.0:heightprof_rd):heightprof0

mass=n_elements(mass0) eq 0?(n_elements(mass_rd) eq 0?3.d16:mass_rd):mass0

nloop=n_elements(nloop0) eq 0?(n_elements(nloop_rd) eq 0?100:nloop):nloop0

cme_distance=n_elements(cme_distance0) eq 0?(n_elements(cme_distance_rd) eq 0?3.15:cme_distance_rd):cme_distance0

edgewidth=n_elements(edgewidth0) eq 0?(n_elements(edgewidth_rd) eq 0?.02:edgewidth_rd):edgewidth0

hydro=n_elements(hydro0) eq 0?(n_elements(hydro_rd) eq 0?0:hydro_rd):hydro0

for_hydrodefaults,$
        hydro=hydro,odensprof=odensprof,ot0=ot0,$
        rodensprof=odensprof_rd,rot0=ot0_rd,$
	vodensprof=vodensprof

label='Croissant CME'
name='croissant'
magmod=0

;
; set up  pram input array
;

pramarray= {Name:name,$
  cme_distance:cme_distance, $
  edgewidth:edgewidth, $
  mass:mass,  $
  nloop:nloop,  $
  orientation:orientation, $
  width:width, $
  angular_extent:angular_extent, $
  htwidthfact:htwidthfact, $
  legsqueeze:legsqueeze, $
  twist:twist, $
  heightprof:heightprof , $
  hydro:hydro,$
  ODensProf:ODensProf,$
  OT0:OT0}

; if requested, save input parameters to a file (stored in file named saveprams if it is a string)

if keyword_set(saveprams) then begin

     savefilename=saveprams
     if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then begin
      if working_dir ne '' then savefilename=working_dir+slash+saveprams
     endif

     case 1 of
         datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
         else: saveprams=pramarray
     endcase
endif

outarray= {Name:name,$
  Label:label,$
  cme_distance:cme_distance, $
  edgewidth:edgewidth, $
  nloop:nloop,$
  mass:mass,  $
  orientation:orientation, $
  width:width, $
  angular_extent:angular_extent, $
  htwidthfact:htwidthfact, $
  legsqueeze:legsqueeze, $
  twist:twist, $
  heightprof:heightprof , $
  magmod:magmod,$
  hydro:hydro,$
  ODensProf:VODensProf,$
  OT0:OT0}

END

