;+
; Name:
;     ps_read_mas_run
;
; Purpose:
;     Reads a full MAS run that was downloaded from the PSI website.
;
;     The data is interpolated to a common spherical grid and normalized 
;     into to physical units.
;
;     This routine assumes that the entire corona.zip file was downloaded
;
; Inputs:
;     rundir:
;         directory where the hdf files are located
;
; Keywords:
;     SEQUENCE:
;         string of the sequence number (default '002')
;     HELP:
;         Flag to print documentation.
;
; Outputs:
;     mas: 
;            An IDL structure containing the following tags:
;     mas.r:
;            radial coordinate of the mesh [Rs]
;     mas.t:  
;            theta (latitudinal) coordinate of the mesh [radians]
;            range is from 0-pi, starting at the north pole
;     mas.p:  
;            phi (longitudinal) coordinate of the mesh [radians]
;            range is from 0-2pi, same as carrington longitude
;     mas.rho:
;            electron density in [cm^-3]
;     mas.temp:
;            temperature in [MK]
;     mas.vr:
;            r-component     of the velocity vector in [km/s]
;     mas.vt:
;            theta-component of the velocity vector in [km/s]
;     mas.vp:
;            phi-component   of the velocity vector in [km/s]
;     mas.br:
;            r-component     of the B-field vector in [Gauss]
;     mas.bt:
;            theta-component of the B-field vector in [Gauss]
;     mas.bp:
;            phi-component   of the B-field vector in [Gauss]
;-

PRO ps_read_mas_run, rundir, mas, $
   SEQUENCE=sequence, $
   HELP=help

ON_ERROR, 2

catch, errorCode

if errorCode ne 0 then begin
  print, 'ERROR! ps_read_mas_run: '+ !error_state.msg
  return
endif

if n_elements(sequence) eq 0 then sequence = '002'

if keyword_set(help) then begin
  doc_library, 'ps_read_mas_run'
  return
endif

;-- unit conversion factors
fn_rho = 1d8      ; MAS --> cm^-3
fn_t = 28.07067   ; MAS --> MK
fn_v = 481.3711   ; MAS --> km/s
fn_b = 2.2068908  ; MAS --> Gauss

;-- read in the hdf files
ps_read_hdf_3d,rundir+'/rho' +sequence+'.hdf', rh, th, ph, rho_raw
ps_read_hdf_3d,rundir+'/t'   +sequence+'.hdf', rh, th, ph, temp_raw
ps_read_hdf_3d,rundir+'/vr'  +sequence+'.hdf', rm, th, ph, vr_raw
ps_read_hdf_3d,rundir+'/vt'  +sequence+'.hdf', rh, tm, ph, vt_raw
ps_read_hdf_3d,rundir+'/vp'  +sequence+'.hdf', rh, th, pm, vp_raw
ps_read_hdf_3d,rundir+'/br'  +sequence+'.hdf', rh, tm, pm, br_raw
ps_read_hdf_3d,rundir+'/bt'  +sequence+'.hdf', rm, th, pm, bt_raw
ps_read_hdf_3d,rundir+'/bp'  +sequence+'.hdf', rm, tm, ph, bp_raw

nr=n_elements(rm)
nt=n_elements(tm)
np=n_elements(pm)

;- MAS variables are on a staggered grid --> need to interpolate them
;- to the common mesh. Use br, bt, bp to get the mesh params. 
;- These are the interpolation indexes for main and half meshes
irm = findgen(nr)                      ; interps rm --> rm
irh = interpol(findgen(nr+1), rh, rm)  ; interps rh --> rm
itm = findgen(nt)                      ; interps tm --> tm
ith = interpol(findgen(nt+1), th, tm)  ; interps th --> tm
ipm = findgen(np)                      ; interps pm --> pm
iph = interpol(findgen(np+1), ph, pm)  ; interps ph --> pm

;- now do reading, interpolation, and unit conversion
rho = interpolate(rho_raw, irh, ith, iph, /grid)*fn_rho
temp= interpolate(temp_raw,irh, ith, iph, /grid)*fn_t
vr  = interpolate(vr_raw , irm, ith, iph, /grid)*fn_v
vt  = interpolate(vt_raw , irh, itm, iph, /grid)*fn_v
vp  = interpolate(vp_raw , irh, ith, ipm, /grid)*fn_v
br  = interpolate(br_raw , irh, itm, ipm, /grid)*fn_b
bt  = interpolate(bt_raw , irm, ith, ipm, /grid)*fn_b
bp  = interpolate(bp_raw , irm, itm, iph, /grid)*fn_b

;- delete the raw variables before making the final structure
; this isnt really necessary and breaks on some IDL systems
;delvar, rho_raw, t_raw, vr_raw, vt_raw, vp_raw, br_raw, bt_raw, bp_raw
rho_raw=0
t_raw=0
vr_raw=0
vt_raw=0
vp_raw=0
br_raw=0
bt_raw=0
bp_raw=0

mas = create_struct($
   'r',    rm, $
   't',    tm, $
   'p',    pm, $
   'rho',  rho, $
   'temp', temp, $
   'vr',   vr, $
   'vt',   vt, $
   'vp',   vp, $
   'br',   br, $
   'bt',   bt, $
   'bp',   bp)

return

END
