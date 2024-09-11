;+
; Name:
;     ps_read_hdf_3d.
;
; Purpose:
;     Reads a 3-dimensional HDF file.
;
; Inputs:
;     filename:
;         HDF filename.
;
; Keywords:
;     PERIODIC_DIM:
;         Dimension number to be made periodic (0 - 2PI). Valid values are:
;             1 - The first dimension.
;             2 - The second dimension.
;             3 - The third dimension.
;
;     HELP:
;         Flag to print documentation.
;
; Outputs:
;     scales1:
;         Scale values of the first dimension.
;
;     scales2:
;         Scale values of the second dimension.
;
;     scales3:
;         Scale values of the third dimension.
;
;     datas:
;         Data values.
;-

PRO ps_read_hdf_3d, filename, scales1, scales2, scales3, datas, $
    PERIODIC_DIM=periodicDim, HELP=help

ON_ERROR, 2

catch, errorCode

if errorCode ne 0 then begin
  print, !error_state.msg
  exit, status=!error_state.code
endif

if keyword_set(help) then begin
  doc_library, 'ps_read_hdf_3d'
  return
endif

if ~ file_test(filename, /READ) then begin
  message, 'HDF file not found: ' + filename
endif

if n_elements(periodicDim) eq 1 then begin
  if (periodicDim lt 1) || (periodicDim gt 3) then begin
    message, 'Invalid dimension number to be made periodic: ' + $
             strtrim(periodicDim, 2)
  endif
endif

sdId = hdf_sd_start(filename)

hdf_sd_fileinfo, sdId, numDatasets, numAttributes

; 4 SD-type objects = 3 dimensions for scale and 1 dimension for data
if numDatasets ne 4 then begin
  hdf_sd_end, sdId
  message, 'Unable to read ' + strtrim(numDatasets - 1, 2) + $
           '-dimensional HDF file'
endif

datasetId = hdf_sd_select(sdId, 0)
hdf_sd_getdata, datasetId, scales3
hdf_sd_endaccess, datasetId

datasetId = hdf_sd_select(sdId, 1)
hdf_sd_getdata, datasetId, scales2
hdf_sd_endaccess, datasetId

datasetId = hdf_sd_select(sdId, 2)
hdf_sd_getdata, datasetId, scales1
hdf_sd_endaccess, datasetId

datasetId = hdf_sd_select(sdId, 3)
hdf_sd_getdata, datasetId, datas
hdf_sd_endaccess, datasetId

hdf_sd_end, sdId

scales3 = double(1.0) * scales3
scales2 = double(1.0) * scales2
scales1 = double(1.0) * scales1
datas = double(1.0) * datas

; Periodic is assumed to have a range from 0 to 2PI
if n_elements(periodicDim) eq 1 then begin
  nx = n_elements(scales1)
  ny = n_elements(scales2)
  nz = n_elements(scales3)
  tmpDatas = datas

  if periodicDim eq 1 then begin
    scales1 = [scales1, scales1[0] + 2.0 * !dpi]
    datas = dblarr(nx + 1, ny, nz)
    datas[0:nx - 1, *, *] = tmpDatas
    datas[nx, *, *] = tmpDatas[0, *, *]
  endif else if periodicDim eq 2 then begin
    scales2 = [scales2, scales2[0] + 2.0 * !dpi]
    datas = dblarr(nx, ny + 1, nz)
    datas[*, 0:ny - 1, *] = tmpDatas
    datas[*, nx, *] = tmpDatas[*, 0, *]
  endif else begin
    scales3 = [scales3, scales3[0] + 2.0 * !dpi]
    datas = dblarr(nx, ny, nz + 1)
    datas[*, *, 0:nz - 1] = tmpDatas
    datas[*, *, nz] = tmpDatas[*, *, 0]
  endelse
endif

return

END
