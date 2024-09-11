function for_mg_get_url_content, url, error_message=errorMsg, $
                             response_code=responseCode, $
                             response_header=responseHeader, _extra=e

;+
; Get the content for the given URL.
;
; :Returns:
;    string, strarr
;	content of the URL
;
; :Inputs:
;    url : in, required, type=string
;       complete URL to get content for, including "http://"
;
; :Keywords:
;    error_message : out, optional, type=string
;       pass a named variable, will be set to a non empty string if an error
;       occurs in getting the content of the URL (in which case the return
;       value will be the empty string)
;    response_code : out, optional, type=boolean
;       response code for get attempt, 200 is OK
;    response_header : out, optional, type=string
;       response header for get attempt
;    _extra : in, optional, type=keywords
;       properties of IDLnetURL
;
; CALLED BY for_mg_url_dirlisting
;
; Written by Mike Galloy
; Renamed (July 2020) with for_prefix to be included in FORWARD distribution
;-
  compile_opt strictarr

  errorMsg = ''
  catch, errorNumber
  if (errorNumber ne 0L) then begin
    catch, /cancel
    errorMsg = !error_state.msg
    ourl->getProperty, response_code=responseCode, response_header=responseHeader
    if (obj_valid(ourl)) then obj_destroy, ourl
    return, ''
  endif

  ourl = obj_new('IDLnetURL', verbose=keyword_set(verbose), _extra=e)
  content = ourl->get(url=url, /string_array)
  ourl->getProperty, response_code=responseCode, response_header=responseHeader
  obj_destroy, ourl

  return, content
end
