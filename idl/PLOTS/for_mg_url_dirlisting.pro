function for_mg_url_dirlisting, url, _ref_extra=e

;+
; Get information from a URL that represents a directory listing for websites
; that enable them. Note: the parser of the directory listing is very brittle,
; and may not work for all web servers.
;
; :Returns:
;   array of structures with fields `name`, `link`, `date`, and `size`
;
; :Inputs:
;   url : in, required, type=string
;     URL to retrieve
;
; Keywords: 
;	_ref_extra  optional extra keywords to pass to for_mg_get_url_content
; 
; CALLS for_mg_get_url_content
;
; CALLED by for_kcorextavgtime
;
; Written by Mike Galloy; renamed with "for" prefix to be included
; in FORWARD package (July 2020)
;-
  compile_opt strictarr

  listing = for_mg_get_url_content(url, _extra=e)

  ; there are 8 header lines and 2 footer lines
  n_header_lines = 8L
  n_footer_lines = 2L
  n_entries = n_elements(listing) - n_header_lines - n_footer_lines

  if n_entries eq 0 then return,!null 

  s = replicate({name: '', link: '', date: '', size: ''}, n_entries)

  re = '<img .*> <a href="(.*)">(.*)</a>[[:space:]]*(....-..-.. ..:..)[[:space:]]*(.*)'
  for e = 0L, n_entries - 1L do begin
    r = stregex(listing[e + n_header_lines], re, /extract, /subexpr)
    s[e].link = r[1]
    s[e].name = r[2]
    s[e].date = r[3]
    s[e].size = r[4]
  endfor

  return, s
end
