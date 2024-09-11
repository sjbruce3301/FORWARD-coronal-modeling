; PRO FOR_GETByURL (see below) 
; 
; Called by MAKEMASCUBE, FOR_DIRECTDOWNLOAD
;
; Calls URL_CALLBACK  (same file)
;
; Version 2.0 July 2014
;  1-Jun-2019 use slash for PC compatibility
;  12-May-2020 (MDG) formatting and commenting
;  1-July-2020 (SG) removed slash and default for writediruse -- should be set
;	in makemascube or for_directdownload
;--

;-----------------------------------------------------------------


;+
; This is a (currently unused) callback routine that is during the `get`,
; `put`, `getFtpDirList`, and `ftpCommand` methods of the IDLnetURL allowing
; them to be cancelled.
;
; :Returns:
;   0 to cancel, 1 to continue
;-

function url_callback, status, progress, data

  ; print the info msgs from the url object
  print, status
  
  ; return 1 to continue, return 0 to cancel
  return, 1
end


;+
; original from L. Sitongia
; modified for I/O by S. Gibson 2014
;
;  Input: URLPATHUSE, WRITEDIRUSE
;
;  Output: FN File name (written to writediruse)
;
;  Keyword: RSPCODE -- tells if file not available

;
; test for PC
;-
pro for_getbyurl, filenames, urlpathuse, $
                  rspcode=rspcode, $
                  writediruse=writediruse, $
                  usehost=usehost

  ; if there are any errors in the rest of this routine, execution will jump
  ; back to this IF statement with a non-zero errorstatus
  catch, errorstatus
  if (errorstatus ne 0) then begin
    catch, /cancel
    
    ; display the error msg in a dialog and in the idl output log
    ;r = dialog_message(!error_state.msg, title='url error', /error)
    ;print, !error_state.msg
    
    ; get the properties that will tell us more about the error
    ourl->getproperty, response_code=rspcode, $
                       response_header=rsphdr, $
                       response_filename=rspfn
    ;print, 'rspcode = ', rspcode
    ;print, 'rsphdr= ', rsphdr
    ;print, 'rspfn= ', rspfn
    
    ; destroy the url object
    obj_destroy, ourl
    
    return
  endif
  
  ; create a new IDLnetURL object, which represents a URL whose contents can be
  ; retrieved
  ourl = obj_new('IDLnetUrl')
  
  ; specify the callback function
  ;ourl->setProperty, callback_function='url_callback'
  
  ; set verbose to 1 to see more info on the transacton
  ;ourl->setProperty, verbose=1
   
  ; set the transfer protocol as HTTP
  oUrl->setProperty, url_scheme = 'http'
  
  ; set the webserver to be accessed
  oUrl->setProperty, url_host=usehost
   
  ; specify the web address, i.e., URL, of the file to download
  oUrl->setProperty, url_path=urlpathuse
  
  ; make a request to the web server, retrieving the file and writing it locally
  ; in the filename given by the FILENAME keyword
  fn = oUrl->get(filename=writediruse + filenames)
  
  ; print the path to the file retrieved from the remote server
  print, 'filename returned = ', fn

  ; destroy the URL object, freeing up its resources
  obj_destroy, ourl
end

