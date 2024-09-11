pro downloadawsom,crotname,writediruse,filenamein,rspcode=rspcode

;
;
; Downloads SWMF AWSoM 'slg' 'idl_real4' saved file
; model output form csem website
;
; will save an unzip a file e.g. 'CR2107_AWSOM.dat'
; 	to working_dir
;
;
; Written by Judit Szente 
; Version 0.0 Sept 2022
;
;  Input:
;
;    CROTNAME (string)
;	carrington rotation for the sim
;    WRITEDIRUSE
;	directory to put I/O files
;
;  Output:
; 
;    FILENAMEIN
;    (name of file to be processed into FORWARD-friendly format)
;
;  
; Called by MAKEAWSOMCUBE

  filenamein = 'CR'+crotname+'_ADAPTGONG.out'

  filenames = filenamein+'.gz' 
  urlpathuse='AWSOM/'+filenames
  URLPATHUSE=urlpathuse[0]
  usehost='csem.engin.umich.edu'
  print,'downloading file ',filenames
;  print,'from ',usehost+'/'+urlpathuse
  print,'from ',usehost+'/AWSOM/'
  for_getbyurl,filenames,URLPATHUSE,writediruse=writediruse,usehost=usehost,rspcode=rspcode

; unzip
  cd,writediruse,current=old_dir
  spawn,'gunzip '+filenamein+'.gz'
  cd,old_dir

end
