;h+
; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.
;h-

;+
;    Procedure to combine MicaSense data from multiple directories. Running this procedure will rename and copy
;    images from the directories specified by `dirs` and place them in the first directory in `dirs`.The purpose of 
;    this procedure is to avoid the naming conflict that can be created by combining folders with 1000+ image groups. An image
;    group for MicaSense data is this: IMG_000_1.tif, IMG_000_2.tif, IMG_000_3.tif, IMG_000_4.tif, IMG_000_5.tif.
;    
;    See the example below for more information regarding the renaming mechanism.
;    
;    If you want to reverse this procedure, see `unjoin_rededge_datadir`.
;    
;    ## Example
;    
;    Here is an example illustrating how to use this procedure:
;    
;        dirs = ['C:\path\to\some\images', 'C:\path\to\other\images']
;        join_rededge_datadirs, dirs
;       
;    In this case, all of the files in 'C:\path\to\other\images' will be copied into the folder 'C:\path\to\some\images'
;    and renamed such that an image in 'C:\path\to\some\images' will go from "IMG_000_2.tif" to "IMG_001_1_2.tif" where the additional
;    "1" comes from the directory index location in the dirs array.
;-


;+
;    
;
; :Params:
;    dirs: in, required, type=stringarr
;       This input in required and must be a string array. The first directory in the string array will be where renamed copies
;       of the data in the other directories can be found.
;       
; :Author: Zachary Norman - GitHub: znorman-harris
;
;-
pro join_rededge_datadirs, dirs
  compile_opt idl2, hidden
  
  ;do some error checking
  ;check to make sure that the directories exists already
  foreach dir, dirs do begin
    if ~file_test(dir) then message, 'Directory "' + dir + '" does not exist!'
  endforeach

  ;return if only one directory is passed in
  if (n_elements(dirs) lt 2) then begin
    return
  endif
  fileshash = orderedhash()

  ;search each directory for files ending with '*.tif'
  foreach dir, dirs do begin
    cd, dir, current = first_dir
    files = file_search('*.tif')
    fileshash[dir] = files
    cd, first_dir
  endforeach

  ;now check for possible naming conflicts before copying files
  basefiles = fileshash[dirs[0]]
  for i=1,n_elements(dirs)-1 do begin
    movefiles = fileshash[dirs[i]]
    for j=0, n_elements(movefiles)-1 do begin
      newname = strsplit(movefiles[j],'_', /EXTRACT)
      newname[1] += '_' + strtrim(i,2)
      newname = strjoin(newname,'_')
      file_copy, dirs[i] + path_sep() + movefiles[j], dirs[0] + path_sep() + newname, /OVERWRITE
    endfor
  endfor
end


;cd, 'C:\Users\norm3596\Desktop\Greenhouse', current = first_dir
;dirs = file_search()
;cd, first_dir
;
;join_rededge_datadirs, 'C:\Users\norm3596\Desktop\Greenhouse' + path_sep() + dirs
;
;end
