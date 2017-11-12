; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

;+
;   Procedure used to remove any copied files to the directory and reverse the procedure
;   `join_rededge_datadirs`. This can be useful if you want to revert your folders to their original state after
;   combining multiple directories.
; 
;   Note that files added to `dir` with `join_rededge_datadirs` will be permanently deleted and will not be 
;   found in your recycle bin. That is why `join_rededge_datadirs` copies original files and is meant to be 
;   used with this procedure.
;   
;   ## Example
; 
;   Here is how you can call this procedure::
;    
;       dir = 'C:\Users\norm3596\Desktop\extra'
;       unjoin_rededge_datadir, dir
;       
;-

;+
; :Params:
;    dir: in, required, type=string
;       This input is the fully qualified path to a directory that contains MicaSense data. An error will
;       be thrown if the directory does not exist.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro unjoin_rededge_datadir, dir
    compile_opt idl2, hidden

    ;check to make sure that the directory exists already
    if ~file_test(dir) then message, 'Directory "' + dir + '" does not exist!'

    cd, dir, current = first_dir
    files = file_search('*.tif')
    cd, first_dir

    ;now check for possible naming conflicts before copying files
    for i=0,n_elements(files)-1 do begin
        splitname = strsplit(files[i],'_', /extract)
        if (n_elements(splitname) eq 4) then FILE_DELETE, dir + path_sep() + files[i]
    endfor
end