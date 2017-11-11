; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

;+
;   The purpose of this procedure is to to used to restore the UAV Toolkit as a .sav file
;   into IDL's session and add to let ENVI know about the tasks that are a part of the
;   UAV Toolkit (even if we are just using PRO code).
;   
;   It also allows for a simple interface to open the help contents regarding the UAV Toolkit.
;   
;   
;   ## Example One
;        
;   Instead of needing to restore the UAV Toolkit .sav file every time in your code, you can just add
;   the source to IDL's search path and do the following in your IDL code:
;     
;       uav_toolkit
;       
;   ## Example Two
;   
;   To open the help using this routine, just do the following:
;   
;       uav_toolkit, /HELP
;    
; :Author: Zachary Norman - GitHub: znorman-harris
;-


;+
;
;
; :Keywords:
;    HELP: in, optional, type=boolean, default=false
;       Set this keyword to open up the help contents for the UAV Toolkit. Note that if you move the .sav file
;       then IDL may not be able to open up the help contents.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro uav_toolkit, HELP = help
  compile_opt idl2, hidden
  on_error, 2

  ;get current directory
  thisfilename = routine_filepath()
  thisdir = file_dirname(thisfilename)

  ;load help and return
  if keyword_set(help) then begin
    docsFile = thisdir + path_sep() + 'docs' + path_sep() + 'index.html'
    
    ;check if the file exists
    if file_test(docsFile) then begin
      spawn, 'start "" "'+ docsFile + '"', stdout, stderr, /NOWAIT, /HIDE
    endif else begin
      message, 'Docs files not found next to uav_toolkit source!'
    endelse
    return
  endif
  
  ;start ENVI if not started already
  e = envi(/current)
  if (e eq !NULL) then begin
    message, 'ENVI has not been started yet, requried!'
  endif
  
  ;if we aren't looking for help, then let's find all of the task files
  taskFiles = file_search(thisdir, '*.task')
  foreach taskFile, taskFiles do begin
    ;skip any errors
    catch, err
    if (err ne 0) then begin
      help, /LAST_MESSAGE, OUTPUT = o
      print, o
      catch, /CANCEL
      continue
    endif
    
    ;try to create a task so ENVI knows about the tools
    task = ENVITask(taskFile)
  endforeach
  catch, /CANCEL
end