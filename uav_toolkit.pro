;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT, see LICENSE.txt for more details.
;h-


;+
; :Description:
;    Procedure that adds the buttons to ENVI's toolbox.
;    
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro uav_toolkit_extensions_init
  compile_opt idl2, hidden
  e = envi(/CURRENT)
  uav_toolkit
  e.AddExtension, 'Band Alignment', 'uav_toolkit_extension',$
    UVALUE = 'UAVBandAlignment', PATH='/UAV Toolkit/'
  e.AddExtension, 'Batch RedEdge', 'uav_toolkit_extension',$
    UVALUE = 'UAVBatchRedEdge', PATH='/UAV Toolkit/'
end

;+
; :Description:
;    Displays the task UI for the button we click.
;
; :Params:
;    event: in, required, type=structure
;      The event that is passed on when a user clicks
;      on the button in the ENVI toolbox.
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro uav_toolkit_extension, event
  compile_opt idl2, hidden

  ;error catching
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    message, /RESET
    void = dialog_message('An error occurred while running task:' + $
      !ERROR_STATE.msg, /ERROR)
    return
  endif

  ;get current session of ENVI
  e = awesomeGetENVI(/UI)

  ;return if called as a procedure instead of a button click
  if (event eq !NULL) then begin
    return
  endif

  ;get the name of the task that we want to run
  widget_control, event.ID, GET_UVALUE = taskName

  ;create the UI
  awesomeSelectTaskParameters, taskName
end

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
  
  ;get the current session of ENVI
  e = awesomeGetENVI()
  
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