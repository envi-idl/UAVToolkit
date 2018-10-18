;+
; :Description:
;    Procedure that wraps some fancy ENVI-code for
;    making a non-blocking widget to select and validate
;    task parameters.
;
; :Params:
;    taskName: in, required, type=string
;
; :Keywords:
;    SHOW_DISPLAY_OPTION: in, optional, type=boolean
;      Set this keyword to have options in the UI for
;      being able to display the result.
;    SHOW_PREVIEW: in, optional, type=boolean
;      Set this keyword to allow users to preview what 
;      the results will look like.
;    TASK: in, optional, type=ENVITask
;      Optionally specify an ENVI task that has been created
;      to generate a UI for.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro awesomeSelectTaskParameters, taskName, $
  SHOW_DISPLAY_OPTION = show_display_option,$
  SHOW_PREVIEW = show_preview,$
  TASK = task
  compile_opt idl2, hidden
  on_error, 2
  
  ;validate our inputs
  if ~isa(task, 'ENVITask') then begin
    if (taskName eq !NULL) then begin
      message, 'taskName required, but not specified!', LEVEL = -1
    endif
  endif
  
  ;catch error and report to level above us
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    message, /REISSUE_LAST, LEVEL = -1
  endif
  
  ;get current ENVI and make sure the UI is open
  e = awesomeGetENVI(/UI)
  
  ;get our task
  if ~isa(task, 'ENVITask') then begin
    task = ENVITask(taskName, ERROR = error)
    if keyword_set(error) then begin
      message, error
    endif
  endif
  
  ;create UI
  IDLcf$Get, TOOL=oTool
  oUIsvc = oTool.Get(IDENTIFIER='UI/Services/ENVITaskParameters')
  !null = oUIsvc.DoUIService(task, SHOW_DISPLAY_OPTION = show_display_option, SHOW_PREVIEW = show_preview)
end