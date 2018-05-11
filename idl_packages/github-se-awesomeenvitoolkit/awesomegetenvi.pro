;+
; :Description:
;    Simple function that gets the current ENVI session and
;    validates that, if it is supposed to be open, the ENVI
;    UI is valid. This works around a bug where you can have a bad
;    state with the ENVI UI being open, stop is pressed in 
;    the IDL workbench, the UI closes, but ENVI remains
;    open. In this situation you cannot use ENVI object 
;    methods as they will fail.
;
;
; :Keywords:
;    UI: in, optional, tpye=boolean
;      Set this keyword to make sure that the ENVI is not in 
;      headless mode.
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
function awesomeGetENVI, UI = ui
  compile_opt idl2
  on_error, 2
  
  ;get current ENVI
  e = envi(/CURRENT)
  if (e eq !NULL) then begin
    message, 'ENVI has not started yet, required!', LEVEL = -1
  endif else begin
    ;check if we have a widget ID and, if so, make sure it is valid
    if (e.widget_id gt 0) then begin
      if ~widget_info(e.widget_id, /VALID_ID) then begin
        message, 'ENVI has started and the UI should be present but it is missing.' + $ 
          ' Please reset your IDL session (or restart IDL) and try again.', LEVEL = -1
      endif
    endif
    
    ;validate object
    if ~obj_valid(e) then begin
      message, 'ENVI has started, but is not a valid IDL object.' + $
        ' Please reset your IDL session (or restart IDL/ENVI) and try again.', LEVEL = -1
    endif
    
    ;make sure the UI is up and running
    if keyword_set(ui) then begin
      if (e.widget_id eq 0) then begin
        message, 'ENVI has started, but is in headless mode.', LEVEL = -1
      endif
    endif
  endelse

  ;return ENVI
  return, e
end