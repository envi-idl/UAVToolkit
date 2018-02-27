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
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
function bandAlignment_getCurrentENVI
  compile_opt idl2
  on_error, 2
  
  ;get current ENVI
  e = envi(/CURRENT)
  if (e eq !NULL) then begin
    message, 'ENVI has not started yet, required!'
  endif else begin
    ;check if we have a widget ID and, if so, make sure it is valid
    if (e.widget_id gt 0) then begin
      if ~widget_info(e.widget_id, /VALID_ID) then begin
        message, 'ENVI has started and the UI should be present but it is missing.' + $ 
          ' Please reset your IDL session (or restart IDL) and try again.'
      endif
    endif
  endelse

  ;return ENVI
  return, e
end