;+
; Catch block that can be inserted into any IDL program with
; the syntax "@awesomecatchblock" near the top of your program.
; 
; This catch block also sets `on_error` to not enter the routine
; if an error is caught. The catch block and the `on_error` state
; are both skipped if there is a vaiable defined as `debug`. This
; can commonly be a keyword for the routine you are processing and
; help for production/development use.
; 
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
if ~keyword_set(debug) then begin
  on_error, 2
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    help, /LAST_MESSAGE
    message, /REISSUE_LAST
  endif
endif