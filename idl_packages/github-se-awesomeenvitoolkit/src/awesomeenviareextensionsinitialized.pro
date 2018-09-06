;+
; :Description:
;    Function that is intended for use in code blocks before 
;    adding extensions to ENVI. The reason for this routine is
;    that, for newer versions of ENVI, you can dynamically add
;    extensions to the toolbox. The only downside to this is that
;    you can keep adding the same tools to the toolbox. Here is an 
;    example of how you might use this procedure.
;    
;    ```idl
;    pro routine_extensions_init
;      compile_opt idl2, hidden
;      e = envi(/CURRENT)
;      
;      if ~awesomeENVIAreExtensionsInitialized('routine') then begin
;        e.AddExtension, 'My Awesome  Extension, 'run_routine'
;      endif
;    
;    end
;    ```
;    
;    With the example above, you could call the `routine_extensions_init`
;    multiple times and only have a single button added in the toolbox.
;    
;
; :Params:
;    name: in, required, type=string
;      Specify a unique identifier associated with the extensions
;      that you are checking.
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
function awesomeENVIAreExtensionsInitialized, name, DEBUG = debug, RESET = reset
  compile_opt idl2, hidden
  if ~keyword_set(debug) then on_error,2
  
  ;validate our input
  inputValidator, hash('name', ['string', 'required'])
  
  ;check if our system variable exists or not
  defsysv, '!initializedENVIextensions', EXISTS = exists
  if ~exists then begin
    defsysv, '!initializedENVIextensions', hash(/FOLD_CASE)
  endif else begin
    if keyword_set(reset) then begin
      !initializedENVIextensions = hash(/FOLD_CASE)
    endif
  endelse
  
  ;check if we have our key added to our hash
  flag = !initializedENVIextensions.hasKey(name)
  
  ;add if not present
  if ~flag then begin
    ;have to make variable - IDL parsing is invalid if we try to set directly
    ;not an issue though since we have an objref
    xt = !initializedENVIextensions
    xt[name] = !TRUE
  endif
  
  ;return the flag
  return, flag
end