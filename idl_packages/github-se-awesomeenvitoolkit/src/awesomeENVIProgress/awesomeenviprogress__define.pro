;+
; :Description:
;   Object definition for the `awesomeENVIProgress` class. See
;   the README for more information.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-



;+
; :Description:
;    Function method that will return whether or not a 
;    user pressed the cancel button in ENVI.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function awesomeENVIProgress::AbortRequested
  compile_opt idl2, hidden
  on_error, 2
  
  ;check if we are aborting
  flag = self.ABORTABLE.abort_requested
  
  ;clean system variable if needed
  if (flag) then begin
    !AWESOMEENVIPROGRESS = ptr_new()
  endif
  
  ;return the flag
  return, flag
end



;+
; :Description:
;    Procedure method that checks for cancellation request and
;    throws an error to halt execution. 
;    
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro awesomeENVIProgress::AbortRequested
  compile_opt idl2, hidden
  on_error, 2
  if self.abortRequested() then begin
    ;clean system variable
    !AWESOMEENVIPROGRESS = ptr_new()
    message, 'Process stopped by user', LEVEL = -1
  endif
end



;+
; :Description:
;    Procedure method that will close the current
;    progress message in ENVI.
;    
;  :Keywords:
;    ERROR: in, optional, type=boolean
;      If `PRINT` is set, then the printed message will alert 
;      the user that the progress stopped due to an error.
;    PRINT: in, optional, type=boolean
;      If set, then a small message will be printed to alert
;      the user.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro awesomeENVIProgress::Finish, ERROR = error, PRINT = print
  compile_opt idl2, hidden
  on_error, 2
  
  ;clean system variable
  if ~self.CHILD then begin
    !AWESOMEENVIPROGRESS = ptr_new()
  endif
  
  if keyword_set(print) AND ~self.CHILD then begin
    if keyword_set(error) then begin
      print, 'Finished due to error!'
      print
    endif else begin
      print, 'Finished!'
      print
    endelse
  endif
  self.CHANNEL.Broadcast, ENVIFinishMessage(self.ABORTABLE)
end



;+
; :Description:
;    Simple procedure method that is used to set default object
;    properties in the init method.
;    
; :Keywords:
;    FORCE_PRINT: in, optional, type=boolean
;      Makes sure messages always print when set.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro awesomeENVIProgress::_Initialize, FORCE_PRINT = force_print
  compile_opt idl2, hidden
  
  ;set default object properties
  self.PROGRESS = 0
  self.MESSAGE = ''
  self.INIT = 0
  self.START_TIME = systime(/SECONDS)
  
  ;small wait so that two progress objects that are created right after
  ;one another behave correctly
  wait, 0.001
  
  ;check if we are a child or not
  defsysv, '!AWESOMEENVIPROGRESS', EXISTS = exists
  if ~exists then begin
    defsysv, '!AWESOMEENVIPROGRESS', ptr_new(self.START_TIME)
  endif else begin
    ;if not valid pointer, then make it one
    if ~ptr_valid(!AWESOMEENVIPROGRESS) then begin
      !AWESOMEENVIPROGRESS = ptr_new(self.START_TIME)
    endif
  endelse
  
  ;check if we are the parent or not
  if ~(*(!AWESOMEENVIPROGRESS) eq self.START_TIME) then begin
    self.CHILD = ~keyword_set(force_print)
  endif else begin
    self.CHILD = !FALSE
  endelse
end



;+
; :Description:
;    Init method when creating the better ENVI progress message 
;    object. This is instantiated when you call the object as 
;    a function. i.e.:
;      
;      prog = awesomeENVIProgress()
;
; :Params:
;    progressTitle: in, required, type=string
;      Set this required argument to a string that represents
;      the title in the progress bar when initialized.
;
; :Keywords:
;    FORCE_PRINT: in, optional, type=boolean
;      By default, when there is another progress message going, 
;      all printed progress statements are "silenced" and not
;      shown in the IDL console. Set this to force to have them printed.
;    PRINT: in, optional, type=boolean
;      If set, then a message will be printed upon progress creation.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function awesomeENVIProgress::Init, progressTitle,$
  FORCE_PRINT = force_print,$
  PRINT = print
  compile_opt idl2, hidden
  on_error, 2
  
  ;get current session of ENVI
  e = awesomeGetENVI()

  ;make sure we passed in a progress title
  if (progressTitle eq !NULL) then begin
    message, '"progressTitle" argument was not provided, required!', LEVEL = -1
  endif

  ;initialize object properties
  self._Initialize, FORCE_PRINT = force_print
  self.CHANNEL = e.GetBroadCastChannel()
  self.ABORTABLE = ENVIAbortable()
  
  ; Broadcast a start message to the ENVI system
  start = ENVIStartMessage(progressTitle, self.ABORTABLE)
  self.CHANNEL.Broadcast, start
  
  ;check if we need to print
  if keyword_set(print) AND ~self.CHILD then begin
    print, progressTitle
  endif
  
  return, 1
end


;+
; :Description:
;    Procedure method that will set the progress message and
;    progress percent for the current progress bar.
;
;
;
; :Params:
;    msg: in, required, type=string
;      Set this required argument to a string that represents
;      the message that is shown with the updated progress.
;    percent: in, required, type=int/long/byte, range=[0:100]
;      This required parameter is the percent (from zero to 
;      100) that will set the progress on the progress bar.
;  
;  :Keywords:
;    APPEND: in, optional, type=string, undoc
;      If set, then the string contents will be added to the message. An
;      example of where this is used would be the same base message with
;      an addition to the end. As an alternative you could always add the
;      content to the message as well.
;    NO_TAB: in, optional, type=boolean
;      If set, no spaces will be added before the messages are printed to
;      the screen.
;    PRINT: in, optional, type=boolean, default=false
;      If set, then the progress will also be printed to the console.
;    TIME: in, optional, type=boolean
;      If set, then the approximate time until finish will be calculated
;      and attached to the progress message.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro awesomeENVIProgress::SetProgress, msg, percent,$
  APPEND = append,$
  NO_TAB = no_tab,$
  PRINT = print,$
  TIME = time
  compile_opt idl2, hidden
  on_error, 2
  
  ;make sure we passed in our arguments
  if ~keyword_set(msg) AND ~keyword_set(self.PREV_MESSAGE) then begin
    message, '"msg" not provided and previous message not cached, required!'
  endif
  
  ;save our message if we have one
  if keyword_set(msg) then begin
    self.PREV_MESSAGE = msg
  endif
  
  ;use our previous message
  if ~keyword_set(msg) then msg = self.PREV_MESSAGE
  
  if keyword_set(append) then begin
    msg += strtrim(append,2)
  endif
  
  if (percent eq !NULL) then begin
    message, '"percent" not provided, required!'
  endif
  
  ;round our percent
  usePercent = round(percent)
  
  ;calculate the approximate time it should take to finish
  if keyword_set(time) AND (percent gt 0) then begin
    tLeft = strtrim((100 - percent)*((systime(/SECONDS) - self.START_TIME)/percent),2)
    add = ' [' + strmid(tLeft, 0, strpos(tLeft, '.')+2) + ' (s) left]'
  endif else begin
    add = ''
  endelse
  
  ;make sure OK to send message
  if self.CHILD then return
  
  ;make our progress message
  progress = ENVIProgressMessage(msg + add , usePercent, self.ABORTABLE)
  self.CHANNEL.Broadcast, progress
  
  ;check if we also need to print the message
  if keyword_set(print) then begin
    if keyword_set(no_tab) then begin
      print, msg + ', Progress=' + strtrim(usePercent,2) + add
    endif else begin
      print, '  ' + msg + ', Progress=' + strtrim(usePercent,2) + add
    endelse
  endif
end


;+
; :Description:
;    Helper routine for miscellaneous uses.
;
;
;
; :Keywords:
;    CLEAR_PROGRESS: in, optional, type=boolean
;      If set, any references to current progress messages will
;      be cleare. Useful when you have errors during development.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro awesomeENVIProgress, CLEAR_PROGRESS = clear_progress
  compile_opt idl2, hidden
  if keyword_set(clear_progress) then !AWESOMEENVIPROGRESS = ptr_new()
end

;+
; :Description:
;    Core object definition.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro awesomeENVIProgress__define
  compile_opt idl2, hidden
  e = awesomeGetENVI()
  void = {awesomeENVIProgress,$
    CHANNEL:e.GetBroadcastChannel(),$
    CHILD: !FALSE,$
    ABORTABLE: ENVIAbortable(),$
    PROGRESS: 0l,$
    MESSAGE: '',$
    PREV_MESSAGE:'',$
    START_TIME:1d,$
    INIT: 0 $
  }
end

; start ENVI
e = envi(/HEADLESS)

; create our progress object
prog = awesomeENVIProgress('My Super Progress Message', /PRINT)

; send some progress messages
for i=0, 9 do begin
  ; check for cancellation
  prog.abortRequested

  ; send progress messages
  prog.setProgress, 'Processing...', 100*((i+1)/10.0), /PRINT, /TIME

  wait, 1
endfor

; close progress message
prog.finish, /PRINT
end