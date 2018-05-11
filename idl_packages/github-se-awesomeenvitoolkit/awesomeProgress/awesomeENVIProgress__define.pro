
;+
; :Description:
;    Simple function method that will return whether or not a 
;    user pressed the cancel button in ENVI.
;
;
; :Author: Zachary Norman - GitHub: znorman17
;-
function awesomeENVIProgress::AbortRequested
  compile_opt idl2
  on_error, 2
  return, self.ABORTABLE.abort_requested
end



;+
; :Description:
;    Simple procedure method that will close the current
;    progress message in ENVI
;
;
; :Author: Zachary Norman - GitHub: znorman17
;-
pro awesomeENVIProgress::Finish, PRINT = print
  compile_opt idl2
  on_error, 2
  if keyword_set(print) then begin
    print, 'Finished!'
    print
  endif
  self.CHANNEL.Broadcast, ENVIFinishMessage(self.ABORTABLE)
end



;+
; :Description:
;    Simple procedure method that is used to set default object
;    properties in the init method.
;
;
; :Author: Zachary Norman - GitHub: znorman17
;-
pro awesomeENVIProgress::_Initialize
  compile_opt idl2
  
  ;set default object properties
  self.PROGRESS = 0
  self.MESSAGE = ''
  self.INIT = 0
  self.START_TIME = systime(/SECONDS)
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
;
; :Author: Zachary Norman - GitHub: znorman17
;-
function awesomeENVIProgress::Init, progressTitle, PRINT = print
  compile_opt idl2
  on_error, 2
  
  ;get current session of ENVI
  e = awesomeGetENVI()

  ;make sure we passed in a progress title
  if (progressTitle eq !NULL) then begin
    message, '"progressTitle" argument was not provided, required!'
  endif

  ;initialize object properties
  self._Initialize
  self.CHANNEL = e.GetBroadCastChannel()
  self.ABORTABLE = ENVIAbortable()
  
  ; Broadcast a start message to the ENVI system
  start = ENVIStartMessage(progressTitle, self.ABORTABLE)
  self.CHANNEL.Broadcast, start
  
  ;check if we need to print
  if keyword_set(print) then begin
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
;    PRINT: in, optional, type=boolean, default=false
;      If set, then the progress will also be printed to the console.
;
; :Author: Zachary Norman - GitHub: znorman17
;-
pro awesomeENVIProgress::SetProgress, msg, percent, PRINT = print, NO_TAB = no_tab, TIME = time
  compile_opt idl2
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
  
  if (percent eq !NULL) then begin
    message, '"percent" not provided, required!'
  endif
  
  ;round our percent
  usePercent = round(percent)
  
  ;calculate the approximate time it should take to finish
  if keyword_set(time) AND (percent gt 0) then begin
    tLeft = strtrim((100 - percent)*((systime(/SECONDS) - self.START_TIME)/percent),2)
    add = ' [ ' + strmid(tLeft, 0, strpos(tLeft, '.')+2) + ' (s) left ]'
  endif else begin
    add = ''
  endelse
  
  ;make our progress message
  progress = ENVIProgressMessage(msg + add , usePercent, self.ABORTABLE)
  self.CHANNEL.Broadcast, progress
  
  ;check if we also need to print the message
  if keyword_set(print) then begin
    if keyword_set(no_tab) then begin
      print, msg + ', Progress = ' + strtrim(usePercent,2) + add
    endif else begin
      print, '  ' + msg + ', Progress = ' + strtrim(usePercent,2) + add
    endelse
  endif
end

;+
; :Description:
;    Core object definition.
;
;
; :Author: Zachary Norman - GitHub: znorman17
;-
pro awesomeENVIProgress__define
  compile_opt idl2
  e = envi()
  void = {awesomeENVIProgress,$
    CHANNEL:e.GetBroadcastChannel(),$
    ABORTABLE: ENVIAbortable(),$
    PROGRESS: 0l,$
    MESSAGE: '',$
    PREV_MESSAGE:'',$
    START_TIME:1d,$
    INIT: 0 $
  }
end