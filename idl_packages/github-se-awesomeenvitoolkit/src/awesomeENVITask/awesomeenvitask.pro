;+
; :Description:
;    Procedure that adds a nice, basic level of logic
;    to creating ENVI task objects that will check to see
;    if the task is in ENVI's task catalog and, if not,
;    will search IDL's path for a corresponding task file
;    for processing.
;    
;    For example, if you have a task named `MySuperTask` this
;    routine will check ENVI's task catalog to see if it is
;    present and, if not, it will then search IDL's path for
;    a file called `mysupertask.task` and attempt to open that
;    as a task.
;
; :Params:
;    taskName: in, required, type=string
;      Specify the name of a task that you want to request from
;      ENVI.
;
; :Keywords:
;    DEBUG: in, optional, type=boolean
;      If set, errors will be stopped on.
;    IGNORE_CASE: in, optional, type=boolean
;      When set, it will check the system path for a file
;      exactly matching the case of the `taskName` argument. By
;      default the task name is converted to lower case as it is
;      the standard for IDL programs and ENVI task file naming. This
;      is more of a routine that may be needed on Linux/Mac systems
;      as it will not affect the behavior of windows systems.
;    PATH: in, optional, type=boolean
;      Setting this keyword will force the routine to use the 
;      search path instead of ENVI's task catalog. This is useful
;      if you are developing a task and want to use the newest 
;      content on disk. 
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
function awesomeENVITask, taskName,$
  DEBUG = debug,$
  IGNORE_CASE = ignore_case,$
  PATH = path
  compile_opt idl2, hidden
  if ~keyword_set(debug) then on_error, 2

  ;validate our input
  inputValidator, hash('taskName', ['required', 'string'])
  
  ;make sure that ENVI has started
  e = awesomeGetENVI()
  
  ;try to get the task if we are not using the path
  if ~keyword_set(path) then task = ENVITask(taskName, ERROR = err)
  
  ;check if there was an error or if we are using the path
  if keyword_set(err) OR keyword_set(path) then begin
    ;specify task file
    findFile = (keyword_set(ignore_case) ? taskName : strlowcase(taskName)) + '.task' 
    
    ;check path
    taskFile = file_which(findFile)
    
    ;make sure we found it
    if ~keyword_set(taskFile) then begin
      message, 'No task file was found matching "' + findFile + '"', LEVEL = -1 
    endif
    
    ;attempt to open the taskfile as a task
    task = ENVITask(taskFile)
  endif
  
  ;return our task
  return, task
end