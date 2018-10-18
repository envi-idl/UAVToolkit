;+
; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.
;
; :Description:
;   Object definition to easily manage child processes through one interface and
;   perform simple batch processing with ENVI or IDL.
;
;  :Examples:
;
;     Running procedures and funtions:
;     
;        oBdgit = bridge_it(1)
;        void = oBdgt.run('plot', ARG1 = findgen(10), ARG2 = findgen(10))
;        oBdgit.run, 'plot', ARG1 = findgen(10), ARG2 = findgen(10)
;
;     Passing keywords uses the following mechanism:
;     
;        oBdgit = bridge_it(1)
;        void = oBdgit.run( 'plot', ARG1 = findgen(10), ARG2 = findgen(10), _KW_XRANGE = [0,5], _KW_YRANGE = [0,3])
;        oBdgit.run, 'plot', ARG1 = findgen(10), ARG2 = findgen(10), _KW_XRANGE = [0,5], _KW_YRANGE = [0,3]
;
;     To get results back from running a function in the object (you get a hash with a random number for the key):
;     
;        oBdgit = bridge_it(1)
;        void = oBdgit.run( 'max', ARG1 = findgen(10), /CALLBACK)
;        results = oBdgit.GetResults()
;        oBdgit.cleanup
;        print, results
;
;     To clear accumulated results:
;     
;        oBdgit.ClearResults
;
;     To capture the output from the IDL-IDLBridge child process with log files:
;     
;        oBdgit = bridge_it(1, LOGDIR = 'C:\some\dir')
;        void = oBdgit.run( 'max', ARG1 = findgen(10), /CALLBACK)
;        oBdgit.run, 'print', ARG1 = 'something to print'
;        oBdgit.cleanup
;        print, results
;
;        ;look in the log dir and there will be a text file calles all_logs.txt
;        ;that gets created once the cleanup method is invoked.
;
;     Refresh the bridges after a certain number of runs. Useful for having a 
;     'clean slate' occasionally in the processes. When reset, the bridges will
;     be reset to the initial state with the same INIT argument
;     
;       oBdgit = bridge_it(1, NREFRESH = 5)
;       for i=0, 9 do void = oBdgit.run('max', ARG1 = randomu(seed, 10), /CALLBACK)
;       ;after 5 runs the bridge will reset
;
;     To run ENVI tasks easily in parallel (see RunENVITask method for complete example):
;     
;       oBdgit.RunENVITask, task
;       
;     For exporting rasters to disk (see ExportRaster for complete example):
;       
;       oBdgit.ExportRaster, raster, outFile
;       oBdgit.ExportRaster, raster, outFile, DATA_IGNORE_VALUE = 0
;
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-



;+
; :Description:
;    Function which will initialize the bdige_it object and start all bridges. When bridges are initialized
;    they are done so in parallel, but whe function will not return until every bridge is idle again.
;
; :Params:
;    nbridges: in, required, type=int
;       The number of bridges that you want to create
;
; :Keywords:
;    INIT : in, optional, type=strarr
;       Optional argument which allows you to pass in a string array of extra commands
;       to have each IDL_IDLBridge object execute upon creation.
;    MSG : in, optional, type=string
;       Optional argument to show the message prefix when a bridge process has completed for the TIME
;       keyword in bridge_it::run and bridge_it::run().
;    LOGDIR : in, optional, type=string
;       Specify the directory that the log file will be written to. The log file is just a text file with
;       all of the IDL Console output from each child process.
;    NREFRESH : in, optional, type=long
;       Specify the number of bridge processes to execute before closing and re-starting the
;       child process. Necessary for some ENVI routines so that we don't have memory fragmentation
;       regarding opening lots of small rasters.
;    PREFIX : in, optional, type=string, default='_KW_'
;       This optional keyword specifies the prefix which is used to differentiate between arguments and
;       keywords when it comes time to parse the arguments and keyword that will be passed into a routine.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
function bridge_it::Init, nbridges, INIT = init, MSG = msg, LOGDIR = logdir, NREFRESH = nrefresh, PREFIX = prefix
  compile_opt idl2, hidden
  on_error, 2

  if (n_elements(nbridges) eq 0) then begin
    message, '"nbridges" not specified, required argument!'
  endif

  ;check to see if different messag eneeds to be used
  if ~keyword_set(msg) then self.msg = 'Time to complete bridge process (sec): ' else self.msg = msg

  ;set default keyword prefix for parsing
  if ~keyword_set(prefix) then prefix = '_KW_'
  self.prefix = prefix

  ;check to see if we have a number of processes to complete before refreshing the bridges
  ;otherwise set the refresh number to an absurbly high result!
  if keyword_set(nrefresh) then self.nrefresh = nrefresh else self.nrefresh = 1000000l

  ;check if init keyword is set, if so then we want to save the init string
  if keyword_set(init) then self.init = strjoin(init, ' & ')

  ;check if logdir is specified
  if keyword_set(logdir) then begin
    if file_test(logdir) then begin
      self.logdir = logdir
    endif else begin
      message, 'Specified LOGDIR does not exist!'
    endelse
  endif

  ;remember the number of bridges
  self.nbridges = nbridges

  ;pre-allocate object array for bridge
  bridges = objarr(nbridges)

  ;save the last bridge to run a process
  self.lastrun=0

  ;initialize brige objects
  for i=0, nbridges-1 do begin
    if keyword_set(logdir) then begin
      bridges[i] = IDL_IDLBridge(OUTPUT = logdir + path_sep() + 'out' + strtrim(i,2) +'.txt')
    endif else begin
      bridges[i] = IDL_IDLBridge()
    endelse
    ;optional statements to execute??
    ;      (bridges[i]).execute, '@' + pref_get('IDL_STARTUP')
    ;      (bridges[i]).execute, 'cd,' + pref_get('IDL_START_DIR')
    if keyword_set(init) then (bridges[i]).execute, strjoin(init, ' & '), /nowait
    ;have a short pause
    wait, .5
  endfor
  ;wait for every bridge to be idle before proceeding
  count = 0*indgen(nbridges)
  while 1 do begin
    for i=0, nbridges-1 do count[i] = (1 < (bridges[i]).status())
    if ~total(count) then break else wait, .05
  endwhile

  ;update the array for the bridge objects
  self.bridges = ptr_new(bridges)

  ;make some stuff for keeping track of our runs so we can clear ENVI
  self.nruns = ptr_new({total:make_array(nbridges, /long, value = 0), since_reset:make_array(nbridges, /long, value = 0)})

  ;initialize variable to keep track of the bridge logs
  self.logs = list()
  
  ;initialize a variable to hold the callback results
  self.callback = orderedhash()

  return, 1
end



;+
; :Description:
;    Simple procedure to remember results from a child process.
;
; :Params:
;    keyname: in, required, type=string
;       This argument is the name for the hash key that will contain the results.
;    results: in, required
;       This argument is the information that you want the BRIDGE_IT object to
;       associate with the `keyname` argument.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro bridge_it::AddResults, keyname, results
  compile_opt idl2, hidden
  (self.callback)[keyname] = results
end




;+
; :Description:
;    Callback function to pass results back and forth between bridges. Should not be 
;    manually called by a user
;
; :Params:
;    status: in, required, type=int
;       An integer representing the return value of the bridge process. A value of 2 represents
;       that the bridge completed the process, 3 means that an error ocurred, and a value
;       of 4 represents that the bridge process was aborted.
;    error: in, required, type=string
;       If an error occurred while executing the child process, then this string will contain
;       the error. It is included in the results if present.
;    oBridge: in, required, type=objref
;       This argument represents the ILD_IDLBridge child process which ran the procedure.
;    userdata: in, required
;       This argument represents the userdata stored in an IDL_IDLBridge object. The variable's
;       type is user defined. For this case, they will all be hashes.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
pro bridge_it_callback, status, error, oBridge, userdata
  compile_opt idl2, hidden
  on_error, 2
  ;disable callback
  oBridge.SetProperty, CALLBACK = ''

  if (status eq 3) then begin
    print, 'Error while executing child process. Message from child: ' + strtrim(error,2)
    return
  endif
  
  ;initialize error message variable
  msg = ''

  ;create a flag in case we should save results or not
  ;potential to not save results because this procedure can just be used to get the
  ;time that has passed while the bridge process is running
  save_results = 0

  ;check if we need to print the starting time
  if userdata.haskey('$START_TIME') then begin
    time = userdata['$START_TIME']
    runtime = systime(/SECONDS)-time
    userdata['$RUN_TIME'] = runtime
    print, string(9b) + userdata['SELF'].GetMessage() + strtrim(runtime,2)
    save_results = 1
  endif
  oBridge.SetProperty, USERDATA = !NULL

  ;handle errors for returning data type that are not allowed (i.e. objects and pointers)
  ;return from procedure without throwing an error, but still print out information about the error
  catch, error
  if (error ne 0) then begin
    CATCH, /CANCEL
    help, /LAST_MESSAGE, OUTPUT = o
    print, o, /IMPLIED_PRINT
    return
  endif

  ;check if we have a function value to return
  if userdata.haskey('$FUNCTION_RETURN') then begin
    userdata['$FUNCTION_RESULTS'] = oBridge.GetVar('output')
    save_results = 1
  endif

  ;see if there are keywords to get back
  if userdata.haskey('$KEYWORDS_OUT') then begin
    ;get each keyword from the child process
    foreach keyword, userdata['$KEYWORDS_OUT'] do begin
      userdata[userdata['SELF'].GetPrefix() + keyword] = oBridge.GetVar(keyword)
    endforeach
    save_results = 1
  endif

  ;check if there are arguments to return
  if userdata.haskey('$ARGUMENTS_OUT') then begin
    ;get each keyword from the child process
    foreach argument, userdata['$ARGUMENTS_OUT'] do begin
      userdata[argument] = oBridge.GetVar(argument)
    endforeach
    save_results = 1
  endif

  ;check if we also need to get variables out
  if userdata.haskey('$VARIABLES_OUT') then begin
    ;get each keyword from the child process
    foreach argument, userdata['$VARIABLES_OUT'] do begin
      userdata[argument] = oBridge.GetVar(argument)
    endforeach
    save_results = 1
  endif

  ;check if we ran a task
  if userdata.haskey('$TASK') then begin
    userData.Remove, '$TASK'
    ;get the task back
    oBridge.Execute, 'output = json_serialize(output.dehydrate())'
    hashResults = json_parse(oBridge.GetVar('output'))
    userdata['$TASK'] = ENVITask(hashResults)
    
    ;check if there were errors while running our task
    taskError = oBridge.getVar('taskError')
    if (taskError ne '') then begin
      userdata['$TASK_ERROR'] = taskError
;      msg = 'Error while executing task "' + hashResults['NAME'] + '":' + taskError
    endif
    
    save_results = 1
  endif

  ;check if we exported a virtual raster and if there were any issues
  if userdata.haskey('$EXPORTRASTER') then begin
    userData.Remove, '$EXPORTRASTER'
    ;check if there were errors while running our task
    exportError = oBridge.getVar('exportError')
    if (exportError ne '') then begin
      userData.Remove, '$OUTPUT_RASTER_URI'
      userdata['$EXPORT_ERROR'] = exportError
      msg = 'Error while exporting virtual raster: ' + exportError
    endif
    
    save_results = 1
  endif

  ;see if an error was returned while running the child process
  ;if so, lets add it to the userdata
  if (error ne '') then begin
    userdata['$ERROR'] = error
    save_results = 1
  endif

  ;see if we have results to save
  if (save_results eq 1) then begin
    ;build a unique key for our results hash using the time and some randomly
    ;generated numbers
    timekey = strtrim(fix(1000000d*systime(/seconds),  TYPE=13),2) + $
      strtrim(abs(floor(total(randomn(!NULL, 15)))),2) + $
      strtrim(abs(floor(total(randomn(!NULL, 15)))),2)
    for i=0,3 do timekey += strmid(strjoin(strtrim(round(10000*randomu(seed)),2),''),2)
    userdata['SELF'].AddResults, timekey, userdata
    userdata.Remove, 'SELF'
  endif
  
  ;throw our error message
  if (msg ne '') then begin
    ;joint he strings together into a single line
    bytMsg = byte(msg)
    idxKeep = where(bytMsg ne 10b, countKeep)
    if (countKeep gt 0) then begin
      msg = strjoin(bytMsg[idxKeep])
    endif
    print, msg
  endif
end


;+
; :Description:
;    Objet method to destroy all bridge objects when told. The child processes will
;    abort processing and then be destroyed. This method is called automatically when
;    the bridge_it object is destroyed
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
pro bridge_it::BurnBridges
  compile_opt idl2, hidden
  nbridges = self.nbridges
  bridges = *self.bridges
  for i=0,nbridges-1 do begin
    if ((bridges[i]).status() ne 0) then (bridges[i]).abort
    obj_destroy, bridges[i]
  endfor
end

;+
; :Description:
;    Object method to destroy the bridge_it object and kill each child process
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
pro bridge_it::Cleanup
  compile_opt idl2, hidden
  self.BurnBridges

  ;check the log files
  ;if there are logs, then read them and print out to a text file
  if (self.logdir ne '') then begin

    ;read all of the log files and delete them
    for i=0, self.nbridges-1 do begin
      logfile = self.logdir + path_sep() + 'out' + strtrim(i,2) +'.txt'
      if file_test(logfile) then begin

        header = ['===================================================================',$
          '===================================================================',$
          '===================================================================',$
          'Bridge results for bridge number ' + strtrim(i,2)]

        nlines = file_lines(logfile)
        strings = strarr(nlines)
        openr, lun, logfile, /GET_LUN
        readf, lun, strings
        free_lun, lun

        outstrings = strarr(4 + nlines)
        outstrings[0] = header
        outstrings[4] = strings
        (self.logs).add, outstrings
        FILE_DELETE, logfile, /quiet
      endif
    endfor

    ;write a new log file with the results from the IDL Bridges
    outfile = self.logdir + path_sep() + 'all_logs.txt'
    logs = self.logs
    openw, lun, outfile, /GET_LUN
    for i=0, n_elements(logs)-1 do begin
      printf, lun, logs[i], /IMPLIED_PRINT
    endfor
    free_lun, lun
  endif

  obj_destroy, self
end



;+
; :Description:
;    Simple procedure to to clear the results stored by the BRIDGE_IT object
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
pro bridge_it::ClearResults
  compile_opt idl2, hidden
  (self.callback).remove, /ALL
end



;+
; :Description:
;    This object method will allow you to get references to the child IDL_IDLBridge
;    processes so that you may manipulate them as you see fit.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
function bridge_it::GetBridges
  compile_opt idl2, hidden
  return, *self.bridges
end


;+
; :Description:
;     Method used to get the message that is stored in the BRIDGE_IT object.
;     This message is the prefix for when the bridge finishes its processing
;     and the time gets deisplayed.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
function bridge_it::GetMessage
  return, self.msg
end


;+
; :Description:
;    This object method allows you to return the number of processes that
;    each child process has executed.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
function bridge_it::GetNruns
  compile_opt idl2, hidden
  return, *self.nruns
end


;+
; :Description:
;    This object method lets you get the prefix for how keywords are to
;    be distinguished from arguments in the BRIDGE_IT object.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
function bridge_it::GetPrefix
  compile_opt idl2, hidden
  return, self.prefix[0]
end

;+
; :Description:
;    This object method will return all of the stored results in the BRIDGE_IT
;    object. It will return a hash of hashes where the keys represent the time that the
;    callback function was called plus some random numbers at the end.
;
;    If you want to get the start time and processing time for the child process,
;    then you should specify the TIME keyword when executing a routine with the
;    BRIDGE_IT object. If you do this, then there will be keys called START_TIME and
;    RUN_TIME in the hashes containing the results for each child process.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
function bridge_it::GetResults
  compile_opt idl2, hidden
  return, self.callback
end


;+
; :Description:
;    Procedure method to run procedures in a child IDL_IDLBridge process.
;
; :Params:
;    routine : in, required, type=string
;       String representing the name of a procedure to run.
;
; :Keywords:
;    _extra: in, optional
;       This keyword hold the arguments and keywords which will be passed on to the child
;       processes. The keywords must start with _KW_keyword_name and arguments can be named
;       anything, but their calling order is alphabetical.
;    ARGUMENTS_OUT: in, optionsal, type=string/strarr
;       Similar to `KEYWORDS_OUT`, this keyword is set the the names of arguments that will ahve their values
;       returned in the callback function. If you want to returnt he value of a positional argument, then
;       you must also pass that in as a keyword. In other words you need to do this::
;           void = bdg_it.run('min', ARG1=findgen(10), ARG2 = 1, ARGUMENTS_OUT = 'ARG2', KEYWORDS_OUT = 'MAX', /CALLBACK, /TIME)
;       Where you specify ARG2 as a positional argument and it is listed in the ARGUMENTS_OUT keyword.
;    CUSTOM_ARGS: in, optional, type=string/strarr
;       This optional keyword should be a string or string array for arguments that you want to manually
;       add to the procedure execution (that are undefined in the parent IDL session).
;    KEYWORDS_OUT: in, optional, type=string/strarr
;       Set this keyword to a string or string array with the names for all of the keywords that
;       return values for the procedure you are running. This will automatically send the results
;       to a hash where a separate key will be created for the individual variables that are
;       being passed back and forth.
;    PRE: in, optional, type=string/strarr
;       Set this to a string or string array of expressions that you want to execute 
;       prior to the procedure being executed.
;    POST:in, optional, type=string/strarr
;       Set this to a string or string array of expressions that you want to execute
;       after the procedure being executed.
;    TIME: in, optional
;       This keyword sets a callback for the IDL_IDLBridge process to print out the time that it
;       takes to complete the procedure.
;    VARIABLES_OUT: in, optional, type=string/strarr
;       Set this to a string or string array that contains the names of variables that you want
;       to return from the procedure that was just executed. These will be contained in the 
;       results hash for the bridge_it object.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
; 
;-
pro bridge_it::Run, routine,$
  _EXTRA = args,$
  ARGUMENTS_OUT = arguments_out,$
  CUSTOM_ARGS = custom_args,$
  KEYWORDS_OUT = keywords_out,$
  TIME = time,$
  PRE = pre,$
  POST = post,$
  VARIABLES_OUT = variables_out,$
  _UVALUE = uvalue
  compile_opt idl2, hidden

  self.RunRoutine, routine, _EXTRA = args, $
    CALLBACK = callback,$
    ARGUMENTS_OUT = arguments_out, $
    CUSTOM_ARGS = custom_args,$
    IS_FUCNTION = 0, $
    KEYWORDS_OUT = keywords_out, $
    TIME = time,$
    PRE = pre,$
    POST = post,$
    VARIABLES_OUT = variables_out,$
    _UVALUE = uvalue
end

;+
; :Description:
;    Function method for executing functions in child IDL_IDLBridge processes
;
; :Params:
;    routine: in, required, type=string
;       String representing the name of a function to run.
;
; :Keywords:
;    _extra: in, optional
;       This keyword represents all of the parameters and keywords which are to be passed to the
;       child process. Similar to the procedure method for the bridge_it object, keyword must
;       start with "_KW_" and arguments are sorted alphabetically and can have any name other
;       than starting with '_KW_"
;    ARGUMENTS_OUT: in, optionsal, type-string/stringarr
;       Similar to `KEYWORDS_OUT`, this keyword is set the the names of arguments that will ahve their values
;       returned in the callback function. If you want to returnt he value of a positional argument, then
;       you must also pass that in as a keyword. In other words you need to do this::
;           void = bdg_it.run('min', ARG1=findgen(10), ARG2 = 1, ARGUMENTS_OUT = 'ARG2', KEYWORDS_OUT = 'MAX', /CALLBACK, /TIME)
;       Where you specify ARG2 as a positional argument and it is listed in the ARGUMENTS_OUT keyword.
;    CUSTOM_ARGS: in, optional, type=string/strarr
;       This optional keyword should be a string or string array for arguments that you want to manually
;       add to the procedure execution (that are undefined in the parent IDL session).
;    CALLBACK : in, optional
;       If this keyword is set, you can use it to return the function results from the child
;       process. You can get these results with the bridge_it::getresults method.
;    EXPORTRASTER: in, optional, type=string
;       This keyword is only for the ExportRaster object method. It is set to the fully-qualified 
;       filepath for the output raster URI that is being exported to disk. See that method for
;       more information.
;    KEYWORDS_OUT: in, optional, type=string/stringarr
;       Set this keyword to a string or string array with the names for all of the keywords that
;       return values for the procedure you are running. This will automatically send the results
;       to a hash where a separate key will be created for the individual variables that are
;       being passed back and forth.
;    PRE: in, optional, type=string/strarr
;       Set this to a string or string array of expressions that you want to execute 
;       prior to the procedure being executed.
;    POST:in, optional, type=string/strarr
;       Set this to a string or string array of expressions that you want to execute
;       after the procedure being executed.
;    TIME : in, optional
;       Setting this keyword will have the IDL_IDLBridge child process use the callback procedures
;       to print out the time that it took to run the child process.
;    VARIABLES_OUT: in, optional, type=string/strarr
;       Set this to a string or string array that contains the names of variables that you want
;       to return from the procedure that was just executed. These will be contained in the 
;       results hash for the bridge_it object.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
function bridge_it::Run, routine, $
  _EXTRA = args,$
  ARGUMENTS_OUT = arguments_out,$
  CUSTOM_ARGS = custom_args,$
  CALLBACK = callback,$
  KEYWORDS_OUT = keywords_out,$
  PRE = pre,$
  POST = post,$
  TIME = time,$
  VARIABLES_OUT = variables_out,$
  EXPORTRASTER = exportRaster,$
  _UVALUE = uvalue
  compile_opt idl2, hidden

  self.RunRoutine, routine, _EXTRA = args, $
    CALLBACK = callback,$
    CUSTOM_ARGS = custom_args,$
    ARGUMENTS_OUT = arguments_out, $
    IS_FUCNTION = 1, $
    KEYWORDS_OUT = keywords_out, $
    TIME = time,$
    PRE = pre,$
    POST = post,$
    VARIABLES_OUT = variables_out,$
    EXPORTRASTER = exportRaster,$
    _UVALUE = uvalue

  return, 1
end


;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    varName: in, required, type=string
;      Specify the name of the variable as it will appear in the child process.
;    var: in, requried, type=any
;      Specify the variable that will be sent to the child process. Any
;      hashes, dictionaries, or structures will be converted to strings and
;      parsed on the other side.
;
; :Keywords:
;    LIST: in, optional, type=boolean, default=false
;      If set, and if your object is a hash, orderedhash, or structure, then the
;      rehydrated version of the variable will use lists instead of arrays where
;      applicable.
;
; :Author: Zachary Norman - GitHub : [znorman-harris](https://github.com/znorman-harris)
;-
pro bridge_it::SetVar, varName, var, LIST = list, POST_EXECUTE = post_execute
  compile_opt idl2

  ;init string for post processing
  post = ''

  ;check what kind of variable we have to process
  ; TODO: figure out the best order below for optimal performance bc
  ; there seems to be an issue with dictionaries going back and forth
  case (1) of
    isa(var, 'HASH'):begin
      set = json_serialize(var)
      if ~keyword_set(list) then begin
        post = varName + ' = json_parse(' + varName + ', /TOARRAY)'
      endif else begin
        post = varName + ' = json_parse(' + varName + ')'
      endelse
    end
    isa(var, 'DICTIONARY'):begin
      set = json_serialize(var)
      if ~keyword_set(list) then begin
        post = varName + ' = json_parse(' + varName + ', /DICTIONARY, /TOARRAY)'
      endif else begin
        post = varName + ' = json_parse(' + varName + ', /DICTIONARY)'
      endelse
    end
    isa(var, 'STRUCT'):begin
      set = json_serialize(var)
      if ~keyword_set(list) then begin
        post = varName + ' = json_parse(' + varName + ', /STRUCT, /TOARRAY)'
      endif else begin
        post = varName + ' = json_parse(' + varName + ', /STRUCT)'
      endelse
    end
    else:set = var
  endcase

  ;set the variables on each child process
  foreach bridge, *self.bridges do begin
    bridge.SetVar, varName, set
    if keyword_set(post) then bridge.Execute, post
    if keyword_set(post_execute) then bridge.Execute, strjoin(post_execute, ' & ')
  endforeach
end


;+
; :Description:
;    Procedure method to export a raster or virtual raster to disk in 
;    parallel. This can be helpful especially with mosaics because they are low
;    CPU high-time to process.
;    
;    You must h ave ENVI started on all processes for this method to work. You 
;    will have an error thrown if ENVI has not started in the parent process. 
;    
;    If there is no error during processing, then there will be a key called
;    'OUTPUT_RASTER_URI' in the results hash that contains the output location.
;    The error will be contained in 'EXPORT_ERROR' if present.
;
; :Example:
;    ;Start ENVI so that we have access to rasters and tasks
;    e = envi(/HEADLESS)
;    compile_opt idl2
;    
;    ; Open an input file
;    File = Filepath('qb_boulder_msi', Subdir = ['data'], $
;      Root_Dir = e.Root_Dir)
;    Raster = e.OpenRaster(File)
;    
;    ; Get the task from the catalog of ENVITasks
;    task = ENVITask('SpectralIndices')
;    
;    ; Define inputs
;    task.INDEX = ['Normalized Difference Vegetation Index', $
;      'Visible Atmospherically Resistant Index']
;    task.INPUT_RASTER = Raster
;    
;    ; init our object
;    oBdg = bridge_it(1, INIT = 'e = envi(/HEADLESS)')
;    
;    ;export the raster a few times
;    for i=0, 2 do begin
;      ;specify output file
;      outFile = e.GetTemporaryFilename()
;      
;      ;export to disk
;      oBdg.ExportRaster, raster, outFile, DATA_IGNORE_VALUE = 0
;    endfor
;    
;    ;wait for everything to finish
;    oBdg.wait
;    
;    ;get the results from running our tasks
;    results = oBdg.GetResults()
;    
;    ;loop over and print our output raster location
;    foreach result, results do begin
;      if result.hasKey('$EXPORT_ERROR') then begin
;        print, 'Error exporting raster : ' + result['$EXPORT_ERROR']
;      endif else begin
;        ;print the output file
;        print, 'Output raster URI: ' + result['$OUTPUT_RASTER_URI']
;      endelse
;    endforeach
;
; :Params:
;    raster: in, required, type=ENVIRaster
;      Set this to any raster that has the export method to be written to disk. 
;    outFile: in, required, type=string
;      Must be set to the fully-qualified filepath on disk for where the virtual 
;      raster will be written to disk.
;    format: in, optional, type=string
;      Specify the output file format. Supports any value of format from the 
;      default `raster.export` method (see the docs).
;
; :Keywords:
;    DATA_IGNORE_VALUE: in, optional, type=number
;      Set the optional data ignore value for the virtual raster when
;      exporting to disk.
;    TIME: in, optional, type=boolean
;      Set this keyword to have the process be timed.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
; 
;-
pro bridge_it::ExportRaster, raster, outFile, format, DATA_IGNORE_VALUE = data_ignore_value, TIME = time
  compile_opt idl2
  
  ;get current session of ENVI
  e = envi(/CURRENT)
  
  ;make sure that ENVI has been started
  if (e eq !NULL) then begin
    message, 'ENVI has not been started yet, required for exporting virtual rasters to disk'
  endif
  
  ;make sure we passed in all of our arguments
  if (raster eq !NULL) then begin
    message, 'Virtual raster not specified, required argument!'
  endif
  
  if (outFile eq !NULL) then begin
    message, 'Output file not specified, required argument!'
  endif
  
  if (format eq !NULL) then begin
    format = 'ENVI'
  endif

  ;dehydrate our virtual raster into strings
  strings = json_serialize(raster.dehydrate())

  ;check if we have a data ignore value that we need to pass through
  ; can't use keyword set becausse we may pass in a value of '0'
  if (data_ignore_value ne !NULL) then begin
    ;run our task
    !NULL = self.Run('ENVIHydrate', $
      ARG1 = strings, $
      PRE = 'arg1 = json_parse(arg1)', $
      POST = 'output.Export, "' + outFile +'", "' + format + '", ERROR = exportError, DATA_IGNORE_VALUE = ' + strtrim(data_ignore_value,2),$
      EXPORTRASTER = outFile, TIME = time)
  endif else begin
    ;run our task
    !NULL = self.Run('ENVIHydrate', $
      ARG1 = strings, $
      PRE = 'arg1 = json_parse(arg1)', $
      POST = 'output.Export, "' + outFile +'", "' + format + '", ERROR = exportError',$
      EXPORTRASTER = outFile, TIME = time)
  endelse
end




;+
; :Description:
;    Procedure method that will run a task on an IDL_IDLBridge object. If successful,
;    then the results with contain a key called 'TASK', otherwise there will be a 
;    value called 'TASK_ERROR' containing the issue from the child process.
;
; :Params:
;    task: in, required, type=ENVITask
;      Specify the task that you want to run on an IDL_IDLBridge. You must start ENVI
;      on the bridges ahead of time in the INIT keyword when creating the bridge_it
;      object. See the example below.
; :Keywords:
;    TIME: in, optional, type=boolean
;      Set this keyword to have the process be timed.
;      
; :Example:
;    ;Start ENVI so that we have access to rasters and tasks
;    e = envi(/HEADLESS)
;    compile_opt idl2
;    
;    ; Open an input file
;    File = Filepath('qb_boulder_msi', Subdir = ['data'], $
;      Root_Dir = e.Root_Dir)
;    Raster = e.OpenRaster(File)
;    
;    ; Get the task from the catalog of ENVITasks
;    task = ENVITask('SpectralIndices')
;    
;    ; Define inputs
;    task.INDEX = ['Normalized Difference Vegetation Index', $
;      'Visible Atmospherically Resistant Index']
;    task.INPUT_RASTER = Raster
;    
;    ; init our object
;    oBdg = bridge_it(1, INIT = 'e = envi(/HEADLESS)')
;    
;    ;run the task a few times
;    for i=0, 2 do begin
;      ;set a new output raster URI - MUST DO THIS or all rasters have the same output
;      task.OUTPUT_RASTER_URI = e.GetTemporaryFilename()
;      oBdg.RunENVITask, task
;    endfor
;    
;    ;wait for everything to finish
;    oBdg.wait
;    
;    ;get the results from running our tasks
;    results = oBdg.GetResults()
;    
;    ;loop over and print our output raster location
;    foreach result, results do begin
;      if result.hasKey('$TASK_ERROR') then begin
;        print, 'Error for task: ' + result['$TASK_ERROR']
;      endif else begin
;        ;get the task from our results
;        task = result['$TASK']
;    
;        ;print the output file
;        print, 'Output raster URI: ' + task.OUTPUT_RASTER_URI
;      endelse
;    endforeach
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
; 
;-
pro bridge_it::RunENVITask, task, TIME = time, _UVALUE = uvalue
  compile_opt idl2
  ;get current session of ENVI
  e = envi(/CURRENT)

  ;make sure that ENVI has been started even though they shouldn't have the ability to make
  ;a task without ENVI up and running...
  if (e eq !NULL) then begin
    message, 'ENVI has not been started yet, required for exporting virtual rasters to disk'
  endif
  
  ;check our inputs
  if (task eq !NULL) then begin
    message, 'task not specified, required argument!'
  endif

  ;dehydrate our task into strings
  strings = json_serialize(task.dehydrate())
  
  ;run our task
  !NULL = self.Run('ENVITask', $
    ARG1 = strings, $
    PRE = 'arg1 = json_parse(arg1)', $
    POST = 'output.Execute, ERROR = taskError',$
    /TASK, TIME = time, _UVALUE = uvalue)
  
end



;+
; :Description:
;    Procedure method for running either procedures or functions in for the BRIDGE_IT object.
;
; :Params:
;    routine: in, required, type=sting
;       Name of a function or procedure that you will run with the BRIDGE_IT object. If you have
;       a function, then you must also set the `IS_FUNCTION` keyword.
;
; :Keywords:
;    _extra : in, required
;       This keyword represents all of the parameters and keywords which are to be passed to the
;       child process. Similar to the procedure method for the bridge_it object, keyword must
;       start with "_KW_" and arguments are sorted alphabetically and can have any name other
;       than starting with '_KW_"
;    ARGUMENTS_OUT: in, optionsal, type-string/stringarr
;       Similar to `KEYWORDS_OUT`, this keyword is set the the names of arguments that will ahve their values
;       returned in the callback function. If you want to returnt he value of a positional argument, then
;       you must also pass that in as a keyword. In other words you need to do this::
;           void = bdg_it.run('min', ARG1=findgen(10), ARG2 = 1, ARGUMENTS_OUT = 'ARG2', KEYWORDS_OUT = 'MAX', /CALLBACK, /TIME)
;       Where you specify ARG2 as a positional argument and it is listed in the ARGUMENTS_OUT keyword.
;    CALLBACK : in, optional
;       If this keyword is set, you can use it to return the function results from the child
;       process. You can get these results with the bridge_it::getresults method. For this keyword to
;       be considered, you need to also set `IS_FUNCTION`.
;    IS_FUNCTION: in, optional, type=int, default=0
;       Set this keyword to 1 so that the `routine` argument will be treated as a function instead of a procuedure.
;    KEYWORDS_OUT: in, optional, type=string/stringarr
;       Set this keyword to a string or string array with the names for all of the keywords that
;       return values for the procedure you are running. This will automatically send the results
;       to a hash where a separate key will be created for the individual variables that are
;       being passed back and forth.
;    TIME : in, optional
;       Setting this keyword will have the IDL_IDLBridge child process use the callback procedures
;       to print out the time that it took to run the child process.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
pro bridge_it::RunRoutine, routine, _extra = args, $
  CALLBACK = callback,$
  CUSTOM_ARGS = custom_args,$
  ARGUMENTS_OUT = arguments_out, $
  IS_FUCNTION = is_function, $
  KEYWORDS_OUT = keywords_out, $
  TIME = time,$
  PRE = pre,$
  POST = post,$
  VARIABLES_OUT = variables_out,$
  TASK = task,$
  EXPORTRASTER = exportRaster,$
  _UVALUE = uvalue
  compile_opt idl2

  ;get info about the object
  nbridges = self.nbridges
  bridges = *self.bridges

  ;find first free bridge
  count = 0
  while 1 do begin
    if ~count then begin
      istart = self.lastrun+1
      count++
    endif else istart = 0
    for i=istart, nbridges-1 do if ((bridges[i]).status() eq 0) then GOTO, endloop
    if (1 eq 0) then begin
      ENDLOOP:
      runbridge = bridges[i]
      bridgenum = i
      self.lastrun = bridgenum
      break
    endif
    ;small wait so we dont just spend whole CPU checking for free bridges
    wait, .02
  endwhile


  ;check if bridge needs to be refreshed
  runs = *self.nruns
  if (runs.since_reset[bridgenum] ge self.nrefresh) then begin

    ;update counts to be 0 because we are resetting the bridge
    runs.since_reset[bridgenum] = 0

    ;destroy the bridge object
    if obj_valid((*(self.bridges))[bridgenum]) then obj_destroy, (*(self.bridges))[bridgenum]
    wait, .05
    ;manage existing log files
    if (self.logdir ne '') then begin

      logfile = self.logdir + path_sep() + 'out' + strtrim(bridgenum,2) +'.txt'

      ;have a log file, so save the contents if we have more than 0 lines
      if file_test(logfile) then begin
        nlines = file_lines(logfile)
        if (nlines gt 0) then begin
          strings = strarr(nlines)
          openr, lun, logfile, /GET_LUN
          readf, lun, strings
          free_lun, lun
          (self.logs).add, strings
        endif
      endif

      (*(self.bridges))[bridgenum] = IDL_IDLBridge(OUTPUT = logfile)
    endif else begin
      (*(self.bridges))[bridgenum] = IDL_IDLBridge()
    endelse

    ;define runbridge
    runbridge = (*(self.bridges))[bridgenum]

    ;see if we have to execute our init string
    if (self.init ne '') then begin
      runbridge.execute, self.init
    endif
  endif

  ;create a hash for saving information
  runhash = hash()
  callback_flag = 0

  ;check if we have anything that we need to do after the bridge is done running
  if (n_elements(post) gt 0) then begin
    runhash['$POST_CALL'] = post
    callback_flag = 1
  endif

  ;init strings for args and keywords
  kw_add = ''
  var_add = ''


  ;get inputs and check for keywords to pass along
  if (args ne !NULL) then begin
    var_names = tag_names(args)
    ;filter out keywords to pass along which should be prefaces by "_KW_"
    if (total(var_names.startswith(self.prefix)) gt 0) then begin
      keywords = var_names[where(var_names.startswith(self.prefix) eq 1)]
      for i=0, n_elements(keywords)-1 do begin
        kwname = strmid(keywords[i],4)
        keywords[i] = kwname
        runbridge.setvar, kwname, args.((where(var_names eq self.prefix + kwname))[0])
      endfor
      kw_add = strjoin(strupcase(keywords)+' = '+strlowcase(keywords),', ')
    endif

    ;check variables
    if (total(~var_names.startswith(self.prefix)) gt 0) then begin
      variables = var_names[where(var_names.startswith(self.prefix) eq 0)]

      ;send variables to child process
      for i=0, n_elements(variables)-1 do begin
        runbridge.setvar, variables[i], args.((where(var_names eq variables[i]))[0])
      endfor
      var_add = strjoin(variables, ', ')
    endif
  endif
  
  ;check if we have any custom arguments to also pass in (variables that may be present)
  if (n_elements(custom_args) ne 0) then begin
    if (var_add ne '') then var_add += ','
    var_add += strjoin(custom_args, ',')
  endif

  ;check if we need to remember the time difference
  if keyword_set(time) then begin
    runhash['$START_TIME'] = systime(/seconds)
    callback_flag = 1
  endif

  ;check if we have keywords to get the return values for
  if keyword_set(keywords_out) then begin
    ;no keyword specified previously
    if (kw_add eq '') then begin
      kw_return_add = strjoin(strupcase(keywords_out) + ' = ' + strlowcase(keywords_out),', ')
      kw_add = kw_return_add
      ;do have keywords specified
    endif else begin
      ;check and see if we already have the keyword as an input
      foreach kw_out, keywords_out do begin
        if (total(kw_out eq keywords) eq 0) then kw_add += ', ' + strupcase(kw_out) + ' = ' + strlowcase(kw_out)
      endforeach
    endelse

    runhash['$KEYWORDS_OUT'] = keywords_out

    callback_flag = 1
  endif

  ;see if we need to return positional arguments as well
  if keyword_set(arguments_out) then begin
    arg_out_list = list()
    ;check if we have passed in our outward arguments as arguments already
    ;if not, then lets add then to our arguments string in the order they are specified
    ;in our keyword
    foreach arg_out, arguments_out do begin
      if (total(strlowcase(arg_out) eq strlowcase(variables)) eq 0) then begin
        if (var_add ne '') then begin
          var_add += ', ' + arg_out
        endif else begin
          var_add += arg_out
        endelse
      endif

      arg_out_list.add, arg_out
    endforeach
    if (n_elements(arg_out_list) gt 0) then begin
      runhash['$ARGUMENTS_OUT'] = arg_out_list.toarray()
      callback_flag = 1
    endif
  endif

  if (n_elements(variables_out) gt 0) then begin
    runhash['$VARIABLES_OUT'] = variables_out
    callback_flag = 1
  endif

  ;add an extra comma between the variables and keywords if we have both
  if (var_add ne '') AND (kw_add ne '') then begin
    comma_add = ', '
  endif else begin
    comma_add = ''
  endelse

  ;see if we have a function or procedure to run
  if keyword_set(is_function) then begin
    ;if we have a function, check to see if we need to get the results back
    if keyword_set(callback) then begin
      runhash['$FUNCTION_RETURN'] = 1
      callback_flag = 1
    endif
    ;make runstring to execute
    runstring = 'output = ' + routine + '(' + var_add + comma_add + kw_add + ')'
  endif else begin
    ;make string to execute
    runstring = routine + ', ' + var_add + comma_add + kw_add
  endelse

  ;check if we have any code we want to run ahead of time
  if (n_elements(pre) gt 0) then begin
    ;runhash['$PRE_CALL'] = pre
    runstring = strjoin(pre, ' & ') + ' & ' + runstring
  endif

  if (n_elements(post) gt 0) then begin
    ;runhash['$POST_CALL'] = post
    runstring += ' & ' + strjoin(post, ' & ')
  endif

  if keyword_set(task) then begin
    runhash['$TASK'] = 1
    callback_flag = 1
  endif

  if keyword_set(exportRaster) then begin
    runhash['$EXPORTRASTER'] = 1
    runhash['$OUTPUT_RASTER_URI'] = exportRaster
    callback_flag = 1
  endif

  ;create a hash to hold the results if needed
  runhash['$RUNSTRING'] = runstring

  ;check if we need a callback function
  if (callback_flag) then begin
    ;remember the bridge numner, total number of runs, and the runs since reset
    runhash['$BRIDGE_NUMBER'] = bridgenum
    runhash['$RUN_NUMBER'] = runs.total[bridgenum]+1
    runhash['$RUNS_SINCE_RESET'] = runs.since_reset[bridgenum]+1
    runhash['SELF']= self
    runbridge.SetProperty, CALLBACK = 'bridge_it_callback'
  endif
  
  ;check if we have a uval
  if (n_elements(uvalue) gt 0) then begin
    runhash['$UVALUE'] = uvalue
  endif

  ;set the runhash as a property
  runbridge.SetProperty, USERDATA = runhash

  ;update the number of processes executed
  runs.since_reset[bridgenum] += 1
  runs.total[bridgenum] += 1

  *self.nruns = runs
  ;execute process
  runbridge.execute, runstring, /NOWAIT
end


;+
; :Description:
;    Method to force the main IDL procedure to halt until each bridge is idle again
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;
;-
pro bridge_it::Wait
  compile_opt idl2, hidden
  nbridges = self.nbridges
  bridges = *self.bridges
  ;wait for every bridge to be idle before proceeding
  count = 0*indgen(nbridges)
  while 1 do begin
    for i=0, nbridges-1 do count[i] = (1 < (bridges[i]).status())
    if ~total(count) then break else wait, .05
  endwhile
end


;+
; :Description:
;    Object definition for the BRIDGT_IT object.
;
;-
pro bridge_it__define
  compile_opt idl2, hidden
  struct = {BRIDGE_IT, $
    prefix:'',$             ;string representing the prefix keywords need to be differentiated from arguments
    bridges:ptr_new(1),$    ;pointer to an array containing the IDL_IDLBridge object references
    lastrun:0l,$             ;last bridge to run a process, for determining the next bridge to check and see if it is free
    nruns:ptr_new(1),$      ;pointer to a structure containing the total number of runs and the runs since resetting a child process
    nbridges:0l,$           ;remember the number of bridges we will create
    callback:hash(),$       ;for returning results from keywords, arguments, and functions so that we can save them
    nrefresh:0l,$           ;number of processes that have to be executed before the bridge will refresh
    logdir:'',$             ;the location of the log directory
    logs:list(),$           ;list to hold the logs from each child process
    init:'',$               ;string that is executed each time we initialize a bridge object
    msg:''}                 ;text message to be displayed before the time to run a process is printed
end