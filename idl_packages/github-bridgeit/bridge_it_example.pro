;+
;   
;   Example for how to use the bridge_it object. This object allows for a user to easily 
;   distribute processing between IDL_IDLBridge objects without needing to do anything to
;   set up the bridges, which can be hard at times. The examples below cover the syntax
;   for most of the methods that you can use with the bridge_it object.
;
; :Author: Zachary Norman
;-



pro bridge_it_example
  compile_opt idl2
  
  ;catch statement is necessary to properly clean up the bridge processes so we
  ;don't have zombie child processes running around
  ;following this catch block will also keep IDL from freezing when you try to reset
  ;while a child process is running.
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    bdg_it.cleanup
    help, /LAST_MESSAGE, output = msg
    print, msg
    retall
  endif
  
  ;number of bridge processes to create
  nbridges = 1
  thisdir = FILE_DIRNAME(((SCOPE_TRACEBACK(/STRUCT))[-1]).FILENAME)

  ;optional init keyword to set preferences in the child process
  init = $
    ;add this dir to path
    ['PREF_SET, "IDL_PATH", !PATH + path_sep(/SEARCH_PATH) + "' + thisdir + '",/COMMIT']

  ;create the child processes and specify the log directory which wil have all_logs.txt for the 
  ;output/print statements from all child processes
  ;this will also greatly help with debugging child processes executing procedures/functions
  bdg_it = bridge_it(nbridges, INIT = init, LOGDIR = thisdir, NREFRESH = 10)
  
  ;=================================================================================
  ;do some processing
  ;create 1 plot with function graphics
  ;use the time keyword to display the time is takes to complete the bridge process
  void = bdg_it.run('plot',ARG1 = findgen(10), ARG2 = 2*findgen(10),  _KW_COLOR='Red', /TIME)
  
  ;create 1 plot with direct graphics
  bdg_it.run, 'plot', ARG1=findgen(10), ARG2 = 2*findgen(10)
  
  ;run a function and get the function result back
  void = bdg_it.run('findgen', ARG1=3, ARG2 = 3, /CALLBACK, /TIME)
   
  ;run a procedure and get the results back from some one keyword
  bdg_it.run, 'triangulate', ARG1=randomu(!NULL, 25), ARG2 = randomu(!NULL, 25), ARG3 = 1, _KW_CONNECTIVITY = 1, ARGUMENTS_OUT = 'ARG3', KEYWORDS_OUT = 'CONNECTIVITY', /TIME

  ;run a function and get back a positional keyword and a positional argument
  void = bdg_it.run('min', ARG1=findgen(10), ARG2 = 1, ARGUMENTS_OUT = 'ARG2', KEYWORDS_OUT = 'MAX', /CALLBACK, /TIME)
  
  ;=================================================================================

  ;wait for all bridges to be done before proceeding
  bdg_it.wait
  print
  
  ;get the results from the bridges
  results = bdg_it.getResults()
  print, 'Bridge results: '
  print, results, /IMPLIED_PRINT
  print
  
  ;get the number of runs for each bridge
  nruns = bdg_it.getNruns()
  print, 'Information for bridge runs:'
  print, nruns, /IMPLIED_PRINT
  
  ;get the actual references to the IDL_IDLBridge objects
  bridges = bdg_it.getBridges()
  
  ;stop here so the graphics don't totally disappear
  ;they go away when the process that created them dies
  stop
  
  ;clean up the bridges
  bdg_it.cleanup
end





