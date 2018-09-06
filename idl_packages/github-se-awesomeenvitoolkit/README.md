# SE-AwesomeENVIToolkit

A collection of tools to help make programming with the ENVI API easier than ever and streamline the process to create new analytics/routines. Below you will find a short description of each of the routines that are included.

## AwesomeENVITask

This function has an alternative approach to instantiating ENVI tasks that does the same thing as the `IDLTask` function. If the task you want to create does not exist in ENVI's task catalog, then it will search IDL's path and load the task from disk. This is very useful for programmers and those that do not want to install ENVI tasks in the `custom_code` directory. Here is an example:

```idl
; open a task not in ENVI's custom_code folder
task = awesomeENVITask('MySuperTask')

; open a task that is in ENVI's task catalog
task = awesomeENVITask('ISODATAClassification')
```

## AwesomeENVIProgress

Helpful tool that simplies sending progress messages to the ENVI interface with custom routines and includes features for printing messages to the IDL console, timing processes, and checking for users cancelling progress.

Here is an example that uses most of the keywords for processing.

```idl
; start ENVI
e = envi()

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
```

The results of the above code should look something like this:

```
My Super Progress Message
  Processing..., Progress=10 [0.1 (s) left]
  Processing..., Progress=20 [4.1 (s) left]
  Processing..., Progress=30 [4.7 (s) left]
  Processing..., Progress=40 [4.5 (s) left]
  Processing..., Progress=50 [4.0 (s) left]
  Processing..., Progress=60 [3.3 (s) left]
  Processing..., Progress=70 [2.6 (s) left]
  Processing..., Progress=80 [1.7 (s) left]
  Processing..., Progress=90 [0.9 (s) left]
  Processing..., Progress=100 [0.0 (s) left]
Finished!
```

In addition to the above example, you **must** properly handle errors when using progress messages or, if you have the ENVI interface open, you will see old progress dialogs. Here is how you can do that upon object creation:

```idl
;handle errors to properly clean up our progress
catch, err
if (err ne 0) then begin
  catch, /CANCEL
  prog.finish, /PRINT, /ERROR
  message, /REISSUES_LAST
endif

; create our progress object
prog = awesomeENVIProgress('My Super Progress Message', /PRINT)
```

### Note for Developers

There is some basic logic within the awesome progress that checks to see if a "parent" progress dialog/object has already been created. When this is the case, then all "child" progress messages do not get printed to the IDL Console to make things much cleaner especially for enterprise deployments. If you get into a bad state and messages are no longer printing, then you just need to run the following line to fix the problem:

```idl
awesomeENVIProgress, /CLEAR_PROGRESS
```

## AwesomeCatchBlock

The `awesomeCatchBlock` is a simple catch block that helps simplify and standardize error catching from within different routines. The contents of the catch block are as follows:

```idl
if ~keyword_set(debug) then begin
  on_error, 2
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    help, /LAST_MESSAGE
    message, /REISSUE_LAST
  endif
endif
```

The catch block should be used at the beginning of routines like this:

```idl
pro someProcedure, DEBUG = debug
  compile_opt idl2, hidden
  @awesomeCatchBlock

  ...

end
```

Note that, if you add another catch block after the `awesomeCatchBlock` then you should use `catch, /CANCEL` like this, before

```idl
pro someProcedure, DEBUG = debug
  compile_opt idl2, hidden
  @awesomeCatchBlock

  ...

  ; handle errors and clean up
  catch, /CANCEL
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    prog.finish, /ERROR
    message, /REISSUE_LAST
  endif

  ...

end
```
