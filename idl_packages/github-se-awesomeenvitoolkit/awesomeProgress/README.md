# Better Progress Message

An improved API for setting ENVI progress messages build on what is provided in ENVI. Simply wraps the existing objects that are used to create progress dialogs in ENVI.


## Requirements

ENVI 5.4 or later. Should work on ENVI 5.3 as well (untested).


## Usage

Once you have added the object to IDL's search path, you can simply use the folowing example for how to initialize, update progress, and check to see if a user has pressed the cancel button:

```
;start ENVI
e = envi()

;initialize the progress message 
prog = awesomeENVIProgress('Progress title')

;set our first progress update
prog.SetProgress, 'Processing...', 50

;check to see if a user has tried to cancel processing
print, prog.AbortRequested()

;clean up properly
prog.finish
```


## Notes on Proper Usage

When using progress messages in ENVI, you need to make sure that you are properly handling errors so that the progress messages are closed correctly. Otherwise they can be left in a bad state and new ones will no longer pop up. To do this, simply use a catch statement after you intialize the progress message:

```
;start ENVI
e = envi()

;initialize the progress message 
prog = awesomeENVIProgress('Progress title')

;properly hadle errors
catch, err
if (err ne 0) then begin
    catch, /CANCEL
    prog.finish
    message, /REISSUE_LAST
endif

;set our first progress update
prog.SetProgress, 'Processing...', 50

;check to see if a user has tried to cancel processing
print, prog.AbortRequested()

;clean up properly
prog.finish
```


## Licensing

Licensed under MIT. Full details in LICENSE.txt.