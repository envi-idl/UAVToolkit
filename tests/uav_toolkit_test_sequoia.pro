; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

;+
; :Private:
;
; :Description:
;    Test for sequoia band alingment.
;
;
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro uav_toolkit_test_sequoia
  compile_opt idl2
  on_error, 2

  ;start ENVI headlessly
  e = envi(/HEADLESS)

  ;this folder should be the one that contains the unzipped contents of the sample data.
  flightDir = dialog_pickfile(/DIRECTORY)

  if ~file_test(flightDir, /DIRECTORY) then begin
    message, 'flightDir does not exist!
  endif

  ;get original path and reset
  pathOrig = !PATH
  pref_set, 'IDL_PATH', /DEFAULT, /COMMIT

  ;update path to this parent directory which will mimick a "fresh" installation
  thisDir = file_dirname(routine_filepath())
  parentDir = file_dirname(thisDir)
  newPath = !PATH + path_sep(/SEARCH_PATH) + '+' + parentDir
  pref_set, 'IDL_PATH', newPath, /COMMIT

  ;reset search path
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    pref_set, 'IDL_PATH', pathOrig, /COMMIT
    message, /REISSUE_LAST
  endif

  ;initialize the uav_toolkit
  uav_toolkit

  ;set up task
  alignTask = ENVITask('UAVBandAlignment')
  alignTask.SENSOR = 'sequoia'
  alignTask.INPUTDIR = flightDir
  alignTask.GENERATE_REFERENCE_TIEPOINTS = 0
  alignTask.APPLY_REFERENCE_TIEPOINTS = 1
  alignTask.execute
  
  ;succeeded so reset our path back to the original
  pref_set, 'IDL_PATH', pathOrig, /COMMIT
end