;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
;
; Licensed under MIT, see LICENSE.txt for more details.
;h-

;+
; :Private:
;
; :Description:
;    Test for generic band alignment. This uses 4 of the 5 RedEdge bands to test,
;    so you must specify a folder of RedEdge data.
;
;
;
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro uav_toolkit_test_generic_multipage, flightDir
  compile_opt idl2
  on_error, 2

  ;start ENVI headlessly
  e = envi(/HEADLESS)

  ;this folder should be the one that contains the unzipped contents of the sample data.
  if (flightdir eq !NULL) then begin
    flightDir = dialog_pickfile(/DIRECTORY)
  endif

  ;check that flightDir was specified
  if ~keyword_set(flightDir) then begin
    message, 'flightDir not specified, required!'
  endif

  if ~file_test(flightDir, /DIRECTORY) then begin
    message, 'flightDir specified, but does not exist!
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
  alignTask.SENSOR = 'generic'
  alignTask.RIGOROUS_ALIGNMENT = !FALSE
  alignTask.FILE_IDENTIFIERS = ['.tif']
  alignTask.INPUTDIR = flightDir
  alignTask.GENERATE_REFERENCE_TIEPOINTS = 1
  alignTask.APPLY_REFERENCE_TIEPOINTS = 1
  alignTask.execute

  ;succeeded so reset our path back to the original
  pref_set, 'IDL_PATH', pathOrig, /COMMIT
end