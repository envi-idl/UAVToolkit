;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT, see LICENSE.txt for more details.
;h-

;+
; :Private:
;
; :Description:
;    Test for extracting reflectance panels from RedEdge data.
;
;
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro uav_toolkit_test_rededge_panel_extraction, panelDir
  compile_opt idl2
  on_error, 2

  ;start ENVI headlessly
  e = envi(/HEADLESS)

  ;this folder should be the one that contains the unzipped contents of the sample data.
  if (panelDir eq !NULL) then begin
    panelDir = dialog_pickfile(/DIRECTORY)
  endif
  
  ;check that flightDir was specified
  if ~keyword_set(panelDir) then begin
    message, 'panelDir not specified, required!'
  endif

  if ~file_test(panelDir, /DIRECTORY) then begin
    message, 'panelDir specified, but does not exist!
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

  ;search our folder for image groupd
  groups = bandalignment_get_image_groups(panelDir, ['_1.tif', '_2.tif', '_3.tif', '_5.tif', '_4.tif'])
  nGroups = n_elements(groups)
  
  ;skip directory if there are no TIF files
  if (nGroups eq 0) then begin
    message, 'No image groups found in panelDir, required for the test!'
  endif
  
  ;extract panels from each group
  foreach group, groups, i do begin
    ;attempt to extract the reflectance panels
    panel_info = get_reflectance_panels(group, 'rededge',$
      PANEL_REFLECTANCE = 70)
    print, 'Processed image group ' + strtrim(i + 1,2) + ' of ' + strtrim(nGroups,2)
  endforeach
    
  ;succeeded so reset our path back to the original
  pref_set, 'IDL_PATH', pathOrig, /COMMIT
end
