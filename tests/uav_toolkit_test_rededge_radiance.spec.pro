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
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro uav_toolkit_test_rededge_radiance, file
  compile_opt idl2
;  on_error, 2
  ireset, /NO_PROMPT

  ;start ENVI headlessly
  e = envi(/HEADLESS)

  ;check that flightDir was specified
  if ~keyword_set(file) then begin
    message, 'file not specified, required!'
  endif

  if ~file_test(file) then begin
    message, 'file specified, but does not exist!
  endif

  ;get original path and reset
  pathOrig = !PATH
  pref_set, 'IDL_PATH', /DEFAULT, /COMMIT

  ;update path to this parent directory which will mimick a "fresh" installation
  thisDir = file_dirname(routine_filepath())
  parentDir = file_dirname(thisDir)
  newPath = !PATH + path_sep(/SEARCH_PATH) + '+' + parentDir
  pref_set, 'IDL_PATH', newPath, /COMMIT
  path_cache, /REBUILD

  ;reset search path
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    pref_set, 'IDL_PATH', pathOrig, /COMMIT
    message, /REISSUE_LAST
  endif

  ;initialize the uav_toolkit
  uav_toolkit
  
  ;resolve our necessary routine which has a sub routine that we use for finding the panel
  resolve_routine, 'get_reflectance_panels', /EITHER, /COMPILE_FULL_FILE
  
  ;convert the data to radiance
  rad = rededge_to_radiance(file, /VERBOSE)
  
  ;define the image subset for the panel pixels
  sub = [660, 490, 840, 670]
  
  ;define the percent reflectance that our data is
  panelReflectance = 0.61
  
  ;extract panel data
  panelDat = rad[sub[0]:sub[2], sub[1]:sub[3]]
  
  ;make image to show the panel
  panelDisplay = 0*rad
  panelDisplay[sub[0]:sub[2], sub[1]:sub[3]] = panelDat

  ;convert our panel to reflectance
  radianceToReflectance = panelReflectance / panelDat.mean()
  refl = rad*(panelReflectance/panelDat.mean())
  panelRefl = panelDat*radianceToReflectance
  
  ;auomatically extract panel pixels
  panel_pixels = get_refelctance_panels_extract_panel(rad)
  if (panel_pixels eq !NULL) then begin
    message, 'No reflectance panel pixels found in the scene, required!'
  endif
  
  ;create separate variable for displaying the panel information
  AutoPanelDisplay = 0*rad
  AutoPanelDisplay[panel_pixels] = rad[panel_pixels]
  
  ;print information for the panel
  print
  print, 'Min radiance        : ', strtrim(panelDat.min(),2)
  print, 'Max radiance        : ', strtrim(panelDat.max(),2)
  print, 'Mean radiance       : ', strtrim(panelDat.mean(),2)
  print, 'Radiance std. dev.  : ', strtrim(stddev(panelDat),2)
  print
  print, 'Radiance to refl.   : ', strtrim(radianceToReflectance, 2)
  print
  print, 'Min reflectance        : ', strtrim(panelRefl.min(),2)
  print, 'Max reflectance        : ', strtrim(panelRefl.max(),2)
  print, 'Mean reflectance       : ', strtrim(panelRefl.mean(),2)
  print, 'Reflectance std. dev.  : ', strtrim(stddev(panelRefl),2)
  
  ;create window for displaying graphics
  w = window(DIMENSIONS = [1200, 900])
  im1 = image(rad, LAYOUT = [2,2,1], CURRENT = w, TITLE = 'Radiance Image', /ORDER)
  im2 = image(panelDisplay, LAYOUT = [2,2,2], CURRENT = w, TITLE = 'Reflectance Panel Pixels (manual)', /ORDER)
  im3 = image(smooth(panelRefl, 5, /EDGE_MIRROR), LAYOUT = [2,2,3], CURRENT = w, TITLE = 'Reflectance Panel as % reflectance', /ORDER)
  im2 = image(AutoPanelDisplay, LAYOUT = [2,2,4], CURRENT = w, TITLE = 'Reflectance Panel Pixels (automated)', /ORDER)

  ;succeeded so reset our path back to the original
  pref_set, 'IDL_PATH', pathOrig, /COMMIT
end


uav_toolkit_test_rededge_radiance, 'C:\Users\Traininglead\Downloads\rededgeTest\calibration-micasense\000\reflectance_panels\IMG_0000_4.tif'
end
