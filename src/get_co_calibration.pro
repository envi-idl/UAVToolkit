;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

;+
; :Private:
; 
;   Internal routine used in the bandalignmenttask.pro file. Docs here may not be up to date.
;
;   Generates .sav files containing three variables for each image:
;      cocalibration_constant, reference_means, reference_stddevs
;   
;   If the first is present, then the images are scaled in value to account for differences
;   in exposure and ISO speed. The second variable is used to convert the images to reflectance
;   after the image registration has happened and it will be scaled from 0 to 10000. The last
;   variable is not used, but passed in case used in the future.
; 
; 
; 
;-


;+
;
; :Keywords:
;    REFERENCE_IES : in, optional, type=floatarr
;       Set this keyword to the reference IE (ISO*exposure) values for the reflectance panels.
;    REFERENCES_MEANS : in, optional, type=floatarr
;       Set this keyword to the reference pixel values for the reflectance panels. If present, then
;       the IE's generated for each image will contain the corresponding mean so that they can be converted to 
;       reflectance after the image registration process.
;    REFERENCE_STDDEVS : in, optional, type=floatarr
;       Set this keyword to the standard deviations of the pixel values for the reflectance panels. These 
;       values are not currently used, just passed in for future use if needed.
;       
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro get_co_calibration, $ 
  DEBUG = debug,$
  GROUPS = groups,$
  PANEL_INFO = panel_info
  compile_opt idl2, hidden
  if ~keyword_set(debug) then on_error, 2
  
  ;add some progress information
  prog = awesomeENVIProgress('Generating Calibration Information', /PRINT)

  ;extract information from our panel_info
  wasCalibrated = panel_info.hasKey('WAS_CALIBRATED') ? panel_info.WAS_CALIBRATED : 0
  
  ;determine what our progress tring should be
  if (wasCalibrated) then begin
    progStr = 'Calibrating ' + strtrim(n_elements(groups),2) + ' images...'
  endif else begin
    progStr = 'Calibrating ' + strtrim(n_elements(groups),2) + ' images to reference...'
  endelse

  ; alert user
  prog.setProgress, progStr, 0, /PRINT

  ;get info on how many files we need to process
  gNames = groups.keys()
  nPrint = floor(n_elements(gnames)/20) > 10
  nBands = n_elements(groups[gNames[0]])
  
  ;extract information from our panel info
  reference_means = panel_info.hasKey('PANEL_MEANS') ? panel_info.PANEL_MEANS : fltarr(nBands) + 1
  reference_stddevs = panel_info.hasKey('PANEL_STDDEVS') ? panel_info.PANEL_STDDEVS : fltarr(nBands)

  ;check to see if we have a reference group or not
  ;if not pick the first image that we found
  if ~keyword_set(reference_group_name) then begin
    reference_group_name = gNames[0]
  endif

  ;generate relative reference intensity and exposure values if we have not been provided reference information
  if ~panel_info.hasKey('PANEL_IES') then begin
    ;get the reference exposure and IS) settings
    ;the ISO is the same for all images captured at the same time, but
    ;exposure seems to change from band to band
    ; ref_ie = reference iso and exposure
    ref_imgs = groups[reference_group_name]
    ref_ie = make_array(nbands, TYPE=5, /NOZERO)

    ;loop over each band
    for i=0, nbands-1 do begin
      ;extract information
      oImageinfo = obj_new('image_info', ref_imgs[i], /NO_SPATIALREF)
      res = oImageInfo.Get(['EXPOSURETIME', 'ISO'])

      ;check to make sure that we have information for this image
      if ((res.EXPOSURETIME eq -!DPI) OR (res.ISO eq -!DPI)) then begin
        return
      endif

      ;save value
      ref_ie[i] =  res.EXPOSURETIME*res.ISO
    endfor
  endif else begin
    ref_ie = panel_info.PANEL_IES
  endelse

  ;get the reference ISO and exposure time so that we can correctly calibrate
  ;the images to a reference group

  ;calculate transformation function for each image
  gcount = 0
  foreach group_name, gNames do begin
    ;init hash to store data
    cocalibration = hash()

    ;get the image group
    images = groups[group_name]

    ;loop over each image
    for i=0, nBands-1 do begin
      if (wasCalibrated) then begin
        cocalibration_constant = 1.0
      endif else begin
        ;extract information from our images
        oImageinfo = obj_new('image_info', images[i], /NO_SPATIALREF)
        res = oImageInfo.Get(['EXPOSURETIME', 'ISO'])

        ;validate that we have the right metadata
        if ((res.EXPOSURETIME eq -!DPI) OR (res.ISO eq -!DPI)) then begin
          print, 'Found image without exposure and ISO information so we cannot complete for rest of scene, removing generated files and returning...'
          cd, inputdir, CURRENT = first_dir
          files = file_search('*.sav')
          if (files[0] ne '') then file_delete, files
          cd, first_dir
          return
        endif

        ;calcualte the cocalibration constant
        cocalibration_constant = ref_ie[i]/(res.EXPOSURETIME*res.ISO)
      endelse

      ;save to disk
      outfile = strmid(images[i], 0, strpos(images[i], '.', /REVERSE_SEARCH)) + '_co_calibration.sav'
      save, cocalibration_constant, reference_means, reference_stddevs, FILENAME = outfile
    endfor
    
    ;increment counter and check if we want to print to the screen
    gcount++
    if ~(gcount mod nprint) then begin
      prog.setProgress, progStr, 100*float(gCount)/n_elements(gnames), /PRINT, /TIME
    endif

    ; check if someone cancelled
    if prog.abortRequested() then begin
      message, 'Process stopped by user', LEVEL = -1
    endif
  endforeach
  
  ;alert user
  prog.finish, /PRINT
end
