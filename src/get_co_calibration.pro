; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

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
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro get_co_calibration, $
  GROUPS = groups,$
;  REFERENCE_GROUP_NAME = reference_group_name,$
  REFERENCE_IES = reference_ies,$
  REFERENCES_MEANS = reference_means,$
  REFERENCE_STDDEVS = reference_stddevs
  compile_opt idl2, hidden

  ;zero based number for the reference group
  if ~keyword_set(reference_means) then reference_means = -1
  if ~keyword_set(reference_stddevs) then reference_stddevs = -1

  print, 'Found ' + strtrim(n_elements(groups),2) + ' images to calibrate to reference group...'
  
  ;init timer
  tic
  
  ;get info on how many files we need to process
  gNames = groups.keys()
  nPrint = floor(n_elements(gnames)/20) > 10
  nBands = n_elements(groups[gNames[0]])

  ;check to see if we have a reference group or not
  ;if not pick the first image that we found
  if ~keyword_set(reference_group_name) then begin
    reference_group_name = gNames[0]
  endif

  ;generate relative reference intensity and exposure values if we have not been provided reference information
  if ~keyword_set(reference_ies) then begin
    ;get the reference exposure and IS) settings
    ;the ISO is the same for all images captured at the same time, but
    ;exposure seems to change from band to band
    ; ref_ie = reference intensity and exposure
    ref_imgs = groups[reference_group_name]
    ref_ie = make_array(nbands, TYPE=5, /NOZERO)
    
    ;loop over each band
    for i=0, nbands-1 do begin
      ;extract information
      oImageinfo = image_info(ref_imgs[i], /NO_SPATIALREF)
      res = oImageInfo.Get(['EXPOSURETIME', 'ISO'])
      
      ;check to make sure that we have information for this image
      if ((res.EXPOSURETIME eq -!DPI) OR (res.ISO eq -!DPI)) then begin
        return
      endif
      
      ;save value
      ref_ie[i] =  res.EXPOSURETIME*res.ISO
    endfor
  endif else begin
    ref_ie = reference_ies
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
    for i=0, nbands-1 do begin
      ;extract information from our images
      oImageinfo = image_info(images[i], /NO_SPATIALREF)
      res = oImageInfo.Get(['EXPOSURETIME', 'ISO'])
      
      ;validate that we have the right metadata
      if ((res.EXPOSURETIME eq -!DPI) OR (res.ISO eq -!DPI)) then begin
        print, 'Found image without exposure and ISO information so we cannot complete for rest of scene, removing generated files and returning...'
        cd, inputdir, CURRENT = first_dir
        files = file_search('*.sav')
        if (files[0] ne '') then FILE_DELETE, files
        cd, first_dir
        return
      endif
      
      ;calcualte the cocalibration constant
      cocalibration_constant = ref_ie[i]/(res.EXPOSURETIME*res.ISO)
      
      ;save to disk
      outfile = strmid(images[i], 0, strpos(images[i], '.', /REVERSE_SEARCH)) + '_co_calibration.sav'
      save, cocalibration_constant, reference_means, reference_stddevs, FILENAME = outfile
    endfor

    gcount++
    if ~(gcount mod nprint) then begin
      print, '  Calculated ' + strtrim(nbands*gcount,2) + ' of ' + strtrim(nbands*n_elements(gNames),2) + ' co calibration scales'
      print, '  Approx. time remaining (sec): ' + strtrim((n_elements(gnames)-gcount)*(toc()/gcount),2)
      print
    endif
  endforeach

  print, 'Finished calculating calibration functions!'
end