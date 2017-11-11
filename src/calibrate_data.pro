function calibrate_data, group, $
  BAND_DIMS = band_dims,$
  INPUT_GAINS = input_gains,$
  INPUT_OFFSETS = input_offsets,$
  MAX_PIXEL_VALUE = max_pixel_value, $
  MAX_VALUE_DIVISOR = max_value_divisor,$
  ORIG_DAT = orig_dat, $
  OUTPUT_GAINS = output_gains,$
  OUTPUT_OFFSETS = output_offsets,$
  PTR_ARR = ptr_arr,$
  SENSOR = sensor,$
  WAS_CALIBRATED = was_calibrated
  compile_opt idl2
  
  e = envi(/CURRENT)
  if (e eq !NULL) then begin
    message, 'ENVI has not started yet, requried!'
  endif
  
  ;flag if we need to save our original data
  saveOrig = arg_present(orig_dat)

  ;get the number of bands
  nBands = n_elements(group)
  
  ;initialize gains and offsets
  output_gains = fltarr(nBands) + 1.0
  output_offsets = fltarr(nBands)
  
  ;flag if we need to scale
  scaleFlag = 0
  
  ;flags for calibration success
  success = 0
  succeededPrev = 0
    
  ;copy sensor
  useSensor = sensor

  ;goto statement for starting from scratch if there is an error while calibrating
  ;this adds some rigor to the code and a fail-safe if there are issues
  restart:

  ;loop over each file in our group
  foreach file, group, i do begin
    ;check our sensor type
    switch (useSensor) of
      ('rededge'):begin
        ;error catching block in case there are problems
        catch, err
        if (err ne 0) then begin
          catch, /CANCEL
          success = 0
          useSensor = !NULL
          ;check if we need to restart or not (restart if we correctly processed one file)
          if (succeededPrev) then begin
            succeededPrev = 0
            goto, restart
          endif else begin
            goto, default
          endelse
        endif
        
        ;read in the image data and attempt to convert to radiance
        dat = rededge_to_radiance(file)
        
        ;cancel catch because we are OK
        catch, /CANCEL
        
        ;update our flags
        success = 1
        succeededPrev = 1

        ;stop switch execution
        break
      end

      ;default cases
      (!NULL):
      else:begin
        default:
        
        ;open raster and get data
        raster = e.openRaster(file)
        dat = raster.getData(INTERLEAVE = 'BSQ')
        raster.close
        
        ;extract image metadata and get our gains for our panel
        meta = read_exif(file)
        output_gains[i] = meta.ISOSpeed*meta.ExposureTime
        
        ;check if we need to perform any scaling if over a certain pixel threshold
        if keyword_set(max_pixel_value) AND keyword_set(max_value_divisor) then begin
          ;check if we are higher than we need to be for the data
          if (max(dat) gt max_pixel_value) then begin
            scaleFlag = 1
          endif
        endif
      end
    endswitch
    
    ;save our data dimensionality
    if (i eq 0) then begin
      band_dims = size(dat,/DIMENSIONS)
    endif
    
    ;check if we need to save our original dat
    if (saveOrig) then begin
      ;extract pixels, allocate array if first image (assume all are same size)
      if (i eq 0) then begin
        orig_dat = ptrarr(nBands)
      endif
      orig_dat[i] = ptr_new(dat)
    endif
    
    ;check if we want to return a pointer array or a data array
    if keyword_set(ptr_arr) then begin
      ;extract pixels, allocate array if first image (assume all are same size)
      if (i eq 0) then begin
        all_bands = ptrarr(nBands)
      endif
      all_bands[i] = ptr_new(dat, /NO_COPY)
    endif else begin
      ;extract pixels, allocate array if first image (assume all are same size)
      if (i eq 0) then begin
        dims = size(dat, /DIMENSIONS)
        all_bands = make_array(dims[0], dims[1], nBands, TYPE = dat.typecode)
      endif
      all_bands[*,*,i] = dat
    endelse
  endforeach
  
  ;check if we need to scale our data
  if (scaleFlag) then begin
    if keyword_set(ptr_arr) then begin
      foreach ptr, all_bands, i do all_bands[i] = *ptr/max_value_divisor
    endif else begin
      all_bands = temporary(all_bands)/max_value_divisor
    endelse
  endif
  
  ;check if we have gains to apply
  if (n_elements(input_gains) eq nBands) then begin
    if keyword_set(ptr_arr) then begin
      foreach ptr, all_bands, i do all_bands[i] = (*ptr)*input_gains[i]
    endif else begin
      for i=0, nBands-1 do all_bands[*,*,i] *= input_gains[i]
    endelse
  endif
  
  ;check if we have offsets
  if (n_elements(input_offsets) eq nBands) then begin
    if keyword_set(ptr_arr) then begin
      foreach ptr, all_bands, i do all_bands[i] = (*ptr) + input_offsets[i]
    endif else begin
      for i=0, nBands-1 do all_bands[*,*,i] += input_offsets[i]
    endelse
  endif

  ;set flag if our data was calibrated or not
  if (success) then begin
    was_calibrated = success
  endif

  ;return the data
  return, all_bands
end