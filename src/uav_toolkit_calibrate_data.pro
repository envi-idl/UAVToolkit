;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT, see LICENSE.txt for more details.
;h-

;+
; :Private:
;
;-
function uav_toolkit_calibrate_data, group, sensor, $
  BAND_DIMS = band_dims,$
  INPUT_GAINS = input_gains,$
  INPUT_OFFSETS = input_offsets,$
  MAX_PIXEL_VALUE = max_pixel_value, $
  MAX_VALUE_DIVISOR = max_value_divisor,$
  ORIG_DAT = orig_dat, $
  OUTPUT_IES = output_ies,$
  PTR_ARR = ptr_arr,$
  WAS_CALIBRATED = was_calibrated
  compile_opt idl2, hidden
  
  e = envi(/CURRENT)
  if (e eq !NULL) then begin
    message, 'ENVI has not started yet, requried!'
  endif
  
  ;flag if we need to save our original data
  saveOrig = arg_present(orig_dat)
  
  ;check if we are a multi-page tiff
  raster = bandAlignment_group_to_virtualRaster(group)
  if (raster.NBANDS ne n_elements(group)) then begin
    nBands = raster.NBANDS
    mPage = 1
  endif else begin
    nBands = n_elements(group)
    mPage = 0
  endelse
  
  ;initialize gains and offsets
  output_ies = fltarr(nBands) + 1.0
  
  ;flag if we need to scale
  scaleFlag = 0
  
  ;flags for calibration success
  success = 0
  succeededPrev = 0
    
  ;copy sensor
  if (sensor ne !NULL) then begin
    useSensor = strlowcase(sensor)
  endif else begin
    useSensor = 'generic'
  endelse

  ;goto statement for starting from scratch if there is an error while calibrating
  ;this adds some rigor to the code and a fail-safe if there are issues
  restart:

  ;loop over each file in our group
  foreach file, group, i do begin
    ;check if we are a multi-page TIFF or not
    if ~mPage then begin
      ;check our sensor type
      switch (1) of
        (useSensor eq 'rededge'):begin
          ;error catching block in case there are problems
          catch, err
          if (err ne 0) then begin
            catch, /CANCEL
            success = 0
            useSensor = !NULL ;use generic sensor because there was an error for processing
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
        (useSensor eq !NULL):
        else:begin
          default:

          ;open raster and get data
          raster = e.openRaster(file)
          dat = raster.getData(INTERLEAVE = 'BSQ')
          raster.close

          ;extract image metadata and get our gains for our panel
          meta = read_exif(file)

          ;check to see if we have calibration information
          if meta.hasKey('ExposureTime') then begin
            ;check the key type
            case (1) of
              meta.hasKey('ISOSpeed'): output_ies[i] = meta.ISOSpeed*meta.ExposureTime
              meta.hasKey('ISOSpeedRatings'): output_ies[i] = meta.ISOSpeedRatings*meta.ExposureTime
              else: output_ies[i] = 1.0
            endcase
          endif else begin
            output_ies[i] = 1.0
          endelse

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
    endif else begin
      for j=0, nBands-1 do begin
        subset = ENVISubsetRaster(raster, BANDS = j)
        dat = subset.getData(INTERLEAVE = 'BSQ')
        subset.close
        
        ;save our data dimensionality
        if (j eq 0) then begin
          band_dims = size(dat,/DIMENSIONS)
        endif

        ;check if we need to save our original dat
        if (saveOrig) then begin
          ;extract pixels, allocate array if first image (assume all are same size)
          if (j eq 0) then begin
            orig_dat = ptrarr(nBands)
          endif
          orig_dat[j] = ptr_new(dat)
        endif

        ;check if we want to return a pointer array or a data array
        if keyword_set(ptr_arr) then begin
          ;extract pixels, allocate array if first image (assume all are same size)
          if (j eq 0) then begin
            all_bands = ptrarr(nBands)
          endif
          all_bands[j] = ptr_new(dat, /NO_COPY)
        endif else begin
          ;extract pixels, allocate array if first image (assume all are same size)
          if (j eq 0) then begin
            dims = size(dat, /DIMENSIONS)
            all_bands = make_array(dims[0], dims[1], nBands, TYPE = dat.typecode)
          endif
          all_bands[*,*,j] = dat
        endelse
      endfor
    endelse
  endforeach
  
  ;check if we need to scale our data
  if (scaleFlag) then begin
    if keyword_set(ptr_arr) then begin
      foreach ptr, all_bands, i do all_bands[i] = ptr_new(*ptr/max_value_divisor,/NO_COPY)
    endif else begin
      all_bands = temporary(all_bands)/max_value_divisor
    endelse
  endif
  
  ;check if we have gains to apply
  if (n_elements(input_gains) eq nBands) then begin
    if keyword_set(ptr_arr) then begin
      foreach ptr, all_bands, i do all_bands[i] = ptr_new((*ptr)*input_gains[i], /NO_COPY)
    endif else begin
      for i=0, nBands-1 do all_bands[*,*,i] *= input_gains[i]
    endelse
  endif
  
  ;check if we have offsets
  if (n_elements(input_offsets) eq nBands) then begin
    if keyword_set(ptr_arr) then begin
      foreach ptr, all_bands, i do all_bands[i] = ptr_new((*ptr) + input_offsets[i], /NO_COPY)
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
