; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

;+
; :Private:
; 
;   Internal routine used in the bandalignmenttask.pro file. Docs here may not be up to date.
; 
;   This procedure is used to find the reflectance panels in an image. This routine uses an algorithm I
;   developed which uses label_region to find different ares in each image. I then filter the regions
;   by different pixel values and by size. I then shrink the leftover regions that are outlined by dark
;   pixels. After this step, the reflectance panel is the only white portion of the image that is left.
;   The last step is to use region_grow to have the reflectance panel pixels fill the entire panel.
;
;   This procedure corrects the RedEdge data for bad data ranges.
;
;
;-


;+
;
; :Keywords:
;    BITDEPTH : in, optional, type=int, default=12
;      Expected bit depth of the iamges in `INPUTDIR`, if greater than 2^bitdepth-1, then the data
;      is scaled down. This is currently only set for RedEdge data.
;    GROUP : in, required, type=stringarr
;      If desired, you can specify a string array with N fully qualified file paths to images in
;      on group of RedEdge data where N is set by `NBANDS`.
;    PANEL_REFLECTANCE : in, optional, type=double/doublearr
;      If your reflectance panels are greyscale panels with a certain percentage of reflectance, then set this keyword
;      equal to the percent reflectance of these areas (0 to 100). If you know the panel reflectance for the
;      different bands of your dataset, then you can specify an array of values.
;    PANEL_INFO : out, requried, type=dictionary
;      Set this keyword to a named variable that will contain the information on the reflectance panels that 
;      were extracted. this will contain the
;    SENSOR: in, optional, type=string
;      Specify the name of the sensor and, if there is any custom calibration, then it will be performed on
;      the data.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
function get_reflectance_panels, group, sensor, $
  MAX_PIXEL_VALUE = max_pixel_value,$
  MAX_VALUE_DIVISOR = max_value_divisor,$
  PANEL_REFLECTANCE = panel_reflectance
  compile_opt idl2, hidden

  ;hard coded parameters
  ;set a threshold for the region size (square pixels)
  size_min = 7500

  ;grow the regions that are 0
  dmin = 5 ;pixel distance to 0 pixels that makes you a zero
  kernel = bytarr(dmin, dmin) + 1 ;kernel used for masking
  niter = 8 ;number of iterations to shrink areas next to black pixels
  stddev_mult = 10 ;for growing the panel region

  e = envi(/current)
  if (e eq !NULL) then begin
    message, 'ENVI has not been started yet, requried!'
  endif
  print, 'Getting reflectance panel calibration information...'

  ;set some defaults for keywords
  nBands = n_elements(group)

  ;initialize the panel information
  panel_info = dictionary()

  ;check the value of reflectanace that we will use
  if ~keyword_set(panel_reflectance) then begin
    useRefl = fltarr(nBands) + 100.0
  endif else begin
    if (n_elements(panel_reflectance) eq 1) then begin
      useRefl = fltarr(nBands) + float(panel_reflectance)
    endif else begin
      useRefl = float(panel_reflectance)
    endelse
    useRefl >= 0
    useRefl <= 100
  endelse

  ;lets read in all of the data for our group and calibrate if possible
  all_bands = uav_toolkit_calibrate_data(group, sensor, $
    MAX_PIXEL_VALUE = max_pixel_value, $
    MAX_VALUE_DIVISOR = max_value_divisor,$
    OUTPUT_IES = output_ies,$
    ORIG_DAT = orig_ptrs,$
    /PTR_ARR,$
    WAS_CALIBRATED = wasCalibrated)
  
  ;save the gains and offsets (either ones and zeros respectively, or the ISO*exposure)
  panel_info['PANEL_IES'] = output_ies
  panel_info['WAS_CALIBRATED'] = wasCalibrated

  ;simple flag if we were successful or not at calibrating our data
  success = 0
  
  ;preallocate some arrays to hold information on our panels
  panel_means = fltarr(nBands)
  panel_stddevs = fltarr(nBands)
  panel_counts = lonarr(nBands)

  ;outfile for creating a GIF
;  outfile = inputdir + path_sep() + 'panel_locations.gif'
;  if file_test(outfile) then FILE_DELETE, outfile, /quiet

  print, '    Finding reflectance panels in the images...'
  for i=0, nBands-1 do begin
    ;mask pixels less than the mean plus some factor of the standard deviation
    bandMask = *all_bands[i] ge (mean(*all_bands[i]) +1.0*stddev(*all_bands[i]))

    ;add a border around the image that is dmin pixels large
    bandMask[*,0:dmin-1] = 0
    bandMask[*,-dmin-2:-1] = 0
    bandMask[0:dmin-1,*] = 0
    bandMask[-dmin-2:-1,*] = 0

    ;shrink good regions if they are close to black pixels
    for z=0,niter-1 do begin
      bandMask *= ~convol(~bandMask,kernel)
    endfor

    ;split our sobel image into regions, helps find areas with no edges
    label = label_region(bandMask)

    ; Count number of pixels in each region,
    ; ignore background and edges
    h = histogram(label, MIN = 1, REVERSE_INDICES = r, LOCATIONS = loc)

    ; find our largest region
    idx = !NULL
    for j=0, n_elements(h)-1 do if (r[j] ne r[j+1]) then begin
      if (h[j] eq max(h)) then begin
        idx = j
        break
      endif
    endif
    
    ;make sure that we found a panel
    if (idx eq !NULL) then begin
      goto, failed
    endif
    
    ;get the original data since we have manipulated our current data a lot
    banddat = *all_bands[i]
    
    ;grow the region to try and fill the entire reflectance panel
    panel_pixels = region_grow(banddat, r[r[idx]:r[idx+1]-1], STDDEV_MULTIPLIER = stddev_mult)

    panel_means[i] = mean(banddat[panel_pixels] * (100d/useRefl[i]))
    panel_stddevs[i] = stddev(banddat[panel_pixels] * (100d/useRefl[i]))
    panel_counts[i] = h[idx]

    ;print some useful information regarding the panel values
    print, '    Found panel ' + strtrim(i+1,2) + '!'
    print, '    Number of pixels in panel   : [ ' + strtrim(h[idx],2) + ' ]'
    print, '    Mean of pixels in panel     : [ ' + strtrim(panel_means[i],2) + ' ]'
    print, '    Stddev of pixels in panel   : [ ' + strtrim(panel_stddevs[i],2) + ' ]'
    print

    ;display some images to check that we found the panel
;    grown = 0*banddat
;    grown[panel_pixels] = banddat[panel_pixels]
;    w = window(DIMENSIONS = [1200,550], TITLE = 'Reflectance Panel for Band ' + strtrim(i+1,2), FONT_SIZE = 16, FONT_STYLE = 'bold')
;    ;original band
;    im1 = image(all_bands[*,*,i], LAYOUT = [2,1,1], CURRENT = w)
;    ;refectance panel pixels
;    im2 = image(grown, LAYOUT = [2,1,2], CURRENT = w)
    ;        w.save, inputdir + path_sep() + 'band' + strtrim(i,2) + '.jpg'
    ;
    ;        ;convert the image to 8 bit
    ;        img = w.copywindow()
    ;        w.close
    ;        img = color_quan(img, 1, r, g, b)
    ;        ;duplicate frames otherwise low frame-rate can be a problem with some players
    ;        WRITE_GIF, outfile, img, r, g, b, delay_time = 100 ,$
    ;          /multiple, repeat_count = 0
  endfor
  
  if (1 eq 0) then begin
    failed:
    print, 'Did not find all reflectance panels, defaulting values and using dynamic range correction'
    panel_means = fltarr(nBands) + 1.0
    panel_stddevs = fltarr(nBands)
    panel_counts = lonarr(nBands)
    panel_info['WAS_CALIBRATED'] = 0
    stop
  endif else begin
    print, 'Found all reflectance panels!'
  endelse

  ;save the information on our mean and standard deviations
  panel_info['PANEL_MEANS'] = panel_means      ;mean pixel value
  panel_info['PANEL_STDDEV'] = panel_stddevs   ;standard deviation of the pixel value
  panel_info['PANEL_COUNTS'] = panel_counts    ;number of pixels in the panel

  ; Close the file and window
  ;    WRITE_GIF, outpath, /CLOSE

  ;return the panel information
  return, panel_info
end
