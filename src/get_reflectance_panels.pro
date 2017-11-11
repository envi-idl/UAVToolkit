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
;       Expected bit depth of the iamges in `INPUTDIR`, if greater than 2^bitdepth-1, then the data
;       is scaled down. This is currently only set for RedEdge data.
;    GROUP : in, optional, type=stringarr
;       If desired, you can specify a string array with N fully qualified file paths to images in
;       on group of RedEdge data where N is set by `NBANDS`.
;    PANEL_REFLECTANCE : in, optional, type=double
;       If your reflectance panels are greyscale panels with a certain percentage of reflectance, then set this keyword
;       equal to the percent reflectance of these areas (0 to 100%).
;    EXTENSION : in, optional, type=string, default = 'tif'
;       File extension for the images in INPUTDIR.
;    INPUTDIR : in, required, type=string
;       Directory that contains the reflectance panel images.
;    NBANDS : in, optional, type=int, default=5
;       The number of bands per image groups.
;    PANEL_IES : out, optional, type=floatarr
;       Set this keyword to a named variable that will contain the ISO*Exposure values for each panel.
;       This value is used to remove the need to do color balancing.
;    PANEL_MEANS : out, optional, type=floatarr
;       Set this keyword to a named variable that will contain the mean pixel value for each reflectance panel.
;       This value is used to convert each data set to reflectance values scaled from 0 to 10000.
;    PANEL_STDDEVS : out, optional, type=floatarr
;       Set this keyword to a named variable that will contain the standard deviations of the reflectance
;       panels in each image. This value is not needed, but just returned in case I wanted it for future use.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro get_reflectance_panels, $
  GROUP = group, $
  MAX_PIXEL_VALUE = max_pixel_value,$
  MAX_VALUE_DIVISOR = max_value_divisor,$
  PANEL_REFLECTANCE = panel_reflectance,$
  PANEL_INFO = panel_info,$
  SENSOR = sensor
  compile_opt idl2, hidden

  ;hard coded parameters
  ;set a threshold for the region size (square pixels)
  size_min = 7500

  ;grow the regions that are 0
  dmin = 5 ;pixel distance to 0 pixels that makes you a zero
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

  ;check the value of grayscale
  if ~keyword_set(panel_reflectance) then begin
    panel_reflectance = fltarr(nBands) + 100.0
  endif else begin
    panel_reflectance = fltarr(nBands) + fltarr(panel_reflectance)
    panel_reflectance >= 0
    panel_reflectance <= 100
  endelse

  ;lets read in all of the data for our group
  all_bands = calibrate_data(group, $
    MAX_PIXEL_VALUE = max_pixel_value, $
    MAX_VALUE_DIVISOR = max_value_divisor,$
    OUTPUT_GAINS = gains,$
    OUTPUT_OFFSETS = offsets,$
    /PTR_ARR,$
    SENSOR = sensor,$
    WAS_CALIBRATED = wasCalibrated)
  
  ;save the gains and offsets (either ones and zeros respectively, or the ISO*exposure)
  panel_info['GAINS'] = gains
  panel_info['OFFSETS'] = offsets

  ;simple flag if we were successful or not at calibrating our data
  success = 0

  panel_means = make_array(nBands, TYPE=4)
  panel_stddevs = make_array(nBands, TYPE=4)

  ;outfile for creating a GIF
;  outfile = inputdir + path_sep() + 'panel_locations.gif'
;  if file_test(outfile) then FILE_DELETE, outfile, /quiet

  print, string(9b) + 'Finding reflectance panels in the images...'
  for i=0, nbands-1 do begin
    banddat = *all_bands[i]
    band_mean = mean(banddat)
    band_stddev = stddev(banddat)

    ;mask pixels less than the mean plus some factor of the standard deviation
    banddat[where(banddat le band_mean +1.0*band_stddev)] = 0

    ;add a border around the image that is dmin pixels large
    banddat[*,0:dmin-1] = 0
    banddat[*,-dmin-2:-1] = 0
    banddat[0:dmin-1,*] = 0
    banddat[-dmin-2:-1,*] = 0

    ;shrink good regions if they are close to black pixels
    for z=0,niter-1 do begin
      changes = 0*banddat
      ;invert banddat
      inv = ~banddat
      kernel = bytarr(dmin, dmin) + 1
      zero = ~convol(inv,kernel)
      banddat *= zero
    endfor

    ;perform light smoothing on the data
    ;banddat = smooth(temporary(banddat),15)

    ;find the edges with sobel
    edge_band= sobel(banddat) ;gt 200
    ;add a buffer around all of the edges
    edge_band[*,0:1] = 1
    edge_band[*,-2:-1] = 1
    edge_band[0:1,*] = 1
    edge_band[-2:-1,*] = 1

;    ;make our edge image binary
;    edge_band gt= 0

    ;split our sobel image into regions, helps find areas with no edges
    label = label_region(edge_band)

    ; Count number of pixels in each region,
    ; ignore background and edges
    h = histogram(label, MIN = 1, REVERSE_INDICES = r, LOCATIONS = loc)

    regions = list()
    npx = list()
    stddevs = list()
    means = list()
    region_locations = list()
    ; get the mean and standard deviation of each region
    for j=0, n_elements(h)-1 do if (r[j] ne r[j+1]) then begin
      indices = r[r[j]:r[j+1]-1]
      ;if our region is greater than the minimum size, then save some information about it
      if (h[j] gt size_min) then begin
        regions.add, loc[j]
        npx.add, h[j]
        stddevs.add, stddev(banddat[indices])
        means.add, mean(banddat[indices])
        region_locations.add, indices
      endif
    endif

    ;error out if we have no regions over the size_min threshold
    if (n_elements(regions) eq 0) then message, 'No regions found!'

    ;convert lists to arrays and get the relative errors for stddev and mean pixel values
    regions = regions.toArray()
    npx = npx.toArray()
    stddevs = stddevs.toArray()
    means = means.toArray()
    rel_errors = stddevs/means

    ;get the region with the most pixels, after shrinking regions, the max should be the panel
    panel = (regions[where(npx eq max(npx))])[0]

    ;get the original data since we have manipulated our current data a lot
    banddat = all_bands[*,*,i]
    
    ;grow the region to try and fill the entire reflectance panel
    idx = where(label eq panel)
    panel_pixels = region_grow(banddat, idx, STDDEV_MULTIPLIER = stddev_mult)

    panel_means[i] = mean(banddat[panel_pixels] * (100d/panel_reflectance))
    panel_stddevs[i] = stddev(banddat[panel_pixels] * (100d/panel_reflectance))

    ;print some useful information regarding the panel values
    print, '    Found panel ' + strtrim(i+1,2) + '!'
    print, '    Number of pixels in panel   : [ ' + strtrim(n_elements(idx),2) + ' ]'
    print, '    Mean of pixels in panel     : [ ' + strtrim(panel_means[i],2) + ' ]'
    print, '    Stddev of pixels in panel   : [ ' + strtrim(panel_stddevs[i],2) + ' ]'
    print, ''


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


  stop
  ; Close the file and window
  ;    WRITE_GIF, outpath, /CLOSE
  print, 'Found all reflectance panels!'
  
  ;save the reference intensity*exposure
  save, panel_ies, FILENAME = file_dirname(group[0]) + path_sep() + 'panel_ies.sav'
end
