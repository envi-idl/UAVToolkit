;h+
; Copyright (c) 2019 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

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

;gets the bounding box of labels and returns the major/minor axis
;PIXEL_SIZE = pixel_size, specify xy pixel size in meters to get approximate length of
;axes in meters
function get_reflectance_panels_bbox, xy, PIXEL_SIZE = pixel_size, MAJORAXIS=majorAxis, MINORAXIS = minorAxis, THETA = theta, MAJORLEN=majorlen, MINORLEN=minorlen
  compile_opt idl2, hidden

  ;extract the points
  xOrig = xy[0,*]
  yOrig = xy[1,*]

  ; Test for collinearity
  isaColin = CORRELATE(xOrig, yOrig)
  if FINITE(isaColin, /NAN) OR isaColin eq 1 then begin
    message, 'Problem'
    theta = !VALUES.F_NAN
    minorAxis = !VALUES.F_NAN
    majorAxis = !VALUES.F_NAN
    return, !VALUES.F_NAN
  endif

  ;check for no pixels
  if n_elements(pixel_size) ne 2 then begin
    pixel_size = [1.0,1.0]
  endif

  ; Compute the offset
  xoffset = mean([max(xOrig),min(xOrig)], /DOUBLE)
  yoffset = mean([max(yOrig),min(yOrig)], /DOUBLE)

  ; Move the points to the origin
  x = xOrig - xoffset
  y = yOrig - yoffset

  ; comput the convex hull
  Triangulate, x, y, !null, hull

  ; create list of connections
  xc1 = x[hull]
  xc2 = shift(xc1,1)
  yc1 = y[hull]
  yc2 = shift(yc1,1)

  ; compute the angle
  theta = ATAN(yc2-yc1, xc2-xc1)

  ; compute the points with the given rotation
  a = xc1#cos(-1d*theta) - yc1#sin(-1d*theta)
  b = yc1#cos(-1d*theta) + xc1#sin(-1d*theta)

  ; find the min area box with respect to the roation
  length = max(a,DIMENSION=1) - min(a,DIMENSION=1)
  width = max(b,DIMENSION=1) - min(b,DIMENSION=1)

  ; find the min area rotation
  area = length * width
  !null = min(area,pos)

  ; get the corners of the bounding box
  rotx = [min(a[*,pos]),max(a[*,pos]),max(a[*,pos]),min(a[*,pos]),min(a[*,pos])]
  roty = [min(b[*,pos]),min(b[*,pos]),max(b[*,pos]),max(b[*,pos]),min(b[*,pos])]

  ; rotate the bounding box back
  rotx2 = rotx*cos(theta[pos]) - roty*sin(theta[pos]) + xoffset
  roty2 = roty*cos(theta[pos]) + rotx*sin(theta[pos]) + yoffset

  ; find the four mid points
  midx = mean([[rotx2[0:-2]] , [rotx2[1:-1]]],DIMENSION=2)
  midy = mean([[roty2[0:-2]] , [roty2[1:-1]]],DIMENSION=2)

  ; populate the keywords
  majorAxis = [TRANSPOSE(midx[[0,2]]),TRANSPOSE(midy[[0,2]])]
  minorAxis = [TRANSPOSE(midx[[1,3]]),TRANSPOSE(midy[[1,3]])]

  majorLen = sqrt(((majorAxis[0,0]*pixel_size[0] - majorAxis[0,1]*pixel_size[0])^2) + ((majorAxis[1,0]*pixel_size[1] - majorAxis[1,1]*pixel_size[1])^2))
  minorLen = sqrt(((minorAxis[0,0]*pixel_size[0] - minorAxis[0,1]*pixel_size[0])^2) + ((minorAxis[1,0]*pixel_size[1] - minorAxis[1,1]*pixel_size[1])^2))

  if majorLen lt minorLen then begin
    temp = minorAxis
    minorAxis = majorAxis
    majorAxis = temp
    temp = majorLen
    majorLen = minorLen
    minorLen = temp
  endif

  ; return the results
  return, [TRANSPOSE(rotx2),TRANSPOSE(roty2)]
end


;+
; :Private:
; 
; :Description:
;    Function that extracts the reflectance panels from a given data array. It is
;    assumed that the reflectance panels are the largest, solid-color item in the scene
;    and that the background is darker than the panel.
;
; :Params:
;    dat: in required, type=array[n,m]
;      Specify the array of data that you want to extract the reflectance panel from. It
;      can be any data type.
;
;
;
; :Author: Zachary Norma - GitHub:[znorman-harris](https://github.com/znorman-harris)
;-
function get_refelctance_panels_extract_panel, dat
  compile_opt idl2
  
  if (n_elements(dat) eq 0) then begin
    message, 'dat not specified, required!'
  endif
  
  ;hard coded parameters
  ;set a threshold for the region size (square pixels)
  size_min = 5000
  fit_min = 0.5

  ;grow the regions that are 0
  dmin = 5 ;pixel distance to 0 pixels that makes you a zero
  kernel = bytarr(dmin, dmin) + 1 ;kernel used for masking
  nIter = 10 ;number of iterations to shrink areas next to black pixels
  stddev_mult = 10 ;for growing the panel region
  dims = size(dat, /DIMENSIONS)
  
  ;specify our threshold
  meanVal = mean(dat)
  stddev = stddev(dat)
  threshold = meanVal
  
  ;recursively try to identify where our panel pixels are
  while (1) do begin
    ;mask pixels less than the mean plus some factor of the standard deviation
    bandMask = dat ge threshold

    ;add a border around the image
    bandMask[*,0:1] = 0
    bandMask[*,-2:-1] = 0
    bandMask[0:1,*] = 0
    bandMask[-2:-1,*] = 0

    ;shrink good regions if they are close to black pixels
    for z=0,nIter-1 do bandMask *= ~convol(~bandMask,kernel)

    ;split our sobel image into regions, helps find areas with no edges
    label = label_region(bandMask)

    ; Count number of pixels in each region,
    ; ignore background and edges
    h = histogram(label, MIN = 1, REVERSE_INDICES = r, LOCATIONS = loc)
    
    ;check if we have clusters at our threshold
    idxCheck = where(h ge size_min, countCheck)
    
    ;decrease our threshold
    if (countCheck eq 0) then begin
      threshold -= 0.1*stddev
      continue
    endif
    
    ;get the max value
    max = max(h[idxCheck], j)
    idx = idxCheck[j]
    break
  endwhile
  
  ;we have some items to check, so lets check them if needed
  if (countCheck eq 0) then begin
    idx = idxCheck[0]
  endif else begin
    ;allocate an array to save our "squareness"
    squares = fltarr(countCheck)
    
    ;process each segment
    foreach idx, idxCheck, i do begin
      ;create a mask for creating our outline
      mask = bytarr(dims)
      mask[r[r[idx]:r[idx+1]-1]] = 1
      
      ;get our vertices
      contour, mask, PATH_INFO = path_info, PATH_XY = vertices, /PATH_DATA_COORDS, NLEVELS = 1
      
      ;init a counter for the area that we have to subtract for holes
      subtract = 0.0
      
      ;if more than one vertex, extract the primary outline as the reference
      ;this can be when we have a shape with holes in it where contour generates
      ;polygons for the interior shapes
      if (n_elements(path_info) gt 1) then begin
        ;process each part and get the area
        for z=1,n_elements(path_info)-1 do begin
          interior = vertices[*, path_info[z].OFFSET + [[0l:path_info[z].N-1], 0]]
          
          ;close vertices
          interior = [[interior], [interior[*,0]]]
          
          ;create polygon
          roi = IDLanROI(interior)
          !NULL = roi.ComputeGeometry(AREA = area)
          subtract += area
        endfor
        
        ;extract our primary vertices
        vertices = vertices[*, path_info[0].OFFSET + [[0l:path_info[0].N-1], 0]]
      endif
      
      ;close our vertices
      vertices = [[vertices], [vertices[*,0]]]
      
      ;get our area
      area = float(h[idx]) - subtract
      
      ;fit to bbox and get square area
      !NULL = get_reflectance_panels_bbox(vertices, MAJORLEN=major, MINORLEN=minor)
      
      ;get size of our "square"
      square = (major > minor)^2.0
      
      ;create our objective function
      squares[i] = ((area/square)*((minor<major)/(major>minor)))^2
    endforeach
    
    ;check our results
    idxOk = where(squares ge fit_min, countOk)
    if (countOk gt 0) then begin
      !NULL = max(squares[idxOk], i)
      idx = idxCheck[idxOk[i]]
    endif else begin
      idx = !NULL
    endelse
  endelse
  
  ;check if we found a panel or not
  if (idx eq !NULL) then begin
    return, !NULL
  endif else begin
    return, region_grow(dat, r[r[idx]:r[idx+1]-1], STDDEV_MULTIPLIER = stddev_mult)
  endelse
end


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
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function get_reflectance_panels, group, sensor, $
  MAX_PIXEL_VALUE = max_pixel_value,$
  MAX_VALUE_DIVISOR = max_value_divisor,$
  PANEL_REFLECTANCE = panel_reflectance
  compile_opt idl2, hidden

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
      useRefl = fltarr(nBands) + float(panel_reflectance[0])
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
  ;outfile = 'super-animation.gif'
  ;if file_test(outfile) then file_delete, outfile, /quiet

  print, '    Finding reflectance panels in the images...'
  for i=0, nBands-1 do begin
    ;get our data
    banddat = *all_bands[i]
    
    ;extract the reflectance panel
    panel_pixels = get_refelctance_panels_extract_panel(banddat)
    if (panel_pixels eq !NULL) then begin
      goto, failed
    endif

    ;save information on our panel
    panel_means[i] = mean(banddat[panel_pixels] * (100d/useRefl[i]))
    panel_stddevs[i] = stddev(banddat[panel_pixels] * (100d/useRefl[i]))
    panel_counts[i] = n_elements(panel_pixels)

    ;print some useful information regarding the panel values
    print, '    Found panel ' + strtrim(i+1,2) + '!'
    print, '    Number of pixels in panel   : [ ' + strtrim(panel_counts[i],2) + ' ]'
    print, '    Mean of pixels in panel     : [ ' + strtrim(panel_means[i],2) + ' ]'
    print, '    Stddev of pixels in panel   : [ ' + strtrim(panel_stddevs[i],2) + ' ]'
    print

;    ;display some images to check that we found the panel
;    grown = 0*banddat
;    grown[panel_pixels] = banddat[panel_pixels]
;    w = window(DIMENSIONS = [1200,550], TITLE = 'Reflectance Panel for Band ' + strtrim(i+1,2), FONT_SIZE = 16, FONT_STYLE = 'bold')
;    ;original band
;    im1 = image(*all_bands[i], LAYOUT = [2,1,1], CURRENT = w)
;    ;refectance panel pixels
;    im2 = image(grown, LAYOUT = [2,1,2], CURRENT = w)
;  
;    ;convert the image to 8 bit
;    img = w.copywindow()
;    img = color_quan(img, 1, r, g, b)
;    ;duplicate frames otherwise low frame-rate can be a problem with some players
;    WRITE_GIF, outfile, img, r, g, b, delay_time = 100 ,$
;      /multiple, repeat_count = 0
  endfor
  
  if (1 eq 0) then begin
    failed:
    print, 'Did not find all reflectance panels, defaulting values and using dynamic range correction'
    panel_means = fltarr(nBands) + 1.0 
    panel_stddevs = fltarr(nBands)
    panel_counts = lonarr(nBands)
    panel_info['WAS_CALIBRATED'] = 0
  endif else begin
    print, 'Found all reflectance panels!'
  endelse

  ;save the information on our mean and standard deviations
  panel_info['PANEL_MEANS'] = panel_means      ;mean pixel value
  panel_info['PANEL_STDDEV'] = panel_stddevs   ;standard deviation of the pixel value
  panel_info['PANEL_COUNTS'] = panel_counts    ;number of pixels in the panel

  ; Close the file and window
  ;write_gif, outfile, /CLOSE

  ;return the panel information
  return, panel_info
end
