;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT, see LICENSE.txt for more details.
;h-


;+
; :Private:
;   Internal routine used in the bandalignmenttask.pro file. Docs here may not be up to date.
;   
;   This routine also does not currently work, so do not use.
;
; :Author: Zachary Norman - GtiHub : znorman-harris
;-


pro get_histograms, BASEBAND = baseband, BITDEPTH = bitdepth, EXTENSION = extension, HISTOGRAM_COLOR_BALANCING = histogram_color_balancing, INPUTDIR = inputdir, NBANDS = nbands
  compile_opt idl2, hidden

  e = envi(/current)
  if (e eq !NULL) then e = envi(/headless)

  if (extension eq !NULL) then extension = 'tif'
  if (nbands eq !NULL) then nbands = 5
  if (baseband eq !NULL) then baseband = 3
  baseband--

  ;number of expected bits per sample
  if (bitdepth eq !NULL) then bitdepth = 12

  if (histogram_color_balancing eq !NULL) then dist=20 else dist = histogram_color_balancing


  if ~keyword_set(inputdir) then message, 'INPUTDIR not specified!'
  if ~file_test(inputdir) then message, 'Input directory "' + inputdir + '" does not exist!'
  cd, inputdir, CURRENT=first_dir
  files = file_search('*.' + extension)
  cd, first_dir
  groups = get_image_groups(files, NBANDS = nbands)

  histogram_hash = orderedhash()

  print, 'Found ' + strtrim(n_elements(files),2) + ' images to calculate histograms for...'
  tic
  gnames = groups.keys()
  gcount = 0
  nprint = floor(n_elements(gnames)/20) > 10

  ;track the global max/min values (arbitrary numbers here)
  gmax = -1000000l
  gmin = 1000000l

  maxval = 2^bitdepth-1
  minval = 0

  ;pre-allocate an array witht eh positions of each image
  img_pos = make_array(2, n_elements(gnames), /FLOAT, /NOZERO)

  ;convert degrees to easting/northing
  convert = (180d/(!CONST.R_EARTH*!DPI))
  
  ;find the maximum values for each band
  maxvalue = 0
  
  foreach group_name, gnames do begin
    images = groups[group_name]
    count = 1
    infile = inputdir + path_sep() + images[baseband]
    meta = read_exif(infile)

    ;use read_exif to easily search for the GPS info (only need to supply the last part of a tag)
    lat = meta.GPSLatitude
    lon = meta.GPSLongitude

    ;account for S/W latitude/longitude
    if (meta.GPSLatitudeRef eq 'S') then lat = -lat
    if (meta.GPSLongitudeRef eq 'W') then lon = -lon

    ;lat/lon to easting and northing
    northing = (lat[0] + lat[1]/60d + (lat[2]/3600d))*convert
    easting = (lon[0] + lon[1]/60d + (lon[2]/3600d))*convert

    img_pos[*,gcount] = [easting, northing]
    dathash = hash()
    dathash['EASTING'] = easting
    dathash['NORTHING'] = northing

    foreach img, images, i do begin
      infile = inputdir + path_sep() + img
      ;assume micasense so we need to divide by 16 (12 bit images stored in 16 bit RAW format)
      ; this converts the data scale from 0 to 4096
      ;using only IDL
      ;            dat = read_tiff(infile)

      ;Using ENVI, takes about 3.7 times longer than just IDL
      raster = e.openraster(infile)
      dat = raster.getdata()
      raster.close

      if (dat.max() gt maxval) then dat = temporary(dat)/16s

      co_calibration_file = file_dirname(img) + path_sep() + strmid(file_basename(img), 0, strpos(file_basename(img),'.', /REVERSE_SEARCH)) + '_co_calibration.sav'
      ;check if we have our co calibration files to scale the DNs accordingly
      if file_test(co_calibration_file) then begin
        restore, co_calibration_file
        if (cocalibration_constant ne 0) then begin
          dat = fix(dat*cocalibration_constant, TYPE=12)
        endif
        ;print the reference means from the calibration data and convert our images to reflectance
        if (reference_means[0] ne -1) then begin
          dat = fix(10000l*(dat/reference_means[i]), TYPE=12)
        endif
        
        histmaxval = 10000
      endif
      
      maxvalue >= dat.max()
      
      dat_hist = histogram(dat, BINSIZE=1, MIN=minval, MAX = histmaxval)
      dathash['BAND' + strtrim(count,2)] =  {HISTOGRAM:dat_hist, NELEMENTS:n_elements(dat)}
      count++
    endforeach
    histogram_hash[group_name] = dathash
    gcount++
    if ~(gcount mod nprint) then begin
      print, string(9b) + 'Calculated ' + strtrim(nbands*gcount,2) + ' of ' + strtrim(n_elements(files),2) + ' histograms'
      print, string(9b) + 'Approx. time remaining (sec): ' + strtrim((n_elements(gnames)-gcount)*(toc()/gcount),2)
      print, ''
    endif

  endforeach
  print, 'Finished calculating histograms in ' + strtrim(toc(),2) + ' seconds!'
  print, ''
  print, 'Calculating transform functions for  ' + strtrim(n_elements(files),2) + ' images...'
  print, 'Using nearby images within ' + strtrim(dist,2) + ' meters to color balance'
  tic
  
  ;rename the transform function maximum values
  transform_maxvalue = reform(maxvalue[0])
  
  ;calculate transformation function for each image
  gcount = 0
  foreach group_name, gnames do begin

    transform = hash()
    images = groups[group_name]

    dist_other = ((img_pos[0,*] - img_pos[0,gcount])^2 + (img_pos[1,*] - img_pos[1,gcount])^2)^.5
    close_locs = where(dist_other lt dist, close_count)
    ;only calculate a histogram matching function if we have nearby groups, otherwise only save a 1-element array that has 0 as a value
    if (close_count gt 1) then begin
      base_histograms = lonarr(histmaxval,nbands)
      cdf_bases = fltarr(histmaxval,nbands)
      ntot=0*lonarr(nbands)
      ;iterate over each close image to build our average histograms
      for k=0, close_count-1 do begin
        for kk=0, nbands-1 do begin
          bandstruct = (histogram_hash[gnames[close_locs[k]]])['BAND' + strtrim(kk+1,2)]
          base_histograms[*,kk] += bandstruct.HISTOGRAM
          cdf_bases[*,kk] += total(bandstruct.HISTOGRAM,/CUMULATIVE)
          ntot[kk]+=bandstruct.NELEMENTS
        endfor
      endfor
      ;take an average of the cdf function
      for kk=0,nbands-1 do  cdf_bases[*,kk]*=(1./ntot[kk])


      ;iterate over each band for the current image
      for kk=0, nbands-1 do begin
        bandstruct = (histogram_hash[group_name])['BAND' + strtrim(kk+1,2)]

        ;MatchHistograms( BaseData, CompareData)
        ;basedata is the data that is to be matched to comparedata
        ;match_histogram is the histogram that is to be matched to the base_histogram
        match_histogram = bandstruct.HISTOGRAM
        cdf_match = total(match_histogram, /CUMULATIVE)/bandstruct.NELEMENTS

        ;base histogram is the average for the surrounding ares within dist
        base_histogram = base_histograms[*,kk]
        cdf_base = cdf_bases[*,kk]

        ;build the transform
        ;pre-allocate the transform array
        z = lonarr(histmaxval)
        FOR j=0,n_elements(cdf_base)-1 DO BEGIN
          i = Where(cdf_match LT cdf_base[j], count)
          IF (count GT 0) THEN z[j] = i[-1] ELSE z[j]=0
        ENDFOR

        transform['BAND' + strtrim(kk+1,2)] = z
      endfor

    endif else begin
      for kk=0, nbands-1 do begin
        transform['BAND' + strtrim(kk+1,2)] = [0]
      endfor
    endelse

    for i=0, nbands-1 do begin
      ;output .sav file which will contain the histogram transformation
      outfile = inputdir + path_sep() + file_basename(images[i], '.' + extension) + '_transform.sav'
      if ~file_test(outfile) then file_delete, outfile, /QUIET
      transform_function = transform['BAND' + strtrim(i+1,2)]
      save, transform_function, transform_maxvalue, FILENAME=outfile
    endfor

    gcount++
    if ~(gcount mod nprint) then begin
      print, string(9b) + 'Calculated ' + strtrim(nbands*gcount,2) + ' of ' + strtrim(n_elements(files),2) + ' transform functions'
      print, string(9b) + 'Approx. time remaining (sec): ' + strtrim((n_elements(gnames)-gcount)*(toc()/gcount),2)
      print, ''
    endif
  endforeach
  print, 'Finished calculating transform functions!'

end


;;main level program
;inputdir = 'C:\Users\norm3596\Documents\opportunities\lumen_aero\sample_data'
;get_histograms, INPUTDIR = inputdir
;end


