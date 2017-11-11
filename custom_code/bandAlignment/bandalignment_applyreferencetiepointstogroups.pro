; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

;+
;
;  Contains the code used for performing the band-band registration on all of the
;  image groups that you want to process.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-



;+
; :Private:
;
; :Description:
;    Function to histogram match the input CompareData to the input BaseData.
;    
;    NOT USED.
;
; :Returns:
;    Returns a histogram matched 2D image array.
;
; :Params:
;    BaseData: in, required
;       Reference gray scale image array to use as a reference for CompareData
;    CompareData: in, required
;       Comparison gray scale image to be matched to BaseData
;-
function BandAlignment_ApplyReferenceTiePointsToGroup_MatchHistograms, BaseData, CompareData
  compile_opt idl2, hidden

  ;extract histograms
  match_histogram = histogram(BaseData, MIN = min(CompareData), MAX = max(CompareData))
  base_histogram = histogram(CompareData, MIN = min(CompareData), MAX = max(CompareData))

  ;get the cumulative histogram
  cdf_match = total(match_histogram, /CUMULATIVE)/n_elements(BaseData)
  cdf_base = total(base_histogram, /CUMULATIVE)/n_elements(CompareData)

  ;preallocate arrat and create transform
  z = lonarr(max(CompareData))
  for j=0,n_elements(cdf_base)-1 do begin
    i = where(cdf_match lt cdf_base[j], count)
    if (count gt 0) then z[j] = i[-1] else z[j]=0
  endfor

  ;match the compare to the first
  matchedImage = z[CompareData]

  ; at this point displaying an image of the data will show us where the values are bad i.e.
  ; really large compared to the base image and they will be zeros in the MatchedImage array
  ; from here, we can replace the bad data with whatever we want (like the median value)
  baddata = where(matchedimage eq 0)
  datadims = size(matchedimage, /DIMENSIONS)
  xbad = baddata mod datadims[0]
  ybad = (baddata/datadims[0]) mod datadims[1]

  ;only replace zeros if we have them
  if (baddata[0] ne -1) then begin
    ;replace data with average from neighbors
    nn = 2 ;number of neighbors to take into account around center pixel
    for i=0, n_elements(xbad)-1 do begin
      ;get extents
      xl = xbad[i] - nn
      xr = xbad[i] + nn
      yl = ybad[i] - nn
      yr = ybad[i] - nn

      ;cehck each index to make sure that it is an ok value
      if (xl lt 0) then xl = xbad[i]
      if (xr gt datadims[0]-1) then xr = xbad[i]
      if (yl lt 0) then yl = ybad[i]
      if (yl gt datadims[1] - 1) then yl = ybad[i]

      sub_arr = matchedimage[xl:xr, yl:yr]
      ;find the positiong where the matchedimage has no data
      gooddata = where(sub_arr ne 0)
      ;replace bad values only if there is close gooddata
      if (gooddata[0] ne -1) then begin
        sub_arr = sub_arr[gooddata]
        matchedimage[xbad[i], ybad[i]] = mean(sub_arr)
      endif
    endfor
  endif
  return, matchedimage
end

;+
; :Private:
;
; :Description:
;    Simple procedure that re-hydrates our parameters and restores the data
;    structure to a dictionary after we convert to a string and pass through
;    our IDL-IDLBridges.
;
; :Params:
;    parameters: in, requried, type=orderedhash
;      Orderedhash sent to child process. Modified in place to become a dictionary
;      again and to be used in the processing
;    
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro BandAlignment_ApplyReferenceTiePointsToGroup_hydrate_parameters, parameters
  compile_opt idl2

  ;loop over each parameter and dehydrate all ENVI parameters
  foreach paramVal, parameters, paramKey do begin
    catch, err
    if (err ne 0) then begin
      catch, /CANCEL
      continue
    endif
    
    ;check if we have custom rehydration 
    if isa(paramVal, 'hash') then begin
      ;check what we have to process
      case (1) of
        ;task
        paramVal.hasKey('name') AND paramVal.hasKey('parameters'):begin
          parameters[paramKey] = ENVITask(json_parse(json_serialize(paramVal)))
        end

        ;hydratable such as raster or metadata
        paramVal.hasKey('factory'):parameters[paramKey] = ENVIHydrate(paramVal)
      endcase
    endif
  endforeach
  catch, /CANCEL
  
  ;convert hash back to dictionary, not sure why we have to do this.
  outParameters = dictionary()
  foreach val, parameters, key do outParameters[key] = val
  parameters = outParameters
end

;+
; :Description:
;    Orchestrator for applying tie points to single image group.
;
; :Params:
;    group: in, required, type=stringarray
;      String array of the files that belong to our image group.
;    groupName: in, requried, type=string
;      Specify the base name of our group that is being processed. Used for the
;      output files.
;    parameters: in, required, type=dictionary
;      Dictionary with all the parameters and values needed to be able to run 
;      our processing. Used for convenience if adding/removing parameters up the chain.
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro BandAlignment_ApplyReferenceTiePointsToGroup, group, groupName, parameters
  compile_opt idl2, hidden

  e = envi(/current)
  if (e eq!NULL) then begin
    e = envi(/headless)
  endif

  ;convert out groups into a raster
  raster = bandAlignment_group_to_virtualRaster(group)
  nbands = raster.NBANDS

  ;specify the expected tiepoints file
  pointsFile = parameters.POINTS_FILE

  ;check if our points are valid
  tiePoints = bandAlignmentTiePoints(SAVE_FILE = pointsFile)

  ;register our tiepoints with one another
  bandAlignment_ApplyReferenceTiepointsWithIDL,$
    INPUT_RASTER = raster,$
    INPUT_BANDALIGNMENTTIEPOINTS = tiePoints,$
    OUTPUT_SPATIALREF = outSref,$
    OUTPUT_DATA_POINTER = datPtr

  ;get the data we want to write to disk
  write_data = *datPtr

  ;check if we want to perform a secondary image-image registration to line our scenes up
  ;to account for variations in elevation
  if ~parameters.NO_SECONDARY_MATCH then begin
    ;close orginal raster
    raster.close

    ;save second raster to disk so that we can generate a second set of tiepoints
    raster = ENVIRaster(write_data, SPATIALREF = outSref)
    raster.save

    ;TODO: add code for registration again
    ;generate the reference tie points
    BandAlignment_ProcessSensor_GetReferenceTiePoints,$
      group, parameters, RASTER = raster, $
      /NO_OUTPUT,$
      FILTERED_TIEPOINTS = secondaryTiepoints

    ;register our tiepoints with one another
    bandAlignment_ApplyReferenceTiepointsWithIDL,$
      INPUT_RASTER = raster,$
      INPUT_BANDALIGNMENTTIEPOINTS = secondaryTiepoints,$
      OUTPUT_SPATIALREF = outSref,$
      OUTPUT_DATA_POINTER = datPtr
  endif

  ;determine type of data so that we can write correctly sized TIFF files
  ;although I bet all sample data is the same, it might be a good idea to use this
  ;just in case we have a special typecode for an image
  typecode = write_data.typecode
  case typecode of
    ;unsigned long
    13: lng = 1
    ;signed long
    3:BEGIN
      lng = 1
      signed = 1
    END
    ;unsigned 64 bit long
    15:  l64 = 1
    ;signed 64 bit long
    14:BEGIN
      l64 = 1
      signed = 1
    END
    ;double
    5:  dbl = 1
    ;float
    4:  flt = 1
    ;signed integer
    2:BEGIN
      short = 1
      signed = 1
    END
    ;unsigned integer
    12: short = 1
    ELSE: message, 'Unknown type'
  endcase


  ;check if we need to perform any scaling if over a certain pixel threshold
  if parameters.hasKey('MAX_PIXEL_VALUE') AND parameters.hasKey('MAX_VALUE_DIVISOR') then begin
    ;check the maximum data value
    ;convert read incorrectly if
    maxval = max(write_data)

    ;check if we are higher than we need to be for the data
    if (maxval gt parameters.MAX_PIXEL_VALUE) then begin
      print, '    Image has data type for 16 bit integers, maximum value of 4095 expected so dividing by 16'
      write_data = temporary(write_data)/parameters.MAX_VALUE_DIVISOR
    endif
  endif

  ;adjust the data accordingly
  for i=0, nBands-1 do begin
    ;get band data
    write_this = write_data[*,*,i]

    ;check for scaling of data
    co_calibration_file = strmid(group[i], 0, strpos(group[i],'.', /REVERSE_SEARCH)) + '_co_calibration.sav'

    ;check if we have our co calibration files to scale the DNs accordingly
    if file_test(co_calibration_file) then begin
      ;restore data
      restore, co_calibration_file

      ;scale relative to reference
      if (cocalibration_constant ne 0) then begin
        print, '    Band ' + strtrim(i+1,2) + ' co-calibration constant            :    [ ' + strtrim(cocalibration_constant,2) + ' ] '
        write_this = fix(write_this*cocalibration_constant, TYPE=typecode)
      endif

      ;print the reference means from the calibration data and convert our images to reflectance
      if (reference_means[0] ne -1) then begin
        print, '    Reflectance panel mean, scaled image max  :    [ ' + strtrim(reference_means[i],2) + ', ' + strtrim(max(write_this),2) + ' ] '
        write_this = fix(parameters.REFLECTANCE_SCALE_FACTOR*(write_this/reference_means[i]), TYPE=typecode)
      endif
      
      ;clean up
      file_delete, co_calibration_file, /QUIET
    endif

    ;check if we have a color transform
    transform_file = strmid(group[i], 0, strpos(group[i],'.', /REVERSE_SEARCH)) + '_transform.sav'
    if file_test(transform_file) then begin
      ;restore the transform_function which will be in a variable called transform_function
      restore, transform_file

      ;update our data
      write_this = transform_function[write_this]

      ;clean up transform file
      file_delete, transform_file, /QUIET

      ;also need to check for the transform maximum values
      write_this = fix(round(write_this*(65535d/transform_maxvalue[0])) , TYPE = 12)

      ; at this point displaying an image of the data will show us where the values are bad i.e.
      ; really large compared to the base image and they will be zeros in the MatchedImage array
      ; from here, we can replace the bad data with whatever we want (like the median value)
      ;        baddata = where(matchedimage eq 0)
      ;        datadims = size(matchedimage, /DIMENSIONS)
      ;        xbad = baddata mod datadims[0]
      ;        ybad = baddata/datadims[0] mod datadims[1]
      ;
      ;        ;only replace zeros if we have them
      ;        if (baddata[0] ne -1) then begin
      ;          ;replace data with average from neighbors
      ;          nn = 2 ;number of neighbors to take into account around center pixel
      ;          for i=0, n_elements(xbad)-1 do begin
      ;            xl = xbad[i] - nn
      ;            xr = xbad[i] + nn
      ;            yl = ybad[i] - nn
      ;            yr = ybad[i] - nn
      ;            ;cehck each index to make sure that it is an ok value
      ;            if (xl lt 0) then xl = xbad[i]
      ;            if (xr gt datadims[0]-1) then xr = xbad[i]
      ;            if (yl lt 0) then yl = ybad[i]
      ;            if (yl gt datadims[1] - 1) then yl = ybad[i]
      ;
      ;            sub_arr = matchedimage[xl:xr, yl:yr]
      ;            ;find the positiong where the matchedimage has no data
      ;            gooddata = where(sub_arr ne 0)
      ;            ;replace bad values only if there is close gooddata
      ;            if (gooddata[0] ne -1) then begin
      ;              sub_arr = sub_arr[gooddata]
      ;              matchedimage[xbad[i], ybad[i]] = sub_arr.mean()
      ;            endif
      ;          endfor
      ;        endif
    endif

    ;save our updates to the data
    write_data[*,*,i] = write_this
  endfor

  ;specify output file
  outfile = parameters.OUTPUT_DIR + path_sep() + groupName + '.tif'
  print, outFile
  if file_test(outFile) then file_delete, outFile, /QUIET
  if file_test(outFile) then begin
    print, '  Cannot create TIFF file, file exists and locked by another program. File: '
    print, '    ' + outFile
  endif else begin
    ;save our data as a TIFF (multi-channel or multi-page)
    if ~keyword_set(parameters.MULTI_CHANNEL) then begin
      for i=0,nBands-1 do begin
        write_tiff, outfile, write_data[*,*,i], /APPEND, $
          LONG = lng, L64 = l64, DOUBLE = dbl, FLOAT = flt, SHORT = short, SIGNED = signed
      endfor
    endif else begin
      write_tiff, outFile, write_data, $
        LONG = lng, L64 = l64, DOUBLE = dbl, FLOAT = flt, SHORT = short, SIGNED = signed
    endelse
  endelse

  ;check to see if we want an ENVI formatted file
  ;this is to output an ENVI format raster and header with correct metadata, no need for this
  ;with TIFFs
  if keyword_set(parameters.MAKE_ENVI_FILE) then begin
    ;remove output file if it exists
    outfile = parameters.OUTPUT_DIR + path_sep() + groupName + '.dat'

    ;delete file if it doesnt exist
    if file_test(outfile) then file_delete, outfile, /QUIET

    ;only wirte if output does not exist - might be locked by ENVI
    if ~file_test(outfile) then begin
      ;init metadata if we have it
      if parameters.hasKey('METADATA') then meta = parameters.METADATA else meta = ENVIRasterMetadata()

      ;init, save, and close
      bandstackedraster = ENVIRaster(write_data, SPATIALREF = spatialref, URI = outfile, METADATA = meta)
      bandstackedraster.save
      bandstackedraster.close
    endif else begin
      print, '  Cannot create ENVI file, file exists and locked by another program. File: '
      print, '    ' + outFile
    endelse
  endif

  ;clean up ENVI so we don't have locks on datasets
  raster.close
  foreach file, group do begin
    raster = e.openraster(file)
    raster.close
  endforeach
  print, 'Finished group'
  print
end



;+
; :Description:
;    Orchestrator for applying tie points to all of our image groups that we found
;    previously.
;
; :Params:
;    groups: in, requried, type=orderedhash
;      Hash where the key/value pairs correspond to the group name and the 
;      files that belong in the group/
;    parameters: in, required, type=dictionary
;      Dictionary with all the parameters and values needed to be able to run 
;      our processing. Used for convenience if adding/removing parameters up the chain.
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro BandAlignment_ApplyReferenceTiePointsToGroups, groups, parameters
  compile_opt idl2, hidden
  
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    if obj_valid(oBdg) then oBdg.cleanup
    message, /REISSUE_LAST
  endif
  
  ;get the number of groups
  nGroups = n_elements(groups)
  
  if (nGroups eq 1) then bonus = '' else bonus = 's'
  print, 'Applying reference tie points...'
  print, '  Found ' + strtrim(nGroups,2) + ' group' + bonus + ' to process!'
  
  ;specify our output log file
  logDir = filepath('', /TMP)
  logDir = strmid(logDir, 0, strlen(logDir)-1)
  
  ;duplicate our json
  parametersUse = dictionary(parameters.keys(), parameters.values())

  ;loop over each parameter and dehydrate all ENVI parameters
  foreach paramVal, parametersUse, paramKey do begin
    catch, err
    if (err ne 0) then begin
      catch, /CANCEL
      continue
    endif
    parametersUse[paramKey] = paramVal.dehydrate()
  endforeach
  catch, /CANCEL

  ;check if single or multi-threaded
  if (parameters.NP_GROUPS eq 1) then begin
    ;start timer and process each group
    tic
    tocp = toc()
    foreach group, groups, groupName do begin
      BandAlignment_ApplyReferenceTiePointsToGroup, group, groupName, parameters
      print, 'Time to process group (sec): ' + strtrim(toc() - tocp,2)
      tocp = toc()
    endforeach
  endif else begin
    print, '  Initializing child processes...'
    
    ;start our IDL-IDLbridges
    oBdg = bridge_it(parameters.NP_GROUPS,$
      LOGDIR = logDir,$
      INIT = parameters.INIT,$
      MSG = 'Time to process group (sec): ',$
      NREFRESH = 5000)
      
    ;send the parameters to each child process
    oBdg.SetVar, 'parameters', parametersUse,$
      POST_EXECUTE = 'resolve_routine, "BandAlignment_ApplyReferenceTiePointsToGroups", /COMPILE_FULL_FILE, /EITHER & ' + $
        'BandAlignment_ApplyReferenceTiePointsToGroup_hydrate_parameters, parameters'
    
    ;start timer and process each group
    tic
    foreach group, groups, groupName do begin
      oBdg.run,'BandAlignment_ApplyReferenceTiePointsToGroup', $
        ARG1 = group, $
        ARG2 = groupName,$
        CUSTOM_ARGS = 'parameters',$
        /TIME
      wait, .1
    endforeach

    ;wait for everything to finish
    oBdg.wait

    ;get information on our runs
    runs = oBdg.getNRuns()
    print, ''
    print, 'Number of bridge runs: '
    print, runs.total

    ;kill child processes once finished!
    oBdg.cleanup
  endelse
  
  print, 'Finished processing groups!'
  print, '  Time to process ' + strtrim(nGroups,2) +' groups (sec): ' + strtrim(toc(),2)
  print, '  Average time per group (sec) : ' + strtrim(toc()/nGroups,2)
  print, ''
  
  ;check how many output files we have  
  outfiles = file_search(parameters.OUTPUT_DIR, '*.tif', COUNT = nOut)

  ;check to see if all image groups were processed
  if (nOut ne nGroups) then begin
    print, '****************************************************************'
    print, 'Not all image groups were processed by the bridges'
    print, 'Only ' + strtrim(nOut,2) + ' groups were processed of ' + strtrim(nGroups,2)
    print, 'See the IDL console or the following log file (if bridges were used) for details: '
    print, '  ' + logDir + path_sep() + 'all_logs.txt'
    print, '****************************************************************'
  endif
end