;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT, see LICENSE.txt for more details.
;h-

;+
; :Description:
;    Routine that returns key names for the image groups that likely fall
;    within a straight line. This can remove images from the candidate pool
;    from points where a fixed wing UAV is turning. Helps ensure better quality
;    image groups are found for generating reference tiepoints.
;
; :Params:
;    groups: in, required, type=hash/orderedhash
;      The image groups that you want to apply kappa filtering
;      for.
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
function bandalignment_simple_kappa_filter, groups, DEBUG = debug
  compile_opt idl2

;  ;check error state we want to use
;  if ~keyword_set(debug) then begin
;    on_error, 2
;  endif

  ;=======================================
  ;hard coded parameters
  ;number of left-right images to check
  ncheck = 2

  ;threshold for maximum allowable degree separation for straight lines
  theta_diff = 5 ;degrees
  theta_diff *= ncheck
  ;=======================================

  ;get the names of our groups
  groupNames = groups.keys()
  ngroups = n_elements(groups)

  ;preallocate an array to hold positions
  locations = dblarr(2,ngroups) ;[lon, lat]

  ;get all iamge locations
  i=0
  foreach images, groups, group do begin
    oInfo = image_info(images[0], /NO_SPATIALREF, /NO_PRINT, /ERROR_GPS)
    locations[*,i] = oInfo.Get('LON_LAT')
    i++
  endforeach

  ;preallocate ana rray to hold our kappa angles
  kappas = dblarr(ngroups-ncheck)

  ;iterate over each group for kappa
  for i=ncheck, ngroups-1-ncheck do begin
    ;preallocate arrays
    inFront = 0.0
    inBack = 0.0

    ;find all angle differences
    for j=1,ncheck do begin
      inFront += atan((locations[1,i+j] - locations[1,i]), (locations[0,i+j] - locations[0,i]))*(1/(!DTOR))
      inBack += atan((locations[1,i] - locations[1,i-j]), (locations[0,i] - locations[0,i-j]))*(1/(!DTOR))
    endfor

    ;save value
    kappas[i] = inFront - inBack
  endfor

  ;get stats
  meanval = mean(kappas)
  stddev = stddev(kappas)

  ;find which groups we can use
  available = where((abs(kappas) lt theta_diff) AND ([0:ngroups-1-ncheck] gt (ncheck-1)), count_valid)

  ;make sure that we found some results
  if (count_valid eq 0) then begin
    message, 'Flight lines are not straight enough to filter out turning points in the flight lines!' + $
      string(10b) + string(9b) + ' No valid images found to check for automating tie point generation process.'
  endif

  ;debug plots to show before/after filtering
  if keyword_set(debug) then begin
    sp1 = scatterplot(locations[0,*],locations[1,*])
    sp2 = scatterplot(locations[0,available],locations[1,available], $
      SYM_COLOR = 'red', /OVERPLOT, current = SP1)
  end

  ;return our best group
  return, groupNames[available]
end
