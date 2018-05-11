;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
;h-

;+
; :Description:
;    Procedure that uses virtual rasters to intersect two scenes. Makes two rasters
;    have the same spatial extent and spatial pixel size. The smaller pixel size is
;    used for the output images.
;
;    Virtual rasters have other considerations for processing. See the documentation
;    for more information:
;    
;      http://www.harrisgeospatial.com/docs/VirtualRasterList.html
;
;
; :Keywords:
;    INPUT_RASTER1: in, required, type=ENVIRaster
;      Specify the first raster for itnersection.
;    INPUT_RASTER2: in, required, type=ENVIRaster
;      Specify the second raster for intersection.
;    OUTPUT_RASTER1: out, required, type=ENVIRaster
;      Returns the regridded first raster.
;    OUTPUT_RASTER2: out, required, type=ENVIRaster
;      returns the regridded second raster.
;    OUTPUT_GRID_DEFININTION: out, optional, type=ENVIRaster
;      Optionally return the ENVIGridDefinition object used to get the intersection
;      of the two scenes.
;    RESAMPLING: in, optional, type=string, choices=[Nearest Neightbor, Bilinear, Cubic convolution]
;      Optionally return the ENVIGridDefinition object used to get the intersection
;      of the two scenes.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro awesomeVirtualRasterIntersection, $
  INPUT_RASTER1 = input_raster1, $
  INPUT_RASTER2 = input_raster2, $
  OUTPUT_RASTER1 = output_raster1,$
  OUTPUT_RASTER2 = output_raster2,$
  OUTPUT_GRID_DEFININTION = output_grid_definition,$
  RESAMPLING = resampling
  compile_opt idl2

  e = envi(/current)
  if (e eq !NULL) then BEGIN
    message, 'ENVI has not been started yet, requried!'
  endif

  ;get our coordinate system
  if (input_raster1.SPATIALREF.COORD_SYS_CODE eq 0) then begin
    Raster1CoordSys = ENVICoordSys(COORD_SYS_STR = input_raster1.SPATIALREF.COORD_SYS_STR)
  endif else begin
    Raster1CoordSys = ENVICoordSys(COORD_SYS_CODE = input_raster1.SPATIALREF.COORD_SYS_CODE)
  endelse

  ; Create a grid definition for the first raster
  Raster1Grid = ENVIGridDefinition(Raster1CoordSys, $
    PIXEL_SIZE=input_raster1.SPATIALREF.PIXEL_SIZE, $
    NROWS=input_raster1.NROWS, $
    NCOLUMNS=input_raster1.NCOLUMNS, $
    TIE_POINT_MAP=input_raster1.SPATIALREF.TIE_POINT_MAP, $
    TIE_POINT_PIXEL=input_raster1.SPATIALREF.TIE_POINT_PIXEL)

  ;get our coordinate system
  if (input_raster1.SPATIALREF.COORD_SYS_CODE eq 0) then begin
    Raster2CoordSys = ENVICoordSys(COORD_SYS_STR = input_raster2.SPATIALREF.COORD_SYS_STR)
  endif else begin
    Raster2CoordSys = ENVICoordSys(COORD_SYS_CODE = input_raster2.SPATIALREF.COORD_SYS_CODE)
  endelse

  ; Create a grid definition for the second raster
  Raster2CoordSys = ENVICoordSys(COORD_SYS_STR=input_raster2.SPATIALREF.COORD_SYS_STR)
  Raster2Grid = ENVIGridDefinition(Raster2CoordSys, $
    PIXEL_SIZE=input_raster2.SPATIALREF.PIXEL_SIZE, $
    NROWS=input_raster2.NROWS, $
    NCOLUMNS=input_raster2.NCOLUMNS, $
    TIE_POINT_MAP=input_raster2.SPATIALREF.TIE_POINT_MAP, $
    TIE_POINT_PIXEL=input_raster2.SPATIALREF.TIE_POINT_PIXEL)

  ;reproject our second raster to our first and get the pixel size for comparison
  reprojected = ENVIReprojectRaster(input_raster2, COORD_SYS = Raster1CoordSys)

  ; Get the intersection of the two rasters
  SpatialExtent = Raster1Grid.Intersection(Raster2Grid)
  output_grid_definition = ENVIGridDefinition(Raster1CoordSys, $
    EXTENT=SpatialExtent, $
    PIXEL_SIZE=input_raster1.SPATIALREF.PIXEL_SIZE < reprojected.SPATIALREF.PIXEL_SIZE)

  ;clean up
  reprojected.close

  ;regrid our rasters
  output_raster1 = ENVISpatialgridRaster(input_raster1, GRID_DEFINITION = output_grid_definition, RESAMPLING = resampling)
  output_raster2 = ENVISpatialgridRaster(input_raster2, GRID_DEFINITION = output_grid_definition, RESAMPLING = resampling)
end
