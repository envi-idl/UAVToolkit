# BetterTileIterator

A tile iterator for ENVI that has more options than the default tile iterator which ships with ENVI, including adding a buffer around the edges of scenes. See background for detailed information on why this is useful and the two different examples where it is beneficial to use this type of tiling.

In addition to this, there is a modified tile iterator which generates tiles that are optimized for the interleave format you are processing.


## Usage

The tile iterator is introduced as a procedure called ```createAwesomeTileIterator``` and can be called with:

```idl
createAwesomeTileIterator,$
  INPUT_RASTER = raster,$
  TILE_SIZE = [1024,1024],$
  OUTPUT_SUB_RECTS = sub_rects
```

Where OUTPUT_SUB_RECTS returns an IDL list of each sub_rect (also called subset) of our image/raster in file coordinates.

## Background

When you are working with raster data it is common for the files to be hundreds of MB or a few GB in size. With today's computers you can typically process an entire image in memory, but solutions which rely on this are not necessarily portable to different platforms or machines. Additionally, if you are running multiple processes on the same machine, then the amount of memory on your computer may not be enough to support the algorithms.

To ensure that you have enough memory for all of your processes, it is a good idea to use tiling for rasters if you are creating a custom algorithm that processes the pixels. Tiling is simply processing smaller chunks of the image at a time and writing those smaller sections of the data to disk in an output file.

In order to tile a raster for processing you need to do a few things ahead of time.

1. Create a tile iterator with ENVI. This can be one of the built in routines or it can a custom routine as we will use in this chapter.

2. Initialize your output raster ahead of time so that ENVI knows what to expect for the data type, number of rows, number of columns, and the number of bands.


## Complete Example without Tile Buffer

To demonstrate the above steps, we will talk through an example of running an edge detection filter on an input raster. This example will cover almost all of the steps to initialize an output raster, use the tile iterator, get and set data, and save our finalized image.

First, let's assume that we have an input raster with a spatial reference defined as `raster` in IDL's memory scope. To create a new raster with one band from our original raster we can use the ```ENVIRaster()``` function.


```idl
newRaster = ENVIRaster(URI = newFile, $
  NROWS = raster.NROWS, $
  NCOLUMNS = raster.NCOLUMNS, $
  NBANDS = 1, $
  DATA_TYPE = 'uint',$
  SPATIALREG = raster.SPATIALREF)
```

These are the minimum parameters that must be set when creating a new raster. We need to manually set the number of bands for the output because we will likely not have the same number of bands as the input raster (depending on our process). You may also want to manually specify the output data type as was set above,  otherwise you can use `raster.DATATYPE` to inherit the data type of another raster in place of `UINT` above.

On a side note, the short-hand representation for creating a new raster with the exact same properties as a source raster is to use the `INHERITS_FROM` keyword for ```idl ENVIRaster()```.

```idl
newRaster = ENVIRaster(INHERITS_FROM = raster)
``` 

The reason that we cannot use this for tiling is because the number of bands are likely different as is the data type.

Once you have your raster initialized then you just need to start working with and manipulating raster data. When getting and setting data for a raster, this is typically done in **file coordinates.**

File coordinates are zero-based, [x,y] positions with respect to the top left corner of the image. This system is used because we are typically in the same file coordinate system when creating new rasters. Using file coordinates we then have a basis for tiling: we get data from [xMin, yMin, xMax, yMax], manipulate it, and place the data in our new raster. For our rasters this chunk of data that we are working with is called a sub-rect (also a subset).

In order to tile our raster we now just need to have access to the sub-rects of our scene that we want to get data from. The custom routine that will be used for this is a procedure called ```idl createAwesomeTileIterator```. Here is a syntax example for how to use the procedure:

```idl 
createAwesomeTileIterator,$
  INPUT_RASTER = raster,$
  TILE_SIZE = [1024,1024],$
  OUTPUT_SUB_RECTS = sub_rects
```

The tile iterator will return a ```list()``` in the variable `sub_rects` that contains the tile locations for our raster. We can then iterate over the sub-rects with a ```foreach``` loop using the ```raster.GetData()``` and ```raster.SetData``` methods to read the original raster data and write our new raster data. To get/place the data in the right place we just need to use the `SUB_RECT` keywords when getting/setting the data. 

Here is an example:

```idl
createAwesomeTileIterator,$
  INPUT_RASTER = raster,$
  TILE_SIZE = [1024,1024],$
  OUTPUT_SUB_RECTS = sub_rects

foreach sub_rect, sub_rects do begin
  ;get the data from the source raster 
  dat = raster.GetData(SUB_RECT = sub_rect)

  ;manipulate the data in some way
  newDat = ...

  ;write the data to disk
  newRaster.SetData, newDat, SUB_RECT = sub_rect
endforeach

;save the results
newRaster.save
```

Next we can tie together the few steps above into a single workflow in IDL. Here is what that looks like:

```idl
;make sure ENVI is running
e = envi(/CURRENT)

;specify our input file 
file = 'C:\some\file\on\disk.dat'

if ~file_test(file) then begin
    message, 'file does not exist!'
endif

;open our raster
raster = e.OpenRaster(file)

;specif our output filename
outFile = e.GetTemporaryFilename()

;set up our output raster
newRaster = ENVIRaster(URI = outFile, $
    NROWS = raster.NROWS, $
    NCOLUMNS = raster.NCOLUMNS, $
    NBANDS = 1, $
    DATA_TYPE = 'uint',$
    SPATIALREF = raster.SPATIALREF)

;get our tiles for processing
createAwesomeTileIterator,$
    INPUT_RASTER = raster,$
    TILE_SIZE = [1024,1024],$
    OUTPUT_SUB_RECTS = sub_rects

;loop over our tiles
foreach sub_rect, sub_rects do begin
    ;get the data from the source raster
    dat = raster.GetData(SUB_RECT = sub_rect, INTERLEAVE = 'bsq')

    ;detect the edges
    newDat = sobel(dat[*,*,0])

    ;write the data to disk
    newRaster.SetData, newDat, SUB_RECT = sub_rect
endforeach

;save the results
newRaster.save

;display the raster in ENVI
view = e.GetView()
layer = view.CreateLayer(newRaster, /CLEAR_DISPLAY)
```

If you want to run this code, once you have the code open in the IDL workbench, make sure that you adjust the file path for a file on disk (hopefully larger than [1024, 1024] to see the tiling artifacts). After you compile and run the code, the results will be displayed in ENVI. If you put a color table on the raster and pan around, you will see that there are tiling artifacts at the edges of our tiles. 


## Short Example with Tile Buffer

While the above example is useful to understand the basic syntax, there are going to be tiling artifacts present in our scene. These artifacts are introduced because, when using the ```sobel()``` function, there is a one pixel boundary around the edge of each tile that does not get processed because ```sobel()``` uses a kernel to calculate the slope. To correct for this, we can use our tile iterator with a buffer to remove the artifact. When we add a buffer to our tile, there are a few things that we need:

1. The sub-rects for our tiles based on the tile size with X additional pixels added on the edges.

2. After we get the get the data from our raster and process it, we need to know what subset of the array corresponds to the actual tile data.

3. Once we subset our data, we need to know the sub-rect in our output raster where the processed data needs to go.

With the procedure ```createAwesomeTileIterator``` there are a few additional keywords that can be used to get this information. The `BUFFER` keyword is for specifying how many additional pixels we want around the edges of our tiles (X and Y buffers are assumed to be the same), the keyword `OUTPUT_TILE_SUB_RECTS` gives us the index location of our tile data that excludes the buffer, and the `OUTPUT_RASTER_TILE_LOCATIONS` will tell us where our data belongs in our output raster. Here is a modified example of the code above that will remove all tiling artifacts:

```idl
;make sure ENVI is running
e = envi(/CURRENT)
if (e eq !NULL) then begin
    e = envi()
endif

;specify our input file
file = 'C:\some\file\on\disk.dat'

if ~file_test(file) then begin
    message, 'file does not exist!'
endif

;open our raster
raster = e.OpenRaster(file)

;specif our output filename
outFile = e.GetTemporaryFilename()

;set up our output raster
newRaster = ENVIRaster(URI = outFile, $
    NROWS = raster.NROWS, $
    NCOLUMNS = raster.NCOLUMNS, $
    NBANDS = 1, $
    DATA_TYPE = 'uint',$
    SPATIALREF = raster.SPATIALREF)

;get our tiles for processing
createAwesomeTileIterator,$
    INPUT_RASTER = raster,$
    TILE_SIZE = [1024,1024],$
    TILE_BUFFER = 1,$    
    OUTPUT_SUB_RECTS = sub_rects,$
    OUTPUT_RASTER_TILE_LOCATIONS = tile_locations,$
    OUTPUT_TILE_SUB_RECTS = tile_data_subsets

;loop over our tiles
foreach sub_rect, sub_rects, idx do begin
    ;get the data from the source raster
    dat = raster.GetData(SUB_RECT = sub_rect, INTERLEAVE = 'bsq')

    ;detect the edges
    edgeDat = sobel(dat[*,*,0])

    ;get the subset of our data
    subset = tile_data_subsets[idx]

    ;subset our data
    newDat = edgeDat[subset[0]:subset[2], subset[1]:subset[3]]

    ;write the data to disk
    newRaster.SetData, newDat, SUB_RECT = tile_locations[idx]
endforeach

;save the results
newRaster.save

;display the raster in ENVI
view = e.GetView()
layer = view.CreateLayer(newRaster, /CLEAR_DISPLAY)
```

## Advanced Usage: createAwesomeOptimizedTileIterator

When trying to optimize the speed of your algorithsm, the efficiency of reading and writing data can be dramatically affected by the tiles you read/write. For all scenarios, if you can avoid it, it is best to **not** use square tiles to read/write your data. The reason behind this is because, when accessing and writing data, your hard drive will need to jump around to many different locations to read/write the data. Instead, you should use the "optimized" tiles for reading/writing data in a more efficient manner.

### Usage

The optimized tile iterator is used in a very similar way to the normal iterator. Here is an example for BSQ rasters:

```idl
;set up an output raster
output_raster = ENVIRaster( $
  INTERLEAVE = 'BSQ',$
  NBANDS = nBands, $
  NCOLUMNS = nColumns, $
  NROWS = nRows, $
  DATA_TYPE = 'uint')

;create tile iterator
createAwesomeOptimizedTileIterator,$
  INPUT_RASTER = output_raster,$
  PIXELS_PER_TILE = long(1024)*1024,$
  OUTPUT_SUB_RECTS = sub_rects

;process
tic
foreach sub, sub_rects, idx do begin
  ; get the tile size
  tileSize = [sub[2] - sub[0] + 1, sub[3] - sub[1] + 1]
  tArea = tileSize[0] * tileSize[1]

  ;make some data
  dat = uintarr(tileSize[0], tileSize[1], output_raster.NBANDS, /NOZERO)

  ;write the data
  output_raster.setData, dat, SUB_RECT = sub
endforeach
```


With the optimized tiles, we can then compare different approaches to tiling. Here are the time results to write an 8 band, 9000 column, 5000 row, uint dataset:

```
Time with square tiles and all bands (sec) : 15.018000
Time with optimized tiles and writing all bands per tile (sec): 11.063000
Time with optimized tiles and writing band per tile (sec): 1.2660000
```

## License

(c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

See LICENSE.txt for additional details and information.