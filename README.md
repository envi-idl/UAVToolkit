# UAVToolkit

This collection of code is meant for preprocessing drone data that has band-band alignment issues. Specifically, it was designed to work with the MicaSense RedEdge sensor in order to process the data and make it ready for ingestion into image stitching software packages. The benefit of processing on your own for the RedEdge sensor is that you don't need to pay monthly fees to process your data on the Atlas platform.

Over time these tools have grown into more than just processing for the RedEdge sensor. There is now an option for working with generic sensor formats where each band comes as a separate image format. Additional support has also been added for the Parrot Sequoia sensor which is another common, inexpensive, multispectral drone sensor.

If you have downloaded this repository, then the best way to view the documentation is to open up **index.html** in the **docs** folder.



## Major Changes

There have been many overall updates to the UAV Toolkit since it's initial release which consists of:

- Complete automation of the process for generating and applying reference tie points to imagery.

    There is a new routine that works behind the scenes to find an image group that the reference tie points can be taken from and it has worked very well on the test datasets that I have used.

- Refactoring and optimization of much of the code

    Behind the scenes a new suite of routines is used for performing the band-band registration that can be used with any type of imagery or raster.

- Generic support for any multispectral sensor that produces separate images for each band.

    If there are other sensors feel free to make a request for adding them, provided sample data can be shared.

- Refocused overall tools for band-band alignment 

    - **This means that some routines have been removed completely from this version of the UAV Toolkit. If there are missing functions/procedures that you want which are missing, then file a bug report via the main GitHub repository and I will add them back in.**

- Packaged the tools to use the ENVI Task framework

### Worth Mentioning

If there are any requests, don't hesitate to add an issue for the repository. I can easily create some ENVI extensions for the tasks to help make things easier to process if you are not an IDL programmer.


## Installation

**Make sure you remove any previous version of the UAV Toolkit from IDL's search path or ENVI's custom_code directories**.

### Basic

To install the code, you just need to add this repository to IDL's search path. You can access this setting in **Preferences -> IDL -> Paths -> Insert**, navigate to the directory and select OK, and then make sure to check the box to have this folder recursively searched for code. Click **Apply** and **OK** once the path has been added.

Note that you may have to reset IDL's session for the changes to take place. To do this you can simply press the **Reset** button in the IDL workbench.

### Advanced Installation - SAVE files

For advanced users that want to create an IDL SAVE file with pre-compiled code, simply run the script `build_uav_toolkit.pro` which will create a SAVE file with accompanying tasks. The folder will be called "UAVToolkit-build" and will be created in your system's temporary directory. In this folder you will also find the task files for the several tasks include din the toolkit. Once you have done this, you have two ways to install the SAVE files. You can take the contents of the build directory and :

- For ENVI 5.3 and admin rights the directory for Windows is **C:\\Program Files\\Exelis\\ENVI53\\custom_code**

- For ENVI 5.4 and admin rights the directory for Windows is **C:\\Program Files\\Harris\\ENVI54\\custom_code**

- If you do not have admin rights then you can find the local user directory in ENVI's preferences under:

    **File -> Preferences -> Directories -> Custom Code Directory**

    Once you place the file in ENVI's custom code folder you **must restart ENVI** before you will have access to them in ENVI.

**Note the location that you installed the SAVE files to. If you get a later version of this code, then you will need to update or remove these files for any changes to take effect.**

## Requirements

ENVI 5.4 and IDL 8.6. 

This should likely work on ENVI 5.3 and IDL 8.5 (*should*) but is untested. If you are on this older version and there are issues, then you will need to upgrade to the latest version of ENVI + IDL.


## Usage

To take advantage of the UAV toolkit, you just need to add the source code for this repository to IDL's search path and include the following in and routines that you want to use the band alignment tasks for:

```idl
;set IDL's compile options
compile_opt idl2

;start ENVI - can be headless or GUI
e = envi()

;run the UAV Toolkit routine
uav_toolkit
```

Including ```idl uav_toolkit``` ensures that the tasks that ship as a part of this repository are found by ENVI without needing to install them in ENVI's custom code directory.

Below you can find several examples for how you can use the different tasks in the UAV Toolkit to process different UAV multispectral datasets, but first a few notes on the tools.

### Running the Examples Below

To run the examples below, the easiest method is to create a new PRO file in the IDL Workbench. To do this, simply:

1. Click on the **New Pro** icon the the workbench 

2. Copy the example code to the window

3. Add and `end` statement as the **last** line in the file

4. Save the code with a **.pro** fifle extension

5. Press **Compile** and then **Run** to execute the code.


### Notes

- **Calibration**: 

    If you have reflectance panels, then these images will need to be separated from the main flight images. To work with the UAV Toolkit, you will need to place all reflectance panels in a folder called "reflectance_panels" in your data directory. This makes sure that these scenes can't be chosen for generating reference tie points. To have the reflectance panels be applied to convert the scenes to reflectance, then you will need to set the `CO_CALIBRATION` parameters to for the `UAVBandAlignment` task. This will correct for any differences in exposure time, ISO sensitivity to light, and perform the calibration step if the panels are present.

    When using reflectance panels, you will also need to specify the percent reflectance for the panel surface (i.e. 70%). To do this, you can use the `PANEL_REFLECTANCE` task parameter to specify a 0 to 100 floating point value for this.

- The algorithm may have a harder time finding reference tie points for the Parrot Sequoia because the multispectral sensor has a fisheye lens, which makes the geometry more complicated. The tools shown below have worked great on sample datasets, but that may not apply to all collections. It should, but it is just worth setting expectations a bit.

### Generic Sensor

In order to process generic sensors with the UAV Toolkit, you will need datasets that contain a single band per separate image file. Each band should have:

1. Similar data range and data type. 

2. Same image dimensions

If these two conditions are met, then you just need to have the unique file identifiers that can be used to group the separate images into a single raster. Here is an example of an image group and the file identifiers:

- img_001_1.tif

- img_001_2.tif

- img_001_3.tif

- img_001_4.tif

And the file identifiers look like:

-  `['_1.tif', '_2.tif', '_3.tif', '_4.tif']`

This groups the images above into a single image. If we want to then generate and apply reference tie points for this generic group then we can simply do the following in IDL:

```idl
;set IDL's compile options
compile_opt idl2

;start ENVI
e = envi(/HEADLESS)

;get UAV Toolkit ready
uav_toolkit

;set up task
alignTask = ENVITask('UAVBandAlignment')
alignTask.SENSOR = 'generic'
alignTask.FILE_IDENTIFIERS = ['_1.tif', '_2.tif', '_3.tif', '_4.tif']
alignTask.INPUTDIR = 'C:\data\directory'
alignTask.GENERATE_REFERENCE_TIEPOINTS = 1
alignTask.APPLY_REFERENCE_TIEPOINTS = 1
alignTask.execute
```


### MicaSense RedEdge

A custom batch processing routine has been developed for the MicaSense RedEdge sensor to make processing the data very simple. To process the data, you can simply do the following using IDL:

```idl
;set IDL's compile options
compile_opt idl2

;start ENVI headlessly
e = envi(/HEADLESS)

;initialize the uav_toolkit
uav_toolkit

;this folder should be the one that contains the unzipped contents of the sample data.
flightDir = 'C:\data\rededge'

;create our batch task
redEdgeTask = ENVITask('UAVBatchRedEdge')
redEdgeTask.FLIGHTDIR = flightDir
redEdgeTask.execute
```

Where `FLIGHTDIR` is the folder that *contains* the RedEdge data.  In other words, `FLIGHTDIR` should contain the folders 000 and 001 as collected by the sensor. These folders must all be from the same sensor and be at approximately the same height off of the ground for the reference tie points to be valid.

If you have reflectance panel images in each data folder, then create a subdirectory called "reflectance_panels" and place them in there. These files will be excluded from processing and only used to calibrate the data.

You can also use the ENVITask `UAVBandAlignment` directly to process a single folder of data using:

```idl
;set IDL's compile options
compile_opt idl2

;start ENVI
e = envi(/HEADLESS)

;get UAV Toolkit ready
uav_toolkit

;set up task
alignTask = ENVITask('UAVBandAlignment')
alignTask.SENSOR = 'rededge'
alignTask.INPUTDIR = 'C:\data\directory'
alignTask.GENERATE_REFERENCE_TIEPOINTS = 1
alignTask.APPLY_REFERENCE_TIEPOINTS = 1
alignTask.execute
```

#### Customizing Batch RedEdge Parameters

If you are running the `UAVBatchRedEdge` task, then you can pass in a custom `UAVBandAlignment` task to edit all paramters of the registration process. Here is an example of how you can do this to have the `UAVBatchRedEdge` generate multi-channel TIFFs instead of multi-page TIFFs.

```idl
;set IDL's compile options
compile_opt idl2

;start ENVI headlessly
e = envi(/HEADLESS)

;initialize the uav_toolkit
uav_toolkit

;create band alignment task to customize settings
alignTask = ENVITask('UAVBandAlignment')
alignTask.MULTI_CHANNEL = 1

;this folder should be the one that contains the unzipped contents of the sample data.
flightDir = 'C:\data\rededge'

;create our batch task
redEdgeTask = ENVITask('UAVBatchRedEdge')
redEdgeTask.FLIGHTDIR = flightDir
redEdgeTask.BAND_ALIGNMENT_TASK = alignTask
redEdgeTask.execute
```

### Parrot Sequoia

You can also process the Parrot Sequoia data with the UAV toolkit. Note that generating the tie points for this data may take longer because the multispectral bands are taken with a fisheye lens. To process the Sequoia data, just use the `UAVBandAlignment` task with:


```idl
;set IDL's compile options
compile_opt idl2

;start ENVI
e = envi(/HEADLESS)

;get UAV Toolkit ready
uav_toolkit

;set up task
alignTask = ENVITask('UAVBandAlignment')
alignTask.SENSOR = 'sequoia'
alignTask.INPUTDIR = 'C:\data\directory'
alignTask.GENERATE_REFERENCE_TIEPOINTS = 1
alignTask.APPLY_REFERENCE_TIEPOINTS = 1
alignTask.execute
```

### Custom Percent Reflectance

If you know the percent reflectance (from 0 to 100) of your reflectance panel for each of your bands, you can specify them inidivually with the `PANEL_REFLECTANCE` task parameter for the tasks demonstrated above. Here is an example with the `UAVBatchRedEdge` task:

```idl
;set IDL's compile options
compile_opt idl2

;start ENVI headlessly
e = envi(/HEADLESS)

;initialize the uav_toolkit
uav_toolkit

;this folder should be the one that contains the unzipped contents of the sample data.
flightDir = 'C:\data\rededge'

;create our batch task
redEdgeTask = ENVITask('UAVBatchRedEdge')
redEdgeTask.FLIGHTDIR = flightDir
redEdgeTask.PANEL_REFLECTANCE = [67, 69, 68, 67, 61] ;must be 0 to 100, NOT 0 to 1.0
redEdgeTask.execute
```

### File Formats

By default the UAV Toolkit generates multi-page TIFFs for the output files. If you want to generate multi-band TIFFs instead, you can do the following for the BandAlignmentTask:

```idl
;set IDL's compile options
compile_opt idl2

;start ENVI
e = envi(/HEADLESS)

;get UAV Toolkit ready
uav_toolkit

;set up task
alignTask = ENVITask('UAVBandAlignment')
alignTask.SENSOR = 'sequoia'
alignTask.INPUTDIR = 'C:\data\directory'
alignTask.GENERATE_REFERENCE_TIEPOINTS = 1
alignTask.APPLY_REFERENCE_TIEPOINTS = 1
alignTask.MULTI_CHANNEL = 1
alignTask.execute
```


### Default Values

For each one of the sensors, there are some default task parameters for the UAVBandAlignment task which are custom to each sensor type. Each of these parameters can be overridden by specifying the accompanying parameter in the `UAVBandAlignment` task object. 

These defaults are taken directly from the source code and are:

```idl
;==============================================================================================
;check the sensor type and set defaults for our processing
case (parameters.SENSOR) of
  'rededge':begin
    parameters['BASE_BAND'] = 2
    parameters['FILE_IDENTIFIERS'] = ['_1.tif', '_2.tif', '_3.tif', '_5.tif', '_4.tif']

    ;specify the metadata for our output scene
    meta = ENVIRasterMetadata()
    meta['band names'] = ['Blue', 'Green', 'Red', 'Red Edge', 'NIR']
    meta['wavelength units'] = 'nm'
    meta['wavelength'] = [450, 550, 650, 750, 850]
    parameters['METADATA'] = meta

    ;check for other parameter values
    if ~parameters.hasKey('SEARCH_WINDOW_FROM_HEIGHT') then parameters['SEARCH_WINDOW_FROM_HEIGHT'] = 1
    if ~parameters.hasKey('CO_CALIBRATION') then parameters['CO_CALIBRATION'] = 1
    if ~parameters.hasKey('MAX_VALUE_DIVISOR') then parameters['MAX_VALUE_DIVISOR'] = 16s
    if ~parameters.hasKey('MAX_PIXEL_VALUE') then parameters['MAX_PIXEL_VALUE'] = 4095
  end
  'sequoia':begin
    parameters['BASE_BAND'] = 1
    parameters['FILE_IDENTIFIERS'] = ['_GRE.TIF', '_RED.TIF', '_REG.TIF', '_NIR.TIF']

    ;specify the metadata for our output scene
    meta = ENVIRasterMetadata()
    meta['band names'] = ['Green', 'Red', 'Red Edge', 'NIR']
    meta['wavelength units'] = 'nm'
    meta['wavelength'] = [550, 650, 750, 850]
    parameters['METADATA'] = meta

    ;check for other parameter values
    if ~parameters.hasKey('SEARCH_WINDOW_FROM_HEIGHT') then parameters['SEARCH_WINDOW_FROM_HEIGHT'] = 0
    if ~parameters.hasKey('CO_CALIBRATION') then parameters['CO_CALIBRATION'] = 0
    requested_number_of_tiepoints = 500
    minimum_filtered_tiepoints = 100
  end
  else:begin
    ;make sure we have information about our panel identifiers
    if ~parameters.hasKey('FILE_IDENTIFIERS') then begin
    message, 'FILE_IDENTIFIERS not specified in parameters, required for generic sensor!'
    endif

    ;set default parameters
    if ~parameters.hasKey('BASE_BAND') then paramers['BASE_BAND'] = 0
    if ~parameters.hasKey('SEARCH_WINDOW_FROM_HEIGHT') then parameters['SEARCH_WINDOW_FROM_HEIGHT'] = 0
    if ~parameters.hasKey('CO_CALIBRATION') then parameters['CO_CALIBRATION'] = 0
  end
endcase
```


## License

Licensed under MIT. See LICENSE.txt for additional details and information.

(c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.
