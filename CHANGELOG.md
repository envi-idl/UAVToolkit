# Changelog

This file summarizes the overall changes for the UAV toolkit.

## Version 2.3.5

Updated copyright statements

Added updated example to README for UAVBatchRededge

Rebuilt docs

Some minor updates to the idl.package.json file

## Version 2.3.1

Comment updates to add tooltips

Documentation updated

LICENSE.txt updated to add "Copyright"

idl.package.json updated for hooks into ENVI Tasks and Extensions

## Version 2.3

Updated the reflectance panel extractor to work better when the background is also bright. As long as the reflectance panels are greater than the mean reflectance of the image, they should be extractable. The new algorithm finds a measure of "squareness" and uses an optmization function to extract the most "square" clump of pixels in the scene.

Documentation has been updated.


## Version 2.2

Updated some of the progress messages when running ENVI headlessly

Added support for data that is already a multi-page TIFF in a single file, created a test for it, and updated the README with an example.


## Version 2.1

Added buttons to the uav_toolkit procedure so that you can have a UI for the tools and updated the readme. With this change, the progress information has also been updated.

Separated code that belogs o other repositories which is now included in the `idl_pacakges` folder.

Fixed a bug with Parrot Sequoia metadata have a different tag name for ISOSpeed.

Updated some routine names, task display names, and reorganized task parameters. This should not affect any code as the proper entry point is through the tasks.

Added a new Basic version of the UAV Toolkit Band Alignment task that has about 1/3 of the parameters to make it easier to use in the ENVI UI.


## Version 2.0.2

When generating the tie points for the bands, we algorithm now only uses ENVI's advanced algorithm for tie point generation. This means that the tie point generation process may take 10-15 minutes, depending on your data. This change removed the `RIGOROUS_ALIGNMENT` task parameter of the `UAVBandAlignment` task.


## Version 2.0.1

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