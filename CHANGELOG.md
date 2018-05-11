# Changes

This file summarizes the overall changes.

## Latest (Version 2.1)

Added buttons to the uav_toolkit procedure so that you can have a UI for the tools and updated the readme. With this change, the progress information has also been updated.

Separated code that belogs o other repositories which is now included in the IDL packages folder.

Fixed a bug with Parrot Sequoia metadata have a different tag name for ISOSpeed.

Updated some routine names and task display names. This should not affect any code as the proper entry point is through the tasks. 


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