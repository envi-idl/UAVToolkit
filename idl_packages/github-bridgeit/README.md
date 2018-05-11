# bridge_it: Simple Parallel Processing for IDL

The bridge_it object was created to make it easy to perform batch processing with ENVI + IDL with procedures, functions, or tasks. Some examples are provided below and there is complete documentation within the source code at each method.


## Important Note

While this routine is easy to use, be aware that starting 16 bridges to process data might not be the most efficient way to plow through a bunch of data that might need processed. Instead, start with a smaller number and slowly increase how many bridges you create. Depending on your processing, you will either run into limitations with CPU, file I/O, or RAM access speeds. In general, the more bridges you have the slower each process with take, but this does change depending on what you are processing. 

See the Examples section for a sample that uses the TIME keyword to track how long each process takes. This can be a great tool to help gauge how many processes you want to run in parallel.


## How it Works

The primary function of this object was to run procedures and functions in parallel on different child processes. To do this, keywords are used for passing in arguments and keywords. Keywords are denoted with "_kw_" and arguments are sorted alphabetically. Here is an example that runs the equivalent to this IDL statement:

```
p = plot(findgen(10), 2*findgen(10), XRANGE = [0,5], YRANGE = [0,3])
```

And here is how you run this with the bridge_it object:

```
bdg = bridge_it(1)
!NULL = bdg.run( 'plot', ARG1 = findgen(10), ARG2 = 2*findgen(10), _KW_XRANGE = [0,5], _KW_YRANGE = [0,3])
```

From the example, we can see that we use the "run" function method to run functions. If we want to run procedures, then we use the "run" procedure method. Here is an example with the plot procedure:

```
bdg = bridge_it(1)
bdg.run, 'plot', ARG1 = findgen(10), ARG2 = findgen(10), _KW_XRANGE = [0,5], _KW_YRANGE = [0,3]
```

### Parameter Passing

For the pbject to pass information to the child processes, all parameters are passed by value. No shared memory is set up for the transfer. This means that you probably don't want to be passing around large arrays to the child processes.

Additionally, you cannot pass objects between the bridges. If you have objects (such as lists, hashes, or dictionary) you will need to use the json_serialize() and json_parse() functions. Here is an example of how you would do that with the bridge using teh PRE and CUSTOM_ARGS keywords:

```
;make some sample data
datHash = orderedhash()
dat['x'] = findgen(10)
dat['y'] = 2*findgen(10)

oBdg = bridge_it(1)
!NULL = obdg.Run('plot', CUSTOM_ARGS = ['x', 'y'], $
  PRE = ["datHash = json_parse('" + json_serialize(datHash) + "', /TOARRAY)", $
  'x = datHash["x"]', $
  'y = datHash["y"]'])
```

The above example does the equivalent of:

```
strings = json_serialize(datHash)
datHash = json_parse(strings, /TOARRAY)
x = datHash['x']
y = datHash['y']
p = plot(x,y)
```

Ideally you want to just use the bridge_it object with routines that have simple inputs. Otherwise you may need to write a simple wrapper routine to run in the bridge prior to executing the actual routine that you are wanting to run. 


### Troubleshooting

To better troubleshoot the processing, you can use the LOGDIR keyword when initialize the object. This will create a file called all_logs.txt after the object has been cleaned up. this file will provide information on any syntax errors or other problems when performing processing.



## Examples

Here are a few examples which demonstrate how to use the other features of the code. See the comments for complete documentation for the different keywords.


### Getting Results

Depending on what you are running, you will have results that you want to retrieve from the child processed. Using the run procedure and function methods you can use the ARGUMENTS_OUT and KEYWORDS_OUT keywords to specify what you want to return to the parent IDL process.

The results are returned in an orderedhash where the keys are based on the system time from when the process completes (with a few random digits also added). For function callbacks, the key to access the results is called "function_output." Other variables and keywords are named accordingly based on their input. 

```
bdg = bridge_it(2, INIT = 'pref_set, "IDL_CPU_TPOOL_NTHREADS", 1, /COMMIT')
for i=0,1 do !NULL = bdg.Run('fft', CUSTOM_ARGS = 'dat', PRE = 'dat = findgen(100)', /CALLBACK)

; wait for the processing to finish
bdg.wait

; get the results in an orderedhash
results = bdg.GetResults()

foreach resultHash, results, key do begin
  print, resultHash['$FUNCTION_RESULTS'], /IMPLIED_PRINT
endforeach
```

With the callback you also get other basic information about the processing time (if requested) and the actual string that was ran. The string is returned in the key "$RUNSTRING."


Note: The ExportRaster and RunENVITask methods you also have results returned by default. See the examples for how to correctly process those results. You also need to be careful about what you are returning: remember that only basic variables can traverse between processes. If you have more complex data types use the PRE and POST keywords with the run methods to make sure that you can correctly get your information back.



### Processing Time

To time the processing, you just need to use the TIME keyword with the run procedure and function methods

obdg = bridge_it(2)
for i=0,1 do !NULL = obdg.Run('fft', CUSTOM_ARGS = 'dat', PRE = 'dat = findgen(5000,5000)', /TIME)


### Controlling the Thread Pool

If you want to control the thread pool, in the INIT keyword for the bridge_it creation, you can specify the number of CPUs that IDL's threadpool will take advantage of. Here is an example:

```
obdg = bridge_it(2, INIT = 'pref_set, "IDL_CPU_TPOOL_NTHREADS", 1, /COMMIT')
for i=0,1 do !NULL = obdg.Run('fft', CUSTOM_ARGS = 'dat', PRE = 'dat = findgen(5000,5000)')
```


### ENVI Tasks

This useful example demonstrates how you can process ENVI tasks on the child process with a simple line of code.

Save the following to a PRO code file prior to running. Note that you may be limited on how many bridges you can start with ENVI running.

```
;Start ENVI so that we have access to rasters and tasks
e = envi(/HEADLESS)
compile_opt idl2

; Open an input file
File = Filepath('qb_boulder_msi', Subdir = ['data'], $
  Root_Dir = e.Root_Dir)
Raster = e.OpenRaster(File)

; Get the task from the catalog of ENVITasks
task = ENVITask('SpectralIndices')

; Define inputs
task.INDEX = ['Normalized Difference Vegetation Index', $
  'Visible Atmospherically Resistant Index']
task.INPUT_RASTER = Raster

; init our object
oBdg = bridge_it(1, INIT = 'e = envi(/HEADLESS)')

;run the task a few times
for i=0, 2 do begin
  ;set a new output raster URI - MUST DO THIS or all rasters have the same output
  task.OUTPUT_RASTER_URI = e.GetTemporaryFilename()
  oBdg.RunENVITask, task
endfor

;wait for everything to finish
oBdg.wait

;get the results from running our tasks
results = oBdg.GetResults()

;loop over and print our output raster location
foreach result, results do begin
  if result.hasKey('$TASK_ERROR') then begin
    print, 'Error for task: ' + result['$TASK_ERROR']
  endif else begin
    ;get the task from our results
    task = result['$TASK']

    ;print the output file
    print, 'Output raster URI: ' + task.OUTPUT_RASTER_URI
  endelse
endforeach
```

### Exporting ENVI Rasters

This method will export ENVI rasters to disk in parallel. This can be useful with virtual rasters like the ENVIMosaicRaster which have slow write speeds so you can efficiently perform the processing in parallel.

Save the following to a PRO code file prior to running. Note that you may be limited on how many bridges you can start with ENVI running.

```
;Start ENVI so that we have access to rasters and tasks
e = envi(/HEADLESS)
compile_opt idl2

; Open an input file
File = Filepath('qb_boulder_msi', Subdir = ['data'], $
  Root_Dir = e.Root_Dir)
Raster = e.OpenRaster(File)

; Get the task from the catalog of ENVITasks
task = ENVITask('SpectralIndices')

; Define inputs
task.INDEX = ['Normalized Difference Vegetation Index', $
  'Visible Atmospherically Resistant Index']
task.INPUT_RASTER = Raster

; init our object
oBdg = bridge_it(1, INIT = 'e = envi(/HEADLESS)')

;export the raster a few times
for i=0, 2 do begin
  ;specify output file
  outFile = e.GetTemporaryFilename()
  
  ;export to disk
  oBdg.ExportRaster, raster, outFile, DATA_IGNORE_VALUE = 0
endfor

;wait for everything to finish
oBdg.wait

;get the results from running our tasks
results = oBdg.GetResults()

;loop over and print our output raster location
foreach result, results do begin
  if result.hasKey('$EXPORT_ERROR') then begin
    print, 'Error exporting raster : ' + result['$EXPORT_ERROR']
  endif else begin
    ;print the output file
    print, 'Output raster URI: ' + result['$OUTPUT_RASTER_URI']
  endelse
endforeach
```

## Contributing

Create a pull request once you have made desired changes. Please try to follow the following guide when contributing:

[IDL ContribGuide](https://github.com/interactive-data-language/ContribGuide)


## License

Licensed under MIT. See LICENSE.txt for additional details and information.
<<<<<<< HEAD

(c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

=======

(c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.
>>>>>>> master
