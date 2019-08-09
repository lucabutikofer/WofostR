See [De Wit et al. (2019)](https://doi.org/10.1016/j.agsy.2018.06.018)
for details.

Python version available at
[ajdewit/pcse](https://github.com/ajwdewit/pcse.git) as part of the
Python Crop Simulation Environment (PCSE/WOFOST).

------------------------------------------------------------------------

### Installation

Install with `devtools::install_github('lucabutikofer/WofostR')`

------------------------------------------------------------------------

### Testing

After installation is complete you can test the program with
`test()`.This command uses the files published by [De Wit et
al.](https://doi.org/10.1016/j.agsy.2018.06.018) downloading a copy from
the [WofostR\_testData
repository](https://github.com/lucabutikofer/WofostR_testData.git) to
test either a specific routine (e.g. `test(component = "leafdynamics")`)
or all available components if none is specified. Currently only
“astro”, “leafdynamics” and “potentialproduction” are implemented. Many
routines are not independent funtions like in
[PCSE/WOFOST](https://github.com/ajwdewit/pcse.git) but are hardcoded
within function `Wofost()`, which is run in the “potentialproduction”
and “waterlimitedproduction” tests. Passing the “potentialproduction” or
“waterlimitedproduction” tests means also passing “assimilation”,
“astro”, “leafdynamics”, “partitioning”, “phenology”, “respiration”,
“rootdynamics” and “transpiration” (although transpiration is not
actually used to copute potential production). By default only three
test sets are randomly chosen to perform tests on. This is done to speed
up the process. To perform tests on all available sets call
`test(complete = TRUE)`.

``` r
test(component = 'phenology', complete = TRUE)
test()
```

------------------------------------------------------------------------

### Input files

Three types of input files are requred to run the Wofost():

-   Crop objects
-   Weather objects
-   Soil objects (not implemented yet, only used for waterlimited
    production)

These are S4 classes containing the variables required by function
`Wofost()`. Crop objects can be generated by calling `dwn.crop()`. This
function retrieves specific crop parameters from
[ajwdewit/WOFOST\_crop\_parameters](https://github.com/ajwdewit/WOFOST_crop_parameters.git)
by specifying the crop and variety names.

``` r
cr <- WofostR::dwn.crop(cropName = "sugarbeet",
                        variety = "Sugarbeet_601")
cr
#> 
#>  WofostR Crop Object: 
#>  >> Crop name: sugarbeet 
#>  >> Variety name: Sugarbeet_601 
#>  >> 92 crop parameters out of 92 are specified. 
#> 
str(cr, list.len = 2)
#> Formal class 'CropObject' [package "WofostR"] with 92 slots
#>   ..@ CROPNAME       : chr "sugarbeet"
#>   ..@ VARNAME        : chr "Sugarbeet_601"
#>   .. [list output truncated]
```

Weather objects must contain minimum and maximum temperature, radiation
and latitude at daily time-steps starting from the day the model is
started. An example weather object is saved in the package.

``` r
load('../data/randomWeather.rda')
randomWeather
#> 
#>  WofostR Weather Object: 
#>  
#>    VARIABLES  LENGTH
#> 1  @DAY       260   
#> 2  @E0        260   
#> 3  @ELEV      260   
#> 4  @ES0       260   
#> 5  @ET0       260   
#> 6  @IRRAD     260   
#> 7  @LAT       260   
#> 8  @LON       260   
#> 9  @RAIN      260   
#> 10 @SNOWDEPTH 260   
#> 11 @TEMP      260   
#> 12 @TMAX      260   
#> 13 @TMIN      260   
#> 14 @VAP       260   
#> 15 @WIND      260
str(randomWeather, list.len = 2)
#> Formal class 'WeatherObject' [package "WofostR"] with 15 slots
#>   ..@ DAY      : Date[1:260], format: "2010-04-16" ...
#>   ..@ E0       : num [1:260] 0.292 0.307 0.303 0.345 0.332 0.295 0.288 0.333 0.369 0.439 ...
#>   .. [list output truncated]
```

Soil objects are not implemented yet and are used only in water-limited
production.

All input files can be generated by feeding to their omonimous builder
function a named list with the same parameter names, by building one
from scratch or modifying an existing one.

------------------------------------------------------------------------

### Running Wofost

Wofost crop simulation model is run by functions `WofostPP()` or
`WofostFD()` for Potential Production and Free Draining water-limited
production respectively. (09.08.2019: This will soon be wrapped in new
function `Wofost()`)

``` r
out <- WofostR::WofostPP(crop = cr, w = randomWeather)
str(out)
#> List of 10
#>  $ dvs : num [1:223] -0.1 -0.0929 -0.0881 -0.082 -0.0733 ...
#>  $ lai : num [1:223] 0.000694 0.000694 0.000694 0.000694 0.000694 ...
#>  $ rd  : num [1:223] 10 10 10 10 10 10 10 10 10 10 ...
#>  $ tagp: num [1:223] 0.408 0.408 0.408 0.408 0.408 0.408 0.408 0.408 0.408 0.408 ...
#>  $ tra : num [1:223] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ twlv: num [1:223] 0.347 0.347 0.347 0.347 0.347 ...
#>  $ twrt: num [1:223] 0.102 0.102 0.102 0.102 0.102 0.102 0.102 0.102 0.102 0.102 ...
#>  $ twso: num [1:223] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ twst: num [1:223] 0.0612 0.0612 0.0612 0.0612 0.0612 0.0612 0.0612 0.0612 0.0612 0.0612 ...
#>  $ gass: num [1:223] 0 0 0 0 0 0 0 0 0 0 ...
plot(randomWeather@DAY[1:length(out$lai)], out$lai,
     main = 'Leaf Area Index', xlab = 'Date', ylab = 'LAI',
     type='l',col=4)
plot(randomWeather@DAY[1:length(out$tagp)], out$tagp,
     main = 'Total Above-Ground Production', xlab = 'Date', ylab = 'TAGP',
     type='l',col=4)
```

![](/private/var/folders/9m/9r747jlx757d63lp4_276k_c0000gn/T/RtmpEZFqLe/preview-15b9ffa6217.dir/my-vignette_files/figure-markdown_github/unnamed-chunk-4-1.png)![](/private/var/folders/9m/9r747jlx757d63lp4_276k_c0000gn/T/RtmpEZFqLe/preview-15b9ffa6217.dir/my-vignette_files/figure-markdown_github/unnamed-chunk-4-2.png)
