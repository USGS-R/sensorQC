Installation
------------

Currently only available via github. Easiest way to install is to use the `devtools` package:

``` r
devtools::install_github("USGS-R/sensorQC")
```

This package is still very much in development, so the API may change at any time.

[![Build status](https://ci.appveyor.com/api/projects/status/pho8872wbnvaw5nt)](https://ci.appveyor.com/project/jread-usgs/sensorqc)

[![Build Status](https://travis-ci.org/USGS-R/sensorQC.svg?branch=master)](https://travis-ci.org/USGS-R/sensorQC)

[![Coverage Status](https://coveralls.io/repos/USGS-R/sensorQC/badge.svg?branch=master&service=github)](https://coveralls.io/github/USGS-R/sensorQC?branch=master)

High-frequency aquatic sensor QAQC procedures. `sensorQC` imports data, and runs various statistical outlier detection techniques as specified by the user.

### `sensorQC` Functions (as of v0.3.0)

| Function | Title                                                  |
|----------|:-------------------------------------------------------|
| `read`   | read in a file for sensor data or a config (.yml) file |
| `window` | window sensor data for processing in chunks            |
| `flag`   | create data flags for a sensor                         |

### example usage

``` r
library(sensorQC)
file <- system.file('extdata', 'test_data.txt', package = 'sensorQC') 
sensor <- read(file, format="wide_burst", date.format="%m/%d/%Y %H:%M")
```

    ## number of observations:5100

``` r
flag(sensor, 'x == 999999', 'n > 3', 'is.na(x)')
```

    ## object of class "sensor"
    ##               DateTime sensor.obs
    ## 1  2013-11-01 00:00:00      48.86
    ## 2  2013-11-01 00:00:01      49.04
    ## 3  2013-11-01 00:00:02      49.50
    ## 4  2013-11-01 00:00:03      48.91
    ## 5  2013-11-01 00:00:04      48.90
    ## 6  2013-11-01 00:00:05      48.96
    ## 7  2013-11-01 00:00:06      48.48
    ## 8  2013-11-01 00:00:07      48.97
    ## 9  2013-11-01 00:00:08      48.97
    ## 10 2013-11-01 00:00:09      48.99
    ## 11 2013-11-01 00:00:10      48.35
    ## 12 2013-11-01 00:00:11      48.51
    ## 13 2013-11-01 00:00:12      49.25
    ## 14 2013-11-01 00:00:13      48.82
    ## 15 2013-11-01 00:00:14      49.22
    ##   ...
    ## x == 999999 (15 flags)
    ## n > 3 (4 flags)
    ## is.na(x) (0 flags)

Use the `MAD` (median absolute deviation) test, and add `w` to the function call to specify "windows" (note, sensor must be windowed w/ `window()` prior to using `w`)

``` r
sensor = window(sensor, 'auto')
flag(sensor, 'x == 999999', 'n > 3', 'MAD(x,w) > 3', 'MAD(x) > 3')
```

    ## object of class "sensor"
    ##               DateTime sensor.obs
    ## 1  2013-11-01 00:00:00      48.86
    ## 2  2013-11-01 00:00:01      49.04
    ## 3  2013-11-01 00:00:02      49.50
    ## 4  2013-11-01 00:00:03      48.91
    ## 5  2013-11-01 00:00:04      48.90
    ## 6  2013-11-01 00:00:05      48.96
    ## 7  2013-11-01 00:00:06      48.48
    ## 8  2013-11-01 00:00:07      48.97
    ## 9  2013-11-01 00:00:08      48.97
    ## 10 2013-11-01 00:00:09      48.99
    ## 11 2013-11-01 00:00:10      48.35
    ## 12 2013-11-01 00:00:11      48.51
    ## 13 2013-11-01 00:00:12      49.25
    ## 14 2013-11-01 00:00:13      48.82
    ## 15 2013-11-01 00:00:14      49.22
    ##   ...
    ## x == 999999 (15 flags)
    ## n > 3 (4 flags)
    ## MAD(x,w) > 3 (129 flags)
    ## MAD(x) > 3 (91 flags)
