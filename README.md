`sensorQC`
========
[![Build status](https://ci.appveyor.com/api/projects/status/pho8872wbnvaw5nt)](https://ci.appveyor.com/project/jread-usgs/sensorqc)  
[![Build Status](https://travis-ci.org/USGS-R/sensorQC.svg?branch=master)](https://travis-ci.org/USGS-R/sensorQC)
High-frequency aquatic sensor QAQC procedures. `sensorQC` imports data, and runs various statistical outlier detection techniques as specified by the user. 

###Installing `sensorQC`
install this package using 

```
  install.packages("sensorQC", 
        repos = c("http://usgs-r.github.com", "http://cran.us.r-project.org"),
        dependencies = TRUE, type = "both")
```

###`sensorQC` Functions (as of v0.2.0)
| Function       | Title           |
| ------------- |:-------------|
| `build_flags` | Creates flag vector based on input data |
| `clean_data`  | Cleans sensor data with user-specified routines |
| `load_sensor` | Loads sensor data into data.frame |
| `load_sqc` | load in configuration file for sensorQC |
| `MAD` | median absolute deviation outlier test |
| `window_data` | Window sensorQC data |

##How does `sensorQC` figure out what statistical test(s) to use?
The `sensorQC` package uses a 'yaml' (file extension .yml) file that is human readable and editable, but is also easily parsed by a computer. The details in the yaml file will tell the package where the sensor data is, which stats to use, and how to parameterize those statistical tests. An example yaml file (and data file) are included in the `sensorQC` package, and they can be accessed through the file path given by typing `system.file('extdata', package = 'sensorQC')` into `R` after the package is loaded. 

Here is the example yaml file:
```
outlier_removal:
    - expression:   x < 0.01
      description:  obs below detection
      type:         threshold

    - expression:   x > 99
      description:  obs above range
      type:         threshold
      
    - expression:   n > 10
      description:  questionable persistent value  
      type:         persistent
      
    - expression:   x = 999999
      description:  logger error code
      type:         error_code
      
    - expression:   missing(x)
      description:  missing value
      type:         error_code      
      
    - expression:   x = -999
      description:  logger missing value code
      type:         error_code
      
    - expression:   MAD(x) > 3
      description:  median absolute deviation exceeded
      type:         stat_window

block_stats:
    - expression:   MAD(CV) > 3
      description:  median absolute deviation of the windowed CVs exceeded
      type:         threshold

    - expression:   flags > 30%
      description:  total number of values within block too low
      type:         threshold

data_source:
    - file_name:    test_data.txt
      folder_name:  ./
      format:       wide_burst
      window:       auto
      date_type:    mm/dd/YYYY HH:MM
```
#####`outlier_removal` specifies operations on instantaneous data
*threshold* and *error_code* types are used to flag data according to their values alone.
*persistent* and *stat_window* types use a little more information to determine outlines. *persistent* flags simply look for repetition in the reported values, therefore sequential information is used. An `n > 10` *persistent* check will look for any repeated values that appear more than 10 times in a row. *stat_window* flagging happens relative to a windowed number of observations. For example, `MAD(x) > 3` calculates the median absolute deviation (see function `MAD`) for a group of observations (the *window*), and flags all outliers that exceed the MAD > 3 criteria. 

#####`block_stats` specifies operations on summary statistics
These are outlier tests that occur after the `outlier_removal` flags have been removed. For example, the *threshold* test for `flags > 30%` will remove any summary value (the average of the *window* measurements, after `outlier_removal` values have been removed) that has less that 30% of the original instantaneous data. 

#####`data_source` specifies the location of the sensor data that is to be processed
*file_name* is the name of the file to be imported  
*folder_name* is the file path to the sensor data (this path is relative to the location of the yaml file)  
*format* is the data format for the sensor data  
*window* is the temporal data window (in seconds) or "auto" (for automatically calculated)  
*date_type* is the date format for the sensor data.  


##What libraries does `sensorQC` need?
This version requires `yaml`. This package is available on CRAN, and will be installed automatically when using the `install.packages()` instructions above.

##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."
