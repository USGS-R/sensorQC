`sensorQC`
========

High-frequency aquatic sensor QAQC procedures. `sensorQC` inports data, and runs various statistical outlier detection techniques as specified by the user. 

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

##What libraries does `sensorQC` need?
This version requires `yaml`. This package is available on CRAN, and will be installed automatically when using the `install.packages()` instructions above.

##Disclaimer
This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

This software is provided "AS IS".