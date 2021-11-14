# cfb4th (development version)

# cfb4th 0.1.1

* Re-categorized some plays as unknown (i.e., `NA`) `go`: Penalties and Timeouts
* Fixed bug with 4th down plays inside own 10 not simulating failed go for it plays correctly
* Fixed bug with some plays having negative timeouts remaining, creating strange results 
* Improved timeout detection in `get_4th_plays()` and renamed some columns to better match cfbfastR
* Implemented 2-pt conversion handling. The model no longer assumes a touchdown is worth 7 points

# cfb4th 0.1.0

* Release as package
