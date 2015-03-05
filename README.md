# movement [![Build Status](https://travis-ci.org/andrew-schofield-tessella/movement.svg?branch=master)](https://travis-ci.org/andrew-schofield-tessella/movement) [![Coverage Status](https://coveralls.io/repos/andrew-schofield-tessella/movement/badge.svg)](https://coveralls.io/r/andrew-schofield-tessella/movement) 
## R package containing useful functions for the analysis of movement data in disease modelling and mapping

This package is a collaborative effort between a group of researchers to foster research into the analysis of human and animal movement for epidemiology. It's still in the very early stages of development, so expect the content to change a great deal in the future.

### installing and loading the package

To install the package from github you first need to install and load Hadley Wickham's [devtools package][devtools], like this:

```
install.packages('devtools')
library(devtools)
```

Then use the `install_github` function

```
install_github('movement', 'SEEG-Oxford')
```

and load the package and you're ready to go

```
library(movement)
```

### Usage

The most common use of the package is to parameterize a movement model based on observed population movements, and then use this model to predict _de novo_ population movements.

```
m <- movement(locations = df_locations$location, coords = df_locations[, c('lon','lat')], population = df_locations$pop, movement_matrix = observed, model = 'radiation with selection')
```
Where df_locations is a data.frame containing location, lon, lat and pop columns corresponding to location ids, coordinates and populations respectively. movement_matrix is a square matrix of observed population movements between the location_ids, and model is the selected movement model (valid models are radiation with selection, original radiation, gravity, intervening opportunities and uniform selection).

This returns an optimisedmodel object which can be used by predict() to predict population movements from a RasterLayer, or a dataframe formatted as df_locations above.

```
prediction <- predict(m, raster)
prediction <- predict(m, df_locations)
```

### Contributors

[Nick Golding][Nick] @ [Spatial Ecology and Epidemiology Group, Oxford][seeg]

Andrew Schofield @ [Tessella][tessella]

### Reporting Bugs

You can report bugs, issues and suggestions for extra functions using the issues button on the right hand side of this page.


[Nick]: http://www.map.ox.ac.uk/about-map/map-team/nicholas-golding/
[seeg]: http://simonhay.zoo.ox.ac.uk/staff.php
[devtools]: http://cran.r-project.org/web/packages/devtools/index.html
[tessella]: http://www.tessella.com/
