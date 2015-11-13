# movement

[![Build Status](https://travis-ci.org/SEEG-Oxford/movement.svg?branch=master)](https://travis-ci.org/SEEG-Oxford/movement)
[![codecov.io](https://codecov.io/github/SEEG-Oxford/movement/coverage.svg?branch=master)](https://codecov.io/github/SEEG-Oxford/movement?branch=master)

## R package containing useful functions for the analysis of movement data in disease modelling and mapping

This package is a collaborative effort between a group of researchers to foster research into the analysis of human and animal movement for epidemiology. It's still in the very early stages of development, so expect the content to change a great deal in the future.

### installing and loading the package

You can install the package from github with [devtools package][devtools]:

```
devtools::install_github('SEEG-Oxford/movement')
library(movement)
```

### Usage

The most common use of the package is to parameterize a movement model based on observed population movements, and then use this model to predict _de novo_ population movements.

Code to fit such a model might look like this:

```
m <- movement(observed_movement ~ location_data, model = radiationWithSelection())
```

where ```observed_movement``` is a ```movement_matrix``` object containing observations about movements between pairs of locations, ```location_data``` is a ```location_dataframe``` object containing the coordinates and populations of those locations, and ```radiationWithSelection()``` creates a ```flux``` object, representing the type of movement model to by fitted. Current supported movement models are: radiation with selection, original radiation, gravity, gravity with distance cutoff, intervening opportunities and uniform selection.

The ```movement``` model fits the parameters of the specified movement model, and returns a ```movement_model``` object. This object can be plotted, or used to predict to populations movements to new ```location_dataframe``` object, or even a ```RasterLayer``` object giving populations in each cell:

```
plot(m)
prediction <- predict(m, location_data)
prediction <- predict(m, raster)
```

### Reporting Bugs

You can report bugs, issues and suggestions for extra functions using the issues button on the right hand side of this page.


#### Funding

Development of this software package is partly funded by the Research for Health in Humanitarian Crises (R2HC) Programme, managed by ELRHA. The Research for Health in Humanitarian Crises (R2HC) programme aims to improve health outcomes by strengthening the evidence base for public health interventions in humanitarian crises. Visit www.elrha.org/work/r2hc for more information. The £8 million R2HC programme is funded equally by the Wellcome Trust and DFID, with Enhancing Learning and Research for Humanitarian Assistance (ELRHA) overseeing the programme’s execution and management.

[devtools]: http://cran.r-project.org/package=devtools
