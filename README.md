*Regionally-structured explanations behind area-level populism: An update to recent ecological analyses* [![DOI](https://zenodo.org/badge/178076172.svg)](https://zenodo.org/badge/latestdoi/178076172)

================
*Roger Beecham*

This repository contains additional code and material for our *PLOS One*
paper [*Regionally-structured explanations behind area-level populism:
An update to recent ecological
analyses*](https://doi.org/10.1371/journal.pone.0229974).

The data in this repository (found in [data/](data/)) is assembled from:

*Voting data*

  - [Electoral Commision](https://www.electoralcommission.org.uk) :
    area-level UK Referendum data.
  - [Tony
    McGovern](https://github.com/tonmcg/County_Level_Election_Results_12-16)
    : area-level US Presidential Election data.

*Demographic and context data*

  - [ONS](https://www.ons.gov.uk) : 2011 UK Census.
  - [Casweb](http://casweb.ukdataservice.ac.uk) : 1971 UK Census.
  - [US Census Bureau](https://www.census.gov/en.html) : 2010 US Census.
  - [NHGIS](https://www.nhgis.org) : 1970 US Census.

*Boundary data*

  - [US Census
    Bureau](https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html)
    : US Boundary data.
  - [ONS Open Geography Portal](http://geoportal.statistics.gov.uk) : GB
    Boundary data.

This repository contains:

  - [README.md](README.md) : This file, which provides code and detail
    around the data analysis that is additional to that appearing in the
    paper.
  - [src/](src/) : R files, introduced below, with more subtantial code
    chunks or with helper functions.
  - [figures/](figures/) : Figures generated from the code used in this
    file.

## Required Libraries

The following are available on CRAN and can be installed with
`install.packages(<package_name>)`:

``` r
library(tidyverse)              # bundle of packages for data manipulation 
library(sf)                     # for working with geospatial data
library(lme4)                   # for multilevel modelling
library(piecewiseSEM)           # for multilevel modelling diagnostics
library(glmnet)                 # for penalised regression

# Set ggplot2 theme_minimal().
theme_set(theme_minimal())
```

## Load data

A script for processing the Census and Boundary data is in the
[src/](src/) folder (`data_processing.R`). Calling this script writes
out processed files separately for GB and US as `.geojson` to the
[/data](/data) folder.

In the code below we first load the Brexit dataset. Using
`glance(<df_name>)`, notice that it consists of 380 observations (Local
Authority Districts – LADs) and 19 variables. These are listed for a
sample LAD in the block below. Vote data are not published at LAD-level
for Northern Ireland and so we analyse the 380 LADs in Great Britain
only.

``` r
# Read in data.
brexit_data <- st_read("./data/brexit.geojson")

# Sample LAD.
# $ lad_code                  <chr> "E09000033" 
# $ lad_name                  <fct> Westminster
# $ region_name               <chr> "London"
# $ region_abbr               <chr> "Lon"
# $ gridX                     <dbl> 3
# $ gridY                     <dbl> 4
# $ total_pop                 <int> 438792
# $ share_leave               <dbl> 0.3103484
# $ net_leave                 <dbl> -0.1896516
# $ degree_educated           <dbl> 0.5755543
# $ not_good_health           <dbl> 0.1585307
# $ household_income          <int> 44027
# $ transport_trade_utilities <dbl> 0.1553891
# $ leisure_hospitality       <dbl> 0.06538105
# $ manufacturing_loss        <dbl> 0.1067614
# $ foreign_born              <dbl> 0.5332321
# $ older_adults              <dbl> 0.111734
# $ pop_density               <dbl> -3.888546
# $ geometry                  <sf_geometry [degree]> MULTIPOLYGON (((-0.111581 5...
```

Next, load the Trump dataset in the same way. This consists of 3108
observations (counties) and 23 variables. Vote data are not published at
county-level for Alaska and Hawaii and so we anlalyse data describing
mainland US only.

``` r
# Read in data.
trump_data <- st_read("./data/trump.geojson", crs=2163)

# Sample county.
# $ county_code               <chr> "37067"
# $ county_name               <chr> "Forsyth County"
# $ state_name                <chr> "North Carolina"
# $ state_abbr                <chr> "NC"
# $ division                  <chr> "South Atlantic"
# $ gridX                     <int> 8
# $ gridY                     <int> 5
# $ total_pop                 <dbl> 368019
# $ share_trump               <dbl> 0.44711
# $ net_trump                 <dbl> -0.1057801
# $ shift_trump               <dbl> -0.01650177
# $ shift_direction           <chr> "away"
# $ flip                      <chr> "Obama Clinton"
# $ degree_educated           <dbl> 0.336
# $ poverty                   <dbl> 0.175
# $ household_income          <dbl> 27593
# $ leisure_hospitality       <dbl> 0.1101108
# $ transport_trade_utilities <dbl> 0.1806304
# $ manufacturing_loss        <dbl> -0.2721169
# $ foreign_born              <dbl> 0.085
# $ older_adults              <dbl> 0.151
# $ pop_density               <dbl> 0.0003332722
# $ geometry                  <sf_geometry [m]> MULTIPOLYGON (((1736685 -78...
```

## Summarise administrative geography

![Plot summarising how population distributes amongst administrative
geographies.](./figures/admin_geogs.png)

We treat as similar US counties and states and GB LADs and Government
Office Regions (GORs). Since this administrative geography determines
the hierarchical structure used in our data analysis, it is worth
quickly inspecting how both populations are distributed amongst these
geographies. Code for this analysis can be viewed at this position of
the repo’s [READE.Rmd](README.Rmd) file.

## Generate maps of results

![Plot of net-Leave, net-Trump and shift-Trump
votes.](./figures/choropleths.png)

We consider how the *net-Leave*, *net-Trump* and *shift-Trump* vote
distributes geographically by generating choropleth maps of the results.
Code for generating the maps can be viewed at this position of the
repo’s [READE.Rmd](README.Rmd) file.

## Generate univariate multilevel models

To explore associations between outcome and explanatory variables, and
the scale and nature of regionally-varying effect, we generate
univariate multilevel models with GB LADs at level 1 and GORs at level 2
and US counties at level 1 and states at level 2. There are a set of
helper functions for generating model summaries and diagnostics.

``` r
source("./src/mutlilevel_helper_functions.R")
```

### Univariate models for the Brexit datasets

![Plots of univariate random slope models on
net-Leave.](./figures/brexit_multilevel.png)

Compute multilevel models and summary statistics for interrogating
regional scale and effects in the Brexit datasets. Code for generating
the models can be viewed at this position of the repo’s
[READE.Rmd](README.Rmd) file.

Plot multilevel Brexit models. Code for generating the model plots can
be viewed at this position of the repo’s [READE.Rmd](README.Rmd) file.

### Univariate models for the Trump datasets

![Plots of univariate random slope models on
net-Leave.](./figures/trump_multilevel.png)

Compute multilevel models and summary statistics. Code for generating
the models can be viewed at this position of the repo’s
[READE.Rmd](README.Rmd) file.

Plot multilevel Trump models. Code for generating the model plots can be
viewed at this position of the repo’s [READE.Rmd](README.Rmd) file.

## Generate global multivariate models using penalised regression

![Plots of penalised global models.](./figures/globals.png)

We develop full multivariate models to investigate the effect of the
selected variables after controlling for variation in each. Models are
first built globally (without controlling for regional structure), then
by modelling the subnational scale as fixed effect dummy variables (as
per the existing ecological literature) and finally by developing an
ensemble of models built separately for each region (all 11 GORs of GB
and 49 states of mainland US).

We use penalised regression for variable selection and regularisation.
The full paper contains a discussion of penalised regression and its
relevance to this study. A key feature is that an upper bound, or
penalty, is applied on the sum of regression coefficients, penalising
against those likely to be unstable. This penalty can be differently
applied. In *ridge regression* variables containing coefficients with
extreme values are shrunk continuously towards zero; in *least absolute
shrinkage and selection operator* (LASSO) penalised coefficients can be
shrunk to zero and removed from the model entirely; and *elastic-net* is
a combination of the two.

Not disucssed in the full paper is the fact that a decision needs to be
made on the *amount* of penalty (\(\lambda\)) to be applied. The aim is
to specify a penalty sufficiently strong to address overfitting while
not overly penalising coefficients. In our models, \(k\)-fold
cross-validation (CV) is used to arrive at this value empirically. Here
the dataset is partitioned in to \(k\) sets (folds) of equal size. An
extreme value for \(\lambda\) is specified and then the data are fit on
\(k\)-1 folds and model parameters used to predict values in the
\(k\)-th fold. This process is repeated for \(k=1...n\) and measures of
fit (means square error) recorded for each value of \(k\) which are then
averaged and that summary statistic recorded for that penalty (value of
\(\lambda\)). The value for \(\lambda\) is then changed and the process
iterates many times. The selected \(\lambda\) is that which results in
the smallest average mean square errors, although following [Hastie et
al. (2017)](https://web.stanford.edu/~hastie/ElemStatLearn/), we use the
*one-standard-error rule* where the least complex model is chosen among
the models within one-standard error range of the best model.

This process is abstracted through a set of functions that can be
accessed via the
[`glmnet`](https://cran.r-project.org/web/packages/glmnet/index.html)
library. In the full paper we present model outputs derived using the
*elastic-net* penalty (a combination of *ridge* and LASSO penalties).
However, in arriving at a specification using *elastic-net*, we
performed sensitivity tests by varying an \(\alpha\) term in `glmnet`
that moves continuously from full *ridge* (\(\alpha=0\)) to full LASSO
(\(\alpha=1\)). The code below presents estimated coefficients with
\(\alpha\) values of `0 - 0.25 - 0.5 - 0.75 - 1` and we develop visual
encodings that support comparison across these different model
specifications. Note that we generate bootstrap confidence intervals
(CIs) for parameters estimated by these model specifications and the
process described above is repreated many times; the code therefore
therefore takes some time to execute.

There are a set of helper functions for generating model outputs and
diagnostics.

``` r
source("./src/penalised_helper_functions.R")
```

### Prep data for global penalised regression

Before generating the penalised regression models we need to create
dummy variables representing subnational controls following
[Essletzbichler et
al. (2018)](https://academic.oup.com/cjres/article-abstract/11/1/73/4821297)
– for GB we create controls for England, Wales and for US controls
representing the nine US Divisions. We also need to z-score standardise
our explanatory variables. Code can be viewed at this position of the
repo’s [READE.Rmd](README.Rmd) file.

### Generate global penalised regression models (takes a while to execute)

Code can be viewed at this position of the repo’s
[READE.Rmd](README.Rmd) file.

### Plot global penalised regression models for sensitivity analysis

Code can be viewed at this position of the repo’s
[READE.Rmd](README.Rmd) file.

## Generate regional multivariate models using penalised regression

![Plots of penalised regional models.](./figures/regionals.png)

### Prep data for regional penalised regression

We need to z-score standardise our variables, but this time do so per
region (GB GOR and US state) since we build region-specific models.
Additionally, for regions that contain too few observations (LADs or
counties) for parametric assumptions to hold, we borrow from
neighbouring regions, defined by straight-line distances between
LAD/county centroids. Code can be viewed at this position of the repo’s
[READE.Rmd](README.Rmd) file.

### Do regional penalised regression (takes some time to execute)

Code can be viewed at this position of the repo’s
[READE.Rmd](README.Rmd) file.

### Plot regional penalised regression models for sensitivity analysis

Code can be viewed at this position of the repo’s
[READE.Rmd](README.Rmd) file.
