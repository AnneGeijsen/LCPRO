# LCPRO  [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) [![Build Status](https://travis-ci.org/wilsontom/LCPRO.svg)](https://travis-ci.org/wilsontom/LCPRO) [![Build status](https://ci.appveyor.com/api/projects/status/490edqjodu6rorjx/branch/master?svg=true)](https://ci.appveyor.com/project/wilsontom/lcpro-efqax/branch/master) [![Coverage Status](https://coveralls.io/repos/wilsontom/LCPRO/badge.svg?branch=master&service=github)](https://coveralls.io/github/wilsontom/LCPRO?branch=master)

> __processing and analysis of LCMS metabolomics data__


`LCPRO` is a collection of functions which are intended to be supplementary functions and helpers to processing LC-MS based metabolomics data using [xcms](http://bioconductor.org/packages/release/bioc/html/xcms.html). Many of the functions can be used as stand-alone implementations of various QC tasks which just a `xcmsSet` object as an input. Some functions are designed (and are more useful) to be part of a workflow/pipeline.

#### Installation

Prior to installing `LCPRO` the necessary dependencies need to be installed.

`xcms` is avaliable on bioconductor
```R
source("https://bioconductor.org/biocLite.R")
biocLite("xcms")
```

`metProc` is also needed. This can be install directly from github using `devtools`

```R
devtools::install_github("wilsontom/metProc")
```

All other dependencies are available from `CRAN`.

Then install `LCPRO` as follows;

```R
devtools::install_github("wilsontom/LCPRO")
```


_[COMING SOON] A GitHub wiki and vignette will provide more detailed documentation on the various functions and how they can be used to build a complete processing/analysis workflow_
