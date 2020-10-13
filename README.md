
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ageR: Age Models

<!-- <img src="inst/images/logo.png" alt="logo" align="right" height=200px/> -->

<!-- badges: start -->

<!-- [![](https://img.shields.io/github/languages/code-size/special-uor/ageR.svg)](https://github.com/special-uor/ageR) -->

[![](https://img.shields.io/badge/devel%20version-0.1.0-yellow.svg)](https://github.com/special-uor/ageR)
[![R build
status](https://github.com/special-uor/ageR/workflows/R-CMD-check/badge.svg)](https://github.com/special-uor/ageR/actions)
[![](https://www.r-pkg.org/badges/version/ageR?color=black)](https://cran.r-project.org/package=ageR)
<!-- [![](https://codecov.io/gh/special-uor/ageR/branch/master/graph/badge.svg?token=Q6SYL7AOGR)](https://codecov.io/gh/special-uor/ageR) -->
<!-- badges: end -->

The goal of ageR is to
…

## Installation

<!-- You can install the released version of ageR from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("ageR") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/special-uor) with:

``` r
# install.packages("remotes")
remotes::install_github("special-uor/ageR")
```

## Example

### Bacon model

#### Input structure

For an entiy X, the input structure should look like

``` r
ageR::file_structure(entity = "X", am = "bacon")
#>                      levelName
#> 1 X                           
#> 2  ¦--Bacon_runs              
#> 3  ¦   °--X                   
#> 4  ¦       ¦--X_depths.txt    
#> 5  ¦       ¦--X_sample_ids.csv
#> 6  ¦       °--X.csv           
#> 7  ¦--hiatus.csv              
#> 8  °--not_used_dates.csv
```

The function `ageR::create_input` can be used to automatically create
the appropriate file structure (as shown previously).

#### Create input structure

1.  Start by creating a data frame for the sampling depths, this should
    have two numeric columns called `id` and `depth`:

<!-- end list -->

``` r
sample_depths <- data.frame(id = 1:100,
                            depth = seq(0, 500, length.out = 100))
knitr::kable(head(sample_depths))
```

| id |     depth |
| -: | --------: |
|  1 |  0.000000 |
|  2 |  5.050505 |
|  3 | 10.101010 |
|  4 | 15.151515 |
|  5 | 20.202020 |
|  6 | 25.252525 |

2.  Create a data frame with the core’s data, this should have at least
    four columns. The first column can be of character type and the
    remaining must be numeric. The column names should be `labID`,
    `age`, `error`, and `depth`. Optionally, a fifth column, `cc`, can
    be included to manually select the calibration curve for each
    observation.

<!-- end list -->

``` r
core <- data.frame(labID = paste0("X", sprintf("%03d", 1:5)),
                   age = c(50, 200, 1150, 2060, 4050),
                   error = c(10, 15, 5, 1, 70),
                   depth = c(5, 100, 230, 300, 450),
                   cc = 1)
knitr::kable(core)
```

| labID |  age | error | depth | cc |
| :---- | ---: | ----: | ----: | -: |
| X001  |   50 |    10 |     5 |  1 |
| X002  |  200 |    15 |   100 |  1 |
| X003  | 1150 |     5 |   230 |  1 |
| X004  | 2060 |     1 |   300 |  1 |
| X005  | 4050 |    70 |   450 |  1 |

3.  Call the `ageR::create_input` function. The first parameter should
    be a list containing the previous data frames, which must be called
    `sample` and `core`. Next, a working directory (`wdir`), the
    entity’s name, and optionally the age model’s name (by default is
    Bacon).

<!-- end list -->

``` r
ageR::create_input(data = list(sample = sample_depths, core = core), 
                   wdir = "./", 
                   entity = "X",
                   am = "bacon")
```

#### Run bacon

``` r
ageR::runBacon(wdir = "./", entity = "X", postbomb = 0, cc = 0)
```
