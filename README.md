# CaribouBC

> Caribou Population Forecasting

[![Linux build
status](https://travis-ci.org/psolymos/CaribouBC.svg?branch=master)](https://travis-ci.org/psolymos/CaribouBC)
[![codecov](https://codecov.io/gh/psolymos/CaribouBC/branch/master/graph/badge.svg)](https://codecov.io/gh/psolymos/CaribouBC)

Try the [Shiny app](https://psolymos.shinyapps.io/CaribouBC/) or run
locally as `shiny::runGitHub("bcgov/CaribouBC", subdir =
"inst/shiny/matpen")`.

## Installation

Stable version:

``` r
remotes::install_github("bcgov/CaribouBC")
```

Development version is available as this fork:

``` r
remotes::install_github("psolymos/CaribouBC")
```

See user visible changes in the [NEWS](NEWS.md) file.

## Usage

``` r
library(CaribouBC)
#> Loading required package: popbio
#> CaribouBC 0.2.2   2019-10-17

## Predefined settings
(s1 <- caribou_settings("mat.pen"))
#> Caribou settings - pen type: mat.pen 
#> 
#>  - c.surv.wild   :0.163
#>  - c.surv.capt   :0.54
#>  - f.surv.wild   :0.853
#>  - f.surv.capt   :0.903
#>  - f.preg.wild   :0.92
#>  - f.preg.capt   :0.92
#>  - pen.cap       :35
#>  - pen.cost.setup:500
#>  - pen.cost.proj :80
#>  - pen.cost.maint:300
#>  - pen.cost.capt :250
#>  - pen.cost.pred :0
(s2 <- caribou_settings("pred.excl"))
#> Caribou settings - pen type: pred.excl 
#> 
#>  - c.surv.wild   :0.163
#>  - c.surv.capt   :0.72
#>  - f.surv.wild   :0.853
#>  - f.surv.capt   :0.95
#>  - f.preg.wild   :0.92
#>  - f.preg.capt   :0.92
#>  - pen.cap       :35
#>  - pen.cost.setup:1868
#>  - pen.cost.proj :80
#>  - pen.cost.maint:600
#>  - pen.cost.capt :200
#>  - pen.cost.pred :80

## Modifying predefined settings
caribou_settings("mat.pen", c.surv.capt=0.65, pen.cap=30)
#> Caribou settings - pen type: mat.pen 
#> 
#>  - c.surv.wild   :0.163
#>  - c.surv.capt   :0.65
#>  - f.surv.wild   :0.853
#>  - f.surv.capt   :0.903
#>  - f.preg.wild   :0.92
#>  - f.preg.capt   :0.92
#>  - pen.cap       :30
#>  - pen.cost.setup:500
#>  - pen.cost.proj :80
#>  - pen.cost.maint:300
#>  - pen.cost.capt :250
#>  - pen.cost.pred :0

## Forecast based on settings for 75% females penned
f1 <- caribou_forecast(s1, fpen.prop = 0.75)
f2 <- caribou_forecast(s2, fpen.prop = 0.75)

## Most important results summarized
summary(f1)
#> Caribou forecast - pen type: mat.pen 
#> 
#>  - tmax     :20
#>  - pop.start:100
#>  - fpen.prop:0.75
#> 
#>  - npens      :5
#>  - lam.pen    :1.02
#>  - lam.nopen  :0.914
#>  - Nend.nopen :17
#>  - Nend.pen   :163
#>  - Nend.diff  :146
#>  - Cost.total :56
#>  - Cost.percap:0.384
summary(f2)
#> Caribou forecast - pen type: pred.excl 
#> 
#>  - tmax     :20
#>  - pop.start:100
#>  - fpen.prop:0.75
#> 
#>  - npens      :16
#>  - lam.pen    :1.09
#>  - lam.nopen  :0.914
#>  - Nend.nopen :17
#>  - Nend.pen   :556
#>  - Nend.diff  :539
#>  - Cost.total :192
#>  - Cost.percap:0.356

## Plot the results
plot(f2)
lines(f1, col = 2)
legend("topleft", col = c(1,1,2), lty = c(2,1,1),
    legend = c("No pen", "Mat pen", "Pred excl"))
```

![](README-example-1.png)<!-- -->

``` r

## Find 'breakeven' proportion of females penned where lambda=1
(b1 <- caribou_breakeven(f1, lambda = 1))
#> [1] 0.5669914
(b2 <- caribou_breakeven(f2, lambda = 1))
#> [1] 0.3441178
f3 <- caribou_forecast(s1, fpen.prop = b1)
f4 <- caribou_forecast(s2, fpen.prop = b2)
## See that lines are truly flat
op <- par(mfrow = c(1, 2))
plot(f3, main = "Mat pen")
plot(f4, main = "Pred excl")
```

![](README-example-2.png)<!-- -->

``` r
par(op)

## Forecast using number of penned females
## - it can be an initial number
## - or a vector of individuals for subsequent years
caribou_forecast(s1, fpen.inds = 5)
#> Caribou forecast - pen type: mat.pen 
#> 
#>  - tmax     :20
#>  - pop.start:100
#>  - fpen.inds:5
caribou_forecast(s2, fpen.inds = c(5, 0, 4, 6))
#> Caribou forecast - pen type: pred.excl 
#> 
#>  - tmax     :20
#>  - pop.start:100
#>  - fpen.inds:5 0 4 6

## Create projection matrices
s <- caribou_settings()
caribou_matrix(s, wild=TRUE)  # wild
#>         [0,1) [1,2) [2,3) [3,Inf]
#> [0,1)   0.000 0.000 0.000 0.39238
#> [1,2)   0.163 0.000 0.000 0.00000
#> [2,3)   0.000 0.853 0.000 0.00000
#> [3,Inf] 0.000 0.000 0.853 0.85300
caribou_matrix(s, wild=FALSE) # captive
#>         [0,1) [1,2) [2,3) [3,Inf]
#> [0,1)    0.00 0.000 0.000 0.41538
#> [1,2)    0.54 0.000 0.000 0.00000
#> [2,3)    0.00 0.903 0.000 0.00000
#> [3,Inf]  0.00 0.000 0.903 0.90300

## Compare scenarios for captive breeding
## out.prop = 0: move only N[t]-in.max youngs
x0 <- caribou_breeding(s,
    tmax = 20,        # projection horizon
    in.inds = rep(10, 5),
    out.prop = 0.5)
x0
#> Caribou captive breeding:
#> 
#>  - tmax        :20
#>  - pop.start   :100
#>  - f.surv.trans:1
#>  - j.surv.trans:1
#>  - j.surv.red  :1
#> 
#>          capt   recip   wild
#> N      67.242 56.3459 16.539
#> lambda  1.004  0.9914  0.914
summary(x0)
#>    Years Nin Nout    Ncapt    Nrecip     Nwild
#> 0      0  10    0 10.00000 100.00000 100.00000
#> 1      1  10    0 23.18380  91.39560  91.39560
#> 2      2  10    1 36.33182  84.53156  83.53156
#> 3      3  10    2 49.22996  79.19717  76.34417
#> 4      4  10    3 61.70599  75.20883  69.77522
#> 5      5   0    3 65.04446  71.69185  63.77148
#> 6      6   0    4 64.97138  69.65794  58.28433
#> 7      7   0    4 64.68270  67.96027  53.26931
#> 8      8   0    4 64.67842  66.32655  48.68581
#> 9      9   0    4 64.94646  65.05482  44.49669
#> 10    10   0    4 65.17513  63.87539  40.66802
#> 11    11   0    4 65.32882  62.79033  37.16878
#> 12    12   0    4 65.46682  61.78874  33.97063
#> 13    13   0    4 65.64046  60.87521  31.04766
#> 14    14   0    4 65.83908  60.04108  28.37620
#> 15    15   0    4 66.04654  59.27907  25.93460
#> 16    16   0    4 66.25912  58.58247  23.70308
#> 17    17   0    4 66.48284  57.94576  21.66357
#> 18    18   0    4 66.72118  57.36382  19.79955
#> 19    19   0    4 66.97435  56.83197  18.09592
#> 20    20   0    4 67.24185  56.34588  16.53888

## out.prop = 1: move all youngs
x1 <- update(x0, out.prop = 1)
x1
#> Caribou captive breeding:
#> 
#>  - tmax        :20
#>  - pop.start   :100
#>  - f.surv.trans:1
#>  - j.surv.trans:1
#>  - j.surv.red  :1
#> 
#>           capt   recip   wild
#> N      17.8090 61.2520 16.539
#> lambda  0.9674  0.9494  0.914

## Visualize the 2 scenarios
op <- par(mfrow=c(1, 2))
plot(x0, main="out.prop = 0")
plot(x1, main="out.prop = 1")
```

![](README-example-3.png)<!-- -->

``` r
par(op)
```

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/CaribouBC/issues/).

## How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

    Copyright 2018 Province of British Columbia
    
    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
    
    http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

-----

*This project was created using the
[bcgovr](https://github.com/bcgov/bcgovr) package.*
