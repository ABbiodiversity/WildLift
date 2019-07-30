# CaribouBC

> Caribou Population Forecasting

[![Linux build
status](https://travis-ci.org/psolymos/CaribouBC.svg?branch=master)](https://travis-ci.org/psolymos/CaribouBC)
[![codecov](https://codecov.io/gh/psolymos/CaribouBC/branch/master/graph/badge.svg)](https://codecov.io/gh/psolymos/CaribouBC)

Try the [Shiny app](https://psolymos.shinyapps.io/matpen/) or run
locally as `shiny::runGitHub("bcgov/CaribouBC", subdir =
"inst/shiny/matpen")`.

![](caribou.gif)

## Installation

Stable version:

``` r
devtools::install_github("bcgov/CaribouBC")
```

Development version is available as this fork:

``` r
devtools::install_github("psolymos/CaribouBC")
```

## Usage

``` r
library(CaribouBC)
#> Loading required package: popbio
#> CaribouBC 0.1.1   2019-07-28

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
