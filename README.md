# RISQ

RISQ is a package for the estimation of Representativity Indicators for Survey Quality.

### Installation

To install RISQ, use the following R command.

```
install.packages("risq")
```

### Usage

Below is an example of how to estimate indicators.

``` r
library(risq)

# Construct RISQ object.
risq_hlc <- risq(predictor = ~ gender + age, data = hlc)

# Estimate R-indicator.
ri(risq_hlc, "response_1")
#> $value
#> [1] 0.9450971
#> 
#> $se
#> [1] 0.005386833

# Estimate coefficient of variation.
cv(risq_hlc, "response_1")
#> $value
#> [1] 0.05022405
#> 
#> $se
#> [1] 0.00492778
```

### More Information

For more information, read the vignettes.

```
browseVignettes("risq")
```
