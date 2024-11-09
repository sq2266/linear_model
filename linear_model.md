Linear Model
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)

set.seed(1)
```

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb |>
  mutate(stars = review_scores_location / 2) |>
  rename(
    borough = neighbourhood_group, 
    neighbourhood = neighbourhood
  )|>
  filter(
    borough != "State Island"
  )|>
  select(price, stars, borough, neighbourhood, room_type)|>
  mutate(
    borough = fct_infreq(borough),
    room_type = fct_infreq(room_type)
  )
```

fit some models

``` r
fit = lm(price ~ stars, data = nyc_airbnb)

summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars, data = nyc_airbnb)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -143.8  -68.8  -32.8   26.2 9889.2 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -66.201     11.827  -5.597  2.2e-08 ***
    ## stars         43.992      2.502  17.584  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 183.3 on 30714 degrees of freedom
    ##   (10037 observations deleted due to missingness)
    ## Multiple R-squared:  0.009966,   Adjusted R-squared:  0.009934 
    ## F-statistic: 309.2 on 1 and 30714 DF,  p-value: < 2.2e-16

``` r
names(summary(fit))
```

    ##  [1] "call"          "terms"         "residuals"     "coefficients" 
    ##  [5] "aliased"       "sigma"         "df"            "r.squared"    
    ##  [9] "adj.r.squared" "fstatistic"    "cov.unscaled"  "na.action"

``` r
fit |>
   broom::tidy()|>
   select(term, estimate, p.value)
```

    ## # A tibble: 2 × 3
    ##   term        estimate  p.value
    ##   <chr>          <dbl>    <dbl>
    ## 1 (Intercept)    -66.2 2.20e- 8
    ## 2 stars           44.0 7.15e-69

``` r
fit = 
  lm(price~stars + borough, data = nyc_airbnb)

fit |>
  broom::tidy()|>
  select(term, estimate, p.value)|>
  mutate(
    term = str_replace(term,"borough", "Borough: ")
  )|>
  knitr::kable(digits = 3)
```

| term                   | estimate | p.value |
|:-----------------------|---------:|--------:|
| (Intercept)            |   20.818 |   0.086 |
| stars                  |   31.785 |   0.000 |
| Borough: Brooklyn      |  -49.782 |   0.000 |
| Borough: Queens        |  -77.075 |   0.000 |
| Borough: Bronx         |  -90.324 |   0.000 |
| Borough: Staten Island |  -76.628 |   0.000 |

``` r
nyc_airbnb|>
  ggplot(aes(x = stars, y = price))+
  geom_point()+
  stat_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 10037 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 10037 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](linear_model_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## some diagnostics

``` r
modelr::add_residuals(nyc_airbnb, fit)|>
  ggplot(aes(x = resid))+
           geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 10037 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](linear_model_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
modelr::add_residuals(nyc_airbnb, fit)|>
  ggplot(aes(x = borough, y = resid))+
  geom_violin()+
  ylim(-100, 100)
```

    ## Warning: Removed 14869 rows containing non-finite outside the scale range
    ## (`stat_ydensity()`).

![](linear_model_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Residual against stars

``` r
modelr::add_residuals(nyc_airbnb, fit)|>
  ggplot(aes(x = stars, y = resid))+
  geom_point()
```

    ## Warning: Removed 10037 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](linear_model_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
nyc_airbnb |>
  modelr::add_residuals(fit) |>
  modelr::add_predictions(fit)|>
  ggplot(aes(x = pred, y = resid))+
  geom_point()
```

    ## Warning: Removed 10037 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](linear_model_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Hypothesis testing

``` r
fit |>
  broom::tidy()
```

    ## # A tibble: 6 × 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)              20.8     12.1       1.72 8.59e-  2
    ## 2 stars                    31.8      2.51     12.6  1.45e- 36
    ## 3 boroughBrooklyn         -49.8      2.23    -22.3  1.31e-109
    ## 4 boroughQueens           -77.1      3.72    -20.7  7.47e- 95
    ## 5 boroughBronx            -90.3      8.55    -10.6  4.57e- 26
    ## 6 boroughStaten Island    -76.6     13.4      -5.73 9.96e-  9

``` r
fit_null = lm(price ~ stars + borough, data = nyc_airbnb)
fit_alt = lm(price ~ stars + borough + room_type, data = nyc_airbnb)
```

look at both

``` r
fit_null |>
  broom::tidy()
```

    ## # A tibble: 6 × 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)              20.8     12.1       1.72 8.59e-  2
    ## 2 stars                    31.8      2.51     12.6  1.45e- 36
    ## 3 boroughBrooklyn         -49.8      2.23    -22.3  1.31e-109
    ## 4 boroughQueens           -77.1      3.72    -20.7  7.47e- 95
    ## 5 boroughBronx            -90.3      8.55    -10.6  4.57e- 26
    ## 6 boroughStaten Island    -76.6     13.4      -5.73 9.96e-  9

``` r
fit_alt |>
  broom::tidy()
```

    ## # A tibble: 8 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              113.      11.7       9.65 5.25e-22
    ## 2 stars                     21.7      2.41      9.00 2.39e-19
    ## 3 boroughBrooklyn          -40.4      2.14    -18.8  1.24e-78
    ## 4 boroughQueens            -55.5      3.58    -15.5  5.46e-54
    ## 5 boroughBronx             -63.1      8.20     -7.70 1.36e-14
    ## 6 boroughStaten Island     -62.8     12.8      -4.90 9.44e- 7
    ## 7 room_typePrivate room   -105.       2.04    -51.4  0       
    ## 8 room_typeShared room    -129.       6.13    -21.1  6.94e-98

``` r
anova(fit_null, fit_alt)|>
  broom::tidy()
```

    ## # A tibble: 2 × 7
    ##   term                        df.residual    rss    df   sumsq statistic p.value
    ##   <chr>                             <dbl>  <dbl> <dbl>   <dbl>     <dbl>   <dbl>
    ## 1 price ~ stars + borough           30710 1.01e9    NA NA            NA       NA
    ## 2 price ~ stars + borough + …       30708 9.22e8     2  8.44e7     1405.       0

``` r
nyc_airbnb |>
  lm(price ~ stars*borough + room_type*borough, data = _) |>
  broom::tidy()
```

    ## # A tibble: 20 × 5
    ##    term                                    estimate std.error statistic  p.value
    ##    <chr>                                      <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)                                95.7      19.1     5.00   5.73e- 7
    ##  2 stars                                      27.1       3.95    6.86   7.24e-12
    ##  3 boroughBrooklyn                           -26.1      25.0    -1.04   2.97e- 1
    ##  4 boroughQueens                              -4.12     40.6    -0.102  9.19e- 1
    ##  5 boroughBronx                               -5.63     77.6    -0.0725 9.42e- 1
    ##  6 boroughStaten Island                       64.8     146.      0.444  6.57e- 1
    ##  7 room_typePrivate room                    -124.        2.99  -41.6    0       
    ##  8 room_typeShared room                     -154.        8.67  -17.7    6.29e-70
    ##  9 stars:boroughBrooklyn                      -6.14      5.22   -1.18   2.40e- 1
    ## 10 stars:boroughQueens                       -17.5       8.52   -2.05   4.04e- 2
    ## 11 stars:boroughBronx                        -22.7      17.1    -1.33   1.84e- 1
    ## 12 stars:boroughStaten Island                -32.7      31.2    -1.05   2.93e- 1
    ## 13 boroughBrooklyn:room_typePrivate room      32.0       4.32    7.41   1.34e-13
    ## 14 boroughQueens:room_typePrivate room        54.9       7.44    7.38   1.57e-13
    ## 15 boroughBronx:room_typePrivate room         71.3      18.0     3.97   7.22e- 5
    ## 16 boroughStaten Island:room_typePrivate …    48.2      25.9     1.87   6.22e- 2
    ## 17 boroughBrooklyn:room_typeShared room       47.8      13.9     3.45   5.64e- 4
    ## 18 boroughQueens:room_typeShared room         58.7      17.9     3.29   1.02e- 3
    ## 19 boroughBronx:room_typeShared room          83.1      42.3     1.96   4.97e- 2
    ## 20 boroughStaten Island:room_typeShared r…    43.5     174.      0.250  8.03e- 1

seperate models

``` r
nyc_airbnb |>
  filter(borough == "Manhattan") |>
  lm(price ~ stars + room_type, data = _)|>
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)               95.7     22.2       4.31 1.62e-  5
    ## 2 stars                     27.1      4.59      5.91 3.45e-  9
    ## 3 room_typePrivate room   -124.       3.46    -35.8  9.40e-270
    ## 4 room_typeShared room    -154.      10.1     -15.3  2.47e- 52

``` r
nyc_airbnb |>
  filter(borough == "Brooklyn") |>
  lm(price ~ stars + room_type, data = _)|>
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)               69.6     14.0       4.96 7.27e-  7
    ## 2 stars                     21.0      2.98      7.05 1.90e- 12
    ## 3 room_typePrivate room    -92.2      2.72    -34.0  6.40e-242
    ## 4 room_typeShared room    -106.       9.43    -11.2  4.15e- 29

use list columns

``` r
nyc_airbnb |>
  nest(data = -borough)|>
  mutate(
    model = map(data, \(x) lm(price ~ stars + room_type, data = x)),
    results = map(model, broom::tidy)
  )|>
  select(borough, results)|>
  unnest(results)|>
  select(borough, term, estimate) |>
  pivot_wider(
    names_from = term, 
    values_from = estimate
  )
```

    ## # A tibble: 5 × 5
    ##   borough      `(Intercept)` stars room_typePrivate roo…¹ `room_typeShared room`
    ##   <fct>                <dbl> <dbl>                  <dbl>                  <dbl>
    ## 1 Bronx                 90.1  4.45                  -52.9                  -70.5
    ## 2 Queens                91.6  9.65                  -69.3                  -95.0
    ## 3 Brooklyn              69.6 21.0                   -92.2                 -106. 
    ## 4 Staten Isla…         160.  -5.64                  -75.9                 -110. 
    ## 5 Manhattan             95.7 27.1                  -124.                  -154. 
    ## # ℹ abbreviated name: ¹​`room_typePrivate room`

## room type across manhattan neighourhood

``` r
nyc_airbnb |>
  filter(borough == "Manhattan",
  neighbourhood == "Chinatown") |>
  lm(price ~ stars + room_type, data = _)|>
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              337.       60.4      5.59 5.66e- 8
    ## 2 stars                    -27.8      13.0     -2.14 3.34e- 2
    ## 3 room_typePrivate room   -109.       11.5     -9.52 1.03e-18
    ## 4 room_typeShared room    -143.       93.2     -1.54 1.25e- 1

``` r
nyc_airbnb |>
  filter(borough == "Manhattan")|>
  nest(data = -(borough:neighbourhood))|>
  mutate(
    model = map(data, \(x) lm(price ~ stars + room_type, data = x)),
    results = map(model, broom::tidy)
  )|>
  select(borough, results)|>
  unnest(results)|>
  select(borough, term, estimate)|>
  pivot_wider(
    names_from = term,
    values_from = estimate
  )
```

    ## Warning: Values from `estimate` are not uniquely identified; output will contain
    ## list-cols.
    ## • Use `values_fn = list` to suppress this warning.
    ## • Use `values_fn = {summary_fun}` to summarise duplicates.
    ## • Use the following dplyr code to identify duplicates.
    ##   {data} |>
    ##   dplyr::summarise(n = dplyr::n(), .by = c(borough, term)) |>
    ##   dplyr::filter(n > 1L)

    ## # A tibble: 1 × 5
    ##   borough   `(Intercept)` stars  `room_typePrivate room` `room_typeShared room`
    ##   <fct>     <list>        <list> <list>                  <list>                
    ## 1 Manhattan <dbl [32]>    <dbl>  <dbl [32]>              <dbl [27]>

``` r
nyc_airbnb |>
  nest(data = -borough)|>
  mutate(
    model = map(data, \(x) lm(price ~ stars + room_type, data = x)),
    results = map(model, broom::tidy)
  )|>
  select(borough, results)|>
  unnest(results)|>
  select(borough, term, estimate)|>
  pivot_wider(
    names_from = term,
    values_from = estimate
  )
```

    ## # A tibble: 5 × 5
    ##   borough      `(Intercept)` stars room_typePrivate roo…¹ `room_typeShared room`
    ##   <fct>                <dbl> <dbl>                  <dbl>                  <dbl>
    ## 1 Bronx                 90.1  4.45                  -52.9                  -70.5
    ## 2 Queens                91.6  9.65                  -69.3                  -95.0
    ## 3 Brooklyn              69.6 21.0                   -92.2                 -106. 
    ## 4 Staten Isla…         160.  -5.64                  -75.9                 -110. 
    ## 5 Manhattan             95.7 27.1                  -124.                  -154. 
    ## # ℹ abbreviated name: ¹​`room_typePrivate room`

``` r
nyc_airbnb |>
  filter(
    borough == "Manhattan",
    neighbourhood == "Chinatown")|>
  lm(price ~ stars + room_type, data = _)|>
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              337.       60.4      5.59 5.66e- 8
    ## 2 stars                    -27.8      13.0     -2.14 3.34e- 2
    ## 3 room_typePrivate room   -109.       11.5     -9.52 1.03e-18
    ## 4 room_typeShared room    -143.       93.2     -1.54 1.25e- 1

``` r
nyc_airbnb |>
  filter(
    borough == "Manhattan",
    neighbourhood == "Chelsea")|>
  lm(price ~ stars + room_type, data = _)|>
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              477.      135.       3.53 4.38e- 4
    ## 2 stars                    -44.5      27.3     -1.63 1.04e- 1
    ## 3 room_typePrivate room   -133.       12.7    -10.5  2.13e-24
    ## 4 room_typeShared room    -153.       36.2     -4.24 2.49e- 5

``` r
manhanttan_neighbourhood_fit_df = 
nyc_airbnb |>
  filter(borough == "Manhattan")|>
  nest(data = -(borough:neighbourhood))|>
  mutate(
    model = map(data, \(x) lm(price ~ stars + room_type, data = x)),
    resutls = map(model, broom::tidy)
  )|>
  select(neighbourhood, results)|>
  unnest(results)
```

    ## Error in `select()`:
    ## ! Can't select columns that don't exist.
    ## ✖ Column `results` doesn't exist.

``` r
manhanttan_neighbourhood_fit_df |>
  filter(str_detect(term, "room_type"))|>
  ggplot(aes(x = term, y = estimate))+
  geom_boxplot()
```

    ## Error in eval(expr, envir, enclos): object 'manhanttan_neighbourhood_fit_df' not found

``` r
manhanttan_neighbourhood_fit_df |>
  filter(str_detect(term, "room_type"))|>
  filter(estimate >0)
```

    ## Error in eval(expr, envir, enclos): object 'manhanttan_neighbourhood_fit_df' not found

``` r
nyc_airbnb |>
  filter(
    neighbourhood == "Noho",
    room_type == "Shared room"
  )
```

    ## # A tibble: 0 × 5
    ## # ℹ 5 variables: price <dbl>, stars <dbl>, borough <fct>, neighbourhood <chr>,
    ## #   room_type <fct>
