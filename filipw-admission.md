What explains district heating consumption?
================
Filip Wästberg
2020-09-14

## Using Generalized Additive Models to predict District Heating consumption

The strength of the model that underlies K2 is its simplicity. The K2
algorithm is simply put four separate regression lines with predefined
breaks. We can see the fit of the model below:

``` r
knitr::include_graphics("img/k2-fit.png")
```

<img src="img/k2-fit.png" width="1800" />

Any observation that is *too far* from the regression line will generate
an alarm. The interpreter, usually an energy analyst at a District
Heating company, can easily see what’s going on and why the alarms are
generated. This technique is called piecewise or segmented regression
and used to be a quite popular technique for modelling non linear data,
because it is relatively easy to calculate and thus fast. But with a
modern computer we are no longer restricted to simple methods, even
though performance is a key when dealing with district heating data,
which usually involves a lot of data.

Furthermore, the model in K2 is not perfect. Firstly, we are trying to
fit a linear model to non linear data. Secondly, the predefined breaks
means we will not get the best fit of the data, because the optimal
breaks might vary.

In this admission I will explore a modelling framework called
Generalized Additive Models. It is a popular modelling technique for non
linear data, but one of its major upsides is that it still keeps the
interpretability that we love in linear models. GAMs are often described
as a middle way between simple linear models and black box models such
as gradient boosting, random forest and neural networks. Even though
interpretability is not a request in this competition I will take this
into account anyway.

## What is a Generlized Additive Model?

To explain what a GAM is it is easiest to compare it to a linear model.
A linear regression where we model
![y](https://latex.codecogs.com/png.latex?y "y") (district heating
consumption) as a result of ![x](https://latex.codecogs.com/png.latex?x
"x") (temperature) is defined as line with an intercept
![\\beta\_0](https://latex.codecogs.com/png.latex?%5Cbeta_0 "\\beta_0"),
the coefficient
![\\beta\_1](https://latex.codecogs.com/png.latex?%5Cbeta_1 "\\beta_1")
which tells us how steep the slope is. Lastly we have the error term,
which is all the variation that our coefficients cannot explain.   
![ consumption = \\beta\_0 + \\beta\_1\*temperature +
\\epsilon](https://latex.codecogs.com/png.latex?%20consumption%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_1%2Atemperature%20%2B%20%5Cepsilon
" consumption = \\beta_0 + \\beta_1*temperature + \\epsilon")  
The K2 algorithm can be defined as a linear regression with a separate
coefficient for each temperature interval.

  
![ consumption = \\beta\_1\*temp\_1 + \\beta\_2\*temp\_2 +
\\beta\_3\*temp\_3 + \\beta\_4\*temp\_4 +
\\epsilon](https://latex.codecogs.com/png.latex?%20consumption%20%3D%20%5Cbeta_1%2Atemp_1%20%2B%20%5Cbeta_2%2Atemp_2%20%2B%20%5Cbeta_3%2Atemp_3%20%2B%20%5Cbeta_4%2Atemp_4%20%2B%20%5Cepsilon
" consumption = \\beta_1*temp_1 + \\beta_2*temp_2 + \\beta_3*temp_3 + \\beta_4*temp_4 + \\epsilon")  

A generalized additive model takes a different approach and defines the
relationship as a *function* of temperature:

  
![consumption = f(x) +
\\epsilon](https://latex.codecogs.com/png.latex?consumption%20%3D%20f%28x%29%20%2B%20%5Cepsilon
"consumption = f(x) + \\epsilon")  

The function may be a linear regression, but when data is non linear,
this is not sufficient. Instead a generalized additive models tries to
solve this problem with a smooth spline going through the data. The
spline is constructed by many functions and in the case of GAM these are
basis functions. The best way is to illustrate this by an example. Below
we can see in `a` a number of basis functions with the same
coefficients, and to the left `b` where the basis functions have
different coefficients so that they fit the data.

``` r
include_graphics("img/gam.png")
```

<div class="figure">

<img src="img/gam.png" alt="Source: https://github.com/noamross/gam-resources/blob/master/2017-11-14-noamross-gams-nyhackr.pdf" width="1910" />

<p class="caption">

Source:
<https://github.com/noamross/gam-resources/blob/master/2017-11-14-noamross-gams-nyhackr.pdf>

</p>

</div>

Now, in theory you could use as many functions as observations, but that
would lead to a very wiggly spline. Instead we use a penalty that
adjusts the amount of “wigglyness” of the spline. Setting this parameter
is crucial, however, we do not need to do this manually, instead we use
the `mgcv` package in R to do this for us.

## Exploring the data

Let’s start by exploring the data and fitting a GAM to one station. We
start by listing the files and write a custom function for importing
them.

``` r
library(tidyverse)

filer <- tibble(filer = list.files("data")) %>% 
  filter(str_detect(filer, "Station")) %>% 
  .$filer

## Here we create a function
read_sm_file = function(path){
  read_csv(paste0("data/",path)) %>% 
    mutate(station = str_replace(path, ".csv", ""))
}
```

The function `map_df()` takes an input (in this case our files) and
applies a function (our customized function) to the input. The function
expects the files to be data.frames, and it binds the data.frames
together when they all have been read.

``` r
all_data <- map_df(.x = filer, read_sm_file) %>% 
  janitor::clean_names()
```

We can now view the data:

``` r
all_data
```

    ## # A tibble: 14,610 x 10
    ##    datum      konsumtion temperatur nederbordsmangd vindriktning vindhastighet
    ##    <date>          <dbl>      <dbl>           <dbl>        <dbl>         <dbl>
    ##  1 2013-01-01       101.        3.9             7.6         249.          3.53
    ##  2 2013-01-02       107.        6.4             0           266.          7.19
    ##  3 2013-01-03       102.        6               0.4         302.          5.83
    ##  4 2013-01-04       108.        2.4             1.2         145.          1.14
    ##  5 2013-01-05       105.        4.4             1.9         197.          1.83
    ##  6 2013-01-06       105.        2.1             5.5         151.          2.47
    ##  7 2013-01-07       112.        4.2             0           227.          2.01
    ##  8 2013-01-08       114.        4.5            14.4         204.          3.10
    ##  9 2013-01-09       109.       -0.1             0           123.          5.88
    ## 10 2013-01-10       121.       -1.7             0.2         168.          5.25
    ## # … with 14,600 more rows, and 4 more variables: lufttryck <dbl>,
    ## #   daggpunktstemperatur <dbl>, k2_regression <dbl>, station <chr>

## Exploratory Data Analysis

If we plot the relationship between temperature and consumption we can
clearly see that there is a relationship and that it is not linear.

``` r
plot_konsumtion <- ggplot(all_data, aes(temperatur, konsumtion, color = station)) +
  geom_point(size = .5, alpha = .2) +
  facet_wrap(~ station) +
  labs(
    title = "Distric Heating Consumption over time"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

plot_konsumtion
```

![](filipw-admission_files/figure-gfm/plot_temp-1.png)<!-- -->

Let’s start with a randomly chosen station. If we fit a GAM to this data
we get one smooth line instead of 4 separate lines we know from K2.

``` r
## The station was randomly chosen
station8 <- filter(all_data, station == "Station_8")

station8 %>% 
  ggplot(aes(x = temperatur, y = konsumtion)) +
  geom_point(color = "lightgreen", alpha = .5) +
  labs(
    title = "Station 8, konsumtion ~ temperatur"
  ) +
  theme_minimal() +
  geom_smooth(method = "gam")
```

![](filipw-admission_files/figure-gfm/plot_gam-1.png)<!-- -->

We can specify the model into a model object and run a summary on it. We
see that with this model we can explain around 90% of the variation.

``` r
library(mgcv)

gam_fit <- gam(konsumtion ~ s(temperatur, bs = "cs"),
               data = station8,
               method = "REML",
               family = scat)

summary(gam_fit)
```

    ## 
    ## Family: Scaled t(4.981,8.271) 
    ## Link function: identity 
    ## 
    ## Formula:
    ## konsumtion ~ s(temperatur, bs = "cs")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    56.57       0.25   226.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                 edf Ref.df Chi.sq p-value    
    ## s(temperatur) 7.118      9  16282  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.902   Deviance explained = 81.4%
    ## -REML = 5480.4  Scale est. = 1         n = 1461

We can do some model diagnostics to make sure that the residuals are
normally distributed.

``` r
library(gratia)
appraise(gam_fit)
```

![](filipw-admission_files/figure-gfm/model_diag_1-1.png)<!-- -->

The residuals seem to be normally distributed but since we are dealing
with temporal data we are also interested in how the residuals
distribute over time. How does the residuals distribute over time?

``` r
station8 %>% 
  add_residuals(gam_fit) %>% 
  ggplot(aes(datum, .residual)) +
  geom_point()
```

![](filipw-admission_files/figure-gfm/resid_time-1.png)<!-- -->

This is actually a problem. We’d expect the residuals to be equally
distributed over time but we can see that they are not. This is usually
an indication that the observations are not independent. That is,
today’s consumption is not independent to yesterday’s. That makes
sense intuitively but independence is usually an assumption many models
have.

In statistical terms this is called autocorrelation and we can calculate
this with the `partial autocorrelation function`(PACF). The plot below
illustrates that there is a strong correlation between today’s
consumption and yesterday’s.

``` r
library(feasts)
library(tsibble)
ts <- station8 %>% 
  select(datum, konsumtion) %>% 
  as_tsibble()

ts %>% 
  PACF() %>% 
  autoplot() +
  scale_x_continuous(breaks = scales::pretty_breaks(20))
```

![](filipw-admission_files/figure-gfm/pacf-1.png)<!-- -->

There are a couple of ways we can model this. Either we can just use the
lag of consumption as an explanatory variable in our model or we can try
to model this with a mixed model. For this problem we’ll just use the
lagged variable since it’s a bit more straight forward to calculate.

Now, we have a couple of other variables in our dataset that we also
would like to include in the model. We are confident that temperature is
the main driver here. However, temperature may interact with our other
variables, such as wind speed, amount of rain/snow and air pressure.
However, dew point temperature has a strong correlation to temperature
and is thus excluded.

``` r
library(corrplot)
all_data %>%
  select_if(is.numeric) %>% 
  select(-k2_regression) %>% 
  as.matrix() %>% 
  cor() %>% 
  corrplot()
```

![](filipw-admission_files/figure-gfm/corrplot-1.png)<!-- -->

One more thing that might affect our model is the seasonality of the
model.

``` r
gg_season(ts)
```

![](filipw-admission_files/figure-gfm/season-1.png)<!-- -->

The monthly seasonality may imply that temperature has a different
effect depending on what month it is. That is, that a temperature of 15
degrees in September may have a different effect than 15 degrees in
March.

Furthermore, we know that weekly seasonality is a common issue in
district heating. Different days in the week may have different
consumption patterns.

``` r
all_data %>% 
  mutate(weekday = weekdays(datum)) %>% 
  group_by(weekday, station) %>% 
  summarise(median_cons = median(konsumtion)) %>% 
  ggplot(aes(y = weekday, x = median_cons)) +
  geom_col() +
  facet_wrap(~station)
```

![](filipw-admission_files/figure-gfm/weekday-1.png)<!-- --> These
metering stations does not have a strong variation between weekdays so
we’ll move on with only mothly seasonality, which we can fit by
including a seasonal component using `s(x, bs = "cc")`.

``` r
station8 <- station8 %>% 
  mutate(month = lubridate::month(datum),
         lag_konsumtion = lag(konsumtion),
         lag_temperatur = lag(temperatur),
         weekday = as.factor(weekdays(datum)))

gam_te <- gam(konsumtion ~ 
                s(temperatur, lag_temperatur) +
                s(nederbordsmangd) +
                s(lufttryck) +
                s(lag_konsumtion) + 
                te(temperatur, vindhastighet) +
                s(month, bs = "cc"),
              data = station8,  method = "REML")

summary(gam_te)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## konsumtion ~ s(temperatur, lag_temperatur) + s(nederbordsmangd) + 
    ##     s(lufttryck) + s(lag_konsumtion) + te(temperatur, vindhastighet) + 
    ##     s(month, bs = "cc")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  56.6653     0.1068   530.6   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                                 edf Ref.df       F p-value    
    ## s(temperatur,lag_temperatur) 17.693 22.612  43.084  <2e-16 ***
    ## s(nederbordsmangd)            1.000  1.001   5.545  0.0187 *  
    ## s(lufttryck)                  1.001  1.002   0.327  0.5680    
    ## s(lag_konsumtion)             5.779  6.995 339.419  <2e-16 ***
    ## te(temperatur,vindhastighet)  2.003  2.006   3.246  0.0394 *  
    ## s(month)                      4.797  8.000   8.333  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.985   Deviance explained = 98.5%
    ## -REML =   4151  Scale est. = 16.649    n = 1460

This result is interesting since it suggests that extra weather
variables does not have a significant effect on consumption, for these
metering stations.

Let’s have a look at the model diagnostics.

``` r
appraise(gam_te)
```

![](filipw-admission_files/figure-gfm/model_diag_all_vars-1.png)<!-- -->

If we again have a look at the residuals over time we can see that they
are now distributed evenly over time and we have thus redeemded the
problem with autocorrelation.

``` r
station8 %>% 
  filter(!is.na(lag_konsumtion)) %>% 
  add_residuals(gam_te) %>% 
  ggplot(aes(datum, .residual)) +
  geom_point()
```

![](filipw-admission_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

We can fit the model to our data and see how it behaves:

``` r
station8 <- station8 %>% 
  filter(!is.na(lag_konsumtion)) %>% 
  mutate(gam_fitted = fitted.values(gam_te))

station8 %>% 
  ggplot(aes(x = temperatur, y = konsumtion)) +
  geom_point(color = "lightgreen", alpha = .5) +
  labs(
    title = "Station 8, konsumtion ~ temperatur",
    subtitle = "GAM model fitted "
  ) +
  theme_minimal() +
  geom_line(aes(x = temperatur, y = gam_fitted))
```

![](filipw-admission_files/figure-gfm/plot_fit-1.png)<!-- -->

However, our goal here is to predict consumption. In order to evaluate
our model we split the data in train and validation data for every
station, fit the GAM to the training data and evaluate it on the
validation data.

We can start by nesting the data by station.

``` r
nested_data <- all_data %>% 
  group_by(station) %>% 
  mutate(month = lubridate::month(datum),
         lag_konsumtion = lag(konsumtion),
         lag_temperatur = lag(temperatur),
         weekday_chr = weekdays(datum),
         weekday = lubridate::wday(datum)) %>% 
  nest()

nested_data
```

    ## # A tibble: 10 x 2
    ## # Groups:   station [10]
    ##    station    data                 
    ##    <chr>      <list>               
    ##  1 Station_1  <tibble [1,461 × 14]>
    ##  2 Station_10 <tibble [1,461 × 14]>
    ##  3 Station_2  <tibble [1,461 × 14]>
    ##  4 Station_3  <tibble [1,461 × 14]>
    ##  5 Station_4  <tibble [1,461 × 14]>
    ##  6 Station_5  <tibble [1,461 × 14]>
    ##  7 Station_6  <tibble [1,461 × 14]>
    ##  8 Station_7  <tibble [1,461 × 14]>
    ##  9 Station_8  <tibble [1,461 × 14]>
    ## 10 Station_9  <tibble [1,461 × 14]>

Now, let’s create a random split where we use the large proportion of
data to train the model and a smaller amount of the data to test the
model. To investigate if our model performs better with the additional
variables besides temperature, we also fit a really simple GAM.

``` r
library(tidymodels)
nested_data <- nested_data %>% 
  mutate(split = map(data, initial_split),
         train = map(split, training),
         test = map(split, testing))

nested_data <- nested_data %>% 
  mutate(gam_fit_simple = map(train, ~gam(konsumtion ~ s(temperatur), data = .x, method = "REML")),
           gam_fit = map(train, ~gam(konsumtion ~ te(temperatur, lag_temperatur) + s(lag_konsumtion) + s(month, bs = "cc"), data = .x,  method = "REML")))

pred <- nested_data %>% 
  mutate(pred_simple = map2(gam_fit_simple, test, predict),
         pred_comp = map2(gam_fit, test, predict)) %>% 
  select(station, pred_simple, pred_comp, test) %>% 
  unnest(pred_simple, pred_comp, test)
```

The result shows that the inclusion of lagged variables brings out more
performance in the model.

``` r
library(yardstick)
bind_rows(
rmse(ungroup(pred), konsumtion, pred_simple) %>% mutate(model = "pred_simple"),
rmse(ungroup(pred), konsumtion, pred_comp) %>% mutate(model = "pred_complex")
)
```

    ## # A tibble: 2 x 4
    ##   .metric .estimator .estimate model       
    ##   <chr>   <chr>          <dbl> <chr>       
    ## 1 rmse    standard       12.6  pred_simple 
    ## 2 rmse    standard        6.09 pred_complex

The last thing we do is to fit the model to all the data and predict
consumption for each station on the validation data. However, the
validation data is future timestamps and thus do not have consumption,
therefore we’ll omit the lagged consumption from the model.

``` r
read_val <- function(path){
  read_csv(path) %>% 
    janitor::clean_names() %>% 
    mutate(month = lubridate::month(datum),
         lag_temperatur = lag(temperatur))
}

read_validation("data/Väder_valideringsperiod.csv") 

result <- nested_data %>% 
  mutate(final_model = map(data, ~gam(konsumtion ~ te(temperatur, lag_temperatur) + s(month, bs = "cc"), data = .x,  method = "REML")),
         validation_data = map("data/Väder_valideringsperiod.csv", read_val),
         pred = map2(.x = final_model, .y = validation_data, predict)) %>% 
  select(station, pred, validation_data) %>% 
  unnest(pred, validation_data)
```

The last thing we do is to write the data to a csv-file and wait for the
result…

``` r
write_csv(result, "data/result-tavling.csv")
```
