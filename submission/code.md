

``` r
options(prompt = 'R> ', continue = '+ ')

## .bordered {
##   border: solid;
## }

# ?ztpoisson

# install.packages("singleRcapture")

library(singleRcapture)

data(netherlandsimmigrant)
head(netherlandsimmigrant)
```

```
##   capture gender    age       reason       nation
## 1       1   male <40yrs Other reason North Africa
## 2       1   male <40yrs Other reason North Africa
## 3       1   male <40yrs Other reason North Africa
## 4       1   male <40yrs Other reason         Asia
## 5       1   male <40yrs Other reason         Asia
## 6       2   male <40yrs Other reason North Africa
```

``` r
summary(netherlandsimmigrant)
```

```
##     capture         gender         age                reason                        nation    
##  Min.   :1.000   female: 398   <40yrs:1769   Illegal stay: 259   American and Australia: 173  
##  1st Qu.:1.000   male  :1482   >40yrs: 111   Other reason:1621   Asia                  : 284  
##  Median :1.000                                                   North Africa          :1023  
##  Mean   :1.162                                                   Rest of Africa        : 243  
##  3rd Qu.:1.000                                                   Surinam               :  64  
##  Max.   :6.000                                                   Turkey                :  93
```

``` r
table(netherlandsimmigrant$capture)
```

```
## 
##    1    2    3    4    5    6 
## 1645  183   37   13    1    1
```

``` r
basicModel <- estimatePopsize(
  formula = capture ~ gender + age + nation,
  model   = ztpoisson(),
  data    = netherlandsimmigrant,
  controlMethod = controlMethod(silent = TRUE)
)
summary(basicModel)
```

```
## 
## Call:
## estimatePopsize.default(formula = capture ~ gender + age + nation, 
##     data = netherlandsimmigrant, model = ztpoisson(), controlMethod = controlMethod(silent = TRUE))
## 
## Pearson Residuals:
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## -0.486442 -0.486442 -0.298080  0.002093 -0.209444 13.910844 
## 
## Coefficients:
## -----------------------
## For linear predictors associated with: lambda 
##                      Estimate Std. Error z value  P(>|z|)    
## (Intercept)           -1.3411     0.2149  -6.241 4.35e-10 ***
## gendermale             0.3972     0.1630   2.436 0.014832 *  
## age>40yrs             -0.9746     0.4082  -2.387 0.016972 *  
## nationAsia            -1.0926     0.3016  -3.622 0.000292 ***
## nationNorth Africa     0.1900     0.1940   0.979 0.327398    
## nationRest of Africa  -0.9106     0.3008  -3.027 0.002468 ** 
## nationSurinam         -2.3364     1.0136  -2.305 0.021159 *  
## nationTurkey          -1.6754     0.6028  -2.779 0.005445 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## AIC: 1712.901
## BIC: 1757.213
## Residual deviance: 1128.553
## 
## Log-likelihood: -848.4504 on 1872 Degrees of freedom 
## Number of iterations: 8
## -----------------------
## Population size estimation results: 
## Point estimate 12690.35
## Observed proportion: 14.8% (N obs = 1880)
## Std. Error 2808.165
## 95% CI for the population size:
##           lowerBound upperBound
## normal      7186.449   18194.25
## logNormal   8431.277   19718.31
## 95% CI for the share of observed population:
##           lowerBound upperBound
## normal     10.332933   26.16035
## logNormal   9.534288   22.29793
```

``` r
set.seed(123456)
modelInflated <- estimatePopsize(
    formula = capture ~ nation,
    model   = oiztgeom(omegaLink = "cloglog"),
    data    = netherlandsimmigrant,
    controlModel = controlModel(
        omegaFormula = ~ gender + age
    ),
    popVar = "bootstrap",
    controlPopVar = controlPopVar(bootType = "semiparametric")
)
```

```
## Warning in estimatePopsize.default(formula = capture ~ nation, model = oiztgeom(omegaLink = "cloglog"), : The (analytically computed) hessian of the score function is not negative define.
## NOTE: Second derivative test failing does not 
##         necessarily mean that the maximum of score function that was found 
##         numericaly is invalid since R^k is not a bounded space.
## Additionally in one inflated and hurdle models second derivative test often fails even on valid arguments.
```

```
## Warning in estimatePopsize.default(formula = capture ~ nation, model = oiztgeom(omegaLink = "cloglog"), :
## Switching from observed information matrix to Fisher information matrix because hessian of log-likelihood is not
## negative define.
```

``` r
summary(modelInflated)
```

```
## 
## Call:
## estimatePopsize.default(formula = capture ~ nation, data = netherlandsimmigrant, 
##     model = oiztgeom(omegaLink = "cloglog"), popVar = "bootstrap", 
##     controlModel = controlModel(omegaFormula = ~gender + age), 
##     controlPopVar = controlPopVar(bootType = "semiparametric"))
## 
## Pearson Residuals:
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.41643 -0.41643 -0.30127  0.00314 -0.18323 13.88376 
## 
## Coefficients:
## -----------------------
## For linear predictors associated with: lambda 
##                      Estimate Std. Error z value  P(>|z|)    
## (Intercept)           -1.2552     0.2149  -5.840 5.22e-09 ***
## nationAsia            -0.8193     0.2544  -3.220  0.00128 ** 
## nationNorth Africa     0.2057     0.1838   1.119  0.26309    
## nationRest of Africa  -0.6692     0.2548  -2.627  0.00862 ** 
## nationSurinam         -1.5205     0.6271  -2.425  0.01532 *  
## nationTurkey          -1.1888     0.4343  -2.737  0.00619 ** 
## -----------------------
## For linear predictors associated with: omega 
##             Estimate Std. Error z value  P(>|z|)    
## (Intercept)  -1.4577     0.3884  -3.753 0.000175 ***
## gendermale   -0.8738     0.3602  -2.426 0.015267 *  
## age>40yrs     1.1745     0.5423   2.166 0.030326 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## AIC: 1677.125
## BIC: 1726.976
## Residual deviance: 941.5416
## 
## Log-likelihood: -829.5625 on 3751 Degrees of freedom 
## Number of iterations: 10
## -----------------------
## Population size estimation results: 
## Point estimate 6699.953
## Observed proportion: 28.1% (N obs = 1880)
## Boostrap sample skewness: 1.621389
## 0 skewness is expected for normally distributed variable
## ---
## Bootstrap Std. Error 1719.353
## 95% CI for the population size:
## lowerBound upperBound 
##   5001.409  11415.969 
## 95% CI for the share of observed population:
## lowerBound upperBound 
##   16.46816   37.58941
```

``` r
popSizeEst(basicModel)    # alternative: basicModel$populationSize
```

```
## Point estimate: 12690.35
## Variance: 7885790
## 95% confidence intervals:
##           lowerBound upperBound
## normal      7186.449   18194.25
## logNormal   8431.277   19718.31
```

``` r
popSizeEst(modelInflated) # alternative: modelInflated$populationSize
```

```
## Point estimate: 6699.953
## Variance: 2956175
## 95% confidence intervals:
## lowerBound upperBound 
##   5001.409  11415.969
```

``` r
library(lmtest)
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

``` r
lrtest(basicModel, modelInflated,
       name = function(x) {
    if (family(x)$family == "ztpoisson")
        "Basic model"
    else "Inflated model"
})
```

```
## Likelihood ratio test
## 
## Model 1: Basic model
## Model 2: Inflated model
##   #Df  LogLik Df  Chisq Pr(>Chisq)    
## 1   8 -848.45                         
## 2   9 -829.56  1 37.776  7.936e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
margFreq <- marginalFreq(basicModel)
summary(margFreq, df = 1, dropl5 = "group")
```

```
## Test for Goodness of fit of a regression model:
## 
##                  Test statistics df P(>X^2)
## Chi-squared test           50.06  1 1.5e-12
## G-test                     34.31  1 4.7e-09
## 
## -------------------------------------------------------------- 
## Cells with fitted frequencies of < 5 have been grouped 
## Names of cells used in calculating test(s) statistic: 1 2 3
```

``` r
margFreq_inf <- marginalFreq(modelInflated)
summary(margFreq_inf, df = 1, dropl5 = "group")
```

```
## Test for Goodness of fit of a regression model:
## 
##                  Test statistics df P(>X^2)
## Chi-squared test            1.88  1    0.17
## G-test                      2.32  1    0.13
## 
## -------------------------------------------------------------- 
## Cells with fitted frequencies of < 5 have been grouped 
## Names of cells used in calculating test(s) statistic: 1 2 3 4
```

``` r
plot(   basicModel, plotType = "rootogram", main = "ZT Poisson model")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

``` r
plot(modelInflated, plotType = "rootogram", main = "OI ZT Geometric model")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

``` r
#dev.off()

dfb <- dfbeta(basicModel)
round(t(apply(dfb, 2, quantile)*100), 4)
```

```
##                           0%     25%     50%    75%    100%
## (Intercept)          -0.9909 -0.1533  0.0191 0.0521  8.6619
## gendermale           -9.0535 -0.0777 -0.0283 0.1017  2.2135
## age>40yrs            -2.0010  0.0179  0.0379 0.0691 16.0061
## nationAsia           -9.5559 -0.0529  0.0066 0.0120 17.9914
## nationNorth Africa   -9.6605 -0.0842 -0.0177 0.0087  3.1260
## nationRest of Africa -9.4497 -0.0244  0.0030 0.0083 10.9787
## nationSurinam        -9.3138 -0.0065  0.0021 0.0037 99.3383
## nationTurkey         -9.6198 -0.0220  0.0079 0.0143 32.0980
```

``` r
dfi <- dfbeta(modelInflated)
round(t(apply(dfi, 2, quantile)*100), 4)
```

```
##                            0%     25%     50%     75%    100%
## (Intercept)           -1.4640  0.0050  0.0184  0.0557  9.0600
## nationAsia            -6.6331 -0.0346  0.0157  0.0347 12.2406
## nationNorth Africa    -7.2770 -0.0768 -0.0170  0.0085  1.9415
## nationRest of Africa  -6.6568 -0.0230  0.0081  0.0262  7.1710
## nationSurinam         -6.2308 -0.0124  0.0162  0.0421 62.2045
## nationTurkey          -6.4795 -0.0273  0.0204  0.0462 21.1338
## (Intercept):omega     -6.8668 -0.0193  0.0476  0.0476  9.3389
## gendermale:omega      -2.2733 -0.2227  0.1313  0.2482 11.1234
## age>40yrs:omega      -30.2130 -0.2247 -0.1312 -0.0663  2.0393
```

``` r
dfb_pop <- dfpopsize(basicModel, dfbeta = dfb)
dfi_pop <- dfpopsize(modelInflated, dfbeta = dfi)
summary(dfb_pop)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## -4236.407     2.660     2.660     5.445    17.281   117.445
```

``` r
summary(dfi_pop)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## -456.6443   -3.1121   -0.7243    3.4333    5.1535  103.5949
```

``` r
plot(basicModel, plotType = "dfpopContr",
     dfpop = dfb_pop, xlim = c(-4500, 150))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

``` r
plot(modelInflated, plotType = "dfpopContr",
     dfpop = dfi_pop, xlim = c(-4500, 150))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

``` r
#dev.off()

# ?plot.singleRStaticCountData

popSizestrata <- stratifyPopsize(basicModel)
cols <- c("name", "Observed", "Estimated", "logNormalLowerBound",
          "logNormalUpperBound")
popSizestrata_report <- popSizestrata[, cols]
cols_custom <- c("Name", "Obs", "Estimated", "LowerBound", "UpperBound")
names(popSizestrata_report) <- cols_custom
popSizestrata_report
```

```
##                              Name  Obs  Estimated LowerBound UpperBound
## 1                  gender==female  398  3811.0911  2189.0443   6902.133
## 2                    gender==male 1482  8879.2594  6090.7762  13354.880
## 3                     age==<40yrs 1769 10506.8971  7359.4155  15426.455
## 4                     age==>40yrs  111  2183.4535   872.0130   5754.876
## 5  nation==American and Australia  173   708.3688   504.6086   1037.331
## 6                    nation==Asia  284  2742.3147  1755.2548   4391.590
## 7            nation==North Africa 1023  3055.2033  2697.4900   3489.333
## 8          nation==Rest of Africa  243  2058.1533  1318.7466   3305.786
## 9                 nation==Surinam   64  2386.4513   505.2457  12287.983
## 10                 nation==Turkey   93  1739.8592   638.0497   5068.959
```

``` r
popSizestrata_inflated <- stratifyPopsize(modelInflated)
popSizestrata_inflated_report <- popSizestrata_inflated[, cols]
names(popSizestrata_inflated_report) <- cols_custom
popSizestrata_inflated_report
```

```
##                              Name  Obs Estimated LowerBound UpperBound
## 1  nation==American and Australia  173  516.2432   370.8463   768.4919
## 2                    nation==Asia  284 1323.5377   831.1601  2258.9954
## 3            nation==North Africa 1023 2975.8801  2254.7071  4119.3050
## 4          nation==Rest of Africa  243 1033.9753   667.6106  1716.4484
## 5                 nation==Surinam   64  354.2236   193.8891   712.4739
## 6                  nation==Turkey   93  496.0934   283.1444   947.5309
## 7                  gender==female  398 1109.7768   778.7197  1728.7066
## 8                    gender==male 1482 5590.1764  3838.4550  8644.0776
## 9                     age==<40yrs 1769 6437.8154  4462.3472  9862.2147
## 10                    age==>40yrs  111  262.1379   170.9490   492.0347
```

``` r
library(sandwich)
popSizestrataCustom <- stratifyPopsize(
  object  = basicModel,
  strata = ~ gender + age,
  alpha   = rep(c(0.1, 0.05), each=2),
  cov     = vcovHC(basicModel, type = "HC4")
)

popSizestrataCustom_report <- popSizestrataCustom[, c(cols, "confLevel")]
names(popSizestrataCustom_report) <- c(cols_custom, "alpha")
popSizestrataCustom_report
```

```
##             Name  Obs Estimated LowerBound UpperBound alpha
## 1 gender==female  398  3811.091  2275.6416   6602.161  0.10
## 2   gender==male 1482  8879.259  6261.5125  12930.751  0.10
## 3    age==<40yrs 1769 10506.897  7297.2081  15580.138  0.05
## 4    age==>40yrs  111  2183.453   787.0676   6464.009  0.05
```

``` r
# list(
#   "Stratum 1" = netherlandsimmigrant$gender == "male"   &
#     netherlandsimmigrant$nation == "Suriname",
#   "Stratum 2" = netherlandsimmigrant$gender == "female" &
#     netherlandsimmigrant$nation == "North Africa"
# )

par(mar = c(2.5, 8.5, 4.1, 2.5), cex.main = .7, cex.lab = .6)
plot(basicModel, plotType = "strata")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png)

``` r
plot(modelInflated, plotType = "strata")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-6.png)

``` r
#dev.off()

(popEst <- popSizeEst(basicModel))
```

```
## Point estimate: 12690.35
## Variance: 7885790
## 95% confidence intervals:
##           lowerBound upperBound
## normal      7186.449   18194.25
## logNormal   8431.277   19718.31
```

``` r
coef(summary(basicModel))
```

```
##                        Estimate Std. Error    z value      P(>|z|)
## (Intercept)          -1.3410661  0.2148870 -6.2407965 4.353484e-10
## gendermale            0.3971793  0.1630155  2.4364504 1.483220e-02
## age>40yrs            -0.9746058  0.4082420 -2.3873235 1.697155e-02
## nationAsia           -1.0925990  0.3016259 -3.6223642 2.919228e-04
## nationNorth Africa    0.1899980  0.1940007  0.9793677 3.273983e-01
## nationRest of Africa -0.9106361  0.3008092 -3.0272880 2.467587e-03
## nationSurinam        -2.3363949  1.0135639 -2.3051284 2.115938e-02
## nationTurkey         -1.6753917  0.6027744 -2.7794674 5.444812e-03
```

``` r
set.seed(1234567890)
N <- 10000
gender <- rbinom(N, 1, 0.2)
eta <- -1 + 0.5*gender
counts <- simulate(ztpoisson(), eta = cbind(eta), seed = 1)
summary(data.frame(gender, eta, counts))
```

```
##      gender            eta              counts      
##  Min.   :0.0000   Min.   :-1.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:-1.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :-1.0000   Median :0.0000  
##  Mean   :0.2036   Mean   :-0.8982   Mean   :0.4196  
##  3rd Qu.:0.0000   3rd Qu.:-1.0000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :-0.5000   Max.   :5.0000
```

``` r
# estimatePopsize(
#   TOTAL_SUB ~ .,
#   data = farmsubmission,
#   model = ztoigeom(),
#   controlModel(
#     omegaFormula = ~ 1 + log_size + C_TYPE
#   )
# )

X <- matrix(data = 0, nrow = 2 * NROW(farmsubmission), ncol = 7)

X[1:NROW(farmsubmission), 1:4] <- model.matrix(
  ~ 1 + log_size + log_distance + C_TYPE,
  farmsubmission
)
X[-(1:NROW(farmsubmission)), 5:7] <- model.matrix(
  ~ 1 + log_distance + C_TYPE,
  farmsubmission
)
attr(X, "hwm") <- c(4, 3)

start <- glm.fit(
  y = farmsubmission$TOTAL_SUB,
  x = X[1:NROW(farmsubmission), 1:4],
  family = poisson()
)$coefficients
start
```

```
## [1] -0.82583943  0.33254499 -0.03277732  0.32746933
```

``` r
res <- estimatePopsizeFit(
  y            = farmsubmission$TOTAL_SUB,
  X            = X,
  method       = "IRLS",
  priorWeights = 1,
  family       = ztoigeom(),
  control      = controlMethod(silent = TRUE),
  coefStart    = c(start, 0, 0, 0),
  etaStart     = matrix(X %*% c(start, 0, 0, 0), ncol = 2),
  offset       = cbind(rep(0, NROW(farmsubmission)),
                       rep(0, NROW(farmsubmission)))
)

ll <- ztoigeom()$makeMinusLogLike(y = farmsubmission$TOTAL_SUB, X = X)

res2 <- estimatePopsizeFit(
  y = farmsubmission$TOTAL_SUB,
  X = X,
  method = "optim",
  priorWeights = 1,
  family = ztoigeom(),
  coefStart = c(start, 0, 0, 0),
  control = controlMethod(silent = TRUE, maxiter = 10000),
  offset = cbind(rep(0, NROW(farmsubmission)), rep(0, NROW(farmsubmission)))
)

data.frame(IRLS  = round(c(res$beta, -ll(res$beta), res$iter), 4),
           optim = round(c(res2$beta, -ll(res2$beta), res2$iter[1]), 4))
```

```
##          IRLS       optim
## 1     -2.7845     -2.5971
## 2      0.6170      0.6163
## 3     -0.0646     -0.0825
## 4      0.5346      0.5431
## 5     -3.1745     -0.1504
## 6      0.1281     -0.1586
## 7     -1.0865     -1.0372
## 8 -17278.7613 -17280.1189
## 9     15.0000   1696.0000
```

``` r
# Implementing a custom \pkg{singleRcapture} family function {short-title="Implementing custom singleRcapture family function"}

#Suppose we want to implement a very specific zero truncated family function as presented in Appendix B


myFamilyFunction <- function(lambdaLink = c("logit", "cloglog", "probit"),
                             piLink     = c("logit", "cloglog", "probit"),
                             ...) {
  if (missing(lambdaLink)) lambdaLink <- "logit"
  if (missing(piLink))         piLink <- "logit"

  links <- list()
  attr(links, "linkNames") <- c(lambdaLink, piLink)

  lambdaLink <- switch(lambdaLink,
                       "logit"   = singleRcapture:::singleRinternallogitLink,
                       "cloglog" = singleRcapture:::singleRinternalcloglogLink,
                       "probit"  = singleRcapture:::singleRinternalprobitLink
  )

  piLink <- switch(piLink,
                   "logit"   = singleRcapture:::singleRinternallogitLink,
                   "cloglog" = singleRcapture:::singleRinternalcloglogLink,
                   "probit"  = singleRcapture:::singleRinternalprobitLink
  )

  links[1:2] <- c(lambdaLink, piLink)

  mu.eta <- function(eta, type = "trunc", deriv = FALSE, ...) {
    pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
    lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2

    if (!deriv) {
      switch (type,
              "nontrunc" = pi + 2 * lambda,
              "trunc" = 1 + lambda / (pi + lambda)
      )
    } else {
      # Only necessary if one wishes to use standard errors in predict method
      switch (type,
              "nontrunc" = {
                matrix(c(2, 1) * c(
                  lambdaLink(eta[, 1], inverse = TRUE, deriv = 1) / 2,
                  piLink(eta[, 2], inverse = TRUE, deriv = 1) / 2
                ), ncol = 2)
              },
              "trunc" = {
                matrix(c(
                  pi / (pi + lambda) ^ 2,
                  -lambda / (pi + lambda) ^ 2
                ) * c(
                  lambdaLink(eta[, 1], inverse = TRUE, deriv = 1) / 2,
                  piLink(eta[, 2], inverse = TRUE, deriv = 1) / 2
                ), ncol = 2)
              }
      )
    }
  }

  variance <- function(eta, type = "nontrunc", ...) {
    pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
    lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2

    switch (type,
            "nontrunc" = pi * (1 - pi) + 4 * lambda * (1 - lambda - pi),
            "trunc" = lambda * (1 - lambda) / (pi + lambda)
    )
  }

  Wfun <- function(prior, y, eta, ...) {
    pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
    lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2

    G01 <- ((lambda + pi) ^ (-2)) * piLink(eta[, 2], inverse = TRUE, deriv = 1) *
      lambdaLink(eta[, 1], inverse = TRUE, deriv = 1) * prior / 4

    G00 <- ((lambda + pi) ^ (-2)) - (pi ^ (-2)) - lambda / ((lambda + pi) * (pi ^ 2))
    G00 <- G00 * prior * (piLink(eta[, 2], inverse = TRUE, deriv = 1) ^ 2) / 4

    G11 <- ((lambda + pi) ^ (-2)) - (((lambda + pi) * lambda) ^ -1)
    G11 <- G11 * prior * (lambdaLink(eta[, 1], inverse = TRUE, deriv = 1) ^ 2) / 4

    matrix(
      -c(G11, # lambda
         G01, # mixed
         G01, # mixed
         G00  # pi
      ),
      dimnames = list(rownames(eta), c("lambda", "mixed", "mixed", "pi")),
      ncol = 4
    )
  }

  funcZ <- function(eta, weight, y, prior, ...) {
    pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
    lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2

    weight <- weight / prior

    G0 <- (2 - y) / pi     - ((lambda + pi) ^ -1)
    G1 <- (y - 1) / lambda - ((lambda + pi) ^ -1)

    G1 <- G1 * lambdaLink(eta[, 1], inverse = TRUE, deriv = 1) / 2
    G0 <- G0 *     piLink(eta[, 2], inverse = TRUE, deriv = 1) / 2

    uMatrix <- matrix(c(G1, G0), ncol = 2)

    weight <- lapply(X = 1:nrow(weight), FUN = function (x) {
      matrix(as.numeric(weight[x, ]), ncol = 2)
    })

    pseudoResid <- sapply(X = 1:length(weight), FUN = function (x) {
      #xx <- chol2inv(chol(weight[[x]])) # less computationally demanding
      xx <- solve(weight[[x]]) # more stable
      xx %*% uMatrix[x, ]
    })
    pseudoResid <- t(pseudoResid)
    dimnames(pseudoResid) <- dimnames(eta)
    pseudoResid
  }

  minusLogLike <- function(y, X, offset,
                           weight    = 1,
                           NbyK      = FALSE,
                           vectorDer = FALSE,
                           deriv     = 0,
                           ...) {
    y <- as.numeric(y)
    if (is.null(weight)) {
      weight <- 1
    }
    if (missing(offset)) {
      offset <- cbind(rep(0, NROW(X) / 2), rep(0, NROW(X) / 2))
    }

    if (!(deriv %in% c(0, 1, 2)))
      stop("Only score function and derivatives up to 2 are supported.")
    deriv <- deriv + 1

    switch (deriv,
            function(beta) {
              eta <- matrix(as.matrix(X) %*% beta, ncol = 2) + offset
              pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
              lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2
              -sum(weight * ((2 - y) * log(pi) + (y - 1) * log(lambda) - log(pi + lambda)))
            },
            function(beta) {
              eta <- matrix(as.matrix(X) %*% beta, ncol = 2) + offset
              pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
              lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2

              G0 <- (2 - y) / pi     - ((lambda + pi) ^ -1)
              G1 <- (y - 1) / lambda - ((lambda + pi) ^ -1)

              G1 <- G1 * weight * lambdaLink(eta[, 1], inverse = TRUE, deriv = 1) / 2
              G0 <- G0 * weight *     piLink(eta[, 2], inverse = TRUE, deriv = 1) / 2

              if (NbyK) {
                XX <- 1:(attr(X, "hwm")[1])
                return(cbind(as.data.frame(X[1:nrow(eta), XX]) * G1,
                             as.data.frame(X[-(1:nrow(eta)), -XX]) * G0))
              }
              if (vectorDer) {
                return(cbind(G1, G0))
              }

              as.numeric(c(G1, G0) %*% X)
            },
            function (beta) {
              lambdaPredNumber <- attr(X, "hwm")[1]
              eta <- matrix(as.matrix(X) %*% beta, ncol = 2) + offset
              pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
              lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2

              res <- matrix(nrow = length(beta), ncol = length(beta),
                            dimnames = list(names(beta), names(beta)))

              # pi^2 derivative
              dpi <- (2 - y) / pi - (lambda + pi) ^ -1
              G00 <- ((lambda + pi) ^ (-2)) - (2 - y) / (pi ^ 2)

              G00 <- t(as.data.frame(X[-(1:(nrow(X) / 2)), -(1:lambdaPredNumber)] *
                                       (G00 * ((piLink(eta[, 2], inverse = TRUE, deriv = 1) / 2) ^ 2) +
                                          dpi * piLink(eta[, 2], inverse = TRUE, deriv = 2) / 2) * weight)) %*%
                as.matrix(X[-(1:(nrow(X) / 2)), -(1:lambdaPredNumber)])
              # mixed derivative
              G01 <- (lambda + pi) ^ (-2)

              G01 <- t(as.data.frame(X[1:(nrow(X) / 2), 1:lambdaPredNumber]) *
                         G01 * (lambdaLink(eta[, 1], inverse = TRUE, deriv = 1) / 2) *
                         (piLink(eta[, 2], inverse = TRUE, deriv = 1) / 2) * weight) %*%
                as.matrix(X[-(1:(nrow(X) / 2)), -(1:lambdaPredNumber)])
              # lambda^2 derivative
              G11 <- ((lambda + pi) ^ (-2)) - (y - 1) / (lambda ^ 2)
              dlambda <- (y - 1) / lambda - ((lambda + pi) ^ -1)

              G11 <- t(as.data.frame(X[1:(nrow(X) / 2), 1:lambdaPredNumber] *
                                       (G11 * ((lambdaLink(eta[, 1], inverse = TRUE, deriv = 1) / 2) ^ 2) +
                                          dlambda * lambdaLink(eta[, 1], inverse = TRUE, deriv = 2) / 2) * weight)) %*%
                X[1:(nrow(X) / 2), 1:lambdaPredNumber]

              res[-(1:lambdaPredNumber), -(1:lambdaPredNumber)] <- G00
              res[1:lambdaPredNumber, 1:lambdaPredNumber] <- G11
              res[1:lambdaPredNumber, -(1:lambdaPredNumber)] <- t(G01)
              res[-(1:lambdaPredNumber), 1:lambdaPredNumber] <- G01

              res
            }
    )
  }

  validmu <- function(mu) {
    (sum(!is.finite(mu)) == 0) && all(0 < mu) && all(2 > mu)
  }

  # this is optional
  devResids <- function(y, eta, wt, ...) {
    0
  }

  pointEst <- function (pw, eta, contr = FALSE, ...) {
    pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
    lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2
    N <- pw / (lambda + pi)
    if(!contr) {
      N <- sum(N)
    }
    N
  }

  popVar <- function (pw, eta, cov, Xvlm, ...) {
    pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
    lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2

    bigTheta1 <- -pw / (pi + lambda) ^ 2 # w.r to pi
    bigTheta1 <- bigTheta1 * piLink(eta[, 2], inverse = TRUE, deriv = 1) / 2
    bigTheta2 <- -pw / (pi + lambda) ^ 2 # w.r to lambda
    bigTheta2 <- bigTheta2 * lambdaLink(eta[, 1], inverse = TRUE, deriv = 1) / 2 # w.r to lambda

    bigTheta <- t(c(bigTheta2, bigTheta1) %*% Xvlm)

    f1 <- t(bigTheta) %*% as.matrix(cov) %*% bigTheta

    f2 <- sum(pw * (1 - pi - lambda) / ((pi + lambda) ^ 2))

    f1 + f2
  }

  dFun <- function (x, eta, type = c("trunc", "nontrunc")) {
    if (missing(type)) type <- "trunc"
    pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
    lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2

    switch (type,
            "trunc" = {
              (pi * as.numeric(x == 1) + lambda * as.numeric(x == 2)) / (pi + lambda)
            },
            "nontrunc" = {
              (1 - pi - lambda) * as.numeric(x == 0) +
                pi * as.numeric(x == 1) + lambda * as.numeric(x == 2)
            }
    )
  }

  simulate <- function(n, eta, lower = 0, upper = Inf) {
    pi     <-     piLink(eta[, 2], inverse = TRUE) / 2
    lambda <- lambdaLink(eta[, 1], inverse = TRUE) / 2
    CDF <- function(x) {
      ifelse(x == Inf, 1,
             ifelse(x < 0, 0,
                    ifelse(x < 1, 1 - pi - lambda,
                           ifelse(x < 2, 1 - lambda, 1))))
    }
    lb <- CDF(lower)
    ub <- CDF(upper)
    p_u <- stats::runif(n, lb, ub)
    sims <- rep(0, n)
    cond <- CDF(sims) <= p_u
    while (any(cond)) {
      sims[cond] <- sims[cond] + 1
      cond <- CDF(sims) <= p_u
    }
    sims
  }

  getStart <- expression(
    if (method == "IRLS") {
      etaStart <- cbind(
        family$links[[1]](mean(observed == 2) * (1 + 0 * (observed == 2))), # lambda
        family$links[[2]](mean(observed == 1) * (1 + 0 * (observed == 1)))  # pi
      ) + offset
    } else if (method == "optim") {
      init <- c(
        family$links[[1]](weighted.mean(observed == 2, priorWeights) * 1 + .0001),
        family$links[[2]](weighted.mean(observed == 1, priorWeights) * 1 + .0001)
      )
      if (attr(terms, "intercept")) {
        coefStart <- c(init[1], rep(0, attr(Xvlm, "hwm")[1] - 1))
      } else {
        coefStart <- rep(init[1] / attr(Xvlm, "hwm")[1], attr(Xvlm, "hwm")[1])
      }
      if ("(Intercept):pi" %in% colnames(Xvlm)) {
        coefStart <- c(coefStart, init[2], rep(0, attr(Xvlm, "hwm")[2] - 1))
      } else {
        coefStart <- c(coefStart, rep(init[2] / attr(Xvlm, "hwm")[2], attr(Xvlm, "hwm")[2]))
      }
    }
  )

  structure(
    list(
      makeMinusLogLike = minusLogLike,
      densityFunction  = dFun,
      links     = links,
      mu.eta    = mu.eta,
      valideta  = function (eta) {TRUE},
      variance  = variance,
      Wfun      = Wfun,
      funcZ     = funcZ,
      devResids = devResids,
      validmu   = validmu,
      pointEst  = pointEst,
      popVar    = popVar,
      family    = "myFamilyFunction",
      etaNames  = c("lambda", "pi"),
      simulate  = simulate,
      getStart  = getStart,
      extraInfo = c(
        mean       = "pi / 2 + lambda",
        variance   = paste0("(pi / 2) * (1 - pi / 2) + 2 * lambda * (1 - lambda / 2 - pi / 2)"),
        popSizeEst = "(1 - (pi + lambda) / 2) ^ -1",
        meanTr     = "1 + lambda / (pi + lambda)",
        varianceTr = paste0("lambda * (1 - lambda / 2) / (pi + lambda)")
      )
    ),
    class = c("singleRfamily", "family")
  )
}

# A quick tests shows us that this implementation in fact works:


set.seed(123)
Y <- simulate(
  myFamilyFunction(lambdaLink = "logit", piLink = "logit"),
  nsim = 1000, eta = matrix(0, nrow = 1000, ncol = 2),
  truncated = FALSE
)
mm <- estimatePopsize(
  formula = Y ~ 1,
  data = data.frame(Y = Y[Y > 0]),
  model = myFamilyFunction(lambdaLink = "logit",
                           piLink = "logit"),
  # the usual observed information matrix
  # is ill-suited for this distribution
  controlPopVar = controlPopVar(covType = "Fisher")
)
summary(mm)
```

```
## 
## Call:
## estimatePopsize.default(formula = Y ~ 1, data = data.frame(Y = Y[Y > 
##     0]), model = myFamilyFunction(lambdaLink = "logit", piLink = "logit"), 
##     controlPopVar = controlPopVar(covType = "Fisher"))
## 
## Pearson Residuals:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.8198 -0.8198  0.8099  0.0000  0.8099  0.8099 
## 
## Coefficients:
## -----------------------
## For linear predictors associated with: lambda 
##             Estimate Std. Error z value P(>|z|)
## (Intercept)  0.01217    0.20253    0.06   0.952
## -----------------------
## For linear predictors associated with: pi 
##             Estimate Std. Error z value P(>|z|)
## (Intercept) -0.01217    0.08926  -0.136   0.892
## 
## AIC: 687.4249
## BIC: 695.8259
## Residual deviance: 0
## 
## Log-likelihood: -341.7124 on 984 Degrees of freedom 
## Number of iterations: 2
## -----------------------
## Population size estimation results: 
## Point estimate 986
## Observed proportion: 50% (N obs = 493)
## Std. Error 70.30092
## 95% CI for the population size:
##           lowerBound upperBound
## normal      848.2127   1123.787
## logNormal   866.3167   1144.053
## 95% CI for the share of observed population:
##           lowerBound upperBound
## normal      43.86951   58.12221
## logNormal   43.09241   56.90759
```

``` r
#where the link functions, such as \code{singleRcapture:::singleRinternalcloglogLink}, are just internal functions in \pkg{singleRcapture} that compute link functions, their inverses and derivatives of both links and inverse links up to the third order:

singleRcapture:::singleRinternalcloglogLink
```

```
## function (x, inverse = FALSE, deriv = 0) 
## {
##     deriv <- deriv + 1
##     if (isFALSE(inverse)) {
##         res <- switch(deriv, log(-log(1 - x)), -1/((1 - x) * 
##             log(1 - x)), -(1 + log(1 - x))/((x - 1)^2 * log(1 - 
##             x)^2), (2 * log(1 - x)^2 + 3 * log(1 - x) + 2)/(log(1 - 
##             x)^3 * (x - 1)^3))
##     }
##     else {
##         res <- switch(deriv, 1 - exp(-exp(x)), exp(x - exp(x)), 
##             (1 - exp(x)) * exp(x - exp(x)), (exp(2 * x) - 3 * 
##                 exp(x) + 1) * exp(x - exp(x)))
##     }
##     res
## }
## <bytecode: 0x127503390>
## <environment: namespace:singleRcapture>
```

``` r
# One could, of course, include the code for computing them manually.

# Session info

sessionInfo()
```

```
## R version 4.4.2 (2024-10-31)
## Platform: aarch64-apple-darwin20
## Running under: macOS Sonoma 14.5
## 
## Matrix products: default
## BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## time zone: Europe/Warsaw
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] sandwich_3.1-1       lmtest_0.9-40        zoo_1.8-12           singleRcapture_0.2.2
## 
## loaded via a namespace (and not attached):
##  [1] doParallel_1.0.17   cli_3.6.3           knitr_1.49          rlang_1.1.5         xfun_0.50          
##  [6] RcppParallel_5.1.10 rticles_0.27        htmltools_0.5.8.1   rsconnect_1.3.4     rmarkdown_2.29     
## [11] grid_4.4.2          evaluate_1.0.3      fastmap_1.2.0       yaml_2.3.10         foreach_1.5.2      
## [16] lifecycle_1.0.4     compiler_4.4.2      mathjaxr_1.6-0      codetools_0.2-20    Rcpp_1.0.14        
## [21] lamW_2.2.4          rstudioapi_0.17.1   lattice_0.22-6      digest_0.6.37       parallel_4.4.2     
## [26] tools_4.4.2         iterators_1.0.14
```

