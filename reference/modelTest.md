# Detailed Tests on Models

TODO: make me!

## Usage

``` r
modelTest(object, ...)

is.modelTest(x)

as.modelTest(x)

# S3 method for class 'vglm'
modelTest(object, ...)

# S3 method for class 'lm'
modelTest(object, ...)
```

## Arguments

- object:

  A fitted model object.

- ...:

  Additional arguments passed to specific methods.

- x:

  A object (e.g., list or a modelTest object) to test or attempt
  coercing to a modelTest object.

## Value

Depends on the method dispatch.

A list with two elements. `Results` contains a data table of the actual
estimates. `Table` contains a nicely formatted character matrix.

A list with two elements. `Results` contains a data table of the actual
estimates. `Table` contains a nicely formatted character matrix.

## Examples

``` r
mtcars$cyl <- factor(mtcars$cyl)
m <- VGAM::vglm(cyl ~ qsec,
  family = VGAM::multinomial(), data = mtcars)
modelTest(m)
#> $FixedEffects
#>      Num  Names   Term   Ref        Est        SE        Pval        LL
#>    <int> <char> <char> <int>      <num>     <num>       <num>     <num>
#> 1:     1   qsec   qsec     1 -0.5795781 0.4006097 0.147969796 -1.364759
#> 2:     2   qsec   qsec     1 -1.2697728 0.4560391 0.005363505 -2.163593
#> 3:     2   qsec   qsec     2 -0.6901947 0.4045653 0.088005136 -1.483128
#>            UL     K  Comp  Labels
#>         <num> <int> <int>  <char>
#> 1:  0.2056026     3     2 2 vs. 1
#> 2: -0.3759525     3     3 3 vs. 1
#> 3:  0.1027386     3     3 3 vs. 2
#> 
#> $RandomEffects
#> [1] NA
#> 
#> $EffectSizes
#>      Term    Chisq    DF         Pval   Type
#>    <char>    <num> <num>        <num> <char>
#> 1:   qsec 14.21315     2 0.0008196964  Fixed
#> 
#> $OverallModel
#> [1] NA
#> 
#> attr(,"class")
#> [1] "modelTest.vglm" "modelTest"     

## clean up
rm(m, mtcars)

if (FALSE) { # \dontrun{
mtcars$cyl <- factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am)
m <- VGAM::vglm(cyl ~ qsec,
  family = VGAM::multinomial(), data = mtcars)
modelTest(m)

m <- VGAM::vglm(cyl ~ scale(qsec),
  family = VGAM::multinomial(), data = mtcars)
modelTest(m)

m2 <- VGAM::vglm(cyl ~ factor(vs) * scale(qsec),
  family = VGAM::multinomial(), data = mtcars)
modelTest(m2)

m <- VGAM::vglm(Species ~ Sepal.Length,
  family = VGAM::multinomial(), data = iris)
modelTest(m)

set.seed(1234)
sampdata <- data.frame(
  Outcome = factor(sample(letters[1:3], 20 * 9, TRUE)),
  C1 = rnorm(20 * 9),
  D3 = sample(paste0("L", 1:3), 20 * 9, TRUE))

m <- VGAM::vglm(Outcome ~ factor(D3),
  family = VGAM::multinomial(), data = sampdata)
modelTest(m)

m <- VGAM::vglm(Outcome ~ factor(D3) + C1,
  family = VGAM::multinomial(), data = sampdata)
modelTest(m)
} # }
m1 <- lm(mpg ~ qsec * hp, data = mtcars)
modelTest(m1)
#> $FixedEffects
#>           Term         Est           LL          UL         Pval
#>         <char>       <num>        <num>       <num>        <num>
#> 1: (Intercept)  8.52461247 -17.15039253 34.19961746 0.5020186630
#> 2:        qsec  1.47683245   0.08321560  2.87044931 0.0385855703
#> 3:          hp  0.23587912   0.08515568  0.38660256 0.0033562173
#> 4:     qsec:hp -0.01949155  -0.02855763 -0.01042546 0.0001411028
#> 
#> $RandomEffects
#> [1] NA
#> 
#> $EffectSizes
#>       Term N_Obs        AIC        BIC       LL  LLDF      Sigma         R2
#>     <char> <num>      <num>      <num>    <num> <num>      <num>      <num>
#> 1:    qsec     0  -2.977231  -1.511495 2.488615     1 -0.1823263 0.03610200
#> 2:      hp     0  -8.004306  -6.538570 5.002153     1 -0.4372429 0.07873595
#> 3: qsec:hp     0 -14.841867 -13.376131 8.420933     1 -0.8177254 0.14859655
#>           F2      AdjR2         F FNumDF FDenDF            P   Type
#>        <num>      <num>     <num>  <num>  <num>        <num> <char>
#> 1: 0.1682869 0.03040174  4.712032      1     28 0.0385855703  Fixed
#> 2: 0.3670219 0.07597596 10.276613      1     28 0.0033562173  Fixed
#> 3: 0.6926720 0.15065453 19.394816      1     28 0.0001411028  Fixed
#> 
#> $OverallModel
#> $Performance
#>     Model N_Obs      AIC      BIC        LL  LLDF    Sigma        R2       F2
#>    <char> <num>    <num>    <num>     <num> <num>    <num>     <num>    <num>
#> 1:     lm    32 165.4972 172.8259 -77.74861     5 2.937243 0.7854734 3.661427
#>        AdjR2        F FNumDF FDenDF            P
#>        <num>    <num>  <num>  <num>        <num>
#> 1: 0.7624884 34.17332      3     28 1.694676e-09
#> 
#> attr(,"class")
#> [1] "modelPerformance.lm" "modelPerformance"   
#> 
#> attr(,"class")
#> [1] "modelTest.lm" "modelTest"   

mtcars$cyl <- factor(mtcars$cyl)
m2 <- lm(mpg ~ cyl, data = mtcars)
modelTest(m2)
#> $FixedEffects
#>           Term        Est        LL        UL         Pval
#>         <char>      <num>     <num>     <num>        <num>
#> 1: (Intercept)  26.663636  24.67608 28.651192 2.688358e-22
#> 2:        cyl6  -6.920779 -10.10796 -3.733599 1.194696e-04
#> 3:        cyl8 -11.563636 -14.21962 -8.907653 8.568209e-10
#> 
#> $RandomEffects
#> [1] NA
#> 
#> $EffectSizes
#>      Term N_Obs       AIC       BIC       LL  LLDF     Sigma        R2      F2
#>    <char> <num>     <num>     <num>    <num> <num>     <num>     <num>   <num>
#> 1:    cyl     0 -38.19157 -35.26009 21.09578     2 -2.803849 0.7324601 2.73776
#>       AdjR2        F FNumDF FDenDF            P   Type
#>       <num>    <num>  <num>  <num>        <num> <char>
#> 1: 0.714009 39.69752      2     29 4.978919e-09  Fixed
#> 
#> $OverallModel
#> $Performance
#>     Model N_Obs     AIC      BIC        LL  LLDF    Sigma        R2      F2
#>    <char> <num>   <num>    <num>     <num> <num>    <num>     <num>   <num>
#> 1:     lm    32 170.564 176.4269 -81.28198     4 3.223099 0.7324601 2.73776
#>       AdjR2        F FNumDF FDenDF            P
#>       <num>    <num>  <num>  <num>        <num>
#> 1: 0.714009 39.69752      2     29 4.978919e-09
#> 
#> attr(,"class")
#> [1] "modelPerformance.lm" "modelPerformance"   
#> 
#> attr(,"class")
#> [1] "modelTest.lm" "modelTest"   

m3 <- lm(mpg ~ hp * cyl, data = mtcars)
modelTest(m3)
#> $FixedEffects
#>           Term          Est            LL          UL         Pval
#>         <char>        <num>         <num>       <num>        <num>
#> 1: (Intercept)  35.98302564  27.988911895 43.97713938 1.042337e-09
#> 2:          hp  -0.11277589  -0.206810088 -0.01874169 2.061364e-02
#> 3:        cyl6 -15.30917451 -30.591135429 -0.02721358 4.962243e-02
#> 4:        cyl8 -17.90295193 -28.714239963 -7.09166390 2.163657e-03
#> 5:     hp:cyl6   0.10516262  -0.035606674  0.24593191 1.367182e-01
#> 6:     hp:cyl8   0.09853177  -0.001415964  0.19847950 5.309562e-02
#> 
#> $RandomEffects
#> [1] NA
#> 
#> $EffectSizes
#>      Term N_Obs        AIC       BIC       LL  LLDF      Sigma         R2
#>    <char> <num>      <num>     <num>    <num> <num>      <num>      <num>
#> 1:     hp     0 -4.7216325 -3.255897 3.360816     1 -0.2724898 0.04949968
#> 2:    cyl     0 -8.3406788 -5.409207 6.170339     2 -0.5104704 0.09965210
#> 3: hp:cyl     0 -0.8128535  2.118618 2.406427     2 -0.1177589 0.03437072
#>           F2      AdjR2        F FNumDF FDenDF           P   Type
#>        <num>      <num>    <num>  <num>  <num>       <num> <char>
#> 1: 0.2337410 0.04748123 6.077266      1     26 0.020613638  Fixed
#> 2: 0.4705643 0.09229362 6.117336      2     26 0.006648256  Fixed
#> 3: 0.1623010 0.02001782 2.109913      2     26 0.141533090  Fixed
#> 
#> $OverallModel
#> $Performance
#>     Model N_Obs      AIC      BIC       LL  LLDF    Sigma        R2       F2
#>    <char> <num>    <num>    <num>    <num> <num>    <num>     <num>    <num>
#> 1:     lm    32 169.0836 179.3437 -77.5418     7 3.028484 0.7882285 3.722071
#>        AdjR2        F FNumDF FDenDF            P
#>        <num>    <num>  <num>  <num>        <num>
#> 1: 0.7475032 19.35477      5     26 5.018882e-08
#> 
#> attr(,"class")
#> [1] "modelPerformance.lm" "modelPerformance"   
#> 
#> attr(,"class")
#> [1] "modelTest.lm" "modelTest"   

m4 <- lm(sqrt(mpg) ~ hp * cyl, data = mtcars)
modelTest(m4)
#> $FixedEffects
#>           Term          Est           LL            UL         Pval
#>         <char>        <num>        <num>         <num>        <num>
#> 1: (Intercept)  6.052499736  5.170711347  6.9342881254 1.070676e-13
#> 2:          hp -0.010956546 -0.021328961 -0.0005841313 3.922162e-02
#> 3:        cyl6 -1.512749623 -3.198421874  0.1729226268 7.650843e-02
#> 4:        cyl8 -1.808475093 -3.001011071 -0.6159391140 4.419878e-03
#> 5:     hp:cyl6  0.010146472 -0.005381043  0.0256739883 1.908190e-01
#> 6:     hp:cyl8  0.009178964 -0.001845741  0.0202036696 9.891437e-02
#> 
#> $RandomEffects
#> [1] NA
#> 
#> $EffectSizes
#>      Term N_Obs        AIC       BIC       LL  LLDF        Sigma         R2
#>    <char> <num>      <num>     <num>    <num> <num>        <num>      <num>
#> 1:     hp     0 -3.3324188 -1.866683 2.666209     1 -0.022238466 0.03882660
#> 2:    cyl     0 -6.5881559 -3.656684 5.294078     2 -0.045762925 0.08397830
#> 3: hp:cyl     0  0.4535321  3.385004 1.773234     2 -0.006189775 0.02509585
#>           F2       AdjR2        F FNumDF FDenDF          P   Type
#>        <num>       <num>    <num>  <num>  <num>      <num> <char>
#> 1: 0.1813267 0.035123021 4.714493      1     26 0.03922162  Fixed
#> 2: 0.3921925 0.074740039 5.098503      2     26 0.01354906  Fixed
#> 3: 0.1172017 0.009548751 1.523623      2     26 0.23674952  Fixed
#> 
#> $OverallModel
#> $Performance
#>     Model N_Obs      AIC      BIC       LL  LLDF     Sigma        R2       F2
#>    <char> <num>    <num>    <num>    <num> <num>     <num>     <num>    <num>
#> 1:     lm    32 27.99504 38.25519 -6.99752     7 0.3340561 0.7858748 3.670165
#>        AdjR2        F FNumDF FDenDF           P
#>        <num>    <num>  <num>  <num>       <num>
#> 1: 0.7446969 19.08486      5     26 5.77082e-08
#> 
#> attr(,"class")
#> [1] "modelPerformance.lm" "modelPerformance"   
#> 
#> attr(,"class")
#> [1] "modelTest.lm" "modelTest"   

m5 <- lm(mpg ~ sqrt(hp) * cyl, data = mtcars)
modelTest(m5)
#> $FixedEffects
#>             Term        Est          LL         UL         Pval
#>           <char>      <num>       <num>      <num>        <num>
#> 1:   (Intercept)  45.320923  30.0821810 60.5596654 1.839533e-06
#> 2:      sqrt(hp)  -2.067923  -3.7442683 -0.3915773 1.757488e-02
#> 3:          cyl6 -23.458371 -54.5954487  7.6787057 1.335616e-01
#> 4:          cyl8 -23.624030 -44.7466708 -2.5013883 2.979562e-02
#> 5: sqrt(hp):cyl6   1.875526  -1.0975943  4.8486458 2.061304e-01
#> 6: sqrt(hp):cyl8   1.608904  -0.3488383  3.5666459 1.031270e-01
#> 
#> $RandomEffects
#> [1] NA
#> 
#> $EffectSizes
#>            Term N_Obs        AIC        BIC       LL  LLDF       Sigma
#>          <char> <num>      <num>      <num>    <num> <num>       <num>
#> 1:     sqrt(hp)     0 -5.0712752 -3.6055393 3.535638     1 -0.28847604
#> 2:          cyl     0 -2.5498555  0.3816163 3.274928     2 -0.20284492
#> 3: sqrt(hp):cyl     0  0.3345454  3.2660172 1.832727     2 -0.06140898
#>            R2        F2      AdjR2        F FNumDF FDenDF          P   Type
#>         <num>     <num>      <num>    <num>  <num>  <num>      <num> <char>
#> 1: 0.05161694 0.2472952 0.05004665 6.429675      1     26 0.01757488  Fixed
#> 2: 0.04740919 0.2271360 0.03471263 2.952768      2     26 0.06988678  Fixed
#> 3: 0.02533174 0.1213636 0.01026974 1.577727      2     26 0.22557763  Fixed
#> 
#> $OverallModel
#> $Performance
#>     Model N_Obs      AIC      BIC        LL  LLDF   Sigma       R2       F2
#>    <char> <num>    <num>    <num>     <num> <num>   <num>    <num>    <num>
#> 1:     lm    32 168.6201 178.8802 -77.31003     7 3.00663 0.791274 3.790969
#>        AdjR2        F FNumDF FDenDF            P
#>        <num>    <num>  <num>  <num>        <num>
#> 1: 0.7511343 19.71304      5     26 4.179396e-08
#> 
#> attr(,"class")
#> [1] "modelPerformance.lm" "modelPerformance"   
#> 
#> attr(,"class")
#> [1] "modelTest.lm" "modelTest"   

## cleanup
rm(m1, m2, m3, m4, m5, mtcars)
```
