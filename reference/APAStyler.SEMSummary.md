# A generic function for pretty printing in (semi) APA Style

A generic function for pretty printing in (semi) APA Style

## Usage

``` r
# S3 method for class 'SEMSummary'
APAStyler(
  object,
  digits = 2,
  type = c("cov", "cor", "both"),
  stars = FALSE,
  file = ifelse(.Platform$OS.type == "windows", "clipboard", FALSE),
  sep = "\t",
  print = TRUE,
  ...
)
```

## Arguments

- object:

  `SEMSummary` object

- digits:

  The number of digits to round results to. Defaults to 2.

- type:

  A character vector giving what to print. Defaults to ‘cov’, the
  covariances. Other options are ‘cor’ and ‘both’.

- stars:

  A logical value whether to include significance values as stars
  (\*\*\* p \< .001, \*\* p \< .01, \* p \< .05).

- file:

  An optional argument indicating whether the output should be written
  to a file.

- sep:

  Character what the separator for the table should be. Defaults to
  tabs.

- print:

  A logical argument, whether or not to print results to screen. This is
  distinct from saving them to a file. Defaults to `TRUE` for back
  compatibility.

- ...:

  Additional argiuments passed on to `write.table`.

## Examples

``` r
m <- SEMSummary(~., data = mtcars)
APAStyler(m, type = "cor", stars = FALSE, file = FALSE)
#>          N  M      SD     1.  2.    3.    4.    5.    6.    7.    8.    9.   
#> 1. mpg   32  20.09   6.03  -  -0.85 -0.85 -0.78  0.68 -0.87  0.42  0.66  0.60
#> 2. cyl   32   6.19   1.79      -     0.90  0.83 -0.70  0.78 -0.59 -0.81 -0.52
#> 3. disp  32 230.72 123.94            -     0.79 -0.71  0.89 -0.43 -0.71 -0.59
#> 4. hp    32 146.69  68.56                  -    -0.45  0.66 -0.71 -0.72 -0.24
#> 5. drat  32   3.60   0.53                        -    -0.71  0.09  0.44  0.71
#> 6. wt    32   3.22   0.98                              -    -0.17 -0.55 -0.69
#> 7. qsec  32  17.85   1.79                                    -     0.74 -0.23
#> 8. vs    32   0.44   0.50                                          -     0.17
#> 9. am    32   0.41   0.50                                                -   
#> 10. gear 32   3.69   0.74                                                    
#> 11. carb 32   2.81   1.62                                                    
#>          10.   11.  
#> 1. mpg    0.48 -0.55
#> 2. cyl   -0.49  0.53
#> 3. disp  -0.56  0.39
#> 4. hp    -0.13  0.75
#> 5. drat   0.70 -0.09
#> 6. wt    -0.58  0.43
#> 7. qsec  -0.21 -0.66
#> 8. vs     0.21 -0.57
#> 9. am     0.79  0.06
#> 10. gear  -     0.27
#> 11. carb        -   
#> 
#> Percentage of coverage for each pairwise covariance or correlation
#> 
#>      mpg cyl disp hp drat wt qsec vs am gear carb
#> mpg  1   1   1    1  1    1  1    1  1  1    1   
#> cyl      1   1    1  1    1  1    1  1  1    1   
#> disp         1    1  1    1  1    1  1  1    1   
#> hp                1  1    1  1    1  1  1    1   
#> drat                 1    1  1    1  1  1    1   
#> wt                        1  1    1  1  1    1   
#> qsec                         1    1  1  1    1   
#> vs                                1  1  1    1   
#> am                                   1  1    1   
#> gear                                    1    1   
#> carb                                         1   
APAStyler(m, type = "cov", stars = FALSE, file = FALSE)
#>          N  M      SD     1.  2.       3.       4.       5.       6.      
#> 1. mpg   32  20.09   6.03  -     -9.17  -633.10  -320.73     2.20    -5.12
#> 2. cyl   32   6.19   1.79      -         199.66   101.93    -0.67     1.37
#> 3. disp  32 230.72 123.94               -        6721.16   -47.06   107.68
#> 4. hp    32 146.69  68.56                        -         -16.45    44.19
#> 5. drat  32   3.60   0.53                                 -          -0.37
#> 6. wt    32   3.22   0.98                                          -      
#> 7. qsec  32  17.85   1.79                                                 
#> 8. vs    32   0.44   0.50                                                 
#> 9. am    32   0.41   0.50                                                 
#> 10. gear 32   3.69   0.74                                                 
#> 11. carb 32   2.81   1.62                                                 
#>          7.       8.       9.       10.      11.     
#> 1. mpg       4.51     2.02     1.80     2.14    -5.36
#> 2. cyl      -1.89    -0.73    -0.47    -0.65     1.52
#> 3. disp    -96.05   -44.38   -36.56   -50.80    79.07
#> 4. hp      -86.77   -24.99    -8.32    -6.36    83.04
#> 5. drat      0.09     0.12     0.19     0.28    -0.08
#> 6. wt       -0.31    -0.27    -0.34    -0.42     0.68
#> 7. qsec   -           0.67    -0.20    -0.28    -1.89
#> 8. vs              -           0.04     0.08    -0.46
#> 9. am                       -           0.29     0.05
#> 10. gear                             -           0.33
#> 11. carb                                      -      
#> 
#> Percentage of coverage for each pairwise covariance or correlation
#> 
#>      mpg cyl disp hp drat wt qsec vs am gear carb
#> mpg  1   1   1    1  1    1  1    1  1  1    1   
#> cyl      1   1    1  1    1  1    1  1  1    1   
#> disp         1    1  1    1  1    1  1  1    1   
#> hp                1  1    1  1    1  1  1    1   
#> drat                 1    1  1    1  1  1    1   
#> wt                        1  1    1  1  1    1   
#> qsec                         1    1  1  1    1   
#> vs                                1  1  1    1   
#> am                                   1  1    1   
#> gear                                    1    1   
#> carb                                         1   
APAStyler(m, type = "both", stars = FALSE, file = FALSE)
#>          N  M      SD     1.    2.       3.       4.       5.       6.      
#> 1. mpg   32  20.09   6.03  -       -9.17  -633.10  -320.73     2.20    -5.12
#> 2. cyl   32   6.19   1.79 -0.85  -         199.66   101.93    -0.67     1.37
#> 3. disp  32 230.72 123.94 -0.85  0.90     -        6721.16   -47.06   107.68
#> 4. hp    32 146.69  68.56 -0.78  0.83     0.79     -         -16.45    44.19
#> 5. drat  32   3.60   0.53  0.68 -0.70    -0.71    -0.45     -          -0.37
#> 6. wt    32   3.22   0.98 -0.87  0.78     0.89     0.66    -0.71     -      
#> 7. qsec  32  17.85   1.79  0.42 -0.59    -0.43    -0.71     0.09    -0.17   
#> 8. vs    32   0.44   0.50  0.66 -0.81    -0.71    -0.72     0.44    -0.55   
#> 9. am    32   0.41   0.50  0.60 -0.52    -0.59    -0.24     0.71    -0.69   
#> 10. gear 32   3.69   0.74  0.48 -0.49    -0.56    -0.13     0.70    -0.58   
#> 11. carb 32   2.81   1.62 -0.55  0.53     0.39     0.75    -0.09     0.43   
#>          7.       8.       9.       10.      11.     
#> 1. mpg       4.51     2.02     1.80     2.14    -5.36
#> 2. cyl      -1.89    -0.73    -0.47    -0.65     1.52
#> 3. disp    -96.05   -44.38   -36.56   -50.80    79.07
#> 4. hp      -86.77   -24.99    -8.32    -6.36    83.04
#> 5. drat      0.09     0.12     0.19     0.28    -0.08
#> 6. wt       -0.31    -0.27    -0.34    -0.42     0.68
#> 7. qsec   -           0.67    -0.20    -0.28    -1.89
#> 8. vs     0.74     -           0.04     0.08    -0.46
#> 9. am    -0.23     0.17     -           0.29     0.05
#> 10. gear -0.21     0.21     0.79     -           0.33
#> 11. carb -0.66    -0.57     0.06     0.27     -      
#> 
#> Percentage of coverage for each pairwise covariance or correlation
#> 
#>      mpg cyl disp hp drat wt qsec vs am gear carb
#> mpg  1   1   1    1  1    1  1    1  1  1    1   
#> cyl      1   1    1  1    1  1    1  1  1    1   
#> disp         1    1  1    1  1    1  1  1    1   
#> hp                1  1    1  1    1  1  1    1   
#> drat                 1    1  1    1  1  1    1   
#> wt                        1  1    1  1  1    1   
#> qsec                         1    1  1  1    1   
#> vs                                1  1  1    1   
#> am                                   1  1    1   
#> gear                                    1    1   
#> carb                                         1   
APAStyler(m, type = "cor", stars = TRUE, file = FALSE)
#>          N  M      SD     1.  2.       3.       4.       5.       6.      
#> 1. mpg   32  20.09   6.03  -  -0.85*** -0.85*** -0.78***  0.68*** -0.87***
#> 2. cyl   32   6.19   1.79      -        0.90***  0.83*** -0.70***  0.78***
#> 3. disp  32 230.72 123.94               -        0.79*** -0.71***  0.89***
#> 4. hp    32 146.69  68.56                        -       -0.45**   0.66***
#> 5. drat  32   3.60   0.53                                 -       -0.71***
#> 6. wt    32   3.22   0.98                                          -      
#> 7. qsec  32  17.85   1.79                                                 
#> 8. vs    32   0.44   0.50                                                 
#> 9. am    32   0.41   0.50                                                 
#> 10. gear 32   3.69   0.74                                                 
#> 11. carb 32   2.81   1.62                                                 
#>          7.       8.       9.       10.      11.     
#> 1. mpg    0.42*    0.66***  0.60***  0.48**  -0.55** 
#> 2. cyl   -0.59*** -0.81*** -0.52**  -0.49**   0.53** 
#> 3. disp  -0.43*   -0.71*** -0.59*** -0.56***  0.39*  
#> 4. hp    -0.71*** -0.72*** -0.24    -0.13     0.75***
#> 5. drat   0.09     0.44*    0.71***  0.70*** -0.09   
#> 6. wt    -0.17    -0.55*** -0.69*** -0.58***  0.43*  
#> 7. qsec   -        0.74*** -0.23    -0.21    -0.66***
#> 8. vs              -        0.17     0.21    -0.57***
#> 9. am                       -        0.79***  0.06   
#> 10. gear                             -        0.27   
#> 11. carb                                      -      
#> 
#> Percentage of coverage for each pairwise covariance or correlation
#> 
#>      mpg cyl disp hp drat wt qsec vs am gear carb
#> mpg  1   1   1    1  1    1  1    1  1  1    1   
#> cyl      1   1    1  1    1  1    1  1  1    1   
#> disp         1    1  1    1  1    1  1  1    1   
#> hp                1  1    1  1    1  1  1    1   
#> drat                 1    1  1    1  1  1    1   
#> wt                        1  1    1  1  1    1   
#> qsec                         1    1  1  1    1   
#> vs                                1  1  1    1   
#> am                                   1  1    1   
#> gear                                    1    1   
#> carb                                         1   
APAStyler(m, type = "cov", stars = TRUE, file = FALSE)
#>          N  M      SD     1.  2.          3.          4.          5.         
#> 1. mpg   32  20.09   6.03  -     -9.17***  -633.10***  -320.73***     2.20***
#> 2. cyl   32   6.19   1.79      -            199.66***   101.93***    -0.67***
#> 3. disp  32 230.72 123.94                  -           6721.16***   -47.06***
#> 4. hp    32 146.69  68.56                              -            -16.45** 
#> 5. drat  32   3.60   0.53                                          -         
#> 6. wt    32   3.22   0.98                                                    
#> 7. qsec  32  17.85   1.79                                                    
#> 8. vs    32   0.44   0.50                                                    
#> 9. am    32   0.41   0.50                                                    
#> 10. gear 32   3.69   0.74                                                    
#> 11. carb 32   2.81   1.62                                                    
#>          6.          7.          8.          9.          10.        
#> 1. mpg      -5.12***     4.51*       2.02***     1.80***     2.14** 
#> 2. cyl       1.37***    -1.89***    -0.73***    -0.47**     -0.65** 
#> 3. disp    107.68***   -96.05*     -44.38***   -36.56***   -50.80***
#> 4. hp       44.19***   -86.77***   -24.99***    -8.32       -6.36   
#> 5. drat     -0.37***     0.09        0.12*       0.19***     0.28***
#> 6. wt     -             -0.31       -0.27***    -0.34***    -0.42***
#> 7. qsec               -              0.67***    -0.20       -0.28   
#> 8. vs                             -              0.04        0.08   
#> 9. am                                         -              0.29***
#> 10. gear                                                  -         
#> 11. carb                                                            
#>          11.        
#> 1. mpg      -5.36** 
#> 2. cyl       1.52** 
#> 3. disp     79.07*  
#> 4. hp       83.04***
#> 5. drat     -0.08   
#> 6. wt        0.68*  
#> 7. qsec     -1.89***
#> 8. vs       -0.46***
#> 9. am        0.05   
#> 10. gear     0.33   
#> 11. carb  -         
#> 
#> Percentage of coverage for each pairwise covariance or correlation
#> 
#>      mpg cyl disp hp drat wt qsec vs am gear carb
#> mpg  1   1   1    1  1    1  1    1  1  1    1   
#> cyl      1   1    1  1    1  1    1  1  1    1   
#> disp         1    1  1    1  1    1  1  1    1   
#> hp                1  1    1  1    1  1  1    1   
#> drat                 1    1  1    1  1  1    1   
#> wt                        1  1    1  1  1    1   
#> qsec                         1    1  1  1    1   
#> vs                                1  1  1    1   
#> am                                   1  1    1   
#> gear                                    1    1   
#> carb                                         1   
APAStyler(m, type = "both", stars = TRUE, file = FALSE)
#>          N  M      SD     1.       2.          3.          4.         
#> 1. mpg   32  20.09   6.03  -          -9.17***  -633.10***  -320.73***
#> 2. cyl   32   6.19   1.79 -0.85***  -            199.66***   101.93***
#> 3. disp  32 230.72 123.94 -0.85***  0.90***     -           6721.16***
#> 4. hp    32 146.69  68.56 -0.78***  0.83***     0.79***     -         
#> 5. drat  32   3.60   0.53  0.68*** -0.70***    -0.71***    -0.45**    
#> 6. wt    32   3.22   0.98 -0.87***  0.78***     0.89***     0.66***   
#> 7. qsec  32  17.85   1.79  0.42*   -0.59***    -0.43*      -0.71***   
#> 8. vs    32   0.44   0.50  0.66*** -0.81***    -0.71***    -0.72***   
#> 9. am    32   0.41   0.50  0.60*** -0.52**     -0.59***    -0.24      
#> 10. gear 32   3.69   0.74  0.48**  -0.49**     -0.56***    -0.13      
#> 11. carb 32   2.81   1.62 -0.55**   0.53**      0.39*       0.75***   
#>          5.          6.          7.          8.          9.         
#> 1. mpg       2.20***    -5.12***     4.51*       2.02***     1.80***
#> 2. cyl      -0.67***     1.37***    -1.89***    -0.73***    -0.47** 
#> 3. disp    -47.06***   107.68***   -96.05*     -44.38***   -36.56***
#> 4. hp      -16.45**     44.19***   -86.77***   -24.99***    -8.32   
#> 5. drat   -             -0.37***     0.09        0.12*       0.19***
#> 6. wt    -0.71***     -             -0.31       -0.27***    -0.34***
#> 7. qsec   0.09       -0.17        -              0.67***    -0.20   
#> 8. vs     0.44*      -0.55***     0.74***     -              0.04   
#> 9. am     0.71***    -0.69***    -0.23        0.17        -         
#> 10. gear  0.70***    -0.58***    -0.21        0.21        0.79***   
#> 11. carb -0.09        0.43*      -0.66***    -0.57***     0.06      
#>          10.         11.        
#> 1. mpg       2.14**     -5.36** 
#> 2. cyl      -0.65**      1.52** 
#> 3. disp    -50.80***    79.07*  
#> 4. hp       -6.36       83.04***
#> 5. drat      0.28***    -0.08   
#> 6. wt       -0.42***     0.68*  
#> 7. qsec     -0.28       -1.89***
#> 8. vs        0.08       -0.46***
#> 9. am        0.29***     0.05   
#> 10. gear  -              0.33   
#> 11. carb  0.27        -         
#> 
#> Percentage of coverage for each pairwise covariance or correlation
#> 
#>      mpg cyl disp hp drat wt qsec vs am gear carb
#> mpg  1   1   1    1  1    1  1    1  1  1    1   
#> cyl      1   1    1  1    1  1    1  1  1    1   
#> disp         1    1  1    1  1    1  1  1    1   
#> hp                1  1    1  1    1  1  1    1   
#> drat                 1    1  1    1  1  1    1   
#> wt                        1  1    1  1  1    1   
#> qsec                         1    1  1  1    1   
#> vs                                1  1  1    1   
#> am                                   1  1    1   
#> gear                                    1    1   
#> carb                                         1   
```
