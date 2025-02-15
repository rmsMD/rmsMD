# Snapshot: Feed non-rms modelfit

         variable                 coef_95CI Pvalue
    1 (Intercept) 18.672 (17.839 to 19.504) <0.001
    2         Age   0.009 (-0.029 to 0.046)  0.649
    3    ExerNone  -0.178 (-1.007 to 0.650)  0.673
    4    ExerSome  -0.391 (-0.899 to 0.118)  0.132

# Snapshot: Warning output for non-rms model without setting exp_coef

    The model fit does not belong to the 'rms' class. You must specify exp_coef argument to determine table output.

# Snapshot: OLS tests - simple model

         variable                 coef_95CI Pvalue
    1         Age   0.010 (-0.020 to 0.039)  0.523
    2   Exer=None   0.243 (-0.456 to 0.942)  0.495
    3   Exer=Some   0.192 (-0.231 to 0.615)  0.373
    4 Smoke=Never -0.962 (-1.875 to -0.050)  0.039
    5 Smoke=Occas -1.326 (-2.455 to -0.197)  0.021
    6 Smoke=Regul  -0.639 (-1.802 to 0.524)  0.282
    7      Height    0.069 (0.042 to 0.096) <0.001
    8    Sex=Male    1.418 (0.889 to 1.948) <0.001

# Snapshot: OLS tests - model with interactions

             variable                coef_95CI Pvalue
    1             Age  0.031 (-0.025 to 0.087)  0.284
    2       Exer=None  0.342 (-2.391 to 3.076)  0.806
    3       Exer=Some  0.484 (-1.200 to 2.168)  0.573
    4 Age * Exer=None -0.025 (-0.148 to 0.097)  0.685
    5 Age * Exer=Some -0.043 (-0.123 to 0.036)  0.285

# Snapshot: OLS tests - model with splines

    $simple
              variable                coef_95CI Pvalue
    1        Exer=None -0.277 (-1.098 to 0.544)  0.508
    2        Exer=Some -0.389 (-0.893 to 0.115)  0.130
    3 RCSoverallP: Age                RCS terms  0.048
    
    $full
       variable                   coef_95CI Pvalue
    1       Age      1.116 (0.311 to 1.921)  0.007
    2      Age' -35.856 (-64.042 to -7.670)  0.013
    3     Age''  64.472 (13.384 to 115.559)  0.013
    4 Exer=None    -0.277 (-1.098 to 0.544)  0.508
    5 Exer=Some    -0.389 (-0.893 to 0.115)  0.130
    

# Snapshot: OLS tests - model with splines and interactions

    $summary
                       variable                  coef_95CI Pvalue
    1                 Exer=None -3.885 (-50.286 to 42.515)  0.870
    2                 Exer=Some  23.159 (-1.786 to 48.104)  0.069
    3                  Sex=Male     2.120 (1.705 to 2.536) <0.001
    4        RCSoverallP: Age                    RCS terms  0.352
    5 RCSoverallP: Age * Exer                    RCS terms  0.251
    
    $anova
                    Analysis of Variance          Response: Wr.Hnd 
    
     Factor                                    d.f. Partial SS MS         F     
     Age  (Factor+Higher Order Factors)          9   24.15963    2.684403   1.12
      All Interactions                           6   18.97364    3.162273   1.32
      Nonlinear (Factor+Higher Order Factors)    6   20.71297    3.452162   1.44
     Exer  (Factor+Higher Order Factors)         8   19.43046    2.428807   1.01
      All Interactions                           6   18.97364    3.162273   1.32
     Sex                                         1  240.73490  240.734900 100.18
     Age * Exer  (Factor+Higher Order Factors)   6   18.97364    3.162273   1.32
      Nonlinear                                  4   16.44860    4.112149   1.71
      Nonlinear Interaction : f(A,B) vs. AB      4   16.44860    4.112149   1.71
     TOTAL NONLINEAR                             6   20.71297    3.452162   1.44
     TOTAL NONLINEAR + INTERACTION               8   22.91957    2.864946   1.19
     REGRESSION                                 12  294.94439   24.578699  10.23
     ERROR                                     222  533.45536    2.402952       
     P     
     0.3515
     0.2510
     0.2016
     0.4287
     0.2510
     <.0001
     0.2510
     0.1484
     0.1484
     0.2016
     0.3047
     <.0001
           
    
    $full
                variable    coef         SE p_values_raw coef_lower95 coef_upper95
    1          Intercept  -1.577  9.4668067   0.86769457      -20.132       16.978
    2                Age   1.083  0.5367668   0.04355897        0.031        2.135
    3               Age' -36.061 18.6037976   0.05257783      -72.524        0.402
    4              Age''  65.108 33.6822916   0.05323623       -0.908      131.124
    5          Exer=None  -3.885 23.6741331   0.86963707      -50.286       42.515
    6          Exer=Some  23.159 12.7271780   0.06881059       -1.786       48.104
    7           Sex=Male   2.120  0.2118430   0.00000000        1.705        2.536
    8    Age * Exer=None   0.173  1.3340034   0.89673899       -2.441        2.788
    9   Age' * Exer=None  11.499 47.6753225   0.80941131      -81.943      104.941
    10 Age'' * Exer=None -23.242 86.6476008   0.78851887     -193.068      146.584
    11   Age * Exer=Some  -1.305  0.7203212   0.07013261       -2.716        0.107
    12  Age' * Exer=Some  41.329 25.2790729   0.10206835       -8.217       90.875
    13 Age'' * Exer=Some -74.331 45.8213626   0.10476079     -164.140       15.477
       Pvalue                     coef_95CI
    1   0.868    -1.577 (-20.132 to 16.978)
    2   0.044        1.083 (0.031 to 2.135)
    3   0.053    -36.061 (-72.524 to 0.402)
    4   0.053    65.108 (-0.908 to 131.124)
    5   0.870    -3.885 (-50.286 to 42.515)
    6   0.069     23.159 (-1.786 to 48.104)
    7  <0.001        2.120 (1.705 to 2.536)
    8   0.897       0.173 (-2.441 to 2.788)
    9   0.809   11.499 (-81.943 to 104.941)
    10  0.789 -23.242 (-193.068 to 146.584)
    11  0.070      -1.305 (-2.716 to 0.107)
    12  0.102     41.329 (-8.217 to 90.875)
    13  0.105  -74.331 (-164.140 to 15.477)
    

# Snapshot: LRM tests - simple model

    [[1]]
         variable                OR_95CI Pvalue
    1         Age 1.016 (0.968 to 1.066)  0.525
    2   Exer=None 1.780 (0.499 to 6.353)  0.374
    3   Exer=Some 1.094 (0.512 to 2.341)  0.816
    4 Smoke=Never 0.243 (0.046 to 1.278)  0.095
    5 Smoke=Occas 0.313 (0.042 to 2.332)  0.257
    6 Smoke=Regul 0.378 (0.040 to 3.596)  0.397
    7      Height 1.113 (1.053 to 1.176) <0.001
    8    Sex=Male 4.191 (1.812 to 9.693) <0.001
    
    [[2]]
         variable         coef         SE p_values_raw coef_lower95 coef_upper95
    1   Intercept -18.33077498 4.88269444 0.0001738727 -27.90068023  -8.76086974
    2         Age   0.01566175 0.02464243 0.5250631102  -0.03263653   0.06396003
    3   Exer=None   0.57655096 0.64918404 0.3744781633  -0.69582637   1.84892830
    4   Exer=Some   0.09026922 0.38801131 0.8160364175  -0.67021898   0.85075742
    5 Smoke=Never  -1.41368625 0.84650157 0.0949126741  -3.07279885   0.24542635
    6 Smoke=Occas  -1.16227089 1.02511643 0.2568809545  -3.17146217   0.84692039
    7 Smoke=Regul  -0.97346924 1.14972724 0.3971646971  -3.22689323   1.27995476
    8      Height   0.10664130 0.02811940 0.0001491710   0.05152828   0.16175432
    9    Sex=Male   1.43304140 0.42774331 0.0008074248   0.59467992   2.27140288
         OR OR_lower95 OR_upper95 Pvalue                OR_95CI
    1 0.000      0.000      0.000 <0.001 0.000 (0.000 to 0.000)
    2 1.016      0.968      1.066  0.525 1.016 (0.968 to 1.066)
    3 1.780      0.499      6.353  0.374 1.780 (0.499 to 6.353)
    4 1.094      0.512      2.341  0.816 1.094 (0.512 to 2.341)
    5 0.243      0.046      1.278  0.095 0.243 (0.046 to 1.278)
    6 0.313      0.042      2.332  0.257 0.313 (0.042 to 2.332)
    7 0.378      0.040      3.596  0.397 0.378 (0.040 to 3.596)
    8 1.113      1.053      1.176 <0.001 1.113 (1.053 to 1.176)
    9 4.191      1.812      9.693 <0.001 4.191 (1.812 to 9.693)
    

# Snapshot: LRM tests - model with interactions

    [[1]]
             variable                 OR_95CI Pvalue
    1             Age  1.015 (0.954 to 1.081)  0.636
    2       Exer=None 1.203 (0.063 to 22.853)  0.902
    3       Exer=Some  0.795 (0.125 to 5.045)  0.807
    4 Age * Exer=None  0.979 (0.857 to 1.118)  0.753
    5 Age * Exer=Some  0.985 (0.903 to 1.076)  0.743
    
    [[2]]
             variable        coef         SE p_values_raw coef_lower95 coef_upper95
    1       Intercept -0.22014638 0.67367644    0.7438316  -1.54052795   1.10023518
    2             Age  0.01512312 0.03195216    0.6359955  -0.04750196   0.07774820
    3       Exer=None  0.18450125 1.50236388    0.9022597  -2.76007785   3.12908034
    4       Exer=Some -0.22979396 0.94298041    0.8074717  -2.07800159   1.61841367
    5 Age * Exer=None -0.02124625 0.06765539    0.7534935  -0.15384837   0.11135587
    6 Age * Exer=Some -0.01462503 0.04467247    0.7433778  -0.10218147   0.07293141
         OR OR_lower95 OR_upper95 Pvalue                 OR_95CI
    1 0.802      0.214      3.005  0.744  0.802 (0.214 to 3.005)
    2 1.015      0.954      1.081  0.636  1.015 (0.954 to 1.081)
    3 1.203      0.063     22.853  0.902 1.203 (0.063 to 22.853)
    4 0.795      0.125      5.045  0.807  0.795 (0.125 to 5.045)
    5 0.979      0.857      1.118  0.753  0.979 (0.857 to 1.118)
    6 0.985      0.903      1.076  0.743  0.985 (0.903 to 1.076)
    

# Snapshot: LRM tests - model with splines

    [[1]]
              variable                OR_95CI Pvalue
    1        Exer=None 0.710 (0.290 to 1.735)  0.452
    2        Exer=Some 0.574 (0.329 to 1.001)  0.050
    3 RCSoverallP: Age              RCS terms  0.249
    
    [[2]]
              variable        coef        SE p_values_raw coef_lower95
    1        Intercept -16.1365807 8.1890419   0.04878007   -32.186808
    2        Exer=None  -0.3431287 0.4563169   0.45207953    -1.237494
    3        Exer=Some  -0.5551805 0.2836103   0.05028320    -1.111047
    4 RCSoverallP: Age          NA        NA   0.24934100           NA
       coef_upper95    OR OR_lower95 OR_upper95 Pvalue                OR_95CI
    1 -0.0863534525 0.000      0.000      0.917  0.049 0.000 (0.000 to 0.917)
    2  0.5512360420 0.710      0.290      1.735  0.452 0.710 (0.290 to 1.735)
    3  0.0006854949 0.574      0.329      1.001  0.050 0.574 (0.329 to 1.001)
    4            NA  <NA>       <NA>       <NA>  0.249              RCS terms
    

# Snapshot: LRM tests - model with splines and interactions

    [[1]]
                       variable                                 OR_95CI Pvalue
    1                 Exer=None 0.000 (0.000 to 900081085822533120.000)  0.377
    2                 Exer=Some  0.628 (0.000 to 40709328863191848.000)  0.981
    3                  Sex=Male                10.212 (5.392 to 19.340) <0.001
    4        RCSoverallP: Age                                 RCS terms  0.971
    5 RCSoverallP: Age * Exer                                 RCS terms  0.970
    
    [[2]]
                       variable        coef         SE p_values_raw coef_lower95
    1                 Intercept  -4.2727358 14.6340888 7.703085e-01   -32.955023
    2                 Exer=None -33.9360553 38.4074999 3.769236e-01  -109.213372
    3                 Exer=Some  -0.4655606 19.7507681 9.811942e-01   -39.176355
    4                  Sex=Male   2.3235720  0.3258152 9.923173e-13     1.684986
    5        RCSoverallP: Age            NA         NA 9.713791e-01           NA
    6 RCSoverallP: Age * Exer            NA         NA 9.702926e-01           NA
      coef_upper95     OR OR_lower95             OR_upper95 Pvalue
    1    24.409551  0.014      0.000        39896368585.756  0.770
    2    41.341261  0.000      0.000 900081085822533120.000  0.377
    3    38.245234  0.628      0.000  40709328863191848.000  0.981
    4     2.962158 10.212      5.392                 19.340 <0.001
    5           NA   <NA>       <NA>                   <NA>  0.971
    6           NA   <NA>       <NA>                   <NA>  0.970
                                      OR_95CI
    1        0.014 (0.000 to 39896368585.756)
    2 0.000 (0.000 to 900081085822533120.000)
    3  0.628 (0.000 to 40709328863191848.000)
    4                10.212 (5.392 to 19.340)
    5                               RCS terms
    6                               RCS terms
    

# Snapshot: CPH tests - simple model

    [[1]]
      variable                HR_95CI Pvalue
    1      age 1.017 (0.999 to 1.036)  0.065
    2      sex 0.599 (0.431 to 0.831)  0.002
    
    [[2]]
      variable       coef          SE p_values_raw coef_lower95 coef_upper95    HR
    1      age  0.0170443 0.009223246  0.064606428 -0.001032932   0.03512153 1.017
    2      sex -0.5130952 0.167454301  0.002183324 -0.841299641  -0.18489084 0.599
      HR_lower95 HR_upper95 Pvalue                HR_95CI
    1      0.999      1.036  0.065 1.017 (0.999 to 1.036)
    2      0.431      0.831  0.002 0.599 (0.431 to 0.831)
    

# Snapshot: CPH tests - model with interactions

    [[1]]
       variable                 HR_95CI Pvalue
    1       age  1.030 (0.976 to 1.088)  0.279
    2       sex 1.100 (0.099 to 12.219)  0.938
    3 age * sex  0.990 (0.953 to 1.029)  0.618
    
    [[2]]
       variable         coef         SE p_values_raw coef_lower95 coef_upper95
    1       age  0.029991344 0.02768149    0.2786114  -0.02426338   0.08424607
    2       sex  0.094916316 1.22861481    0.9384209  -2.31312447   2.50295710
    3 age * sex -0.009684283 0.01941303    0.6178816  -0.04773312   0.02836455
         HR HR_lower95 HR_upper95 Pvalue                 HR_95CI
    1 1.030      0.976      1.088  0.279  1.030 (0.976 to 1.088)
    2 1.100      0.099     12.219  0.938 1.100 (0.099 to 12.219)
    3 0.990      0.953      1.029  0.618  0.990 (0.953 to 1.029)
    

# Snapshot: CPH tests - model with splines

    [[1]]
              variable                HR_95CI Pvalue
    1              sex 0.603 (0.434 to 0.838)  0.003
    2 RCSoverallP: age              RCS terms  0.135
    
    [[2]]
              variable       coef        SE p_values_raw coef_lower95 coef_upper95
    1              sex -0.5054827 0.1678026  0.002592198   -0.8343697   -0.1765958
    2 RCSoverallP: age         NA        NA  0.134956813           NA           NA
         HR HR_lower95 HR_upper95 Pvalue                HR_95CI
    1 0.603      0.434      0.838  0.003 0.603 (0.434 to 0.838)
    2  <NA>       <NA>       <NA>  0.135              RCS terms
    

# Snapshot: CPH tests - model with splines and interactions

    [[1]]
                      variable                  HR_95CI Pvalue
    1                      sex 0.048 (0.000 to 131.415)  0.452
    2       RCSoverallP: age                  RCS terms  0.375
    3 RCSoverallP: age * sex                  RCS terms  0.839
    
    [[2]]
                      variable     coef      SE p_values_raw coef_lower95
    1                      sex -3.03409 4.03704    0.4523139    -10.94654
    2       RCSoverallP: age         NA      NA    0.3753547           NA
    3 RCSoverallP: age * sex         NA      NA    0.8386442           NA
      coef_upper95    HR HR_lower95 HR_upper95 Pvalue                  HR_95CI
    1     4.878363 0.048      0.000    131.415  0.452 0.048 (0.000 to 131.415)
    2           NA  <NA>       <NA>       <NA>  0.375                RCS terms
    3           NA  <NA>       <NA>       <NA>  0.839                RCS terms
    

# Snapshot: Variables with labels and special names

    $summary
                       variable                  coef_95CI Pvalue
    1                 Exer=None -3.870 (-50.426 to 42.685)  0.871
    2                 Exer=Some  22.829 (-2.232 to 47.891)  0.074
    3                  Sex=Male     2.113 (1.696 to 2.530) <0.001
    4                   random1   -0.066 (-0.283 to 0.151)  0.549
    5                 "random2"   -0.063 (-0.264 to 0.137)  0.537
    6        RCSoverallP: Age                    RCS terms  0.372
    7 RCSoverallP: Age * Exer                    RCS terms  0.274
    
    $anova
                    Analysis of Variance          Response: Wr.Hnd 
    
     Factor                                    d.f. Partial SS  MS          F    
     Age  (Factor+Higher Order Factors)          9   23.6940924   2.6326769  1.09
      All Interactions                           6   18.3652812   3.0608802  1.27
      Nonlinear (Factor+Higher Order Factors)    6   20.4981213   3.4163536  1.41
     Exer  (Factor+Higher Order Factors)         8   18.9434487   2.3679311  0.98
      All Interactions                           6   18.3652812   3.0608802  1.27
     Sex                                         1  238.6481748 238.6481748 98.77
     random1                                     1    0.8668897   0.8668897  0.36
     "random2"                                   1    0.9203942   0.9203942  0.38
     Age * Exer  (Factor+Higher Order Factors)   6   18.3652812   3.0608802  1.27
      Nonlinear                                  4   16.0954704   4.0238676  1.67
      Nonlinear Interaction : f(A,B) vs. AB      4   16.0954704   4.0238676  1.67
     TOTAL NONLINEAR                             6   20.4981213   3.4163536  1.41
     TOTAL NONLINEAR + INTERACTION               8   22.5593168   2.8199146  1.17
     REGRESSION                                 14  296.8126578  21.2009041  8.77
     ERROR                                     220  531.5870869   2.4163049      
     P     
     0.3715
     0.2738
     0.2103
     0.4525
     0.2738
     <.0001
     0.5498
     0.5378
     0.2738
     0.1591
     0.1591
     0.2103
     0.3202
     <.0001
           
    
    $hidden
                       variable                  coef_95CI Pvalue
    1                 Exer=None -3.870 (-50.426 to 42.685)  0.871
    2                 Exer=Some  22.829 (-2.232 to 47.891)  0.074
    3                  Sex=Male     2.113 (1.696 to 2.530) <0.001
    4                   random1   -0.066 (-0.283 to 0.151)  0.549
    5                 "random2"   -0.063 (-0.264 to 0.137)  0.537
    6        RCSoverallP: Age                    RCS terms  0.372
    7 RCSoverallP: Age * Exer                    RCS terms  0.274
    

# Snapshot: Variables with reserved/special names

    $structure
     [1] "'data.frame':\t237 obs. of  18 variables:"                                        
     [2] " $ Sex    : Factor w/ 2 levels \"Female\",\"Male\": 1 2 2 2 2 1 2 1 2 2 ..."      
     [3] " $ Wr.Hnd : num  18.5 19.5 18 18.8 20 18 17.7 17 20 18.5 ..."                     
     [4] " $ NW.Hnd : num  18 20.5 13.3 18.9 20 17.7 17.7 17.3 19.5 18.5 ..."               
     [5] " $ W.Hnd  : Factor w/ 2 levels \"Left\",\"Right\": 2 1 2 2 2 2 2 2 2 2 ..."       
     [6] " $ Fold   : Factor w/ 3 levels \"L on R\",\"Neither\",..: 3 3 1 3 2 1 1 3 3 3 ..."
     [7] " $ Pulse  : int  92 104 87 NA 35 64 83 74 72 90 ..."                              
     [8] " $ Clap   : Factor w/ 3 levels \"Left\",\"Neither\",..: 1 1 2 2 3 3 3 3 3 3 ..."  
     [9] " $ Exer   : Factor w/ 3 levels \"Freq\",\"None\",..: 3 2 2 2 3 3 1 1 3 3 ..."     
    [10] " $ Smoke  : Factor w/ 4 levels \"Heavy\",\"Never\",..: 2 4 3 2 2 2 2 2 2 2 ..."   
    [11] " $ Height : num  173 178 NA 160 165 ..."                                          
    [12] " $ M.I    : Factor w/ 2 levels \"Imperial\",\"Metric\": 2 1 NA 2 2 1 1 2 2 2 ..." 
    [13] " $ Age    : num  18.2 17.6 16.9 20.3 23.7 ..."                                    
    [14] " $ high_wr: Factor w/ 2 levels \"FALSE\",\"TRUE\": 1 2 1 2 2 1 1 1 2 1 ..."       
    [15] " $ if     : num  -0.214 0.152 1.712 -0.326 0.373 ..."                             
    [16] "  ..- attr(*, \"label\")= chr \"Random variable with name 'if'\""                 
    [17] " $ for    : num  -0.2128 -0.0936 -0.0867 1.4415 1.1251 ..."                       
    [18] "  ..- attr(*, \"label\")= chr \"Random variable with name 'for'\""                
    [19] " $ while  : num  -0.272 -1.214 -0.141 -1.005 0.156 ..."                           
    [20] "  ..- attr(*, \"label\")= chr \"Random variable with name 'while'\""              
    [21] " $ TRUE   : num  1.168 -0.823 -0.307 1.44 -2.199 ..."                             
    [22] "  ..- attr(*, \"label\")= chr \"Random variable with name 'TRUE'\""               
    [23] " $ NULL   : num  0.29256 0.66875 -0.59418 1.58043 -0.00399 ..."                   
    [24] "  ..- attr(*, \"label\")= chr \"Random variable with name 'NULL'\""               
    
    $summary
                        variable                  coef_95CI Pvalue
    1                  Exer=None -1.015 (-48.194 to 46.163)  0.966
    2                  Exer=Some  24.099 (-1.229 to 49.426)  0.062
    3                   Sex=Male     2.127 (1.705 to 2.549) <0.001
    4                         if   -0.190 (-0.407 to 0.027)  0.085
    5                        for   -0.028 (-0.228 to 0.173)  0.787
    6                      while   -0.058 (-0.269 to 0.153)  0.592
    7                       TRUE   -0.028 (-0.248 to 0.191)  0.802
    8                       NULL    0.057 (-0.147 to 0.262)  0.582
    9         RCSoverallP: Age                    RCS terms  0.485
    10 RCSoverallP: Age * Exer                    RCS terms  0.340
    
    $anova
                    Analysis of Variance          Response: Wr.Hnd 
    
     Factor                                    d.f. Partial SS  MS          F    
     Age  (Factor+Higher Order Factors)          9   20.5649157   2.2849906  0.95
      All Interactions                           6   16.5091568   2.7515261  1.14
      Nonlinear (Factor+Higher Order Factors)    6   18.3360583   3.0560097  1.27
     Exer  (Factor+Higher Order Factors)         8   17.3234399   2.1654300  0.90
      All Interactions                           6   16.5091568   2.7515261  1.14
     Sex                                         1  235.4172620 235.4172620 97.55
     if                                          1    7.1377957   7.1377957  2.96
     for                                         1    0.1755770   0.1755770  0.07
     while                                       1    0.6925481   0.6925481  0.29
     TRUE                                        1    0.1523873   0.1523873  0.06
     NULL                                        1    0.7307896   0.7307896  0.30
     Age * Exer  (Factor+Higher Order Factors)   6   16.5091568   2.7515261  1.14
      Nonlinear                                  4   14.6677962   3.6669491  1.52
      Nonlinear Interaction : f(A,B) vs. AB      4   14.6677962   3.6669491  1.52
     TOTAL NONLINEAR                             6   18.3360583   3.0560097  1.27
     TOTAL NONLINEAR + INTERACTION               8   19.9941540   2.4992692  1.04
     REGRESSION                                 17  304.6896870  17.9229228  7.43
     ERROR                                     217  523.7100577   2.4134104      
     P     
     0.4854
     0.3401
     0.2741
     0.5196
     0.3401
     <.0001
     0.0869
     0.7876
     0.5927
     0.8018
     0.5827
     0.3401
     0.1975
     0.1975
     0.2741
     0.4102
     <.0001
           
    
    $hidden
                        variable                  coef_95CI Pvalue
    1                  Exer=None -1.015 (-48.194 to 46.163)  0.966
    2                  Exer=Some  24.099 (-1.229 to 49.426)  0.062
    3                   Sex=Male     2.127 (1.705 to 2.549) <0.001
    4                         if   -0.190 (-0.407 to 0.027)  0.085
    5                        for   -0.028 (-0.228 to 0.173)  0.787
    6                      while   -0.058 (-0.269 to 0.153)  0.592
    7                       TRUE   -0.028 (-0.248 to 0.191)  0.802
    8                       NULL    0.057 (-0.147 to 0.262)  0.582
    9         RCSoverallP: Age                    RCS terms  0.485
    10 RCSoverallP: Age * Exer                    RCS terms  0.340
    

