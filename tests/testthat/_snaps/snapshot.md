# Snapshot: Feed non-rms modelfit

         variable                 coef_95CI Pvalue
    1 (Intercept) 15.369 (14.175 to 16.563) <0.001
    2         age    0.306 (0.294 to 0.319) <0.001
    3         bmi    0.094 (0.055 to 0.134) <0.001

# Snapshot: Warning output for non-rms model without setting exp_coef

    The model fit does not belong to the 'rms' class.
    You must specify the exp_coef argument to determine table output.

# Snapshot: OLS tests - simple model

             variable               coef_95CI Pvalue
    1             age  0.306 (0.293 to 0.319) <0.001
    2             bmi  0.095 (0.056 to 0.135) <0.001
    3      sex=Female                     Ref      -
    4        sex=Male 0.271 (-0.040 to 0.582)  0.087
    5   smoking=Never                     Ref      -
    6  smoking=Former 0.131 (-0.248 to 0.510)  0.499
    7 smoking=Current  0.431 (0.050 to 0.812)  0.027

# Snapshot: OLS tests - model with interactions

       variable                coef_95CI Pvalue
    1       age   0.342 (0.260 to 0.425) <0.001
    2       bmi  0.167 (-0.001 to 0.336)  0.051
    3 age * bmi -0.001 (-0.005 to 0.002)  0.383

# Snapshot: OLS tests - model with splines

    $simple
              variable              coef_95CI Pvalue
    1              bmi 0.095 (0.056 to 0.134) <0.001
    2 RCSoverallP: age                 F test <0.001
    
    $full
      variable               coef_95CI Pvalue
    1      age  0.293 (0.248 to 0.339) <0.001
    2     age' 0.008 (-0.121 to 0.137)  0.904
    3    age'' 0.047 (-0.470 to 0.563)  0.859
    4      bmi  0.095 (0.056 to 0.134) <0.001
    

# Snapshot: OLS tests - model with splines and interactions

    $summary
                      variable               coef_95CI Pvalue
    1                      bmi 0.058 (-0.359 to 0.474)  0.787
    2               sex=Female                     Ref      -
    3                 sex=Male 0.279 (-0.032 to 0.590)  0.079
    4       RCSoverallP: age                    F test <0.001
    5 RCSoverallP: age * bmi                    F test  0.474
    
    $full
         variable                coef_95CI Pvalue
    1         age  0.264 (-0.024 to 0.551)  0.072
    2        age' -0.001 (-0.812 to 0.810)  0.997
    3       age''  0.576 (-2.656 to 3.809)  0.727
    4         bmi  0.058 (-0.359 to 0.474)  0.787
    5  sex=Female                      Ref      -
    6    sex=Male  0.279 (-0.032 to 0.590)  0.079
    7   age * bmi  0.001 (-0.010 to 0.013)  0.841
    8  age' * bmi  0.000 (-0.032 to 0.033)  0.977
    9 age'' * bmi -0.022 (-0.150 to 0.106)  0.739
    

# Snapshot: Complete case OLS with splines and covariates

              variable               coef_95CI Pvalue
    1       sex=Female                     Ref      -
    2         sex=Male 0.272 (-0.039 to 0.582)  0.087
    3    smoking=Never                     Ref      -
    4   smoking=Former 0.124 (-0.255 to 0.504)  0.521
    5  smoking=Current  0.426 (0.045 to 0.808)  0.029
    6 RCSoverallP: age                  F test <0.001
    7 RCSoverallP: bmi                  F test <0.001

# Snapshot: Complete case LRM without x and y

              variable                OR_95CI Pvalue
    1       sex=Female                    Ref      -
    2         sex=Male 1.078 (0.927 to 1.255)  0.329
    3    smoking=Never                    Ref      -
    4   smoking=Former 0.986 (0.806 to 1.207)  0.895
    5  smoking=Current 2.079 (1.731 to 2.496) <0.001
    6 RCSoverallP: age              Wald test <0.001
    7 RCSoverallP: bmi              Wald test  0.014

# Snapshot: Complete case LRM with x and y (LR test)

              variable                OR_95CI Pvalue
    1       sex=Female                    Ref      -
    2         sex=Male 1.078 (0.927 to 1.255)  0.329
    3    smoking=Never                    Ref      -
    4   smoking=Former 0.986 (0.806 to 1.207)  0.895
    5  smoking=Current 2.079 (1.731 to 2.496) <0.001
    6 RCSoverallP: age                LR test <0.001
    7 RCSoverallP: bmi                LR test  0.015

# Snapshot: Complete case CPH without x and y

              variable                HR_95CI Pvalue
    1       sex=Female                    Ref      -
    2         sex=Male 0.943 (0.889 to 1.000)  0.049
    3    smoking=Never                    Ref      -
    4   smoking=Former 0.982 (0.913 to 1.057)  0.633
    5  smoking=Current 2.118 (1.969 to 2.277) <0.001
    6 RCSoverallP: age              Wald test <0.001
    7 RCSoverallP: bmi              Wald test <0.001

# Snapshot: Complete case CPH with x and y (LR test)

              variable                HR_95CI Pvalue
    1       sex=Female                    Ref      -
    2         sex=Male 0.943 (0.889 to 1.000)  0.049
    3    smoking=Never                    Ref      -
    4   smoking=Former 0.982 (0.913 to 1.057)  0.633
    5  smoking=Current 2.118 (1.969 to 2.277) <0.001
    6 RCSoverallP: age                LR test <0.001
    7 RCSoverallP: bmi                LR test <0.001

# Snapshot: LRM tests - simple model

             variable                OR_95CI Pvalue
    1             age 1.096 (1.089 to 1.103) <0.001
    2             bmi 1.031 (1.015 to 1.047) <0.001
    3      sex=Female                    Ref      -
    4        sex=Male 1.023 (0.904 to 1.159)  0.716
    5   smoking=Never                    Ref      -
    6  smoking=Former 1.067 (0.917 to 1.241)  0.403
    7 smoking=Current 1.186 (1.018 to 1.382)  0.028

# Snapshot: LRM tests - model with interactions

       variable                OR_95CI Pvalue
    1       age 1.108 (1.063 to 1.154) <0.001
    2       bmi 1.053 (0.969 to 1.143)  0.222
    3 age * bmi 1.000 (0.998 to 1.001)  0.603

# Snapshot: LRM tests - model with splines

              variable                OR_95CI Pvalue
    1              bmi 1.030 (1.014 to 1.047) <0.001
    2 RCSoverallP: age              Wald test <0.001

# Snapshot: CPH tests - simple model

        variable                HR_95CI Pvalue
    1        age 1.020 (1.018 to 1.023) <0.001
    2 sex=Female                    Ref      -
    3   sex=Male 0.965 (0.910 to 1.023)  0.235

# Snapshot: CPH tests - model with interactions

            variable                HR_95CI Pvalue
    1            age 1.021 (1.018 to 1.025) <0.001
    2     sex=Female                    Ref      -
    3       sex=Male 1.093 (0.846 to 1.411)  0.496
    4 age * sex=Male 0.998 (0.993 to 1.002)  0.327

# Snapshot: CPH tests - model with splines

              variable                HR_95CI Pvalue
    1       sex=Female                    Ref      -
    2         sex=Male 0.965 (0.910 to 1.023)  0.233
    3 RCSoverallP: age                LR test <0.001

# Snapshot: CPH tests - model with splines and interactions

                      variable                HR_95CI Pvalue
    1               sex=Female                    Ref      -
    2                 sex=Male 0.821 (0.419 to 1.607)  0.565
    3       RCSoverallP: age                  LR test <0.001
    4 RCSoverallP: age * sex                  LR test  0.593

# Snapshot: MI OLS with splines and covariates

              variable               coef_95CI Pvalue
    1       sex=Female                     Ref      -
    2         sex=Male 0.271 (-0.061 to 0.604)  0.110
    3    smoking=Never                     Ref      -
    4   smoking=Former 0.222 (-0.181 to 0.625)  0.280
    5  smoking=Current 0.379 (-0.166 to 0.925)  0.173
    6 RCSoverallP: age               Wald test <0.001
    7 RCSoverallP: bmi               Wald test <0.001

# Snapshot: MI LRM with wald test

              variable                OR_95CI Pvalue
    1       sex=Female                    Ref      -
    2         sex=Male 1.094 (0.934 to 1.281)  0.266
    3    smoking=Never                    Ref      -
    4   smoking=Former 0.996 (0.751 to 1.320)  0.978
    5  smoking=Current 1.917 (1.322 to 2.780) <0.001
    6 RCSoverallP: age              Wald test <0.001
    7 RCSoverallP: bmi              Wald test  0.101

# Snapshot: MI CPH with wald test

              variable                HR_95CI Pvalue
    1       sex=Female                    Ref      -
    2         sex=Male 0.968 (0.910 to 1.030)  0.308
    3    smoking=Never                    Ref      -
    4   smoking=Former 0.960 (0.891 to 1.035)  0.290
    5  smoking=Current 1.866 (1.724 to 2.021) <0.001
    6 RCSoverallP: age              Wald test <0.001
    7 RCSoverallP: bmi              Wald test <0.001

# Snapshot: MI LRM with LR test

              variable                OR_95CI Pvalue
    1       sex=Female                    Ref      -
    2         sex=Male 1.094 (0.934 to 1.281)  0.265
    3    smoking=Never                    Ref      -
    4   smoking=Former 0.994 (0.750 to 1.317)  0.965
    5  smoking=Current 1.913 (1.319 to 2.774) <0.001
    6 RCSoverallP: age                LR test <0.001
    7 RCSoverallP: bmi                LR test  0.103

# Snapshot: MI CPH with LR test

              variable                HR_95CI Pvalue
    1       sex=Female                    Ref      -
    2         sex=Male 0.969 (0.911 to 1.030)  0.311
    3    smoking=Never                    Ref      -
    4   smoking=Former 0.960 (0.891 to 1.035)  0.290
    5  smoking=Current 1.866 (1.723 to 2.020) <0.001
    6 RCSoverallP: age                LR test <0.001
    7 RCSoverallP: bmi                LR test <0.001

# Snapshot: Variables with labels and special names

    $summary
                      variable               coef_95CI Pvalue
    1                      bmi 0.058 (-0.358 to 0.475)  0.783
    2               sex=Female                     Ref      -
    3                 sex=Male 0.279 (-0.032 to 0.590)  0.079
    4                  random1 0.067 (-0.089 to 0.223)  0.401
    5                "random2" 0.011 (-0.144 to 0.166)  0.892
    6       RCSoverallP: age                    F test <0.001
    7 RCSoverallP: age * bmi                    F test  0.464
    
    $hidden
                      variable               coef_95CI Pvalue
    1                      bmi 0.058 (-0.358 to 0.475)  0.783
    2               sex=Female                     Ref      -
    3                 sex=Male 0.279 (-0.032 to 0.590)  0.079
    4                  random1 0.067 (-0.089 to 0.223)  0.401
    5                "random2" 0.011 (-0.144 to 0.166)  0.892
    6       RCSoverallP: age                    F test <0.001
    7 RCSoverallP: age * bmi                    F test  0.464
    

# Snapshot: Variables with reserved/special names

    $structure
     [1] "'data.frame':\t5000 obs. of  13 variables:"                                               
     [2] " $ age              : num  33.4 50.5 40.8 52.5 67.1 58.9 58.4 47.2 52.4 64.5 ..."         
     [3] " $ bmi              : num  26.1 21.5 22.4 25 31.4 27.5 21.5 25.7 29.6 23.3 ..."           
     [4] " $ sex              : Factor w/ 2 levels \"Female\",\"Male\": 2 2 1 1 1 1 1 2 2 1 ..."    
     [5] " $ smoking          : Factor w/ 3 levels \"Never\",\"Former\",..: 2 2 2 1 2 2 3 2 1 2 ..."
     [6] " $ majorcomplication: int  0 0 0 0 0 0 0 0 1 0 ..."                                       
     [7] " $ lengthstay       : num  17.9 29.2 39 25.7 30.1 ..."                                    
     [8] " $ time             : num  102.1 223.5 27.3 226.3 19.3 ..."                               
     [9] " $ event            : int  1 1 1 1 1 1 1 0 1 0 ..."                                       
    [10] " $ if               : num  2.371 -0.167 0.927 -0.568 0.225 ..."                           
    [11] "  ..- attr(*, \"label\")= chr \"Random variable with name 'if'\""                         
    [12] " $ for              : num  -1.354 -0.579 -0.861 0.973 0.619 ..."                          
    [13] "  ..- attr(*, \"label\")= chr \"Random variable with name 'for'\""                        
    [14] " $ while            : num  -0.836 -0.221 -2.104 -1.668 -1.098 ..."                        
    [15] "  ..- attr(*, \"label\")= chr \"Random variable with name 'while'\""                      
    [16] " $ TRUE             : num  -0.795 -1.135 0.58 0.518 0.208 ..."                            
    [17] "  ..- attr(*, \"label\")= chr \"Random variable with name 'TRUE'\""                       
    [18] " $ NULL             : num  -0.194 0.258 -0.538 -1.179 0.901 ..."                          
    [19] "  ..- attr(*, \"label\")= chr \"Random variable with name 'NULL'\""                       
    
    $summary
                       variable                coef_95CI Pvalue
    1                       bmi  0.058 (-0.359 to 0.475)  0.784
    2                sex=Female                      Ref      -
    3                  sex=Male  0.277 (-0.034 to 0.588)  0.081
    4                        if -0.041 (-0.197 to 0.114)  0.603
    5                       for  0.011 (-0.145 to 0.166)  0.894
    6                     while  0.016 (-0.138 to 0.171)  0.834
    7                      TRUE -0.090 (-0.247 to 0.067)  0.259
    8                      NULL  0.035 (-0.121 to 0.191)  0.660
    9        RCSoverallP: age                     F test <0.001
    10 RCSoverallP: age * bmi                     F test  0.484
    
    $hidden
                       variable                coef_95CI Pvalue
    1                       bmi  0.058 (-0.359 to 0.475)  0.784
    2                sex=Female                      Ref      -
    3                  sex=Male  0.277 (-0.034 to 0.588)  0.081
    4                        if -0.041 (-0.197 to 0.114)  0.603
    5                       for  0.011 (-0.145 to 0.166)  0.894
    6                     while  0.016 (-0.138 to 0.171)  0.834
    7                      TRUE -0.090 (-0.247 to 0.067)  0.259
    8                      NULL  0.035 (-0.121 to 0.191)  0.660
    9        RCSoverallP: age                     F test <0.001
    10 RCSoverallP: age * bmi                     F test  0.484
    

