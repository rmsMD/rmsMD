# Snapshot: Feed non-rms modelfit

         variable                 coef_95CI Pvalue
    1 (Intercept) 15.317 (14.124 to 16.509) <0.001
    2         age    0.303 (0.290 to 0.316) <0.001
    3         bmi    0.101 (0.062 to 0.140) <0.001

# Snapshot: Warning output for non-rms model without setting exp_coef

    The model fit does not belong to the 'rms' class. You must specify exp_coef argument to determine table output.

# Snapshot: OLS tests - simple model

             variable                coef_95CI Pvalue
    1             age   0.303 (0.290 to 0.316) <0.001
    2             bmi   0.102 (0.063 to 0.141) <0.001
    3      sex=Female                      Ref      -
    4        sex=Male -0.164 (-0.475 to 0.146)  0.299
    5   smoking=Never                      Ref      -
    6  smoking=Former -0.135 (-0.513 to 0.244)  0.486
    7 smoking=Current   0.418 (0.037 to 0.799)  0.031

# Snapshot: OLS tests - model with interactions

       variable                coef_95CI Pvalue
    1       age   0.323 (0.241 to 0.405) <0.001
    2       bmi  0.142 (-0.026 to 0.310)  0.098
    3 age * bmi -0.001 (-0.004 to 0.002)  0.624

# Snapshot: OLS tests - model with splines

    $simple
              variable              coef_95CI Pvalue
    1              bmi 0.100 (0.061 to 0.139) <0.001
    2 RCSoverallP: age              RCS terms <0.001
    
    $full
      variable                 coef_95CI Pvalue
    1      age    0.343 (0.297 to 0.388) <0.001
    2     age' -0.144 (-0.273 to -0.015)  0.029
    3    age''    0.602 (0.086 to 1.117)  0.022
    4      bmi    0.100 (0.061 to 0.139) <0.001
    

# Snapshot: OLS tests - model with splines and interactions

    $summary
                      variable                coef_95CI Pvalue
    1                      bmi  0.228 (-0.188 to 0.644)  0.283
    2               sex=Female                      Ref      -
    3                 sex=Male -0.153 (-0.464 to 0.157)  0.334
    4       RCSoverallP: age                  RCS terms <0.001
    5 RCSoverallP: age * bmi                  RCS terms  0.678
    
    $full
         variable                coef_95CI Pvalue
    1         age   0.436 (0.149 to 0.723)  0.003
    2        age' -0.503 (-1.313 to 0.307)  0.223
    3       age''  2.269 (-0.958 to 5.497)  0.168
    4         bmi  0.228 (-0.188 to 0.644)  0.283
    5  sex=Female                      Ref      -
    6    sex=Male -0.153 (-0.464 to 0.157)  0.334
    7   age * bmi -0.004 (-0.015 to 0.008)  0.515
    8  age' * bmi  0.014 (-0.018 to 0.047)  0.377
    9 age'' * bmi -0.067 (-0.195 to 0.061)  0.304
    

# Snapshot: LRM tests - simple model

             variable                OR_95CI Pvalue
    1             age 1.094 (1.087 to 1.101) <0.001
    2             bmi 1.035 (1.019 to 1.051) <0.001
    3      sex=Female                    Ref      -
    4        sex=Male 0.955 (0.844 to 1.081)  0.467
    5   smoking=Never                    Ref      -
    6  smoking=Former 0.979 (0.842 to 1.138)  0.783
    7 smoking=Current 1.165 (1.001 to 1.356)  0.049

# Snapshot: LRM tests - model with interactions

       variable                OR_95CI Pvalue
    1       age 1.085 (1.043 to 1.130) <0.001
    2       bmi 1.018 (0.939 to 1.105)  0.662
    3 age * bmi 1.000 (0.999 to 1.002)  0.705

# Snapshot: LRM tests - model with splines

              variable                OR_95CI Pvalue
    1              bmi 1.034 (1.018 to 1.050) <0.001
    2 RCSoverallP: age              RCS terms <0.001

# Snapshot: CPH tests - simple model

        variable                HR_95CI Pvalue
    1        age 0.999 (0.996 to 1.002)  0.507
    2 sex=Female                    Ref      -
    3   sex=Male 1.005 (0.940 to 1.074)  0.884

# Snapshot: CPH tests - model with interactions

            variable                HR_95CI Pvalue
    1            age 0.999 (0.995 to 1.003)  0.588
    2     sex=Female                    Ref      -
    3       sex=Male 0.991 (0.747 to 1.314)  0.947
    4 age * sex=Male 1.000 (0.995 to 1.006)  0.918

# Snapshot: CPH tests - model with splines

              variable                HR_95CI Pvalue
    1       sex=Female                    Ref      -
    2         sex=Male 1.005 (0.940 to 1.074)  0.881
    3 RCSoverallP: age              RCS terms  0.920

# Snapshot: CPH tests - model with splines and interactions

                      variable                HR_95CI Pvalue
    1               sex=Female                    Ref      -
    2                 sex=Male 1.954 (0.967 to 3.949)  0.062
    3       RCSoverallP: age                RCS terms  0.551
    4 RCSoverallP: age * sex                RCS terms  0.228

# Snapshot: Variables with labels and special names

    $summary
                      variable                coef_95CI Pvalue
    1                      bmi  0.233 (-0.184 to 0.649)  0.273
    2               sex=Female                      Ref      -
    3                 sex=Male -0.156 (-0.466 to 0.155)  0.326
    4                  random1 -0.051 (-0.207 to 0.105)  0.524
    5                "random2"  0.150 (-0.005 to 0.305)  0.057
    6       RCSoverallP: age                  RCS terms <0.001
    7 RCSoverallP: age * bmi                  RCS terms  0.667
    
    $hidden
                      variable                coef_95CI Pvalue
    1                      bmi  0.233 (-0.184 to 0.649)  0.273
    2               sex=Female                      Ref      -
    3                 sex=Male -0.156 (-0.466 to 0.155)  0.326
    4                  random1 -0.051 (-0.207 to 0.105)  0.524
    5                "random2"  0.150 (-0.005 to 0.305)  0.057
    6       RCSoverallP: age                  RCS terms <0.001
    7 RCSoverallP: age * bmi                  RCS terms  0.667
    

# Snapshot: Variables with reserved/special names

    $structure
     [1] "'data.frame':\t5000 obs. of  13 variables:"                                               
     [2] " $ age              : num  33.4 50.5 40.8 52.5 67.1 58.9 58.4 47.2 52.4 64.5 ..."         
     [3] " $ bmi              : num  26.1 21.5 22.4 25 31.4 27.5 21.5 25.7 29.6 23.3 ..."           
     [4] " $ sex              : Factor w/ 2 levels \"Female\",\"Male\": 2 2 1 1 1 1 1 2 2 1 ..."    
     [5] " $ smoking          : Factor w/ 3 levels \"Never\",\"Former\",..: 2 2 2 1 2 2 3 2 1 2 ..."
     [6] " $ majorcomplication: int  0 0 0 0 0 0 0 0 1 0 ..."                                       
     [7] " $ lengthstay       : num  29.7 33 31 34.1 38.2 ..."                                      
     [8] " $ time             : num  44.4 47.7 65.6 50.7 51.3 ..."                                  
     [9] " $ status           : int  1 1 0 1 1 1 0 0 0 1 ..."                                       
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
    1                       bmi  0.230 (-0.186 to 0.646)  0.279
    2                sex=Female                      Ref      -
    3                  sex=Male -0.148 (-0.458 to 0.163)  0.352
    4                        if  0.044 (-0.111 to 0.200)  0.575
    5                       for -0.107 (-0.262 to 0.048)  0.176
    6                     while -0.083 (-0.237 to 0.071)  0.292
    7                      TRUE  0.084 (-0.072 to 0.241)  0.292
    8                      NULL  0.015 (-0.141 to 0.170)  0.855
    9        RCSoverallP: age                  RCS terms <0.001
    10 RCSoverallP: age * bmi                  RCS terms  0.679
    
    $hidden
                       variable                coef_95CI Pvalue
    1                       bmi  0.230 (-0.186 to 0.646)  0.279
    2                sex=Female                      Ref      -
    3                  sex=Male -0.148 (-0.458 to 0.163)  0.352
    4                        if  0.044 (-0.111 to 0.200)  0.575
    5                       for -0.107 (-0.262 to 0.048)  0.176
    6                     while -0.083 (-0.237 to 0.071)  0.292
    7                      TRUE  0.084 (-0.072 to 0.241)  0.292
    8                      NULL  0.015 (-0.141 to 0.170)  0.855
    9        RCSoverallP: age                  RCS terms <0.001
    10 RCSoverallP: age * bmi                  RCS terms  0.679
    

