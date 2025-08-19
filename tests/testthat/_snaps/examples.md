# All example scripts run with consistent output

    Code
      res
    Output
      # A tibble: 10 x 4
            id tag         script    status 
         <dbl> <chr>       <chr>     <chr>  
       1     1 First step  success.R success
       2     2 Second step warning.R warning
       3     3 Second step error.R   error  
       4     4 Python log  prg2.py   success
       5     5 R log       success.R success
       6     6 Step 2      error.R   error  
       7     7 Step 2      prg1.R    success
       8     8 Step 2      prg2.py   success
       9     9 Step 2      success.R success
      10    10 Step 2      warning.R warning

