# Biocompute object created correctly

    Code
      str(bco$io_domain)
    Output
      List of 2
       $ input_subdomain :List of 2
        ..$ :List of 1
        .. ..$ uri:List of 1
        .. .. ..$ uri: chr "my_project/params.yml"
        ..$ :List of 1
        .. ..$ uri:List of 1
        .. .. ..$ uri: chr "my_data/data.rds"
       $ output_subdomain:List of 2
        ..$ :List of 2
        .. ..$ mediatype: chr "text/txt"
        .. ..$ uri      :List of 2
        .. .. ..$ filename: chr "output.txt"
        .. .. ..$ uri     : chr "my_output/output.txt"
        ..$ :List of 2
        .. ..$ mediatype: chr " "
        .. ..$ uri      :List of 2
        .. .. ..$ filename: chr "plot.png"
        .. .. ..$ uri     : chr "my_output/plot.png"

