# attach_score() is working for aov

    Code
      print(ex.score_obj@results)
    Output
      # A tibble: 5 x 4
        name  score outcome    predictor   
        <chr> <dbl> <chr>      <chr>       
      1 fstat  94.5 Sale_Price MS_SubClass 
      2 fstat 115.  Sale_Price MS_Zoning   
      3 fstat  NA   Sale_Price Lot_Frontage
      4 fstat  NA   Sale_Price Lot_Area    
      5 fstat  22.9 Sale_Price Street      

