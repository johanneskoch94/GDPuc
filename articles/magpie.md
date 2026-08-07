# Converting magpie objects

`convertGDP` also accepts a magpie object (see the
[magclass](https://github.com/pik-piam/magclass) package) for the `gdp`
argument.

``` r

library(GDPuc)
if (rlang::is_installed("magclass")) {
  my_gdp <- magclass::new.magpie(
    cells_and_regions = c("USA", "FRA"), 
    years = 2010:2014, 
    names = "gdp",
    sets = c("iso3c", "year", "data"),
    fill = 100:109
  )
  my_gdp
  
  convertGDP(
    gdp = my_gdp, 
    unit_in = "constant 2005 LCU", 
    unit_out = "constant 2017 Int$PPP"
  )
}
#> An object of class "magpie"
#> , , data = gdp
#> 
#>      year
#> iso3c    y2010    y2011    y2012    y2013    y2014
#>   USA 122.6146 125.0669 127.5192 129.9715 132.4237
#>   FRA 149.7553 152.7208 155.6862 158.6517 161.6171
```

`convertGDP` does not change or add the “unit” attribute of the magpie
object.
