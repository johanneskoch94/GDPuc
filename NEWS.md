# GDPuc 1.0.4

* Improve internal column detection
* Allow magclass objects without years

# GDPuc 1.0.0

* Update wdi conversion factors and data
* Add the CPI as possible deflator
* Add two shortcut functions for using the CPI as deflator and convert a single value (fixed #20)
* Add example to convertGDP (fixes #21)
* Add options to convert to constant € and use only US deflator
* Add with_USA option to replace_NAs
* Relax restriction on presence of a year column (fixes #17) 
* Improve order of tests
* Use magclass::as_tibble() instead of custom function

# GDPuc 0.11.1

* Clean up Suggest field, in order to comply with CRAN specifications
* Improve documentation

# GDPuc 0.11.0

* Fix issue #15. The cfs data-frame returned when "return_cfs = TRUE" now displays the correct conversion factors.
* Get rid of dplyr warnings and improve tests

# GDPuc 0.10.0

* Get rid of tidyselect warnings and improve tests

# GDPuc 0.9.2

* Transfer base repo to the pik-piam organization and update documentation links accordingly.

# GDPuc 0.9.0

* Add option GDPuc.warn and corresponding vignette.

* Add `replace_NAs` argument option NA, to silence warning about NA creation.

# GDPuc 0.8.0

* Add options "no_conversion" for the `replace_NAs` argument.

* The `replace_NAs` argument can now take a vector allowing for a combination of options.

# GDPuc 0.7.0

* Add argument `return_cfs` to return the conversion factors used.

* Add options "linear" and "linear_regional_average" for the `replace_NAs` argument.

* Add vignettes

* Remove dependency on the stringr package

* Improve tests

# GDPuc 0.5.1

First public release.
