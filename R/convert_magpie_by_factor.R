# Should a magpie object be converted through its conversion factors?
#
# Only worth it when the object holds more than one value per country and year, i.e. when it has a
# second spatial sub-dimension or any data dimensions. With one value per country and year there is
# nothing to collapse, and the generic code path is used, which keeps its results bit-identical.
#
# Regional data is excluded because with_regions disaggregates regions into countries by weighing them
# with their GDP share, which changes the values and the number of rows, and re-aggregates afterwards.
# return_cfs is excluded because it re-runs the conversion anyway.
use_factors_for_magpie <- function(gdp, with_regions, return_cfs) {
  if (!inherits(gdp, "magpie") || !is.null(with_regions) || return_cfs) {
    return(FALSE)
  }
  if (!rlang::is_installed("magclass")) {
    return(FALSE)
  }
  if (any(dim(gdp) == 0)) {
    return(FALSE)
  }
  nFactors <- length(unique(magclass::getItems(gdp, dim = 1.1))) * max(1L, length(magclass::getYears(gdp)))
  length(gdp) > nFactors
}


# Convert a magpie object by scaling it with its conversion factors
#
# Every elemental conversion step multiplies or divides the value column by a factor that depends only
# on the country and the year, so the conversion is linear in the data. The generic code path however
# melts the object into a long data frame holding one row per value before converting it. For a magpie
# object with a second spatial sub-dimension or several data dimensions -- a bilateral trade matrix has
# one row per reporter x partner x year x item -- that data frame is orders of magnitude larger than the
# object itself, and building it, joining the conversion factors onto it row by row and casting the
# result back dominate both runtime and memory.
#
# So instead the conversion is run on an object holding a single 1 per country and year, which yields
# the conversion factors, and the data is scaled with those. Recursing into convertGDP() rather than
# reaching into the conversion functions directly means the argument checking, source adaptation,
# verbosity and NA handling all stay in one place, and the factors are by construction the same ones the
# generic path would have computed.
#
# One deliberate difference: without replace_NAs, the warning about countries lacking conversion factors
# is raised for every such country, whereas the generic path only raises it if that country also has
# data that is not NA.
convert_magpie_by_factor <- function(gdp,
                                     unit_in,
                                     unit_out,
                                     source,
                                     use_USA_cf_for_all,
                                     replace_NAs,
                                     verbose,
                                     iso3c_column,
                                     year_column) {
  iso3c <- unique(magclass::getItems(gdp, dim = 1.1))

  cf <- magclass::new.magpie(cells_and_regions = iso3c, years = magclass::getYears(gdp), fill = 1)
  magclass::getSets(cf) <- c("iso3c", "year", "data")

  cf <- convertGDP(gdp = cf,
                   unit_in = unit_in,
                   unit_out = unit_out,
                   source = source,
                   use_USA_cf_for_all = use_USA_cf_for_all,
                   with_regions = NULL,
                   replace_NAs = replace_NAs,
                   verbose = verbose,
                   return_cfs = FALSE,
                   iso3c_column = iso3c_column,
                   year_column = year_column)

  # Line the factors up with the spatial dimension of gdp by country code, so that the multiplication
  # below matches positionally and does not depend on how the sets of gdp happen to be named. The
  # expanded factors stay one value wide in the data dimension.
  cf <- cf[match(magclass::getItems(gdp, dim = 1.1, full = TRUE), magclass::getItems(cf, dim = 1)), , ]
  magclass::getItems(cf, dim = 1, raw = TRUE) <- magclass::getItems(gdp, dim = 1)
  cf <- magclass::collapseDim(cf, dim = 3)

  x <- gdp * cf

  # The generic path replaces every NA of the result, including those that were already NA in gdp.
  # Guarding this with anyNA() to skip the copy when there is nothing to replace does not pay off:
  # magclass defines no anyNA() method, so anyNA() coerces the object to a vector and costs a full
  # copy of its own -- exactly what the guard was meant to save.
  if (!is.null(replace_NAs) && 0 %in% replace_NAs) x[is.na(x)] <- 0

  magclass::getSets(x) <- magclass::getSets(gdp)
  x
}
