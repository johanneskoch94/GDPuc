# Helpers for checking the magpie conversion factor path against the generic one.
#
# A data frame never takes the factor path -- use_factors_for_magpie() is FALSE for anything that is not
# a magpie -- so the same object melted into long form and converted is a genuine generic-path reference.
# That makes it possible to run one and the same object down both paths and compare, without the package
# having to expose a switch for it.

# Melt a magpie object into the long form convertGDP() accepts: one row per value, the reporting country
# in a column named "iso3c", and the years left numeric in a column named "year". The columns are named
# after the sets of the object, which need not be spelled the way convertGDP() looks for them, so both are
# renamed by position: the reporting country is the first spatial sub-dimension, and the years follow the
# spatial ones.
as_long <- function(x) {
  tb <- tibble::as_tibble(x)
  names(tb)[1] <- "iso3c"
  if (!is.null(magclass::getYears(x))) {
    names(tb)[magclass::ndim(x, dim = 1) + 1] <- "year"
  }
  dplyr::mutate(tb, dplyr::across(tidyselect::where(is.factor), as.character))
}

# Convert gdp both ways and compare value by value, joining on the dimension columns rather than
# relying on row order.
expect_matches_long_form <- function(gdp, ..., label = NULL) {
  fast <- convertGDP(gdp, ...)
  slow <- convertGDP(as_long(gdp), ...)

  joinBy <- setdiff(names(slow), "value")
  cmp <- dplyr::inner_join(dplyr::rename(as_long(fast), "fastValue" = "value"), slow, by = joinBy)

  testthat::expect_equal(nrow(cmp), length(gdp), label = label)
  testthat::expect_equal(cmp$fastValue, cmp$value, label = label)
  invisible(fast)
}
