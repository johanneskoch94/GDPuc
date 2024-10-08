# Transform user input for package internal use
transform_user_input <- function(gdp, unit_in, unit_out, source, use_USA_cf_for_all, with_regions, replace_NAs) {
  . <- NULL

  # Convert to tibble, if necessary
  if (class(gdp)[1] == "magpie") {
    # Check if the magpie object has 2 spatial dimensions
    spat2 <- magclass::ndim(gdp, dim = 1) == 2
    # Check if the magpie object has year info
    hasYears <- !is.null(magclass::getYears(gdp))
    # Transform to tibble and rename columns
    gdp <- gdp %>% tibble::as_tibble() %>% dplyr::rename("iso3c" = 1)
    if (spat2) gdp <- dplyr::rename(gdp, "spatial2" = 2)
    if (hasYears && !spat2) gdp <- dplyr::rename(gdp, "year" = 2)
    if (hasYears && spat2) gdp <- dplyr::rename(gdp, "year" = 3)
  }

  # Extract base years if they exist, and adjust string
  if (grepl("constant", unit_in)) {
    base_x <- regmatches(unit_in, regexpr("[[:digit:]]{4}", unit_in)) %>% as.double()
    unit_in <- sub(base_x, "YYYY", unit_in) %>% paste0(" base x")
  }
  if (grepl("constant", unit_out)) {
    base_y <- regmatches(unit_out, regexpr("[[:digit:]]{4}", unit_out)) %>% as.double()
    unit_out <- sub(base_y, "YYYY", unit_out) %>% paste0(" base y")
  }
  require_year_column <- any(grepl("current", c(unit_in, unit_out)))

  # Rename columns if necessary
  if (! "iso3c" %in% colnames(gdp)) {
    i_iso3c <- smart_select_iso3c(gdp)
    if (length(i_iso3c) != 1) {
      abort("Invalid 'gdp' argument. `gdp` has no 'iso3c' column, and no other \\
               column could be identified in its stead.")
    }
    warn("No 'iso3c' column in 'gdp' argument. Using '{i_iso3c}' column instead.")
    gdp <- dplyr::rename(gdp, "iso3c" = !!rlang::sym(i_iso3c))
  }
  if (require_year_column && ! "year" %in% colnames(gdp)) {
    i_year <- smart_select_year(gdp)
    if (length(i_year) != 1) {
      abort("Invalid 'gdp' argument. 'gdp' does not have a 'year' column, required when converting current values, \\
             and no other column could be identified in its stead.")
    }
    warn("No 'year' column in 'gdp' argument. Using '{i_year}' column instead.")
    gdp <- dplyr::rename(gdp, "year" = !!rlang::sym(i_year))
  }

  # Evaluate source (same steps as performed in check_source)
  source_name <- if (is.character(source)) source else "user_provided"
  source <- check_source(source)

  # If a region mapping is available and a region code (that isn't a
  # country-region) is detected, replace the region with the countries it
  # comprises.
  if (!is.null(with_regions) &&
      any(gdp$iso3c %in% with_regions$region & !gdp$iso3c %in% with_regions$iso3c)) {
    gdp <- replace_regions_with_countries(gdp, unit_in, base_x, with_regions, source)
  }

  # Need this to check for existence of base_y and base_x
  this_e <- environment()

  # Check that base_y and base_x years exist in the source
  if (exists("base_y", envir = this_e, inherits = FALSE) && !base_y %in% source$year) {
    abort("Incompatible 'unit_out' and 'source'. No information in source {crayon::bold(source_name)} for the \\
          year {base_y}.")
  }
  if (exists("base_x", envir = this_e, inherits = FALSE) && !base_x %in% source$year) {
    abort("Incompatible 'unit_in' and 'source'. No information in source {crayon::bold(source_name)} for the \\
          year {base_x}.")
  }
  # Check general overlap
  if (require_year_column && length(intersect(unique(gdp$year), unique(source$year))) == 0) {
    abort("Incompatible 'gdp' and 'source'. No information in source {crayon::bold(source_name)} for years in 'gdp'.")
  }

  # Use different source if required by the use_USA_cf_for_all and replace_NAs argument
  if (use_USA_cf_for_all) source <- adapt_source_USA(gdp, source)
  if (!use_USA_cf_for_all &&
      (!is.null(replace_NAs) && !any(sapply(c(NA, 0, "no_conversion"), setequal, replace_NAs))) ) {
    source <- adapt_source(gdp, source, with_regions, replace_NAs, require_year_column)
    source_name <- paste0(source_name, "_adapted")
  }

  if (length(intersect(unique(gdp$iso3c), unique(source$iso3c))) == 0) {
    abort("No information in source {crayon::bold(source_name)} for countries in 'gdp'.")
  }

  list("gdp" = gdp,
       "unit_in" = unit_in,
       "unit_out" = unit_out,
       "require_year_column" = require_year_column,
       "source" = source,
       "source_name" = source_name) %>%
    {if (exists("base_x", envir = this_e, inherits = FALSE)) c(., "base_x" = base_x) else .} %>%
    {if (exists("base_y", envir = this_e, inherits = FALSE)) c(., "base_y" = base_y) else .}
}





# Transform user input for package internal use
transform_internal <- function(x, gdp, with_regions, require_year_column) {

  if (!is.null(with_regions) && "gdpuc_region" %in% colnames(x)) {
    x_reg <- dplyr::filter(x, !is.na(.data$gdpuc_region))
    x <- x %>%
      dplyr::filter(is.na(.data$gdpuc_region)) %>%
      dplyr::select(-"gdpuc_region")

    x_reg <- x_reg %>%
      dplyr::group_by(dplyr::across(c(-"iso3c", -"value"))) %>%
      dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::rename("iso3c" = "gdpuc_region")

    i_iso3c <- if (! "iso3c" %in% colnames(gdp)) smart_select_iso3c(gdp) else "iso3c"

    x <- x %>%
      dplyr::bind_rows(x_reg) %>%
      dplyr::arrange(factor(.data$iso3c, levels = unique(gdp[[i_iso3c]])))
  }

  # Transform into original gdp type
  if (class(gdp)[1] == "magpie") {
    # Check if the original magpie object had 2 spatial dimensions
    spat2 <- all(grepl("\\.", magclass::getItems(gdp, dim = 1)))
    if (!spat2) {
      x <- magclass::as.magpie(x, spatial = "iso3c", temporal = "year", datacol = "value")
    } else {
      x <- magclass::as.magpie(x, spatial = c("iso3c", "spatial2"), temporal = "year", datacol = "value")
    }
    magclass::getSets(x) <- magclass::getSets(gdp)
    return(x)
  }

  # Get original iso3c and year column names
  if (! "iso3c" %in% colnames(gdp)) {
    i_iso3c <- smart_select_iso3c(gdp)
    x <- dplyr::rename(x, !!rlang::sym(i_iso3c) := "iso3c")
  }
  if (require_year_column && ! "year" %in% colnames(gdp)) {
    i_year <- smart_select_year(gdp)
    x <- dplyr::rename(x, !!rlang::sym(i_year) := "year")
  }

  x
}
