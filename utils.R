pretty_lonlat <- function(x, type, accuracy = 2) {
  positive <- x >= 0
  # Decompose
  x <- abs(x)
  D <- as.integer(x)
  m <- (x - D) * 60
  M <- as.integer(m)
  S <- round((m - M) * 60, accuracy)
  # Get label
  if (type == "lon") {
    lab <- ifelse(positive > 0, "E", "W")
  } else {
    lab <- ifelse(positive > 0, "N", "S")
  }
  # Compose
  label <- paste0(D, "\u00b0 ", M, "' ", S, '\" ', lab)
  return(label)
}

# Derive rotation degrees of the projection given a date and a longitude
get_mst <- function(dt, lng) {
  desired_date_utc <- lubridate::with_tz(dt, "UTC")
  yr <- lubridate::year(desired_date_utc)
  mo <- lubridate::month(desired_date_utc)
  dy <- lubridate::day(desired_date_utc)
  h <- lubridate::hour(desired_date_utc)
  m <- lubridate::minute(desired_date_utc)
  s <- lubridate::second(desired_date_utc)
  if ((mo == 1) || (mo == 2)) {
    yr <- yr - 1
    mo <- mo + 12
  }
  # Adjust times before Gregorian Calendar
  # See https://squarewidget.com/julian-day/
  if (lubridate::as_date(dt) > as.Date("1582-10-14")) {
    a <- floor(yr / 100)
    b <- 2 - a + floor(a / 4)
  } else {
    b <- 0
  }
  c <- floor(365.25 * yr)
  d <- floor(30.6001 * (mo + 1))
  # days since J2000.0
  jd <- b + c + d - 730550.5 + dy + (h + m / 60 + s / 3600) / 24
  jt <- jd / 36525
  # Rotation
  mst <- 280.46061837 + 360.98564736629 * jd +
    0.000387933 * jt^2 - jt^3 / 38710000.0 + lng
  # Modulo 360 degrees
  mst <- mst %% 360
  return(mst)
}

# Cut a sf object with a buffer using spherical s2 geoms
# Optionally, project and flip
sf_spherical_cut <- function(x, the_buff, the_crs = sf::st_crs(x), flip = NULL) {
  # Get geometry type
  geomtype <- unique(gsub("MULTI", "", sf::st_geometry_type(x)))[1]
  # Keep the data frame, s2 drops it
  the_df <- sf::st_drop_geometry(x)
  the_geom <- sf::st_geometry(x)
  # Convert to s2 if needed
  if (!inherits(the_buff, "s2_geography")) {
    the_buff <- sf::st_as_s2(the_buff)
  }
  the_cut <- the_geom %>%
    # Cut with s2
    sf::st_as_s2() %>%
    s2::s2_intersection(the_buff) %>%
    # Back to sf and add the df
    sf::st_as_sfc() %>%
    sf::st_sf(the_df, geometry = .) %>%
    dplyr::filter(!sf::st_is_empty(.)) %>%
    sf::st_transform(crs = the_crs)
  # If it is not POINT filter by valid and non-empty
  # This if for performance
  if (!geomtype == "POINT") {
    # If any is GEOMETRYCOLLECTION extract the right value
    if (any(sf::st_geometry_type(the_cut) == "GEOMETRYCOLLECTION")) {
      the_cut <- the_cut %>%
        sf::st_collection_extract(type = geomtype, warn = FALSE)
    }
    the_cut <- the_cut %>%
      dplyr::filter(!is.na(sf::st_is_valid(.)))
  }
  if (!is.null(flip)) {
    the_cut <- the_cut %>%
      dplyr::mutate(geometry = geometry * flip) %>%
      sf::st_set_crs(the_crs)
  }
  return(the_cut)
}