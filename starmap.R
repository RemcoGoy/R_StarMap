# Spatial manipulation
library(sf)
library(s2)
library(nominatimlite)

# Wrange data and dates
library(dplyr)
library(lubridate)
library(lutz)

# Visualization
library(ggplot2)
library(ggfx)
library(ggshadow)

library(stringi)

source("./data.R")
source("./utils.R")

desired_place <- 'Madrid, Spain'

desired_date <- make_datetime(
  year = 2015,
  month = 9,
  day = 22,
  hour = 3,
  min = 45
)

desired_place_geo <- geo_lite(desired_place, full_results = TRUE)

desired_loc <- desired_place_geo %>%
  select(lat, lon) %>%
  unlist()

get_tz <- tz_lookup_coords(desired_loc[1], desired_loc[2], warn = FALSE)

desired_date_tz <- force_tz(desired_date, get_tz)

lon_prj <- get_mst(desired_date_tz, desired_loc[2])
lat_prj <- desired_loc[1]

# Create proj4string w/ Airy projection
target_crs <- paste0("+proj=airy +x_0=0 +y_0=0 +lon_0=", lon_prj, " +lat_0=", lat_prj)

# We need to flip celestial objects to get the impression of see from the Earth
# to the sky, instead of from the sky to the Earth
# https://stackoverflow.com/a/75064359/7877917
# Flip matrix for affine transformation
flip_matrix <- matrix(c(-1, 0, 0, 1), 2, 2)
# And create an s2 buffer of the visible hemisphere at the given location
hemisphere_s2 <- s2_buffer_cells(
  as_s2_geography(
    paste0("POINT(", lon_prj, " ", lat_prj, ")")
  ),
  9800000,
  max_cells = 5000
)
# This one is for plotting
hemisphere_sf <- hemisphere_s2 %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

mw <- load_celestial("mw.min.geojson")
# Add colors to MW to use on fill
cols <- colorRampPalette(c("white", "yellow"))(5)
mw$fill <- factor(cols, levels = cols)
ggplot(mw) +
  geom_sf(aes(fill = fill)) +
  scale_fill_identity()

# And process it
# Cut to buffer
mw_end <- sf_spherical_cut(mw,
                           the_buff = hemisphere_s2,
                           # Change the crs
                           the_crs = target_crs,
                           flip = flip_matrix
)

ggplot(mw_end) +
  geom_sf(aes(fill = fill)) +
  scale_fill_identity()

const <- load_celestial("constellations.lines.min.geojson")
ggplot(const) +
  geom_sf() +
  coord_sf(expand = FALSE)

# Cut to buffer
const_end <- sf_spherical_cut(const,
                              the_buff = hemisphere_s2,
                              # Change the crs
                              the_crs = target_crs,
                              flip = flip_matrix
)
ggplot(const_end) +
  geom_sf() +
  coord_sf(expand = FALSE)

stars <- load_celestial("stars.6.min.geojson")
ggplot(stars) +
  # We use relative brightness (br) as aes
  geom_sf(aes(size = br, alpha = br), shape = 16) +
  scale_size_continuous(range = c(0.5, 6)) +
  scale_alpha_continuous(range = c(0.1, 0.8)) +
  coord_sf(expand = FALSE)

# Cut to buffer
stars_end <- sf_spherical_cut(stars,
                              the_buff = hemisphere_s2,
                              # Change the crs
                              the_crs = target_crs,
                              flip = flip_matrix
)
ggplot(stars_end) +
  # We use relative brightness (br) as aes
  geom_sf(aes(size = br, alpha = br), shape = 16) +
  scale_size_continuous(range = c(0.5, 6)) +
  scale_alpha_continuous(range = c(0.1, 0.8))

grat <- st_graticule(
  ndiscr = 5000,
  lat = seq(-90, 90, 10),
  lon = seq(-180, 180, 30)
)
ggplot(grat) +
  geom_sf() +
  coord_sf(expand = FALSE)

# Cut to buffer, we dont flip this one (it is not an object of the space)
grat_end <- sf_spherical_cut(
  x = grat,
  the_buff = hemisphere_s2,
  # Change the crs
  the_crs = target_crs
)
ggplot(grat_end) +
  geom_sf() +
  coord_sf(expand = FALSE)

lat_lab <- pretty_lonlat(desired_loc[1], type = "lat")
lon_lab <- pretty_lonlat(desired_loc[2], type = "lon")
pretty_labs <- paste(lat_lab, "/", lon_lab)

# Create final caption to put on bottom
pretty_time <- paste(
  # Pretty Day
  scales::label_date(
    format = "%d %b %Y",
    locale = "en"
  )(desired_date_tz),
  # Pretty Hour
  format(desired_date_tz, format = "%H:%M", usetz = TRUE)
)

# Our final caption
caption <- toupper(paste0(
  "Star Map\n",
  desired_place, "\n",
  pretty_time, "\n",
  pretty_labs
))

# Prepare MULTILINESTRING
const_end_lines <- const_end %>%
  st_cast("MULTILINESTRING") %>%
  st_coordinates() %>%
  as.data.frame()
ggplot() +
  # Graticules
  geom_sf(data = grat_end, color = "grey60", linewidth = 0.25, alpha = 0.3) +
  # A blurry Milky Way
  with_blur(
    geom_sf(
      data = mw_end, aes(fill = fill), alpha = 0.1, color = NA,
      show.legend = FALSE
    ),
    sigma = 8
  ) +
  scale_fill_identity() +
  # Glowing stars
  geom_glowpoint(
    data = stars_end, aes(
      alpha = br, size =
        br, geometry = geometry
    ),
    color = "white", show.legend = FALSE, stat = "sf_coordinates"
  ) +
  scale_size_continuous(range = c(0.05, 0.75)) +
  scale_alpha_continuous(range = c(0.1, 0.5)) +
  # Glowing constellations
  geom_glowpath(
    data = const_end_lines, aes(X, Y, group = interaction(L1, L2)),
    color = "white", size = 0.5, alpha = 0.8, shadowsize = 0.4, shadowalpha = 0.01,
    shadowcolor = "white", linejoin = "round", lineend = "round"
  ) +
  # Border of the sphere
  geom_sf(data = hemisphere_sf, fill = NA, color = "white", linewidth = 1.25) +
  # Caption
  labs(caption = caption) +
  # And end with theming
  theme_void() +
  theme(
    text = element_text(colour = "white"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#191d29", color = "#191d29"),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(
      hjust = 0.5, face = "bold",
      size = rel(1),
      lineheight = rel(1.2),
      margin = margin(t = 40, b = 20)
    )
  )

# Chinese constellations
const_cn <- load_celestial("constellations.lines.cn.min.geojson")
# Cut and prepare for geom_glowpath() on a single step
const_cn_end_lines <- sf_spherical_cut(const_cn,
                                       the_buff = hemisphere_s2,
                                       # Change the crs
                                       the_crs = target_crs,
                                       flip = flip_matrix
) %>%
  # To paths
  st_cast("MULTILINESTRING") %>%
  st_coordinates() %>%
  as.data.frame()
ggplot() +
  # Graticules
  geom_sf(data = grat_end, color = "grey60", linewidth = 0.25, alpha = 0.3) +
  # A blurry Milky Way
  with_blur(
    geom_sf(
      data = mw_end, aes(fill = fill), alpha = 0.1, color = NA,
      show.legend = FALSE
    ),
    sigma = 8
  ) +
  scale_fill_identity() +
  # Glowing stars
  geom_glowpoint(
    data = stars_end, aes(
      alpha = br, size =
        br, geometry = geometry
    ),
    color = "white", show.legend = FALSE, stat = "sf_coordinates"
  ) +
  scale_size_continuous(range = c(0.05, 0.75)) +
  scale_alpha_continuous(range = c(0.1, 0.5)) +
  # Glowing constellations
  geom_glowpath(
    data = const_cn_end_lines, aes(X, Y, group = interaction(L1, L2)),
    color = "white", size = 0.5, alpha = 0.8, shadowsize = 0.4, shadowalpha = 0.01,
    shadowcolor = "white", linejoin = "round", lineend = "round"
  ) +
  # Border of the sphere
  geom_sf(data = hemisphere_sf, fill = NA, color = "white", linewidth = 1.25) +
  # Caption
  labs(caption = caption) +
  # And end with theming
  theme_void() +
  theme(
    text = element_text(colour = "white"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#191d29", color = "#191d29"),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(
      hjust = 0.5, face = "bold",
      size = rel(1),
      lineheight = rel(1.2),
      margin = margin(t = 40, b = 20)
    )
  )