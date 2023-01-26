load_celestial <- function(filename,
                           url = "https://cdn.jsdelivr.net/gh/dieghernan/celestial_data@main/data/",
                           cachedir = tempdir()) {
  if (!dir.exists(cachedir)) {
    stop(
      "Please create ",
      path.expand(cachedir),
      " directory",
      "first"
    )
  }
  url <- file.path(url, filename)
  local_path <- file.path(cachedir, filename)
  if (!file.exists(local_path)) {
    download.file(url, local_path, mode = "wb", quiet = TRUE)
  }
  celestial <- sf::st_read(local_path, quiet = TRUE)
  return(celestial)
}