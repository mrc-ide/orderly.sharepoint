clear_cache <- function() {
  rm(list = ls(cache), envir = cache)
}


create_mock_folder <- function(...,
                               url = NULL, site = "mypath", path = "mysite") {
  url <- url %||% sprintf("https://example.com/orderly/%s", random_str())
  key <- paste(url, site, path)
  folder <- list(...)
  cache[[key]] <- folder
  list(url = url, site = site, path = path, folder = folder, key = key)
}


random_str <- function() {
  paste(sample(letters, 20, replace = TRUE), collapse = "")
}
