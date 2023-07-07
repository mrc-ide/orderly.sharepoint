create_temporary_root <- function(...) {
  path <- tempfile()
  withr::defer_parent(unlink(path, recursive = TRUE))
  orderly2::outpack_init(path, ..., logging_console = FALSE)
}


create_random_packet <- function(root, name = "data", parameters = NULL) {
  src <- fs::dir_create(tempfile())
  on.exit(unlink(src, recursive = TRUE))
  saveRDS(runif(10), file.path(src, "data.rds"))
  p <- orderly2::outpack_packet_start(src, name, parameters = parameters,
                                      root = root)
  orderly2::outpack_packet_end(p)
  p$id
}
