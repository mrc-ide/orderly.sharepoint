##' @export
outpack_location_sharepoint <- R6::R6Class(
  "outpack_location_sharepoint",

  private = list(
    folder = NULL
  ),

  public = list(
    initialize = function(url, site, path) {
      private$folder <- outpack_sharepoint_folder_cached(url, site, path)
    },

    list = function() {
      known <- tryCatch(
        folder_read_path(private$folder, "known.json",
                         function(path) jsonlite::read_json(path, TRUE)),
        error = function(e) {
          data_frame(packet = character(),
                     time = numeric(),
                     hash = character())
        })
      new <- setdiff(private$folder$list("known")$name, known$packet)
      if (length(new) > 0) {
        dat <- lapply(file.path("known", new), function(path_id) {
          folder_read_path(private$folder, path_id, jsonlite::read_json)
        })
        new <- data_frame(packet = vcapply(dat, "[[", "packet"),
                          time = vnapply(dat, "[[", "time"),
                          hash = vcapply(dat, "[[", "hash"))
        known <- rbind(known, new)
        rownames(known) <- NULL
        tmp <- withr::local_tempfile()
        writeLines(jsonlite::toJSON(known), tmp)
        private$folder$upload(tmp, "known.json")
      }
      known
    },

    metadata = function(packet_ids) {
      vcapply(file.path("metadata", packet_ids), function(path_id) {
        folder_read_path(private$folder, path_id, read_string)
      })
    },

    fetch_file = function(hash, dest) {
      src <- file.path("files", sub(":", "_", hash))
      private$folder$download(src, dest)
    },

    list_unknown_packets = function(ids) {
      setdiff(ids, self$list())
    },

    list_unknown_files = function(hashes) {
      known <- sub("_", ":", private$folder$list("files")$name)
      setdiff(hashes, known)
    },

    push_file = function(src, hash) {
      dest <- file.path("files", sub(":", "_", hash))
      private$folder$upload(src, dest)
    },

    push_metadata = function(packet_id, root) {
      hash <- orderly2:::get_metadata_hash(packet_id, root)
      time <- orderly2:::time_to_num()
      path <- file.path(root$path, ".outpack", "metadata", packet_id)
      private$folder$upload(path, file.path("metadata", packet_id))
      dat <- jsonlite::toJSON(
        list(packet = jsonlite::unbox(packet_id),
             time = jsonlite::unbox(time),
             hash = jsonlite::unbox(hash)))
      tmp <- withr::local_tempfile()
      writeLines(dat, tmp)
      private$folder$upload(tmp, file.path("known", packet_id))
    }
  ))


## Seems hard to mock the whole class out, which I think validates my
## general approach of exposing free constructor!
## https://github.com/r-lib/mockery/issues/21
outpack_sharepoint_client <- function(url) {
  spud::sharepoint$new(url) # nocov
}


outpack_sharepoint_folder <- function(url, site, path) {
  client <- outpack_sharepoint_client(url)
  folder <- tryCatch(
    client$folder(site, path, verify = TRUE),
    error = function(e)
      stop(sprintf("Error reading from %s:%s - %s",
                   site, path, e$message), call. = FALSE))
  path <- "outpack.sharepoint"
  exists <- tryCatch({
    folder$download(path)
    TRUE
  }, error = function(e) FALSE)
  if (exists) {
    return(folder)
  }
  if (nrow(folder$list()) > 0L) {
    stop(sprintf(
      "Directory %s:%s cannot be used for outpack; contains other files",
      site, path))
  }
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines("outpack.sharepoint", tmp)
  folder$upload(tmp, path)
  folder$create("metadata")
  folder$create("files")
  folder$create("known")
  folder
}


folder_read_path <- function(folder, path, read) {
  dest <- folder$download(path)
  on.exit(unlink(dest))
  read(dest)
}


cache <- new.env(parent = emptyenv())
outpack_sharepoint_folder_cached <- function(url, site, path) {
  key <- paste(url, site, path)
  if (is.null(cache[[key]])) {
    cache[[key]] <- outpack_sharepoint_folder(url, site, path)
  }
  cache[[key]]
}