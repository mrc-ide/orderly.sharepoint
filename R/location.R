##' @name orderly_location_sharepoint
##' @rdname orderly_location_sharepoint
##' @title Orderly Location for Sharepoint
##'
##' @description A driver to put orderly/orderly2 archives on
##'   Sharepoint. Individual methods are not documented here as this
##'   just needs to satisfy the interface required by orderly/orderly2
##'   and is not for direct user use. See
##'   [orderly2::orderly_location_add] for details.
##'
##' @export
orderly_location_sharepoint <- R6::R6Class(
  "orderly_location_sharepoint",

  private = list(
    folder = NULL
  ),

  public = list(
    ##' @param url The base url of your Office365/Sharepoint site, such
    ##' as `https://myorg.sharepoint.com` (the `https://` prefix is required)
    ##'
    ##' @param site Your site name on Sharepoint
    ##'
    ##' @param path The path within your site name where documents will
    ##' be stored (you may need `Shared Documents` even if Sharepoint
    ##' makes it look like `Documents`
    initialize = function(url, site, path) {
      private$folder <- orderly_sharepoint_folder_cached(url, site, path)
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
      }, USE.NAMES = FALSE)
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

    push_metadata = function(packet_id, hash, path) {
      orderly2:::hash_validate_data(read_string(path), hash)
      private$folder$upload(path, file.path("metadata", packet_id))
      dat <- jsonlite::toJSON(
        list(packet = jsonlite::unbox(packet_id),
             time = jsonlite::unbox(as.numeric(Sys.time())),
             hash = jsonlite::unbox(hash)))
      tmp <- withr::local_tempfile()
      writeLines(dat, tmp)
      private$folder$upload(tmp, file.path("known", packet_id))
    }
  ))


orderly_sharepoint_client <- function(url) {
  spud::sharepoint$new(url) # nocov
}


orderly_sharepoint_folder <- function(url, site, path) {
  client <- orderly_sharepoint_client(url)
  folder <- tryCatch(
    client$folder(site, path, verify = TRUE),
    error = function(e) {
      stop(sprintf("Error reading from %s:%s - %s",
                   site, path, e$message), call. = FALSE)
    })
  path_test <- "orderly.sharepoint"
  exists <- tryCatch({
    folder$download(path_test)
    TRUE
  }, error = function(e) FALSE)
  if (exists) {
    return(folder)
  }
  if (nrow(folder$list()) > 0L) {
    stop(sprintf(
      "Directory %s:%s cannot be used for orderly; contains other files",
      site, path))
  }
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines("orderly.sharepoint", tmp)
  folder$upload(tmp, path_test)
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
orderly_sharepoint_folder_cached <- function(url, site, path) {
  key <- paste(url, site, path)
  if (is.null(cache[[key]])) {
    cache[[key]] <- orderly_sharepoint_folder(url, site, path)
  }
  cache[[key]]
}
