test_that("can a file from a folder", {
  tmp <- withr::local_tempfile()
  writeLines("hello", tmp)
  folder <- create_mock_folder(download = mockery::mock(tmp))
  expect_equal(folder_read_path(folder$folder, "path", readLines),
               "hello")
  mockery::expect_called(folder$folder$download, 1)
  expect_equal(mockery::mock_args(folder$folder$download)[[1]], list("path"))
  expect_false(file.exists(tmp))
})


test_that("can list packets when empty", {
  folder <- create_mock_folder(
    download = mockery::mock(stop("some error")),
    list = mockery::mock(list(packet = character())))
  driver <- orderly_location_sharepoint$new(
    folder$url, folder$site, folder$path)
  res <- driver$list()
  expect_equal(res, data_frame(packet = character(),
                               time = numeric(),
                               hash = character()))
  mockery::expect_called(folder$folder$download, 1)
  expect_equal(mockery::mock_args(folder$folder$download)[[1]],
               list("known.json"))
  mockery::expect_called(folder$folder$list, 1)
  expect_equal(mockery::mock_args(folder$folder$list)[[1]], list("known"))
})


test_that("can list known packets", {
  meta1 <- withr::local_tempfile()
  meta2 <- withr::local_tempfile()
  meta <- data_frame(
    packet = c("20230904-163829-d8f95215", "20230904-164221-ecbe3fcc"),
    time = c(1693845514.5003, 1693845742.282),
    hash = c("sha256:ab5a58831", "sha256:82b259b39"))
  writeLines(jsonlite::toJSON(as.list(meta[1, ]), auto_unbox = TRUE), meta1)
  writeLines(jsonlite::toJSON(as.list(meta[2, ]), auto_unbox = TRUE), meta2)
  folder <- create_mock_folder(
    download = mockery::mock(stop("some error"), meta1, meta2),
    list = mockery::mock(list(name = meta$packet)),
    upload = mockery::mock())
  driver <- orderly_location_sharepoint$new(
    folder$url, folder$site, folder$path)
  res <- driver$list()
  expect_equal(res$packet, meta$packet)
  expect_equal(res$time, meta$time)
  expect_equal(res$hash, meta$hash)

  mockery::expect_called(folder$folder$download, 3)
  expect_equal(mockery::mock_args(folder$folder$download),
               list(list("known.json"),
                    list(file.path("known", meta$packet[[1]])),
                    list(file.path("known", meta$packet[[2]]))))

  mockery::expect_called(folder$folder$list, 1)
  expect_equal(mockery::mock_args(folder$folder$list)[[1]],
               list("known"))
  mockery::expect_called(folder$folder$upload, 1)
})


test_that("only check previously unknown packets when listing", {
  meta1 <- withr::local_tempfile()
  meta2 <- withr::local_tempfile()
  known <- withr::local_tempfile()
  meta <- data_frame(
    packet = c("20230904-163829-d8f95215", "20230904-164221-ecbe3fcc"),
    time = c(1693845514.5003, 1693845742.282),
    hash = c("sha256:ab5a58831", "sha256:82b259b39"))
  writeLines(jsonlite::toJSON(meta[1, ]), known)
  writeLines(jsonlite::toJSON(as.list(meta[1, ]), auto_unbox = TRUE), meta1)
  writeLines(jsonlite::toJSON(as.list(meta[2, ]), auto_unbox = TRUE), meta2)
  folder <- create_mock_folder(
    download = mockery::mock(known, meta2),
    list = mockery::mock(list(name = meta$packet)),
    upload = mockery::mock())
  driver <- orderly_location_sharepoint$new(
    folder$url, folder$site, folder$path)
  res <- driver$list()
  expect_equal(res$packet, meta$packet)
  expect_equal(res$time, meta$time)
  expect_equal(res$hash, meta$hash)

  mockery::expect_called(folder$folder$download, 2)
  expect_equal(mockery::mock_args(folder$folder$download),
               list(list("known.json"),
                    list(file.path("known", meta$packet[[2]]))))
  mockery::expect_called(folder$folder$list, 1)
  expect_equal(mockery::mock_args(folder$folder$list)[[1]],
               list("known"))
  mockery::expect_called(folder$folder$upload, 1)
})


test_that("simpler call when all known", {
  meta1 <- withr::local_tempfile()
  meta2 <- withr::local_tempfile()
  known <- withr::local_tempfile()
  meta <- data_frame(
    packet = c("20230904-163829-d8f95215", "20230904-164221-ecbe3fcc"),
    time = c(1693845514.5003, 1693845742.282),
    hash = c("sha256:ab5a58831", "sha256:82b259b39"))
  writeLines(jsonlite::toJSON(meta), known)
  writeLines(jsonlite::toJSON(as.list(meta[1, ]), auto_unbox = TRUE), meta1)
  writeLines(jsonlite::toJSON(as.list(meta[2, ]), auto_unbox = TRUE), meta2)
  folder <- create_mock_folder(
    download = mockery::mock(known, meta2),
    list = mockery::mock(list(name = meta$packet)),
    upload = mockery::mock())
  driver <- orderly_location_sharepoint$new(
    folder$url, folder$site, folder$path)
  res <- driver$list()
  expect_equal(res$packet, meta$packet)
  expect_equal(res$time, meta$time)
  expect_equal(res$hash, meta$hash)

  mockery::expect_called(folder$folder$download, 1)
  expect_equal(mockery::mock_args(folder$folder$download)[[1]],
               list("known.json"))
  mockery::expect_called(folder$folder$list, 1)
  expect_equal(mockery::mock_args(folder$folder$list)[[1]],
               list("known"))
  mockery::expect_called(folder$folder$upload, 0)
})


test_that("can read metadata", {
  meta1 <- withr::local_tempfile()
  meta2 <- withr::local_tempfile()
  meta3 <- withr::local_tempfile()
  writeLines("meta1", meta1)
  writeLines("meta2", meta2)
  writeLines("meta3", meta3)

  folder <- create_mock_folder(
    download = mockery::mock(meta1, meta2, meta3))

  driver <- orderly_location_sharepoint$new(
    folder$url, folder$site, folder$path)

  expect_equal(driver$metadata(character()), character())
  mockery::expect_called(folder$folder$download, 0)

  expect_equal(driver$metadata("id1"), "meta1")
  mockery::expect_called(folder$folder$download, 1)
  expect_equal(mockery::mock_args(folder$folder$download)[[1]],
               list("metadata/id1"))

  expect_equal(driver$metadata(c("id2", "id3")), c("meta2", "meta3"))
  mockery::expect_called(folder$folder$download, 3)
  expect_equal(mockery::mock_args(folder$folder$download)[2:3],
               list(list("metadata/id2"), list("metadata/id3")))
})


test_that("can download a file", {
  folder <- create_mock_folder(
    download = mockery::mock())
  driver <- orderly_location_sharepoint$new(
    folder$url, folder$site, folder$path)
  driver$fetch_file("md5:abcdef0123", "dest")
  mockery::expect_called(folder$folder$download, 1)
  expect_equal(
    mockery::mock_args(folder$folder$download)[[1]],
    list("files/md5_abcdef0123", "dest"))
})


test_that("can list unknown packets", {
  folder <- create_mock_folder()
  driver <- orderly_location_sharepoint$new(
    folder$url, folder$site, folder$path)
  unlockBinding(quote("list"), driver)
  driver$list <- mockery::mock(c("a", "b", "c"), cycle = TRUE)
  expect_equal(driver$list_unknown_packets(c("a", "d")), "d")
  expect_equal(driver$list_unknown_packets(c("a")), character(0))
  expect_equal(driver$list_unknown_packets(character()), character(0))
  expect_equal(driver$list_unknown_packets(c("d", "e", "f")), c("d", "e", "f"))
  mockery::expect_called(driver$list, 4)
  expect_equal(mockery::mock_args(driver$list),
               rep(list(list()), 4))
})


test_that("can list unknown files", {
  folder <- create_mock_folder()
  hashes <- c("md5_abc", "md5_123", "md5_456")
  folder <- create_mock_folder(
    list = mockery::mock(data_frame(name = hashes), cycle = TRUE))
  driver <- orderly_location_sharepoint$new(
    folder$url, folder$site, folder$path)
  expect_equal(driver$list_unknown_files("md5:789"), "md5:789")
  expect_equal(driver$list_unknown_files(character()), character())
  expect_equal(
    driver$list_unknown_files(c("md5:abc", "md5:123", "md5:789", "md5:321")),
    c("md5:789", "md5:321"))
  mockery::expect_called(folder$folder$list, 3)
  expect_equal(mockery::mock_args(folder$folder$list),
               rep(list(list("files")), 3))
})


test_that("can push files", {
  folder <- create_mock_folder(
    upload = mockery::mock())
  driver <- orderly_location_sharepoint$new(
    folder$url, folder$site, folder$path)
  driver$push_file("/path/to/file", "md5:123")
  mockery::expect_called(folder$folder$upload, 1)
  expect_equal(mockery::mock_args(folder$folder$upload)[[1]],
               list("/path/to/file", "files/md5_123"))
})


test_that("can push metadata", {
  root <- suppressMessages(orderly2::orderly_example("default"))
  id <- suppressMessages(
    orderly2::orderly_run("data", echo = FALSE, root = root))
  path <- file.path(root, ".outpack", "metadata", id)
  hash <- orderly2:::hash_data(read_string(path), "sha256")

  folder <- create_mock_folder(
    upload = mockery::mock())
  driver <- orderly_location_sharepoint$new(
    folder$url, folder$site, folder$path)

  ## Prevent cleanup of temporary files
  mock_tempfile <- mockery::mock(tempfile())
  mockery::stub(driver$push_metadata, "withr::local_tempfile", mock_tempfile)

  driver$push_metadata(id, hash, path)

  mockery::expect_called(folder$folder$upload, 2)
  args1 <- mockery::mock_args(folder$folder$upload)[[1]]
  expect_length(args1, 2)
  expect_type(args1[[1]], "character")
  expect_equal(readLines(args1[[1]]), readLines(path))
  expect_equal(args1[[2]], file.path("metadata", id))

  args2 <- mockery::mock_args(folder$folder$upload)[[2]]
  expect_length(args2, 2)
  expect_type(args2[[1]], "character")
  expect_equal(args2[[2]], file.path("known", id))
  d <- jsonlite::read_json(args2[[1]])
  expect_setequal(names(d), c("packet", "hash", "time"))
  expect_equal(d$packet, id)
  expect_equal(d$hash, hash)
  expect_type(d$time, "double")
})


test_that("create driver when exists already", {
  mock_folder <- list(download = mockery::mock(),
                      upload = mockery::mock(),
                      create = mockery::mock(),
                      list = mockery::mock(data_frame(name = character())))
  client <- list(folder = mockery::mock(mock_folder))
  mock_client <- mockery::mock(client)

  mockery::stub(orderly_sharepoint_folder, "orderly_sharepoint_client",
                mock_client)
  res <- orderly_sharepoint_folder("https://localhost", "site", "path")
  expect_identical(res, mock_folder)

  mockery::expect_called(mock_client, 1)
  expect_equal(mockery::mock_args(mock_client)[[1]],
               list("https://localhost"))

  mockery::expect_called(client$folder, 1)
  expect_equal(mockery::mock_args(client$folder)[[1]],
               list("site", "path", verify = TRUE))

  mockery::expect_called(mock_folder$download, 1)
  expect_equal(mockery::mock_args(mock_folder$download)[[1]],
               list("orderly.sharepoint"))

  mockery::expect_called(mock_folder$list, 0)
  mockery::expect_called(mock_folder$upload, 0)
  mockery::expect_called(mock_folder$create, 0)
})


test_that("create driver when new", {
  mock_folder <- list(download = mockery::mock(stop("does not exist")),
                      upload = mockery::mock(),
                      create = mockery::mock(),
                      list = mockery::mock(data_frame(name = character())))
  client <- list(folder = mockery::mock(mock_folder))
  mock_client <- mockery::mock(client)

  mockery::stub(orderly_sharepoint_folder, "orderly_sharepoint_client",
                mock_client)
  res <- orderly_sharepoint_folder("https://localhost", "site", "path")
  expect_identical(res, mock_folder)

  mockery::expect_called(mock_client, 1)
  expect_equal(mockery::mock_args(mock_client)[[1]],
               list("https://localhost"))

  mockery::expect_called(client$folder, 1)
  expect_equal(mockery::mock_args(client$folder)[[1]],
               list("site", "path", verify = TRUE))

  mockery::expect_called(mock_folder$download, 1)
  expect_equal(mockery::mock_args(mock_folder$download)[[1]],
               list("orderly.sharepoint"))

  mockery::expect_called(mock_folder$list, 1)
  expect_equal(mockery::mock_args(mock_folder$list)[[1]], list())

  mockery::expect_called(mock_folder$upload, 1)

  mockery::expect_called(mock_folder$create, 3)
  expect_equal(mockery::mock_args(mock_folder$create),
               list(list("metadata"), list("files"), list("known")))
})


test_that("error if files exist at destination on creation", {
  mock_folder <- list(download = mockery::mock(stop("does not exist")),
                      upload = mockery::mock(),
                      create = mockery::mock(),
                      list = mockery::mock(data_frame(name = "x")))
  client <- list(folder = mockery::mock(mock_folder))
  mock_client <- mockery::mock(client)

  mockery::stub(orderly_sharepoint_folder, "orderly_sharepoint_client",
                mock_client)
  expect_error(
    orderly_sharepoint_folder("https://localhost", "site", "path"),
    "Directory site:path cannot be used for orderly; contains other files")
})


test_that("create driver if not in cache", {
  folder <- create_mock_folder()

  mock_folder <- mockery::mock(folder$folder)
  mockery::stub(orderly_sharepoint_folder_cached, "orderly_sharepoint_folder",
                mock_folder)

  expect_identical(
    orderly_sharepoint_folder_cached(folder$url, folder$site, folder$path),
    folder$folder)
  mockery::expect_called(mock_folder, 0)

  cache[[folder$key]] <- NULL

  expect_identical(
    orderly_sharepoint_folder_cached(folder$url, folder$site, folder$path),
    folder$folder)
  mockery::expect_called(mock_folder, 1)
  expect_equal(mockery::mock_args(mock_folder)[[1]],
               list(folder$url, folder$site, folder$path))
})
