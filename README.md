# outpack.sharepoint

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/outpack.sharepoint/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/outpack.sharepoint/actions)
[![codecov.io](https://codecov.io/github/mrc-ide/outpack.sharepoint/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/outpack.sharepoint?branch=main)
<!-- badges: end -->

Similar to [orderly.sharepoint](https://github.com/vimc/orderly.sharepoint), but for [orderly2](https://mrc-ide.github.io/orderly2), which is based on outpack.

To configure, add a location:

```
orderly2::outpack_location_add(
  "my-sharepoint-location",
  list(driver = "outpack.sharepoint::outpack_location_sharepoint",
       url = "https://example.sharepoint.com",
       site = "mysite",
       path = "Shared Documents/outpack/real")
```

where

* `url` is the base url of your Office365/Sharepoint site, such as `https://myorg.sharepoint.com` (the `https://` prefix is required)
* `site` is your site name on Sharepoint
* `path` is the path within your site name where documents will be stored (you may need `Shared Documents` even if sharepoint makes it look like `Documents`).

These are the same arguments and interpretationas in `orderly.sharepoint`.

**You must treat all files on the sharepoint as read-only**

## Installation

To install `outpack.sharepoint`:

```r
remotes::install_github("mrc-ide/outpack.sharepoint", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine
