# Access 'iNaturalist' media file metadata

`query_inaturalist` searches for metadata from
[iNaturalist](https://www.inaturalist.org/).

## Usage

``` r
query_inaturalist(
  species = getOption("species"),
  cores = getOption("mc.cores", 1),
  pb = getOption("pb", TRUE),
  verbose = getOption("verbose", TRUE),
  format = getOption("format", c("sound", "image")),
  identified = FALSE,
  verifiable = FALSE,
  all_data = getOption("all_data", FALSE),
  raw_data = getOption("raw_data", FALSE)
)
```

## Arguments

- species:

  Character string with the scientific name of a species in the format:
  "Genus epithet". Required. Can be set globally for the current R
  session via the "term" option (e.g.
  `options(term = "Hypsiboas rufitelus")`).

- cores:

  Numeric vector of length 1. Controls whether parallel computing is
  applied by specifying the number of cores to be used. Default is 1
  (i.e. no parallel computing). Can be set globally for the current R
  session via the "mc.cores" option (e.g. `options(mc.cores = 2)`). Note
  that some repositories might not support parallel queries from the
  same IP address as it might be identified as denial-of-service
  cyberattack.

- pb:

  Logical argument to control if progress bar is shown. Default is
  `TRUE`. Can be set globally for the current R session via the "pb"
  option ( `options(pb = TRUE)`).

- verbose:

  Logical argument that determines if text is shown in console. Default
  is `TRUE`. Can be set globally for the current R session via the
  "verbose" option ( `options(verbose = TRUE)`).

- format:

  Character vector with the media format to query for. Currently 'image'
  and 'sound' are available. Can be set globally for the current R
  session via the "format" option (e.g. `options(format = "sound")`).
  Required.

- identified:

  Logical argument to define if search results are categorized as
  identified by inaturalist.

- verifiable:

  Logical argument to define if search results are categorized as
  verifiable by inaturalist.

- all_data:

  Logical argument that determines if all data available from database
  is shown in the results of search. Default is `TRUE`.

- raw_data:

  Logical argument that determines if the raw data from the repository
  is returned (e.g. without any manipulation). Default is `FALSE`. Can
  be set globally for the current R session via the "raw_data" option (
  `options(raw_data = TRUE)`). If `TRUE` `all_data` is set to `TRUE`
  internally. Useful for developers, or if users suspect that some data
  is mishandled during processing (i.e. date information is lost). Note
  that the metadata obtained when `raw_data = TRUE` is not standardized,
  so most suwo functions for downstream steps will not work on them.

## Value

The function returns a data frame with the metadata of the media files
matching the search criteria. If `all_data = TRUE`, all metadata fields
(columns) are returned. If `raw_data = TRUE`, the raw data as obtained
from the repository is returned (without any formatting).

## Details

This function queries for species observation info in the open-access
online repository [iNaturalist](https://www.inaturalist.org/).
iNaturalist is a free, crowdsourced online platform for nature
enthusiasts to document and identify plants, animals, fungi, and other
organisms in the wild. Note that Inaturalist observations do not include
a 'country' field.

## References

iNaturalist. Available from https://www.inaturalist.org. Accessed
\[date\]

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
# search Bleeding Tooth mushroom images
```
