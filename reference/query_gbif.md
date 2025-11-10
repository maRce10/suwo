# Access 'gbif' recordings and metadata

`query_gbif` searches for metadata from [gbif](https://www.gbif.org/).

## Usage

``` r
query_gbif(
  species = getOption("species"),
  format = c("sound", "image", "video", "interactive resource"),
  cores = getOption("mc.cores", 1),
  pb = getOption("pb", TRUE),
  verbose = getOption("verbose", TRUE),
  dataset = NULL,
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

- format:

  Character vector with the media format to query for. Options are
  'sound', 'image', 'video' and 'interactive resource'. Required.

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

- dataset:

  The name of a specific dataset in which to focus the query (by default
  it searchs across all available datasets). Users can check available
  dataset names by downloading this csv file
  <https://api.gbif.org/v1/dataset/search/export?format=CSV&>. See
  <https://www.gbif.org/dataset/search?q=> for more details.

- all_data:

  Logical argument that determines if all data available from database
  is shown in the results of search. Default is `FALSE`. Can be set
  globally for the current R session via the "all_data" option (
  `options(all_data = TRUE)`).

- raw_data:

  Logical argument that determines if the raw data from the API is
  returned (e.g. without any manipulation). Default is `FALSE`. Can be
  set globally for the current R session via the "raw_data" option (
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
online repository [gbif](https://www.gbif.org/). GBIF (the Global
Biodiversity Information Facility) is an international network and data
infrastructure funded by the world's governments and aimed at providing
open access to data about all types of life on Earth. Note that some of
the records returned by this function could be duplicates of records
returned by other suwo functions (e.g.,
[`query_inaturalist`](https://marce10.github.io/suwo/reference/query_inaturalist.md)).

## References

GBIF.org (2024), GBIF Home Page. Available from: https://www.gbif.org/

## See also

`query_gbif`

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (interactive()){
# search dink frog sound files
}
```
