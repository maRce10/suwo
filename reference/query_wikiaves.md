# Access 'WikiAves' media file metadata

`query_wikiaves` searches for metadata from
[WikiAves](https://www.wikiaves.com.br/).

## Usage

``` r
query_wikiaves(
  species = getOption("species"),
   format = getOption("format", c("image", "sound")),
  cores = getOption("mc.cores", 1),
  pb = getOption("pb", TRUE),
  verbose = getOption("verbose", TRUE),
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
  'sound' or 'image'. Can be set globally for the current R session via
  the "format" option (e.g. `options(format = "sound")`). Required.

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

- all_data:

  Logical argument that determines if all data available from database
  is shown in the results of search. Default is `FALSE`. Can be set
  globally for the current R session via the "all_data" option (
  `options(all_data = TRUE)`).

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

This function queries for avian digital media in the open-access online
repository [WikiAves](https://www.wikiaves.com.br/) and returns its
metadata. WikiAves is a Brazilian online platform and citizen science
project that serves as the largest community for birdwatchers in Brazil.
It functions as a collaborative, interactive encyclopedia of Brazilian
birds, where users contribute georeferenced photographs and sound
recordings, which are then used to build a vast database for research
and conservation.

## References

Schubert, Stephanie Caroline, Lilian Tonelli Manica, and André De
Camargo Guaraldo. 2019. Revealing the potential of a huge
citizen-science platform to study bird migration. Emu-Austral
Ornithology 119.4: 364-373.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
# search
p_nattereri <- query_wikiaves(species = "Phaethornis nattereri",
    format = "image")
#> ✔ Obtaining metadata (108 matching records found) 😸:
```
