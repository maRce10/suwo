# Access 'WikiAves' media file metadata

`query_wikiaves` searches for metadata from
[WikiAves](https://www.wikiaves.com.br/).

## Usage

``` r
query_wikiaves(
  species = getOption("suwo_species"),
  format = getOption("suwo_format", c("image", "sound")),
  cores = getOption("mc.cores", 1),
  pb = getOption("suwo_pb", TRUE),
  verbose = getOption("suwo_verbose", TRUE),
  all_data = getOption("suwo_all_data", FALSE),
  raw_data = getOption("suwo_raw_data", FALSE)
)
```

## Arguments

- species:

  Character string with the scientific name of a species in the format:
  "Genus epithet". Required. Can be set globally for the current R
  session via the "suwo_species" option (e.g.
  `options(suwo_species = "Hypsiboas rufitelus")`).

- format:

  Character vector with the media format to query for. Options are
  'image' or 'sound'. Can be set globally for the current R session via
  the "suwo_format" option (e.g. `options(suwo_format = "image")`).
  Required.

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
  `TRUE`. Can be set globally for the current R session via the
  "suwo_pb" option ( `options(suwo_pb = TRUE)`).

- verbose:

  Logical argument that determines if text is shown in console. Default
  is `TRUE`. Can be set globally for the current R session via the
  "suwo_verbose" option ( `options(suwo_verbose = TRUE)`).

- all_data:

  Logical argument that determines if all data available from database
  is shown in the results of search. Default is `FALSE`. Can be set
  globally for the current R session via the "suwo_all_data" option (
  `options(suwo_all_data = TRUE)`).

- raw_data:

  Logical argument that determines if the raw data from the repository
  is returned (e.g. without any manipulation). Default is `FALSE`. Can
  be set globally for the current R session via the "suwo_raw_data"
  option ( `options(suwo_raw_data = TRUE)`). If `TRUE` `all_data` is set
  to `TRUE` internally. Useful for developers, or if users suspect that
  some data is mishandled during processing (i.e. date information is
  lost). Note that the metadata obtained when `raw_data = TRUE` is not
  standardized, so most suwo functions for downstream steps will not
  work on them.

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
