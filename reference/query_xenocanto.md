# Access 'Xeno-Canto' recordings and metadata

`query_xenocanto` searches for metadata from
[Xeno-Canto](https://www.xeno-canto.org/).

## Usage

``` r
query_xenocanto(
  species = getOption("species"),
  api_key = getOption("xc_api_key"),
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
  `options(term = "Hypsiboas rufitelus")`). Alternatively, a character
  string containing additional tags that follows the Xeno-Canto advanced
  query syntax can be provided. Tags are of the form 'tag:searchterm'.
  For instance, `'type:"song"'` will search for recordings where the
  sound type contains 'song'. Multiple tags can be provided (e.g.,
  `'"cnt:"belize" type:"song"'`). See examples down below and check
  [Xeno-Canto's search help](https://www.xeno-canto.org/help/search) for
  a full description.

- api_key:

  Character string refering to the key assigned by Xeno-Canto as
  authorization for searches. Get yours at
  <https://xeno-canto.org/account>. Required.

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

This function queries metadata for animal sound recordings in the
open-access online repository [Xeno-Canto](https://www.xeno-canto.org/).
[Xeno-Canto](https://www.xeno-canto.org/) hosts sound recordings of
birds, frogs, non-marine mammals and grasshoppers. Complex queries can
be constructed using the [Xeno-Canto](https://www.xeno-canto.org/)
advanced query syntax (see examples).

## References

Planqué, Bob, & Willem-Pier Vellinga. 2008. Xeno-canto: a 21st-century
way to appreciate Neotropical bird song. Neotrop. Birding 3: 17-23.

## See also

[`query_gbif`](https://marce10.github.io/suwo/reference/query_gbif.md),
[`query_wikiaves`](https://marce10.github.io/suwo/reference/query_wikiaves.md),
[`query_inaturalist`](https://marce10.github.io/suwo/reference/query_inaturalist.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (interactive()){
# An API key is required. Get yours at https://xeno-canto.org/account.
XC_API_KEY <- "YOUR_API_KEY_HERE"

# Simple search for a species
p_anth <- query_xenocanto(species = "Phaethornis anthophilus",
api_key = XC_API_KEY)

# Search for same species and add specify country
p_anth_cr <- query_xenocanto(
species = 'sp:"Phaethornis anthophilus" cnt:"Panama"',
raw_data = TRUE, api_key = XC_API_KEY)

# Search for female songs of a species
femsong <-  query_xenocanto(
species = 'sp:"Thryothorus ludovicianus" type:"song" type:"female"',
api_key = XC_API_KEY)
}
```
