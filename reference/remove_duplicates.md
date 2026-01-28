# Remove duplicated media records

`remove_duplicates` removes duplicated media records.

## Usage

``` r
remove_duplicates(
  metadata,
  same_repo = FALSE,
  cores = getOption("mc.cores", 1),
  pb = getOption("suwo_pb", TRUE),
  repo_priority = c("Xeno-Canto", "GBIF", "iNaturalist", "Macaulay Library", "WikiAves",
    "Observation"),
  verbose = getOption("suwo_verbose", TRUE)
)
```

## Arguments

- metadata:

  data frame obtained from possible duplicates with the function
  [`find_duplicates`](https://marce10.github.io/suwo/reference/find_duplicates.md).
  The data frame must have the column 'duplicate_group' returned by
  [`find_duplicates`](https://marce10.github.io/suwo/reference/find_duplicates.md).

- same_repo:

  Logical argument indicating if observations labeled as duplicates that
  belong to the same repository should be removed. Default is `FALSE`.
  If `TRUE`, only one of the duplicated observations from the same
  repository will be retained in the output data frame. This is useful
  as it can be expected that observations from the same repository are
  not true duplicates (e.g. different recordings uploaded to Xeno-Canto
  with the same date, time and location by the same user), but rather
  have not been documented with enough precision to be told apart.

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

- repo_priority:

  Character vector indicating the priority of repositories when
  selecting which observation to retain when duplicates are found.
  Default is
  `c("Xeno-Canto", "GBIF", "iNaturalist", "Macaulay Library", "Wikiaves", "Observation")`,
  which gives priority to repositories in which media downloading is
  more straightforward (Xeno-Canto and GBIF).

- verbose:

  Logical argument that determines if text is shown in console. Default
  is `TRUE`. Can be set globally for the current R session via the
  "suwo_verbose" option ( `options(suwo_verbose = TRUE)`).

## Value

A single data frame with a subset of the 'metadata' with those
observations that were determined not to be duplicates.

## Details

This function removes duplicate observations identified with the
function
[`find_duplicates`](https://marce10.github.io/suwo/reference/find_duplicates.md).
When duplicates are found, one observation from each group of duplicates
is retained in the output data frame. However, if multiple observations
from the same repository are labeled as duplicates, by default
(`same_repo = FALSE`) all of them are retained in the output data frame.
This is useful as it can be expected that observations from the same
repository are not true duplicates (e.g. different recordings uploaded
to Xeno-Canto with the same date, time and location by the same user),
but rather have not been documented with enough precision to be told
apart. This behavior can be modified. If `same_repo = TRUE`, only one of
the duplicated observations from the same repository will be retained in
the output data frame. The function will give priority to repositories
in which media downloading is more straightforward (Xeno-Canto and
GBIF), but this can be modified with the argument 'repo_priority'.

## See also

[`find_duplicates`](https://marce10.github.io/suwo/reference/find_duplicates.md),
[`merge_metadata`](https://marce10.github.io/suwo/reference/merge_metadata.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
# get metadata from 2 repos
gb <- query_gbif(species = "Turdus rufiventris", format =  "sound")
#> ✔ Obtaining metadata (735 matching records found) 🥳:
#> ! 2 observations do not have a download link and were removed from the results (saved at `options('gbif_excluded_results')`). 

if(interactive()){
key <- "YOUR XENO CANTO API KEY"
xc <- query_xenocanto(species = "Turdus rufiventris", api_key = key)

# combine metadata
merged_metadata <- merge_metadata(xc, gb)

# find duplicates
label_dup_metadata <- find_duplicates(metadata = merged_metadata)

# remove duplicates
dedup_metadata <- remove_duplicates(label_dup_metadata)
}
```
