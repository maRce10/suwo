# Find duplicated entries in metadata

`find_duplicates` detect possible duplicated entries from merged
metadata from several repositories.

## Usage

``` r
find_duplicates(
  metadata,
  sort = TRUE,
  criteria = "country > 0.8 & locality > 0.5 & user_name > 0.8 & time == 1 & date == 1",
  verbose = getOption("verbose", TRUE)
)
```

## Arguments

- metadata:

  data frame obtained from combining the output metadata of two or more
  suwo query function using the \`merge_metadata()\` function. Data
  frames obtained from a single suwo query function can also be used but
  duplicates are not really expected within the same repository. The
  data frame must have the following columns: \`user_name\`,
  \`locality\`, \`repository\`, \`country\`, \`format\`, \`time\`, and
  \`date\`.

- sort:

  Logical argument indicating if the output data frame should be sorted
  by the \`duplicate_group\` column added by this function. This will
  group all potential duplicates together in the output data frame.
  Default is \`TRUE\`.

- criteria:

  A character string indicating the criteria to use to determine
  duplicates. By default, the criteria is set to
  `country > 0.8 & locality > 0.5 & user_name > 0.8 & time == 1 & date == 1`
  which means that two entries will be considered duplicates if they
  have a country similarity greater than 0.8, locality similarity
  greater than 0.5, user_name similarity greater than 0.8, and exact
  matches for time and date (similarities range from 0 to 1). These
  values have been found to work well in most cases. Users can modify
  this string to adjust the sensitivity of the duplicate detection based
  on their specific needs.

- verbose:

  Logical argument that determines if text is shown in console. Default
  is `TRUE`. Can be set globally for the current R session via the
  "verbose" option ( `options(verbose = TRUE)`).

## Value

A data frame with the input data frame and an additional column named
\`duplicate_group\` indicating potential duplicates with a common index.
Entries without potential duplicates are labeled as \`NA\` in this new
column.

## Details

This function compares the information in the entries of a combined
metadata data frame (typically the output of
[`merge_metadata`](https://marce10.github.io/suwo/reference/merge_metadata.md))
and labels those possible duplicates with a common index in a new column
named \`duplicate_group\`. The comparison is based on the similarity of
the following fields: \`user_name\`, \`locality\`, \`time\` and
\`country\`. Only rows with no missing data for those fields will be
considered. The function uses the \`RecordLinkage\` package to perform
the a fuzzy matching comparison and identify potential duplicates based
on predefined similarity thresholds (see argument 'criteria'). The
function only spots duplicates from different repositories and assumes
those duplicates should have the same \`format\` and \`date\`. This
function is useful for curating the data obtained by merging data from
multiple sources, as the same observation might be recorded in different
repositories. This is a common issue in citizen science repositories,
where users might upload the same observation to different platforms.
This can also occur as some repositories automatically share data with
other repositories, particularly with GBIF.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
# get metadata from 2 repos
gb <- query_gbif(species = "Turdus rufiventris", format =  "sound")
#> ✔ Obtaining metadata (735 matching records found) 🎊:
#> ! 2 observations do not have a download link and were removed from the results (saved at `options('gbif_excluded_results')`). 
inat <- query_inaturalist(species = "Turdus rufiventris",
  format = "sound")
#> ✔ Obtaining metadata (550 matching records found) 🎊:

# run if queries didnt fail
 if (!is.null(gb) && !is.null(inat)) {
# combine metadata
merged_metadata <- merge_metadata(inat, gb)

# find duplicates
label_dup_metadata <- find_duplicates(metadata = merged_metadata)
}
#> ℹ 39 potential duplicates found 
```
