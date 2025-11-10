# Searches for media files in the Macaulay Library

`query_macaulay` searches for metadata from
[macaulay](https://www.macaulaylibrary.org/).

## Usage

``` r
query_macaulay(
  species = getOption("species"),
  format = c("sound", "image", "video"),
  verbose = getOption("verbose", TRUE),
  all_data = getOption("all_data", FALSE),
  raw_data = getOption("raw_data", FALSE),
  path = ".",
  files = NULL,
  dates = NULL,
  taxon_code_info = ml_taxon_code
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
  'sound', 'image' of 'video'. Required.

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

- path:

  Directory path where the .csv file will be saved. By default it is
  saved into the current working directory (`"."`).

- files:

  Optional character vector with the name(s) of the .csv file(s) to
  read. If not provided, the function will open a browser window to the
  search results page, where the user must download a .csv file with the
  metadata.

- dates:

  Optional numeric vector with years to split the search. If provided,
  the function will perform separate queries for each date range
  (between consecutive date values) and combine the results. Useful for
  queries that return large number of results (i.e. \> 10000 results
  limit). For example, to search for the species between 2010 to 2020
  and between 2021 to 2025 use `dates = c(2010, 2020, 2025)`. If years
  contain decimals searches will be split by months within years as
  well.

- taxon_code_info:

  Data frame containing the taxon code information. By default the
  function will use the internal data frame `"ml_taxon_code"` included
  as example data in the package. This object contains the data from the
  October-2025 eBird taxonomy (downloaded from
  <https://www.birds.cornell.edu/clementschecklist>). If new versions of
  the list become available it will be updated in new package versions.
  However, if users need to update it they can download the new list
  version, read it in R as a data frame and provide it to the function
  through this argument.

## Value

The function returns a data frame with the metadata of the media files
matching the search criteria. If `all_data = TRUE`, all metadata fields
(columns) are returned. If `raw_data = TRUE`, the raw data as obtained
from the repository is returned (without any formatting).

## Details

This function queries for species observation info in the [Macaulay
Library](https://www.macaulaylibrary.org/) online repository and returns
the metadata of media files matching the query. The Macaulay Library is
the world’s largest repository of digital media (audio, photo, and
video) of wildlife, and their habitats. The archive hosts more than 77
million images, 3 million sound recordings, and 350k videos, from more
than 80k contributors, and is integrated with eBird, the world’s largest
biodiversity dataset. This is an interactive function which opens a
browser window to the Macaulay Library's search page, where the user
must download a .csv file with the metadata. The function then reads the
.csv file and returns a data frame with the metadata.

Here are some instructions for using this function properly:

- Valid species names can be checked at `suwo:::ml_taxon_code$SCI_NAME`.

- Users must save the save the .csv file manually

- *If the file is saved overwritting a pre-existing file (i.e. same file
  name) the function will not detect it*

- A maximum of 10000 records per query can be returned, but this can be
  bypassed by using the `dates` argument to split the search into
  smaller date ranges

- Users must log in to the Macaulay Library/eBird account in order to
  access large batches of observations

## References

Scholes III, Ph.D. E (2015). Macaulay Library Audio and Video
Collection. Cornell Lab of Ornithology. Occurrence dataset
https://doi.org/10.15468/ckcdpy accessed via GBIF.org on 2024-05-09.
Clements, J. F., P. C. Rasmussen, T. S. Schulenberg, M. J. Iliff, J. A.
Gerbracht, D. Lepage, A. Spencer, S. M. Billerman, B. L. Sullivan, M.
Smith, and C. L. Wood. 2025. The eBird/Clements checklist of Birds of
the World: v2025. Downloaded from
https://www.birds.cornell.edu/clementschecklist/download/

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (interactive()){
# query sounds
tur_ili <- query_macaulay(species = "Turdus iliacus", format = "sound",
path = tempdir())

# test a query with more than 10000 results paging by date
cal_cos <- query_macaulay(species = "Calypte costae", format = "image",
path = tempdir(), dates = c(1976, 2019, 2022, 2024, 2025, 2026))

# this is how the internal function that splits the search by year intervals
works
# it can split by entire year intervals
suwo:::.date_ranges(x = c(1976, 2020, 2022, 2024, 2025, 2026))

# or by year-month intervals if dates have decimals
# (note that it cannot split across years)
suwo:::.date_ranges(x = seq(2020, 2026, length.out = 10))

## update clement list (note that this is actually the same list used in the
# current 'suwo' version, just for the sake of the example)

# url to the clements list version october 2024
# (split so it is not truncaded by CRAN)
clements_url <- paste0(
"https://www.birds.cornell.edu/clementschecklist/wp-content/uploads/2024/10",
"/Clements-v2024-October-2024-rev.csv"
)

# read list from url
new_clements <- read.csv(clements_url)

# provide "updated" clements list to query_macaulay()
tur_ili2 <- query_macaulay(species = "Turdus iliacus", format = "sound",
 taxon_code_info = new_clements, path = tempdir())
}
```
