# Merge metadata data frames

`merge_metadata` merges metadata data frames from suwo queries.

## Usage

``` r
merge_metadata(...)
```

## Arguments

- ...:

  two or more data frames (each one as a separate entry) referring to
  the metadata obtained from suwo query functions (\`query_x()\`).

## Value

A single data frame with the data from all input data frames combined
and with an additional column named \`source\` indicating the original
data frame from which each row originated.

## Details

This function combines metadata from multiple sources (e.g. WikiAves and
xeno-canto) into a single data frame for easier analysis and comparison.
Each input data frame must be obtained from one of the suwo query
functions (e.g., \`query_wikiaves()\`, \`query_xenocanto()\`, etc.) with
\`raw_data = FALSE\`.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
# get metadata from 2 repos
wa <- query_wikiaves(species = "Glaucis dohrnii", format =  "sound")
gb <- query_gbif(species = "Glaucis dohrnii", format = "sound")
#> Warning: URL 'https://api.gbif.org/v1/occurrence/search?limit=300&&scientificName=Glaucis%20dohrnii&media_type=Sound': status was 'SSL connect error'

# run if queries didnt fail
 if (!is.null(wa) && !is.null(gb)) {
 # combine metadata
 merged_mt <- merge_metadata(wa, gb)
}
```
