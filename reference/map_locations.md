# Maps of media records

`map_locations` creates maps to visualize the geographic spread of media
records.

## Usage

``` r
map_locations(
  metadata,
  cluster = FALSE,
  palette = viridis::viridis,
  by = "species"
)
```

## Arguments

- metadata:

  data frame previously obtained from any suwo query function (i.e.
  \`query_reponame()\`).

- cluster:

  Logical to control if icons are clustered by locality. Default is
  `FALSE`.

- palette:

  Color palette function used for location markers.

- by:

  Name of column to be used for coloring markers. Default is "species".

## Value

An interacrive map with the locations of the observations.

## Details

This function creates maps for visualizing the geographic spread of
observations. Note that only observations with geographic coordinates
are displayed.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre

## Examples

``` r
# search in xeno-canto
e_hochs <- query_gbif(species = "Entoloma hochstetteri", format = "image")
#> ✔ Obtaining metadata (1939 matching records found) 🎉:

# run if query didnt fail
 if (!is.null(e_hochs)) {

# create map
map_locations(e_hochs)
}
#> Error in grDevices::hcl.colors(): argument "n" is missing, with no default
```
