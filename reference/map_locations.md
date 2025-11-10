# Maps of instances of observations by species

`map_locations` creates maps to visualize the geographic spread of suwo
recordings.

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
observations. Note that only recordings with geographic coordinates are
displayed.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre

## Examples

``` r
if (interactive()){
# search in xeno-canto
metadata <- query_xenocanto(species = "Phaethornis anthophilus")

# create map
map_locations(metadata)
}
```
