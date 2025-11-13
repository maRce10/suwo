# Download media files from repositories

`download_media` downloads recordings and metadata from
[Xeno-Canto](https://www.xeno-canto.org/),
[wikiaves](https://www.wikiaves.com.br/) or
[gbif](https://www.gbif.org/).

## Usage

``` r
download_media(
  metadata,
  path = ".",
  pb = getOption("pb", TRUE),
  verbose = getOption("verbose", TRUE),
  cores = getOption("mc.cores", 1),
  overwrite = FALSE,
  folder_by = NULL
)
```

## Arguments

- metadata:

  data frame previously obtained from any suwo query function (i.e.
  \`query_reponame()\`).

- path:

  Directory path where the output media files will be saved. By default
  files are saved into the current working directory (`"."`).

- pb:

  Logical argument to control if progress bar is shown. Default is
  `TRUE`. Can be set globally for the current R session via the "pb"
  option ( `options(pb = TRUE)`).

- verbose:

  Logical argument that determines if text is shown in console. Default
  is `TRUE`. Can be set globally for the current R session via the
  "verbose" option ( `options(verbose = TRUE)`).

- cores:

  Numeric vector of length 1. Controls whether parallel computing is
  applied by specifying the number of cores to be used. Default is 1
  (i.e. no parallel computing). Can be set globally for the current R
  session via the "mc.cores" option (e.g. `options(mc.cores = 2)`). Note
  that some repositories might not support parallel queries from the
  same IP address as it might be identified as denial-of-service
  cyberattack.

- overwrite:

  Logical. If TRUE, existing files (in `"path"`) with the same name will
  be overwritten. Default is FALSE.

- folder_by:

  Character string with the name of a character or factor column in the
  metadata data frame. If supplied the function will use the unique
  values in that column to create subfolders within `"path"` and the
  files will be downloaded into the corresponding folder. By default no
  subfolders are created and all files are saved in the path provided.
  Missing values (NAs) are saved in a folder called
  `paste0("unknown_", folder_by)`. Special characters that are not
  allowed in folder names will be modified or removed. If any of the
  folder names already exist in `"path"`, they will be used as is.

## Value

Downloads media files into the supplied directory path (`"path"`) and
returns (invisibly) the input data frame with two additional columns:
`downloaded_file_name` with the name of the downloaded file (if
downloaded or already in the directory), and `download_status` with the
result of the download process for each file (either "saved",
"overwritten", "already there (not downloaded)", or "failed").

## Details

This function will take the output data frame of any of the "query_x()"
functions and download the associated media files. The function will
download all files into a single directory (argument `"path"`). File
downloading process can be interrupted and resume later as long as the
working directory is the same. By default only the missing files will be
downloaded when resuming. Users only need to rerun the same function
call. Can also be used on a updated query output (see
[`update_metadata`](https://marce10.github.io/suwo/reference/update_metadata.md))
to add the new media files to the existing media pool.

## See also

[`query_gbif`](https://marce10.github.io/suwo/reference/query_gbif.md),
[`query_macaulay`](https://marce10.github.io/suwo/reference/query_macaulay.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
  h_peckii <- query_inaturalist(species = 'Hydnellum peckii',
  format = "image")

# run if query didnt fail
 if (!is.null(h_peckii)) {
  # donwload the first to files
  phae_anth_downl <- download_media(metadata = h_peckii[1:2, ],
  path = tempdir())
}
#> ✔ All files were downloaded successfully 🌈
```
