## code to prepare `DATASET` dataset goes here
# see https://r-pkgs.org/data.html#sec-data-sysdata

# Taxon code for macaulay
ml_taxon_code <- read.csv("./examples/Clements-v2024-October-2024-rev.csv", na.strings = "")

# example data for Turdus rufiventris
options(term = "Turdus rufiventris", all_data = TRUE, verbose = TRUE, mc.cores = 1)
xc_adt <- query_xenocanto(term = term, key = Sys.getenv("XENO_CANTO_API_KEY"))
wa_adt_s <- query_wikiaves(term = term, format = "sound")
wa_adt_i <- query_wikiaves(term = term, format = "image")
gb_adt_s <- query_gbif(term = term, format = "sound")
gb_adt_i <- query_gbif(term = term, format = "image")
in_adt_s <- query_inaturalist(term = term, format = "sound")
in_adt_i <- query_inaturalist(term = term, format = "image")
ml_adt_s <- query_macaulay(term = term, format = "sound", path = tempdir())
ml_adt_i <- query_macaulay(term = term, format = "image", path = tempdir())

adt_list <- list(xc_adt = xc_adt, wa_adt_s = wa_adt_s, wa_adt_i = wa_adt_i, gb_adt_s = gb_adt_s, gb_adt_i = gb_adt_i, in_adt_s = in_adt_s, in_adt_i= in_adt_i, ml_adt_s = ml_adt_s, ml_adt_i = ml_adt_i)

## save turdus_rufiventris results for example data
tur_ruf_list <- lapply(adt_list, subset_metadata, 1:4)

# keep all rows for inaturalist but only basic columns
attrbts <- attributes(tur_ruf_list$in_adt_s)
tur_ruf_list$in_adt_s <- adt_list$in_adt_s

# add missing attributes
attributes(tur_ruf_list$in_adt_s) <- c(attributes(tur_ruf_list$in_adt_s), attrbts[setdiff(names(attrbts), names(attributes(tur_ruf_list$in_adt_s)))])

# check number of rows
sapply(tur_ruf_list, nrow)

names(tur_ruf_list) <- c("xeno-canto_sounds", "wikiaves_sounds", "wikiaves_images", "gbif_sounds", "gbif_images", "inaturalist_sounds", "inaturalist_images", "macaulay_sounds", "macaulay_images")

xc_adf <-  query_xenocanto(all_data = FALSE, api_key = Sys.getenv("XENO_CANTO_API_KEY"))
gb_adf_s <- query_gbif(term = term, format = "sound", all_data = FALSE)
ml_adf_s <- query_macaulay(term = term, format = "sound", path = tempdir(), all_data = FALSE)

merged_metadata <- merge_metadata(xc_adf, gb_adf_s, ml_adf_s)

# get example macaulay data for calypte coste to show spliting by range
cal_cos <- query_macaulay(
  term = "Calypte costae",
  format = "image",
  path = tempdir(),
  dates = c(1976, 2019)
)

# cal_cos <- query_macaulay(term = "Calypte costae", format = "image",
                          #' path = tempdir(), dates = c(1976, 2019, 2022, 2024, 2025, 2026))

tur_ruf_list <- lapply(tur_ruf_list, function(x){
  attributes(x)$all_data <- FALSE
  return(x)
})



usethis::use_data(ml_taxon_code, tur_ruf_list, merged_metadata, internal = TRUE, overwrite = TRUE)


