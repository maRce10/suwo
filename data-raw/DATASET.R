## code to prepare `DATASET` dataset goes here
# see https://r-pkgs.org/data.html#sec-data-sysdata

# Taxon code for macaulay
ml_taxon_code <- read.csv("./examples/Clements-v2024-October-2024-rev.csv", na.strings = "")

# example data for Turdus rufiventris
term <- "Turdus rufiventris"

options(all_data = TRUE, verbose = TRUE, mc.cores = 1)
basic_colms <- .format_query_output(only_basic_columns = T)
xc_adt <- query_xenocanto(term = term)
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
tur_ruf_list <- lapply(adt_list, head)

names(tur_ruf_list) <- c("xeno_canto_sounds", "wiki_aves_sounds", "wiki_aves_images", "gbif_sounds", "gbif_images", "inaturalist_sounds", "inaturalist_images", "macaulay_sounds", "macaulay_images")

usethis::use_data(ml_taxon_code, tur_ruf_list, internal = TRUE, overwrite = TRUE)
