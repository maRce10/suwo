## code to prepare `DATASET` dataset goes here
# see https://r-pkgs.org/data.html#sec-data-sysdata

# Taxon code for macaulay
ml_taxon_code <- read.csv("./examples/Clements-v2024-October-2024-rev.csv",
                          na.strings = "")

# example data for Turdus rufiventris
# options(species = "Turdus rufiventris", all_data = TRUE, verbose = TRUE,
# mc.cores = 1)
# xc_adt <- query_xenocanto(species = "Thyroptera tricolor",
# api_key = Sys.getenv("XENO_CANTO_API_KEY"))
# wa_adt_s <- query_wikiaves(species = term, format = "sound")
# wa_adt_i <- query_wikiaves(species = "Harpia harpyja", format = "image")
# gb_adt_s <- query_gbif(species = term, format = "sound")
# gb_adt_i <- query_gbif(species = "Procyon lotor", format = "video")
# in_adt_s <- query_inaturalist(species = "Chorthippus eisentrauti",
# format = "sound")
# in_adt_i <- query_inaturalist(species = term, format = "image")
# ml_adt_s <- query_macaulay(species = term, format = "sound", path = tempdir())
# ml_adt_i <- query_macaulay(species = term, format = "image", path = tempdir())
#
# adt_list <- list(xc_adt = xc_adt, wa_adt_s = wa_adt_s, wa_adt_i = wa_adt_i,
# gb_adt_s = gb_adt_s, gb_adt_i = gb_adt_i, in_adt_s = in_adt_s,
# in_adt_i= in_adt_i, ml_adt_s = ml_adt_s, ml_adt_i = ml_adt_i)
#
# ## save turdus_rufiventris results for example data
# metadata_list <- lapply(adt_list, subset, 1:4)
#
# # keep all rows for inaturalist but only basic columns
# attrbts <- attributes(metadata_list$in_adt_s)
# metadata_list$in_adt_s <- adt_list$in_adt_s
#
# # add missing attributes
# attributes(metadata_list$in_adt_s) <- c(attributes(metadata_list$in_adt_s),
# attrbts[setdiff(names(attrbts), names(attributes(metadata_list$in_adt_s)))])
#
# # check number of rows
# vapply(metadata_list, nrow, integer(1))
#
# names(metadata_list) <- c("xeno-canto_sounds", "wikiaves_sounds",
# "wikiaves_images", "gbif_sounds", "gbif_images", "inaturalist_sounds",
# "inaturalist_images", "macaulay_sounds", "macaulay_images")

xc_adf <-  query_xenocanto(all_data = FALSE,
                           api_key = Sys.getenv("XENO_CANTO_API_KEY"))
gb_adf_s <- query_gbif(species = term,
                       format = "sound",
                       all_data = FALSE)
ml_adf_s <- query_macaulay(
  species = term,
  format = "sound",
  path = tempdir(),
  all_data = FALSE
)

merged_metadata <- merge_metadata(xc_adf, gb_adf_s, ml_adf_s)

# get example macaulay data for calypte coste to show spliting by range
# cal_cos <- query_macaulay(
#   species = "Calypte costae",
#   format = "image",
#   path = tempdir(),
#   dates = c(1976, 2019)
# )

# cal_cos <- query_macaulay(species = "Calypte costae", format = "image",
#' path = tempdir(), dates = c(1976, 2019, 2022, 2024, 2025, 2026))

# metadata_list <- lapply(metadata_list, function(x){
#   attributes(x)$all_data <- FALSE
#   return(x)
# })


options(all_data = TRUE)
# c_eisentrauti <- head(query_inaturalist(species = "Chorthippus eisentrauti"),
#4)
h_sarapiquensis <- head(query_inaturalist(species = "Heliconia sarapiquensis",
                                          format = "image"),
                        4)
h_harpyja <- head(query_wikiaves(species = "Harpia harpyja"), 4)
a_hahneli <- query_xenocanto(species =
                               'sp:"Ameerega hahneli" cnt:"French Guiana" q:"A"',
                             api_key = Sys.getenv("XENO_CANTO_API_KEY"))
p_lotor <- head(query_gbif(species = "Procyon lotor", format = "video"), 4)
t_rufiventris <- head(query_macaulay(
  species = "Turdus rufiventris",
  format = "sound",
  path = tempdir()
),
4)
t_tricolor <- head(query_xenocanto(
  species = "Thyroptera tricolor",
  api_key = Sys.getenv("XENO_CANTO_API_KEY")
),
4)

p_striigularis <- head(
  query_macaulay(
    species = "Phaethornis striigularis",
    format = "video",
    path = tempdir()
  ),
  4
)

d_holocanthus <- query_gbif(species = "Diodon holocanthus", format = "image")

# keep only JPEG records (for simplicity for this vignette)
d_holocanthus <- d_holocanthus[d_holocanthus$file_extension == "jpeg", ]

# select 6 random JPEG records
set.seed(666)
d_holocanthus <- d_holocanthus[sample(seq_len(nrow(d_holocanthus)), 6), ]

vignette_metadata <- list(
  h_wagneriana = h_wagneriana,
  h_harpyja = h_harpyja,
  a_hahneli = a_hahneli,
  p_lotor = p_lotor,
  t_tricolor = t_tricolor,
  t_rufiventris = t_rufiventris,
  h_sarapiquensis = h_sarapiquensis,
  p_striigularis = p_striigularis,
  d_holocanthus = d_holocanthus
)

usethis::use_data(
  ml_taxon_code,
  merged_metadata,
  vignette_metadata,
  internal = TRUE,
  overwrite = TRUE
)
