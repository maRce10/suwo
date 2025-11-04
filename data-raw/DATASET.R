## code to prepare `DATASET` dataset goes here
# see https://r-pkgs.org/data.html#sec-data-sysdata


# Taxon code for macaulay
# downloaded from https://www.birds.cornell.edu/clementschecklist/introduction
#/updateindex/october-2025/2025-citation-checklist-downloads/
ml_taxon_code <- read.csv("./examples/eBird_taxonomy_v2025.csv",
                          na.strings = "")


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

options(all_data = TRUE)

h_sarapiquensis <- head(query_inaturalist(species = "Heliconia sarapiquensis",
                                          format = "image"),
                        4)
h_harpyja <- head(query_wikiaves(species = "Harpia harpyja"), 4)
a_hahneli <- query_xenocanto(
  species = 'sp:"Ameerega hahneli" cnt:"French Guiana" q:"A"',
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
