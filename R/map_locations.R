#' Maps of instances of observations by species
#'
#' \code{map_locations} creates maps to visualize the geographic spread of suwo
#'   recordings.
#' @param metadata Data frame output from suwo's media query functions.
#' @param cluster Logical to control if icons are clustered by locality. Default is \code{FALSE}.
#' @param palette Color palette function used for location markers.
#' @param by Name of column to be used for coloring markers. Default is "species".
#' @return An interacrive map with the locations of the observations.
#' @export
#' @name map_locations
#' @details This function creates maps for visualizing the geographic spread of observations. Note that only recordings with geographic coordinates are displayed.
#' @examples
#' \dontrun{
#' # search in xeno-canto
#' metadata <- query_xenocanto(term = "Phaethornis anthophilus")
#'
#' # create map
#' map_locations(metadata)
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) and Grace Smith Vidaurre

map_locations <- function(metadata,
                          cluster = FALSE,
                          palette = viridis::viridis,
                          by = "species") {
  # error message if leaflet is not installed
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    .stop("must install 'leaflet' to use leaflet style maps (when 'leaflet.map = TRUE')")
  }

  # make lat lon numeric and remove rows with no coords
  metadata$latitude <- as.numeric(as.character(metadata$latitude))
  metadata$longitude <- as.numeric(as.character(metadata$longitude))

  # remove observations with no lat lon data
  inx_with_coors <- !is.na(metadata$latitude) &
    !is.na(metadata$longitude)

  if (all(!inx_with_coors)){
    .failure_message("Not a single observation (row) has geographic coordinates")
    return(invisible(NULL))
}
  metadata <- metadata[inx_with_coors, , drop = FALSE]

  # make map

  # if only one species use subspecies for color marker
  # labels for hovering
  if (length(unique((metadata$species))) == 1)
    metadata$labels <- metadata[, by, drop = TRUE]

  cols <- palette(n = length(unique(metadata$labels)))

  # color for marker
  marker_color <- cols[as.numeric(as.factor(metadata$labels))]
  marker_color[metadata$labels == "Subsp. not provided"] <- "white"

  # use ios icons with marker colors
  icons <- leaflet::awesomeIcons(
    icon = "ios-close",
    iconColor = "black",
    library = "ion",
    markerColor = marker_color
  )

  # make content for popup
  content <- paste0(
    "<b><a href='https://www.xeno-canto.org/",
    metadata$Recording_ID,
    "'>",
    paste0("metadataC", metadata$Recording_ID),
    "</a></b>",
    "<br/><i>",
    paste(metadata$Genus, metadata$Specific_epithet, sep = " "),
    "</i><br/> Subspecies: ",
    metadata$Subspecies,
    "<br/> Country: ",
    metadata$Country,
    "<br/> Locality: ",
    metadata$Locality,
    "<br/> Voc.type: ",
    metadata$Vocalization_type,
    "<br/> Recordist: ",
    metadata$Recordist,
    paste0(
      "<b><a href='https://www.xeno-canto.org/",
      metadata$Recording_ID,
      "/download'>",
      "<br/>",
      "listen</a>"
    )
  )


  # make base map
  leaf_map <- leaflet::leaflet(metadata)

  # add tiles
  leaf_map <- leaflet::addTiles(leaf_map)

  # add markers
  if (cluster) {
    leaf_map <- leaflet::addAwesomeMarkers(
      map = leaf_map,
      ~ longitude,
      ~ latitude,
      icon = icons,
      label = ~ labels,
      popup = content,
      data = metadata,
      clusterOptions = leaflet::markerClusterOptions(),
      clusterId = "rec.cluster"
    )
  } else {
    leaf_map <- leaflet::addAwesomeMarkers(
      map = leaf_map,
      ~ longitude,
      ~ latitude,
      icon = icons,
      label = ~ labels,
      popup = content,
      data = metadata
    )
  }


  # add minimap view at bottom right
  leaf_map <- leaflet::addMiniMap(leaf_map)

  # add zoom-out button
  leaf_map <- leaflet::addEasyButton(
    leaf_map,
    leaflet::easyButton(
      icon = "fa-globe",
      title = "Zoom to full view",
      onClick = leaflet::JS("function(btn, map){ map.setZoom(1); }")
    )
  )

  if (cluster) {
    leaf_map <- leaflet::addEasyButton(leaf_map, leaflet::easyButton(
      states = list(
        leaflet::easyButtonState(
          stateName = "unfrozen-markers",
          icon = "ion-toggle",
          title = "Freeze Clusters",
          onClick = leaflet::JS(
            "
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'rec.cluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }"
          )
        ),
        leaflet::easyButtonState(
          stateName = "frozen-markers",
          icon = "ion-toggle-filled",
          title = "UnFreeze Clusters",
          onClick = leaflet::JS(
            "
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'rec.cluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }"
          )
        )
      )
    ))
  }


  # let users know that some observations were not
  if (any(!inx_with_coors)){
    cat(
    .color_text(
      paste(
        "{n} observation{?s} d{?oes/o} not have geographic coordinates and w{?as/ere} ignored"
      ),
      n =  sum(!inx_with_coors),
      as = "warning"
     )
    )
    }

  # plot map
  return(leaf_map)
}
