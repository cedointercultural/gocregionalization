clean_shapefile <- function(goc.rev, layer.name){

  goc.rev.nonempty <- goc.rev[!sf::st_is_empty(goc.rev), ]

  clean.goc <- goc.rev.nonempty %>%
    sf::st_make_valid() %>%
    dplyr::select(geometry) %>%
    dplyr::mutate(box_no = 1:dplyr::n(),
                  area_sq_m = sf::st_area(geometry),
                  area_sq_km = units::set_units(area_sq_m, km^2)) %>%
    dplyr::select(-area_sq_m)

  rownames(clean.goc) <- 1:nrow(clean.goc)

  clean.goc.centroids <- clean.goc %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(
      centroid_4326 = sf::st_centroid(geometry),
      midpoint_lat = sf::st_coordinates(centroid_4326)[, 2],
      midpoint_lon = sf::st_coordinates(centroid_4326)[, 1]
    ) %>%
    dplyr::select(-centroid_4326)

  #extract table


  table.goc <- sf::st_drop_geometry(clean.goc.centroids)

  readr::write_csv(table.goc, here::here("outputs", paste0(layer.name, "_table.csv")))

  sf::st_write(clean.goc.centroids, here::here("outputs", paste0(layer.name,".shp")), delete_layer = TRUE)
}
