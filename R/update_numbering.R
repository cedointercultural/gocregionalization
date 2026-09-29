update_numbering <- function(goc.val, layer.name) {

  googlesheets4::gs4_deauth()

  table.rev <- googlesheets4::read_sheet("1KGbhvBVplhrY1Y0cPLe_lY6Bk7e9ZjpFL7TU5TEHNJE", sheet="original_numbers") %>%
    dplyr::select(-ar_sq_k,-mdpnt_lt,-mdpnt_ln)

  rev.num <- dplyr::left_join(goc.val, table.rev, by = c("box_no")) %>%
    dplyr::select(-box_no) %>%
    dplyr::rename(box_no=new_box_no)

  sf::st_write(rev.num, here::here("outputs", paste0(layer.name,".shp")), delete_layer = TRUE)


  # Ensure rev.num is an sf object
  rev.num_sf <- sf::st_as_sf(rev.num)

  # Calculate polygon centroids for labels
  centroids <- sf::st_centroid(rev.num_sf) %>%
    dplyr::mutate(
      x = sf::st_coordinates(.)[, 1],
      y = sf::st_coordinates(.)[, 2]
    ) %>%
    sf::st_set_geometry(NULL)

  # Create publication-quality map with ggplot2
  map_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = rev.num_sf,
      ggplot2::aes(fill = region),
      color = "white",
      size = 0.3
    ) +
    ggplot2::geom_text(
      data = centroids,
      ggplot2::aes(x = x, y = y, label = box_no),
      size = 4,
      fontface = "bold",
      color = "black",
      check_overlap = TRUE
    ) +
    ggplot2::scale_fill_brewer(palette = "Set2", name = "Region") +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, margin = ggplot2::margin(b = 10)),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 9),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 10, face = "bold"),
      legend.text = ggplot2::element_text(size = 9),
      panel.grid = ggplot2::element_line(color = "gray90", size = 0.2),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
    ) +
    ggplot2::labs(
      title = "Atlantis GOC model polygons"
    )

  # Save the map in publication quality formats
   ggplot2::ggsave(
    filename = here::here("outputs", "goc_regionalization_map.png"),
    plot = map_plot,
    width = 12,
    height = 10,
    dpi = 300,
    units = "in"
  )


  return(list(data = rev.num, map = map_plot))
}
