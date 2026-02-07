yield_reduction_plot_strawb <- function(mcSimulation_results) {
  
  ############################################################
  ## 1) Definitions
  ############################################################
  
  scenario_order_codes <- c("today", "ssp1", "ssp2", "ssp3", "ssp5")
  
  scenario_labels <- c(
    today  = "2020",
    ssp1   = "2075\n(SSP 1-2.6)",
    ssp2   = "2075\n(SSP 2-4.5)",
    ssp3   = "2075\n(SSP 3-7.0)",
    ssp5   = "2075\n(SSP 5-8.5)"
  )
  
  stress_labels <- c(
    "summer_drought_stress_occ" = "Trockenstress (Sommer)",
    "summer_insect_occ"          = "Insekten (Sommer)",
    "summer_disease_occ"         = "Krankheit (Sommer)",
    "spring_disease_occ"         = "Krankheit (Frühling)",
    "spring_frost_occ"           = "Frost (Frühling)",
    "fruit_frost_occ"            = "Frost (Fruchtreife)",
    "fruit_drought_stress_occ"   = "Trockenstress (Fruchtreife)",
    "fruit_hail_occ"             = "Hagel (Fruchtreife)",
    "fruit_sunburn_occ"          = "Sonnenbrand (Fruchtreife)",
    "fruit_insect_occ"           = "Insekten (Fruchtreife)",
    "fruit_disease_occ"          = "Krankheit (Fruchtreife)",
    "fruit_snail_occ"            = "Schnecken (Fruchtreife)",
    "harvest_frost_occ"          = "Frost (Ernte)",
    "harvest_heat_occ"           = "Hitze (Ernte)",
    "harvest_hail_occ"           = "Hagel (Ernte)",
    "harvest_rain_occ"           = "Starkregen (Ernte)",
    "harvest_fly_occ"            = "Kirschessigfliege (Ernte)"
  )
  
  phase_order <- c("Sommer", "Frühling", "Fruchtreife", "Ernte")
  
  ############################################################
  ## 2) Data preparation
  ############################################################
  
  scen_pattern <- paste(scenario_order_codes, collapse = "|")
  
  yield_damage_long <- as.data.frame(mcSimulation_results$x) |>
    dplyr::select(dplyr::matches(paste0("_occ_(", scen_pattern, ")$"))) |>
    tidyr::pivot_longer(
      cols      = dplyr::everything(),
      names_to  = "full_name",
      values_to = "damage"
    ) |>
    dplyr::mutate(full_name = sub("^x\\.", "", full_name)) |>
    tidyr::extract(
      full_name, 
      into = c("CleanVar", "scenario"), 
      regex = "(.*_occ)_(.*)$", 
      remove = FALSE
    ) |>
    dplyr::mutate(
      scenario = factor(
        scenario,
        levels = scenario_order_codes,
        labels = scenario_labels[scenario_order_codes]
      )
    )
  
  ############################################################
  ## 3) Summarise & Label Logic
  ############################################################
  
  yield_red_summary <- yield_damage_long |>
    dplyr::filter(CleanVar %in% names(stress_labels)) |>
    dplyr::mutate(
      CleanVar   = factor(CleanVar, levels = names(stress_labels)),
      stress_lab = stress_labels[as.character(CleanVar)],
      Phase      = sub(".*\\((.*)\\).*", "\\1", stress_lab),
      Phase      = factor(Phase, levels = phase_order)
    ) |>
    dplyr::group_by(scenario, CleanVar, stress_lab, Phase) |>
    dplyr::summarise(
      mean_reduction = mean(damage, na.rm = TRUE) * 100,
      .groups        = "drop"
    ) |>
    # Hier wird die Rundung und die "<1%" Regel angewendet
    dplyr::mutate(
      label_text = dplyr::case_when(
        mean_reduction < 0.5 ~ "<1%", 
        TRUE ~ paste0(round(mean_reduction, 0), "%")
      )
    )
  
  heatmap_plot <- ggplot(
    yield_red_summary,
    aes(
      x    = scenario,
      y    = stress_lab,
      fill = mean_reduction
    )
  ) +
    geom_tile(color = "white") +
    geom_text(
      aes(label = label_text), # Nutzt das neue Label
      size = 3.5
    ) +
    scale_fill_gradient(
      name   = "Mittlerer\nSchaden [%]",
      low    = "white",
      high   = "firebrick"
    ) +
    labs(
      x = "Klimaszenario",
      y = "Stressfaktor"
    ) +
    facet_grid(
      rows   = vars(Phase),
      scales = "free_y",
      space  = "free_y"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x        = element_text(size = 10),
      axis.text.y        = element_text(size = 10),
      strip.text.y       = element_text(angle = -90, size = 10), 
      strip.background   = element_rect(fill = "grey95", colour = NA),
      legend.position    = "right",
      legend.key.height  = unit(1.5, "cm"),
      panel.spacing.y    = unit(0.5, "lines")
    )
  
  return(heatmap_plot)
}