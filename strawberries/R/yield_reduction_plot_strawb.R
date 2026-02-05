yield_reduction_plot_strawb <- function(mcSimulation_results) {
  
  ############################################################
  ## 1) Definitions
  ############################################################
  
  # Diese Codes müssen exakt dem Ende deiner Spaltennamen in $x entsprechen
  scenario_order_codes <- c("today", "ssp1", "ssp2", "ssp3", "ssp5")
  
  # Hier mappen wir die kurzen Codes auf die schönen Anzeigenamen
  scenario_labels <- c(
    today  = "2020",
    ssp1   = "2075\n(SSP 1-2.6)",
    ssp2   = "2075\n(SSP 2-4.5)",
    ssp3   = "2075\n(SSP 3-7.0)",
    ssp5   = "2075\n(SSP 5-8.5)"
  )
  
  stress_labels <- c(
    "summer_drought_stress_occ" = "Trockenstress (Sommer)",
    "summer_insect_occ"         = "Insekten (Sommer)",
    "summer_disease_occ"        = "Krankheit (Sommer)",
    "spring_disease_occ"        = "Krankheit (Frühling)",
    "spring_frost_occ"          = "Frost (Frühling)",
    "fruit_frost_occ"           = "Frost (Fruchtreife)",
    "fruit_drought_stress_occ"  = "Trockenstress (Fruchtreife)",
    "fruit_hail_occ"            = "Hagel (Fruchtreife)",
    "fruit_sunburn_occ"         = "Sonnenbrand (Fruchtreife)",
    "fruit_insect_occ"          = "Insekten (Fruchtreife)",
    "fruit_disease_occ"         = "Krankheit (Fruchtreife)",
    "fruit_snail_occ"           = "Schnecken (Fruchtreife)",
    "harvest_frost_occ"         = "Frost (Ernte)",
    "harvest_heat_occ"          = "Hitze (Ernte)",
    "harvest_hail_occ"          = "Hagel (Ernte)",
    "harvest_rain_occ"          = "Starkregen (Ernte)",
    "harvest_fly_occ"           = "Kirschessigfliege (Ernte)"
  )
  
  phase_order <- c("Sommer", "Frühling", "Fruchtreife", "Ernte")
  
  ############################################################
  ## 2) Data preparation (from $x with suffix logic)
  ############################################################
  
  # Erzeuge Pattern für die Suche (z.B. _occ_ssp1, _occ_today, etc.)
  scen_pattern <- paste(scenario_order_codes, collapse = "|")
  
  yield_damage_long <- as.data.frame(mcSimulation_results$x) |>
    # Wähle nur die Spalten, die auf _occ_ und eines der Szenarien enden
    dplyr::select(dplyr::matches(paste0("_occ_(", scen_pattern, ")$"))) |>
    tidyr::pivot_longer(
      cols      = dplyr::everything(),
      names_to  = "full_name",
      values_to = "damage"
    ) |>
    # Entferne x. Präfix falls vorhanden
    dplyr::mutate(full_name = sub("^x\\.", "", full_name)) |>
    # Trenne Variable und Szenario am LETZTEN Unterstrich
    # Regex: (alles bis _occ) _ (Szenario-Code am Ende)
    tidyr::extract(
      full_name, 
      into = c("CleanVar", "scenario"), 
      regex = "(.*_occ)_(.*)$", 
      remove = FALSE
    ) |>
    # Erzeuge Factor für die Szenarien mit den schönen Labels
    dplyr::mutate(
      scenario = factor(
        scenario,
        levels = scenario_order_codes,
        labels = scenario_labels[scenario_order_codes]
      )
    )
  
  ############################################################
  ## 3) Summarise & build heatmap
  ############################################################
  
  yield_red_summary <- yield_damage_long |>
    # Nur Variablen behalten, für die wir Labels definiert haben
    dplyr::filter(CleanVar %in% names(stress_labels)) |>
    dplyr::mutate(
      CleanVar   = factor(CleanVar, levels = names(stress_labels)),
      stress_lab = stress_labels[as.character(CleanVar)],
      # Extrahiere Phase aus dem Text in den Klammern
      Phase      = sub(".*\\((.*)\\).*", "\\1", stress_lab),
      Phase      = factor(Phase, levels = phase_order)
    ) |>
    dplyr::group_by(scenario, CleanVar, stress_lab, Phase) |>
    dplyr::summarise(
      # Mittelwert der Vorkommen/Schäden * 100 für Prozentanzeige
      mean_reduction = mean(damage, na.rm = TRUE) * 100,
      .groups        = "drop"
    )
  
  # Falls manche Kombinationen fehlen, füllen wir sie mit 0 (optional)
  # yield_red_summary <- yield_red_summary |> tidyr::complete(scenario, stress_lab, fill = list(mean_reduction = 0))
  
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
      aes(
        label = paste0(round(mean_reduction, 1), "%")
      ),
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
    # facet_grid mit rows = vars(Phase) setzt die Phasen rechts untereinander
    facet_grid(
      rows   = vars(Phase),
      scales = "free_y",
      space  = "free_y",
      switch = NULL # NULL lässt die Labels rechts (Standard)
    ) +
    theme_minimal(base_size = 10) +
    theme(
      # Achsentexte
      axis.text.x        = element_text(size = 10),
      axis.text.y        = element_text(size = 10),
      
      # Facet-Labels (Phasen) rechts
      strip.text.y       = element_text(angle = -90, size = 10), 
      strip.background   = element_rect(fill = "grey95", colour = NA),
      
      # Legende rechts von oben nach unten
      legend.position    = "right",
      legend.direction   = "vertical",
      legend.title       = element_text(size = 10),
      legend.key.height  = unit(1.5, "cm"), # Macht die Legende etwas länger/schmaler
      
      # Abstände
      panel.spacing.y    = unit(0.5, "lines"),
      plot.margin        = margin(10, 10, 10, 10)
    )
  
  return(heatmap_plot)
}