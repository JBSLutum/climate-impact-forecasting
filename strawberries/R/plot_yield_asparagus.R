#plot_yield_asparagus(MC_results_scenarios)

plot_yield_asparagus <- function(sim_results_scen) {

  
  # --- 1) Szenario-Frames bauen (Suffix entfernen) ---
  mk <- function(sfx, lab) {
    sim_results_scen$y %>%
      dplyr::select(dplyr::ends_with(sfx)) %>%
      dplyr::rename_with(~ stringr::str_remove(.x, paste0(sfx, "$"))) %>%
      dplyr::mutate(scenario = lab)
  }
  
  results_yield_all <- dplyr::bind_rows(
    mk("_today", "2020"),                # vorher: "Year 2020"
    mk("_ssp1",  "SSP1-2.6"),
    mk("_ssp2",  "SSP2-4.5"),
    mk("_ssp3",  "SSP3-7.0"),
    mk("_ssp5",  "SSP5-8.5")
  )
  
  # Spalten benennen (ggf. anpassen)
  names(results_yield_all) <- c("Ertrag", "vermarktbarer_Ertrag", "id", "scenario")
  
  # --- 2) Long-Format + Jahr-Gruppe (Facet) ---
  results_yield_all_longer <- results_yield_all %>%
    tidyr::pivot_longer(cols = c(Ertrag, vermarktbarer_Ertrag)) %>%
    mutate(
      # Facet-Gruppe (Überschrift oben)
      year_grp = if_else(scenario == "2020", "Jahr 2020", "Jahr 2075"),
      # gewünschte Reihenfolge auf der x-Achse (Labels unten)
      scenario = factor(
        scenario,
        levels = c("2020", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
      ),
      # Legendenlabels erst im Scale setzen; hier nur Reihenfolge der beiden Reihen
      name = factor(name, levels = c("Ertrag", "vermarktbarer_Ertrag"))
    )
  
  # --- 3) Summary für Prozent-Label (rohe Namen verwenden) ---
  summary_df <- results_yield_all_longer %>%
    dplyr::group_by(scenario, name) %>%
    dplyr::summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = name, values_from = mean_value) %>%
    dplyr::mutate(
      percent  = (`vermarktbarer_Ertrag` / `Ertrag`) * 100,
      year_grp = if_else(scenario == "2020", "Jahr 2020", "Jahr 2075")
    )
  
  # --- top vom boxplot rausfinden ---
  whisk_df <- results_yield_all_longer |>
    dplyr::group_by(year_grp, scenario, name) |>
    dplyr::summarise(upper = boxplot.stats(value)$stats[5], .groups = "drop")
  
  label_df <- whisk_df |>
    dplyr::group_by(year_grp, scenario) |>
    dplyr::summarise(y_pos = max(upper), .groups = "drop") |>
    dplyr::left_join(
      summary_df |> dplyr::select(year_grp, scenario, percent),
      by = c("year_grp","scenario")
    )
  
  y_lab_global <- max(label_df$y_pos, na.rm = TRUE) * 1.04
  label_df2 <- dplyr::mutate(label_df, y_lab = y_lab_global)
  
  # --- Plot direkt mit y_lab nutzen ---
  plot <- ggplot(results_yield_all_longer,
                 aes(x = scenario, y = value, fill = name)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
    geom_text(
      data = label_df2,
      aes(x = scenario, y = y_lab, label = paste0(round(100 - percent,2), "%"),colour = "Prozent-Label"),
      vjust = -0.3, size = 4, inherit.aes = FALSE
    )+
    geom_hline(
      aes(yintercept = 14.56, linetype = "baseline"),
      colour = "black", linewidth = 0.5, inherit.aes = FALSE
    ) +
    facet_grid(~ year_grp, scales = "free_x", space = "free_x") +
    coord_cartesian(clip = "off") +
    scale_y_continuous(name = "Ertrag [dt/ha]",
                       expand = expansion(mult = c(0.02, 0.10))) +
    scale_x_discrete(name = "Klimaszenario") +
    scale_fill_manual(
      name   = NULL,
      values = c("Ertrag" = "cadetblue", "vermarktbarer_Ertrag" = "firebrick"),
      labels = c("Ertrag" = "Potentieller Ertrag\nohne Schäden",
                 "vermarktbarer_Ertrag" = "Vermarktbarer Ertrag")
    ) +
    scale_linetype_manual(
      name   = NULL,
      values = c(baseline = "dashed"),
      labels = c(baseline = "Durchschnittsertrag Regierungsbezirk\nKöln 14,56 t/ha")
    ) +
    scale_color_manual(
      name   = NULL,
      values = c("Prozent-Label" = "black"),
      labels="Verlust durch Schäden"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "bottom",
      legend.text      = element_text(size = 16),
      strip.placement  = "outside",
      strip.background = element_rect(fill = "lightgrey", colour = NA),
      strip.text       = element_text(size = 12, face = "bold"),
      panel.border     = element_rect(colour = "grey40", fill = NA, linewidth = 0.6),
      panel.spacing.x  = grid::unit(10, "pt")
    )+
    guides(
      colour = guide_legend(
        order = 1,
        override.aes = list(label = "%", size = 5)),
      linetype = guide_legend(order = 2),
      fill     = guide_legend(order = 3)
    )
  return(plot)
}

