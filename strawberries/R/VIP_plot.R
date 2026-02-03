# ------------------------------------------------------------
# Pakete
# ------------------------------------------------------------
library(dplyr)
library(stringr)
library(tibble)
library(ggplot2)
library(purrr)
library(assertthat)
# optional: library(forcats)

# ------------------------------------------------------------
# Labels (erlaubte X-Variablen-Basisnamen -> deutsche Anzeige)
# ------------------------------------------------------------
labels_map <- c(
  "summer_drought_stress_dmg" = "Schaden: Trockenstress Sommer",
  "summer_insect_dmg" = "Schaden: Insekten Sommer",
  "summer_disease_dmg" = "Schaden: Krankheiten Sommer",
  "spring_disease_dmg" = "Schaden: Krankheiten Frühling",
  "spring_frost_dmg" = "Schaden: Frost Frühling",
  "fruit_frost_dmg" = "Schaden: Frost Fruchtreife",
  "fruit_drought_stress_dmg" = "Schaden: Trockenstress Fruchtreife",
  "fruit_hail_dmg" = "Schaden: Hagel Fruchtreife",
  "fruit_sunburn_dmg" = "Schaden: Sonnenbrand Fruchtreife",
  "fruit_insect_dmg" = "Schaden: Insekten Fruchtreife",
  "fruit_disease_dmg" = "Schaden: Krankheiten Fruchtreife",
  "fruit_snail_dmg" = "Schaden: Schnecken Fruchtreife",
  "harvest_frost_dmg" = "Schaden: Frost Ernte",
  "harvest_heat_dmg" = "Schaden: Hitze Ernte",
  "harvest_hail_dmg" = "Schaden: Hagel Ernte",
  "harvest_rain_dmg" = "Schaden: Starkregen Ernte",
  "harvest_fly_dmg" = "Schaden: Kirsch-Essig-Fliege Ernte",
  "summer_drought_stress_risk" = "Risiko: Trockenstress Sommer",
  "summer_insect_risk" = "Risiko: Insekten Sommer",
  "summer_disease_risk" = "Risiko: Krankheiten Sommer",
  "spring_disease_risk" = "Risiko: Krankheiten Frühling",
  "spring_frost_risk" = "Risiko: Spätfrost Frühling",
  "fruit_frost_risk" = "Risiko: Spätfrost Fruchtreife",
  "fruit_drought_stress_risk" = "Risiko: Trockenstress Fruchtreife",
  "fruit_hail_risk" = "Risiko: Hagel Fruchtreife",
  "fruit_sunburn_risk" = "Risiko: Sonnenbrand Fruchtreife",
  "fruit_insect_risk" = "Risiko: Insekten Fruchtreife",
  "fruit_disease_risk" = "Risiko: Krankheiten Fruchtreife",
  "fruit_snail_risk" = "Risiko: Schnecken Fruchtreife",
  "harvest_frost_risk" = "Risiko: Spätfrost Ernte",
  "harvest_heat_risk" = "Risiko: Extrmhitze Ernte",
  "harvest_hail_risk" = "Risiko: Hagel Ernte",
  "harvest_rain_risk" = "Risiko: Strakregen Ernte",
  "harvest_fly_risk" = "Risiko: Kirsch-Essik-Fliege Ernte",
  "harvest_mean_temp"  = "Durchschnittstemperatur Ernte",
  "fruit_mean_temp" = "Durchschnittstemperatur Fruchtreife",
  "spring_mean_temp" = "Durchschnittstemperatur Frühling",
  "winter_mean_temp" = "Durchschnittstemperatur Winter",
  "summer_mean_temp" = "Durchschnittstemperatur Sommer",
  "actual_chill" = "Chillportions über deen Winter",
  "spring_pollinator"  = "Bestäuberaktivität Frühling",
  "pad" = "Anzahl Photosynthesetage über den Sommer",
  "photosynthetic_active_days_needed" = "Bedarf an guten Photosynthesetage über den Sommer",
  "chill_need" = "Kältebedürfnis über den Winter",
  "expected_yield" = "Optimalertrag",
  "T_opt_fruit" = "Optimale Temperatur währned der Fruchtreife"

)

# gewünschte Anzeige-Reihenfolge (genau so wie labels_map)
label_order <- unname(labels_map)

# ------------------------------------------------------------
# Szenario-Codes & Labels
# ------------------------------------------------------------
scenario_order_codes <- c("today","ssp1","ssp2","ssp3","ssp5")
scenario_labels <- c(
  today = "2020",
  ssp1  = "2075 (SSP1-2.6)",
  ssp2  = "2075 (SSP2-4.5)",
  ssp3  = "2075 (SSP3-7.0)",
  ssp5  = "2075 (SSP5-8.5)"
)

# ------------------------------------------------------------
# VIP_table (deine Funktion, minimal gesäubert)
# ------------------------------------------------------------
VIP_table <- function (plsrResults, threshold = 0.8) {
  assertthat::assert_that(inherits(plsrResults, "mvr"),
                          msg = "plsrResults is not class 'mvr'.")
  
  VIP <- function(object) {
    if (object$method != "oscorespls") stop("Only implemented for oscorespls")
    if (nrow(object$Yloadings) > 1)     stop("Only for single-response models")
    SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
    Wnorm2 <- colSums(object$loading.weights^2)
    SSW <- sweep(object$loading.weights^2, 2, SS/Wnorm2, "*")
    sqrt(nrow(SSW) * apply(SSW, 1, cumsum)/cumsum(SS))
  }
  
  vipResult <- if (plsrResults$ncomp == 1) VIP(plsrResults) else VIP(plsrResults)["Comp 1", ]
  coef      <- plsrResults$coefficients[, , 1]
  
  pls_outputs <- tibble(
    Variable    = names(vipResult),
    VIP         = as.numeric(vipResult),
    Coefficient = as.numeric(coef)
  )
  
  filtered_table <- dplyr::filter(pls_outputs, VIP > threshold)
  list(VIP_table_results = filtered_table)
}

# ------------------------------------------------------------
# PLSR je Szenario -> VIP-Tabelle
#  - Y: marketable_yield_<scenario> (robust erkannt)
#  - X: nur Basenames aus labels_map (Suffix ./_today|ssp1..5 entfernt)
#  - Diagnose, falls Variablen kein Label haben
#  - include_unlabeled: solche Variablen trotzdem behalten (mit Fallback-Label)
# ------------------------------------------------------------
run_plsr_for_scenario <- function(
    sim_list,
    scenario = "today",
    vip_threshold = 0.8,
    include_unlabeled = FALSE   # <- wenn TRUE, bleiben Variablen ohne Label drin (mit Fallback-Label)
) {
  # --- Y auswählen: marketable_yield_<scenario>, robust erkennen ---
  # Erlaubt: "y.marketable_yield_today", "marketable_yield.today",
  #          "y_marketable_yield_today", "marketable_yield_today" usw.
  y_pat <- paste0("(?i)^(y[._])?marketable[._]?yield[._]?", scenario, "$")
  y_candidates <- grep(y_pat, names(sim_list$y), value = TRUE, perl = TRUE)
  
  if (length(y_candidates) == 0) {
    cand <- grep("(?i)marketable[._]?yield", names(sim_list$y), value = TRUE, perl = TRUE)
    stop(
      "Konnte die Y-Spalte nicht finden. Gesucht (Regex): ", y_pat, "\n",
      "Kandidaten in sim_list$y: ",
      if (length(cand)) paste(cand, collapse = ", ") else "<keine gefunden>"
    )
  }
  if (length(y_candidates) > 1) {
    message("Mehrere passende Y-Spalten gefunden, nehme die erste: ",
            paste(y_candidates, collapse = ", "))
  }
  y_col <- y_candidates[1]
  y_df  <- as.data.frame(sim_list$y[, y_col, drop = FALSE])
  
  # --- X vorbereiten & auf labels_map begrenzen ---
  x_df  <- as.data.frame(sim_list$x)
  x_all <- names(x_df)
  
  # Basename extrahieren (x.-Präfix weg), Suffix NICHT entfernen (wir brauchen es zur Auswahl)
  base <- sub("^x[.]", "", x_all, ignore.case = TRUE)
  
  # Flags zu Suffixen
  has_suffix   <- grepl("([._](today|ssp\\d))$", x_all, ignore.case = TRUE)
  this_suffix  <- grepl(paste0("([._]", scenario, ")$"), x_all, ignore.case = TRUE)  # exakt dieses Szenario
  is_neutral   <- !has_suffix
  
  # Basename OHNE Suffix (für labels_map-Abgleich)
  base_nosuf <- sub("([._](today|ssp\\d))$", "", base, ignore.case = TRUE)
  
  dx <- dplyr::tibble(
    col = x_all,
    base = base_nosuf,
    has_suffix = has_suffix,
    this_suffix = this_suffix,
    is_neutral = is_neutral
  ) %>%
    dplyr::filter(base %in% names(labels_map))   # nur erlaubte Basenames
  
  # pro Basename genau eine Spalte wählen: erst dieses Szenario, sonst neutral
  choice <- dx %>%
    dplyr::group_by(base) %>%
    dplyr::summarise(
      col = dplyr::coalesce(
        dplyr::first(col[this_suffix]),
        dplyr::first(col[is_neutral])
      ),
      .groups = "drop"
    )
  
  keep_x <- choice$col
  if (!length(keep_x)) stop("Keine passenden X-Variablen gefunden (labels_map + Szenariofilter).")
  
  # --- mcSimulation-Objekt bauen ---
  sim_obj <- list(y = y_df, x = x_df)
  class(sim_obj) <- c("mcSimulation","list")
  
  # --- PLSR ---
  pls_res <- decisionSupport::plsr.mcSimulation(
    object      = sim_obj,
    resultName  = y_col,         # exakt die erkannte Zielspalte
    variables.x = keep_x,
    ncomp       = 1
  )
  
  # --- VIP + Aufbereitung ---
  vip_raw <- VIP_table(pls_res, threshold = vip_threshold)$VIP_table_results %>%
    as_tibble() %>%
    mutate(
      Variable = as.character(Variable),
      # Suffix entfernen: _today/.today/_ssp1/.ssp1 etc.
      CleanVar = str_remove(Variable, regex("([._](today|ssp\\d))$", ignore_case = TRUE))
    )
  
  # Diagnose: welche Basenames haben kein Label?
  missing_map <- setdiff(unique(vip_raw$CleanVar), names(labels_map))
  
  # Nur gelabelte (Standard): strict
  vip_tab <- vip_raw %>%
    { if (!include_unlabeled) filter(., CleanVar %in% names(labels_map)) else . } %>%
    mutate(
      coef_sign = case_when(
        Coefficient >  0 ~ "positive",
        Coefficient <  0 ~ "negative",
        TRUE             ~ "zero"
      ),
      # Anzeige-Label: entweder Mapping oder Fallback (wenn include_unlabeled=TRUE)
      Variable = ifelse(CleanVar %in% names(labels_map), labels_map[CleanVar], CleanVar),
      Variable = as.character(Variable)
    ) %>%
    # harte NAs raus (vor allem falls include_unlabeled=FALSE)
    filter(!is.na(Variable), !is.na(VIP))
  
  # --- Szenario-Metadaten & Diagnose anhängen ---
  vip_tab$scenario_code  <- scenario
  vip_tab$scenario_label <- scenario_labels[[scenario]] %||% scenario
  
  attr(vip_tab, "missing_map") <- missing_map  # <- hier siehst du, was kein Label hatte
  
  list(result = pls_res, vip = vip_tab)
}

# ------------------------------------------------------------
# Alle Szenarien zusammen → Facet-Bubble-Plot (konstante Y-Reihenfolge)
# ------------------------------------------------------------
sim_results<-sim_scenarios_output
plot<-VIP_plot(sim_results)
VIP_plot <- function(sim_results, scen_codes = scenario_order_codes, include_unlabeled = FALSE) {
  vip_list <- lapply(scen_codes, function(sc) {
    res <- run_plsr_for_scenario(sim_results, sc, include_unlabeled = include_unlabeled)
    df  <- as_tibble(res$vip)
    if (nrow(df) == 0) return(NULL)
    # -> Diagnose sammeln (optional anzeigen)
    mm <- attr(res$vip, "missing_map")
    if (length(mm)) message(sprintf("[Diagnose] Szenario %s: Variablen ohne Label: %s",
                                    sc, paste(head(mm, 10), collapse = ", ")))
    df
  })
  vip_list <- Filter(Negate(is.null), vip_list)
  if (!length(vip_list)) stop("Keine VIP-Daten für die angegebenen Szenarien.")
  
  vip_combined <- dplyr::bind_rows(vip_list) %>%
    # Sicherheit: keine harten NAs
    filter(!is.na(Variable), !is.na(VIP))
  
  # Szenario-Factor für Facets
  vip_combined$scenario_label <- factor(
    vip_combined$scenario_label,
    levels = unname(scenario_labels[scen_codes]),
    ordered = TRUE
  )
  
  # 1) Spacer-Level erzeugen
  base_levels   <- label_order                    # deine gewünschte Reihenfolge (Anzeigenamen)
  spacers       <- paste0("SPACER__", seq_along(base_levels))
  spaced_levels <- as.vector(rbind(base_levels, spacers))  # label, spacer, label, spacer, ...
  
  # 2) Variable auf „gespacete“ Levels abbilden
  vip_combined$Variable_spaced <- factor(
    vip_combined$Variable,
    levels = spaced_levels,
    ordered = TRUE
  )
  
  # 3) Plot: breaks = NUR echte Labels -> Gridlines nur dort
  p <- ggplot(vip_combined, aes(x = "VIP", y = Variable_spaced)) +
    geom_point(
      aes(size = VIP, fill = coef_sign),
      shape = 21,  colour = "white", stroke = 0.35
    ) +
    facet_wrap(~ scenario_label, nrow = 1, scales = "fixed") +
    scale_size_continuous(
      range  = c(3, 12),
      limits = c(0.8, 10),
      breaks = c(1, 2, 3, 4),
      name   = "Variable of Importance in Projection (VIP)"
    ) +
    scale_y_discrete(
      limits = spaced_levels,   # alle Levels inkl. Spacer
      breaks = base_levels,     # NUR echte Labels -> keine Gridlines bei Spacern
      labels = base_levels,     # Spacer unsichtbar in Achse
      drop   = FALSE,
      expand = expansion(add = 1.2)
    ) +
    coord_cartesian(clip = "off") +  # nichts am Rand abschneiden
    labs(x = NULL, y = "Variable") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor.y = element_blank(),  # sicherheitshalber
      strip.text         = element_text(hjust = 0.5, size = 12),
      axis.text.y        = element_text(hjust = 0, vjust = 0.5),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      legend.position    = "bottom"
    )
  return(p)
}
