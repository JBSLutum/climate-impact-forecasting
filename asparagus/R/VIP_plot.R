library(dplyr)
library(ggplot2)
library(patchwork)

# -----------------------------
# Funktion: VIP_table
# -----------------------------
VIP_table <- function (plsrResults, input_table = NULL, cut_off_line = 1, 
                       threshold = 0.8, x_axis_name = "Variable Importance in Projection", 
                       y_axis_name = NULL, legend_name = "Coefficient", 
                       legend_labels = c("Positive", "Negative"), 
                       pos_color = "cadetblue", neg_color = "firebrick", 
                       base_size = 11, ...) 
{
  assertthat::assert_that(class(plsrResults) == "mvr", 
                          msg = "plsrResults is not class 'mvr'.")
  
  VIP <- function(object) {
    if (object$method != "oscorespls") 
      stop("Only implemented for oscorespls")
    if (nrow(object$Yloadings) > 1) 
      stop("Only for single-response models")
    SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
    Wnorm2 <- colSums(object$loading.weights^2)
    SSW <- sweep(object$loading.weights^2, 2, SS/Wnorm2, "*")
    sqrt(nrow(SSW) * apply(SSW, 1, cumsum)/cumsum(SS))
  }
  
  if (plsrResults$ncomp == 1) 
    vipResult <- VIP(plsrResults)
  else vipResult <- VIP(plsrResults)["Comp 1", ]
  
  coef <- plsrResults$coefficients[, , 1]
  pls_outputs <- data.frame(Variable = names(vipResult), 
                            VIP = vipResult, 
                            Coefficient = coef)
  rownames(pls_outputs) <- NULL
  
  if (!(is.null(input_table))) 
    combined_table <- dplyr::left_join(pls_outputs, input_table, 
                                       by = c(Variable = "variable"))
  else combined_table <- pls_outputs
  
  filtered_table <- dplyr::filter(combined_table, VIP > threshold)
  
  return(list(VIP_table_results = filtered_table))
}

# -----------------------------
# Labels (deutsche Namen)
# -----------------------------
labels <- c(
  "water_stress_risk" = "Risiko: Schäden durch Trockenheit",
  "insect_risk" = "Risiko: Schäden durch Insekten",
  "disease_risk" = "Risiko: Schäden durch Krankheiten",
  "photosynthetic_active_days" = "gute Photosynthesetage",
  "weather_damage_risk" = "Risiko: Schäden durch Extremwetter",
  "growth_start_doy" = "Start des Stangenwuchs (Tag des Jahres)",
  "speargrowth" = "Tage bis zur ersten Enrte",
  "chill_portions" = "Kältebedarfsdeckung über den Winter",
  "late_frost_risk" = "Risiko: Schäden durch Spätfrost",
  "temp_fluctuation_risk" = "Risiko: Schäden durch Temperaturumschwünge",
  "extreme_rainfall_risk" = "Risiko: Ernteverzögerung durch Starkregen",
  "extreme_heat_risk" = "Risiko: Ernteverzögerung durch Extremhitze",
  "Tsoil_mean" = "Optimale durchschnittliche Bodentemperatur",
  
  "photosynthetic_active_days_needed" = "Bedarf an guten Photosynthesetagen",
  "water_stress_damage" = "Schäden: durch Trockenheit",
  "disease_damage" = "Schäden: durch Krankheiten",
  "insect_damage" = "Schäden: durch Insekten",
  "weather_damage" = "Schäden: durch Extremwetter",
  "chill_need" = "Kältebedarf über den Winter",
  "expected_yield" = "optimaler Ertrag unter perfekten Bedingungen",
  "expected_season_length" = "erwartete Saisonlänge",
  "season_end_doy" = "Ernteende, traditionell Johannitag (175)",
  "late_frost_damage" = "Schäden: durch Spätfrost",
  "temp_fluctuation_damage" = "Schäden: durch Temperaturumschwung",
  "extreme_rainfall_damage" = "Schäden: durch Starkregen",
  "extreme_heat_damage" = "Schäden: durch Extremhitze"
)

# -----------------------------
# Funktion: PLSR und Plot
# -----------------------------
run_plsr_for_scenario <- function(sim_list, scenario = "today") {
  
  # Daten vorbereiten
  sim_y <- sim_list$y %>% select(ends_with(paste0("_", scenario)))
  
  y <- sim_y
  x <- sim_list$x
  
  sim_obj <- list(y = as.data.frame(y), x = as.data.frame(x))
  class(sim_obj) <- c("mcSimulation", "list")
  
  # PLSR
  pls_res <- plsr.mcSimulation(
    object      = sim_obj,
    resultName  = names(sim_obj$y)[2],
    variables.x = names(sim_obj$x),
    ncomp       = 1
  )
  
  # VIP-Tabelle
  #vip_tab <- VIP_table(pls_res, threshold = 0.8)
  
  vip_tab <- VIP_table(pls_res, threshold = 0.8)$VIP_table_results %>%
    as_tibble() %>%
    mutate(
      Variable = as.character(Variable),
      CleanVar = str_remove(Variable, regex(paste0("_", scenario, "$"), ignore_case = TRUE)),
      coef_sign = case_when(
        Coefficient >  0 ~ "positive",
        Coefficient <  0 ~ "negative",
        TRUE      ~ "Zero"
      ),
      scenario=paste0(scenario)
    )
  
  # Präfix entfernen (weather_xxx.)
  #vip_tab$CleanVar <- str_remove(vip_tab$Variable, fixed(paste0("_", scenario, "$"), ignore_case = TRUE))
  
  
  # Labels anwenden
  vip_tab$Variable <- ifelse(vip_tab$CleanVar %in% names(labels),
                             labels[vip_tab$CleanVar],
                             vip_tab$CleanVar)
  
  #vip_tab$Variable <- factor(vip_tab$Variable, levels = unname(labels))
  order_labels <- unname(labels) 
  vip_tab$Variable <- factor(vip_tab$Variable, levels = order_labels, ordered = TRUE)
  vip_tab$scenario <- factor(scenario)#, levels = c(today="Year 2020","Year 2075\nSSP1 2.6", "Year 2075\nSSP2 4.5", "Year 2075\nSSP3 7.0","Year 2075\nSSP5 8.5")) 
  # Plot
    plot <- ggplot(vip_tab, aes(x = scenario, y = Variable)) +
    geom_point(aes(size = VIP, color = coef_sign), shape=16) +
    scale_size_area(max_size = 10, breaks = c(1,2,3,4), name = "VIP") +
    scale_color_manual(values = c("negative"="firebrick", "zero" = "grey70","positive"="cadetblue"), name="Coefficient")  +
    labs(
      title = paste(scenario),
      y     = "Variablen",
      x     = "Variable Importance in Projection (VIP)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(hjust = 0))+
    scale_y_discrete(limits = order_labels, drop = FALSE, expand = expansion(mult = c(0.02, 0.06))) 
  
  return(list(result = pls_res, plot = plot))
}

# -----------------------------
# Szenarien-Liste
# -----------------------------
scenarios <- c("today", "ssp1", "ssp2", "ssp3", "ssp5")
sim_results <- readRDS("asparagus/MC_results/MC_results_scenarios.RDS")
# Alle Szenarien laufen lassen
results <- lapply(scenarios, function(scen) {
  out <- run_plsr_for_scenario(sim_results, scen)
  out$scenario <- scen
  return(out)
})
names(results) <- scenarios

# # -----------------------------
# # Einzelplot (Beispiel ssp370)
# # -----------------------------
# results[["ssp3"]]$plot
# -----------------------------
# Theme für plots
# -----------------------------
no_x <- theme(axis.title.x = element_blank(),
              axis.text.x  = element_blank(),
              axis.ticks.x = element_blank())
no_y <- theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
# -----------------------------
# Vergleichsplot (alle Szenarien nebeneinander)
# -----------------------------


comparison_plot <- (results[["today"]]$plot + no_x+ theme(legend.position = "none"))+
  (results[["ssp1"]]$plot + no_x + no_y+ theme(legend.position = "none"))+
  (results[["ssp2"]]$plot + no_x + no_y+ theme(legend.position = "none"))+
  (results[["ssp3"]]$plot + no_x + no_y+ theme(legend.position = "none"))+
  (results[["ssp5"]]$plot + no_x + no_y)+
  plot_layout(ncol = 5)

comparison_plot
