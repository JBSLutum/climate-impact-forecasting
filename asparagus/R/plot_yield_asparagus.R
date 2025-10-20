plot_yield_asparagus <- function(sim_results_scen)
{
  #easy yield graph
  # sim_results_scen <-
  #   readRDS("asparagus/MC_results/MC_results_scenarios.RDS")
  
  results_marktyield_today <-
    sim_results_scen$y %>% select(ends_with("_today")) %>% rename_with( ~ str_remove(.x, "_today"))
  results_marktyield_126 <-
    sim_results_scen$y %>% select(ends_with("_ssp1")) %>% rename_with( ~ str_remove(.x, "_ssp1"))
  results_marktyield_245 <-
    sim_results_scen$y %>% select(ends_with("_ssp2")) %>% rename_with( ~ str_remove(.x, "_ssp2"))
  results_marktyield_370 <-
    sim_results_scen$y %>% select(ends_with("_ssp3")) %>% rename_with( ~ str_remove(.x, "_ssp3"))
  results_marktyield_585 <-
    sim_results_scen$y %>% select(ends_with("_ssp5")) %>% rename_with( ~ str_remove(.x, "_ssp5"))
  
  #adding row for scenario identification
  results_marktyield_today$scenario <- as.character("Year 2020")
  results_marktyield_126$scenario <- as.character("Year 2075\nSSP1 2.6")
  results_marktyield_245$scenario <- as.character("Year 2075\nSSP2 4.5")
  results_marktyield_370$scenario <- as.character("Year 2075\nSSP3 7.0")
  results_marktyield_585$scenario <- as.character("Year 2075\nSSP5 8.5")
  #all together
  results_yield_all <-
    rbind(
      results_marktyield_today,
      results_marktyield_126,
      results_marktyield_245,
      results_marktyield_370,
      results_marktyield_585
    )
  #rename column
  names(results_yield_all) <-
    c("total_yield", "marketable_yield", "id", "scenario")
  #direktvergleich
  results_yield_all_longer <-
    pivot_longer(results_yield_all, cols = c(total_yield, marketable_yield))
  results_yield_all_longer$name <-
    factor(results_yield_all_longer$name,
           levels = c("total_yield", "marketable_yield"))
  
  results_yield_all_longer <- results_yield_all_longer %>%
    mutate(period = ifelse(scenario == "Year 2020", "2020", "2075"))
  results_yield_all_longer$period <-
    as.factor(results_yield_all_longer$period)
  results_yield_all_longer$scenario <-
    as.factor(results_yield_all_longer$scenario)
  
  #mittelwerte
  summary_df <- results_yield_all_longer %>%
    group_by(scenario, name) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      q25 = quantile(value, 0.25, na.rm = TRUE),
      q75 = quantile(value, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = name,
                values_from = c(mean_value, q25, q75)) %>%
    mutate(
      percent = (mean_value_marketable_yield / mean_value_total_yield) * 100,
      IQR_marketable_yield = q75_marketable_yield - q25_marketable_yield
    )
  
  
  plot <-
    ggplot(results_yield_all_longer,
           aes(x = scenario, y = value, fill = name)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    geom_text(
      data = summary_df,
      aes(
        x = scenario,
        y = max(results_yield_all_longer$value, na.rm = TRUE) + 1,
        label = paste0(round(percent), "%")
      ),
      inherit.aes = FALSE,
      size = 4
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = "right",
      strip.background = element_rect(fill = "lightgrey"),
      strip.text = element_text(size = 12, face = "bold")
    ) +
    scale_x_discrete(name = "Climate scenario") +
    scale_y_continuous(name = "Yield t/ha") +
    geom_hline(yintercept = 5.63, linetype = "dashed",) +
    annotate(
      geom = "text",
      x =  -Inf,
      y = 4,
      label = c("5.63 t/ha\nYield 2020"),
      color = "black",
      fontface = "plain",
      hjust = 0,
      vjust = 1,
      angle = 0,
      size = 3
    )
  
  return(plot)
}