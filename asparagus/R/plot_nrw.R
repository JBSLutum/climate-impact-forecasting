library(tidyverse)


f_list_fullname <- list.files('asparagus/MC_results_NRW/', full.names = TRUE)
cords<-read.csv('weathergenerator/pixel_id.csv')
# dir.create('asparagus/MC_results_NRW/')
# dir_folder <- 'asparagus/MC_results_NRW/'
# f_result<- 'MC_results_scenarios.RDS'


for(i in 1:length(f_list_fullname)){
  
  #get the pixel id
  p <- strsplit(f_list_fullname[i], '/') %>% 
    purrr::map_chr(3) %>% 
    strsplit(split = '_') %>% 
    purrr::map_chr(1) %>% 
    strsplit(split = '')%>% 
    purrr::map_chr(2)
  
  pix<-cords[cords$id==p,]
  
  sim<-readRDS(f_list_fullname[i])
  mean_yield<-
  
  #generate file name
  fname_res <- paste0(dir_folder, p, '_', f_result)
  
  #check if file already exists
  if(file.exists(fname_res)){
    cat('File:', fname_res, 'already exists. Skip\n')
    next
  } 
  
  risk_df <- read.csv(f_list_fullname[i])
  
  sim_scenarios<-mcSimulation(estimate = as.estimate(input),
                              model_function = asparagus_sim_scen,
                              numberOfModelRuns = 10000,
                              functionSyntax = "plainNames",
                              risk_df,
                              scenarios)
  
  sim_scenarios_output<-youtputs_to_xinputs_scenarios(sim_scenarios, outputs)
  saveRDS(sim_scenarios_output, fname_res)
  #write.csv(sim_scenarios_output, "asparagus/MC_results/MC_results_scenarios.csv")
  
  
}


# Ergebnis-Dataframe einmal leer anlegen
results <- data.frame(
  id    = character(),
  today = numeric(),
  ssp1  = numeric(),
  ssp2  = numeric(),
  ssp3  = numeric(),
  ssp5  = numeric(),
  stringsAsFactors = FALSE
)

cols_in_sim <- c(
  "marketable_yield_today",
  "marketable_yield_ssp1",
  "marketable_yield_ssp2",
  "marketable_yield_ssp3",
  "marketable_yield_ssp5"
)

# Schleife über alle Dateien
for (i in seq_along(f_list_fullname)) {
  
  #get the pixel id
  p <- strsplit(f_list_fullname[i], '/') %>% 
    purrr::map_chr(3) %>% 
    strsplit(split = '_') %>% 
    purrr::map_chr(1) %>%
    str_remove("^p")
  
  # Datei laden
  sim <- readRDS(f_list_fullname[i])
  
  # Mittelwerte berechnen
  means_vec <- sapply(cols_in_sim, function(col) {
    mean(sim$y[[col]], na.rm = TRUE)
  })
  
  # Neue Zeile mit ID + Mittelwerten bauen
  new_row <- data.frame(
    id    = p,
    today = means_vec["marketable_yield_today"]*10,
    ssp1  = means_vec["marketable_yield_ssp1"]*10,
    ssp2  = means_vec["marketable_yield_ssp2"]*10,
    ssp3  = means_vec["marketable_yield_ssp3"]*10,
    ssp5  = means_vec["marketable_yield_ssp5"]*10,
    stringsAsFactors = FALSE
  )
  
  # an results anhängen
  results <- rbind(results, new_row)
}

write.csv(results, "asparagus/MC_results_NRW/means/sim_mean_market_nrw.csv")

# Ergebnis-Dataframe einmal leer anlegen
results <- data.frame(
  id    = character(),
  ssp1  = numeric(),
  ssp2  = numeric(),
  ssp3  = numeric(),
  ssp5  = numeric(),
  stringsAsFactors = FALSE
)

cols_in_sim <- c(
  "marketable_yield_today",
  "marketable_yield_ssp1",
  "marketable_yield_ssp2",
  "marketable_yield_ssp3",
  "marketable_yield_ssp5"
)

# Schleife über alle Dateien
for (i in seq_along(f_list_fullname)) {
  
  #get the pixel id
  p <- strsplit(f_list_fullname[i], '/') %>% 
    purrr::map_chr(3) %>% 
    strsplit(split = '_') %>% 
    purrr::map_chr(1) %>%
    str_remove("^p")
  
  # Datei laden
  sim <- readRDS(f_list_fullname[i])
  
  # Mittelwerte berechnen
  means_vec <- sapply(cols_in_sim, function(col) {
    mean(sim$y[[col]], na.rm = TRUE)
  })
  
  # Neue Zeile mit ID + Mittelwerten bauen
  new_row <- data.frame(
    id    = p,
    ssp1  = (means_vec["marketable_yield_ssp1"]-means_vec["marketable_yield_today"])*10,
    ssp2  = (means_vec["marketable_yield_ssp2"]-means_vec["marketable_yield_today"])*10,
    ssp3  = (means_vec["marketable_yield_ssp3"]-means_vec["marketable_yield_today"])*10,
    ssp5  = (means_vec["marketable_yield_ssp5"]-means_vec["marketable_yield_today"])*10,
    stringsAsFactors = FALSE
  )
  
  # an results anhängen
  results <- rbind(results, new_row)
}

write.csv(results, "asparagus/MC_results_NRW/means/sim_mean_compare_nrw.csv")
