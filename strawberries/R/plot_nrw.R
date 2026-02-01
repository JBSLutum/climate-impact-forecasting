
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