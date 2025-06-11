library(decisionSupport)

scenario_mc2<-function (base_estimate, scenarios, model_function, ..., numberOfModelRuns = 10000, 
          randomMethod = "calculate", functionSyntax = "data.frameNames", 
          relativeTolerance = 0.05, verbosity = 0, risk_df) 
{
  scenario_names <- colnames(scenarios)[which(!colnames(scenarios) %in% 
                                                c("Variable", "param"))]
  for (scens in scenario_names) {
    n_Runs <- NA
    if ("Runs" %in% scenarios$Variable) 
      n_Runs <- as.numeric(scenarios[which(scenarios[, 
                                                     1] == "Runs"), scens])
    if (is.na(n_Runs)) 
      n_Runs <- numberOfModelRuns
    if (is.na(n_Runs)) 
      stop(paste("Not clear how many times scenario", scens, 
                 "should be run"))
    if (!(n_Runs == round(n_Runs)) & (n_Runs > 0)) 
      stop(paste("Number of runs for scenario", scens, 
                 "is not a positive integer"))
    estim <- base_estimate
    if (!("Runs" %in% scenarios$Variable)) 
      parameter_lines <- 1:nrow(scenarios)
    else parameter_lines <- which(!(1:nrow(scenarios) == 
                                      which(scenarios$Variable == "Runs")))
    for (i in parameter_lines) {
      vari <- scenarios$Variable[i]
      if (!vari %in% (estim$variable)) 
        stop(paste("Estimate object doesn't contain a parameter called", 
                   vari))
      if (!is.na(scenarios[i, scens])) {
        if (scenarios$param[i] == "both") {
          estim[estim$variable==vari, "lower"] <- as.numeric(scenarios[i, 
                                                                scens])
          estim[estim$variable==vari, "upper"] <- as.numeric(scenarios[i, 
                                                                scens])
          estim[estim$variable==vari, "distribution"] <- "const"
        }
        if (scenarios$param[i] == "lower") 
          if (!is.na(as.numeric(scenarios[i, scens]))) 
            estim[estim$variable==vari, "lower"] <- as.numeric(scenarios[i, 
                                                                  scens])
        if (scenarios$param[i] == "upper") 
          if (!is.na(as.numeric(scenarios[i, scens]))) 
            estim[estim$variable==vari, "upper"] <- as.numeric(scenarios[i, 
                                                                  scens])
        if (scenarios$param[i] == "distribution") 
          if (!is.na(scenarios[i, scens])) 
            estim[estim$variable==vari, "distribution"] <- scenarios[i, 
                                                              scens]
      }
    }
    scen_results <- mcSimulation(estimate = estim, model_function = model_function, 
                                 ..., numberOfModelRuns = n_Runs, functionSyntax = functionSyntax, risk_df)
    scen_results$x[, "Scenario"] <- scens
    if (scens == scenario_names[1]) 
      end_results <- scen_results
    if (!scens == scenario_names[1]) {
      end_results$x <- rbind(end_results$x, scen_results$x)
      end_results$y <- rbind(end_results$y, scen_results$y)
      end_results$call <- "multi-scenario call"
    }
  }
  class(end_results) <- cbind("mcSimulation", class(end_results))
  return(end_results)
}