

library(decisionSupport)
library(readr)


# 1) Base-Estimate: 'id' mit Verteilung (z.B. Uniform)
base_estimate <- as.estimate(
  distribution = "unif",
  lower = 0, upper = 1,          # Platzhalter; werden per Szenario überschrieben
  variable = "id"
)

input<-read.csv("asparagus/asparagus_input.csv", colClasses = c("character", "character", "character", "character", "numeric", "character","numeric"), sep = ",", dec = ".")

# 2) Szenarien einlesen (wichtig: als Zahlen, getrimmt)
scenarios <- read_csv("asparagus/scenarios.csv", show_col_types = FALSE, trim_ws = TRUE)

# 3) Einfaches Modell: ziehe 'id' und gib sie zurück
asparagus_sim <- function(scenarios) {
  output<-list()
  scenario_cols <- setdiff(names(scenarios), c("Variable","param"))
  for (scen in scenario_cols){
    id_range <- as.numeric(c(scenarios[[scen]]))
    
    id_draw <- runif(n=1,min=id_range[1], max=id_range[2])
    id=round(id_draw)
    
    out2 <- risk_df[id,1]
    out3 <- risk_df[id,2]
    
    vars<-c("id",
              "out2",
               "out3")
    
    outs<-list(id,
            out2,
            out3)
    
    output[paste0(vars,"_",scen)]<-outs
  }
    
  return(output)
}


# 4) Simulation über alle Szenarien

sim <- mcSimulation(
  estimate = input,
  model_function = asparagus_sim,
  numberOfModelRuns = 10000,           # oder Zeile 'Runs' im CSV
  functionSyntax = "plainNames",
  scenarios=scenarios
)
sim$x$scenario<-scen
output<-rbind(output,sim)
}
# 5) Ergebnisse prüfen
summary(res)
head(res$x)  # Eingaben inkl. Spalte 'scenario'
head(res$y)  # Ausgaben


