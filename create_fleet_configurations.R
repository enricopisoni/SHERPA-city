# --------------------------------------------------
# Create fleet configurations from an overview file
# --------------------------------------------------

create.fleet.configs <- function(fleet.config.folder, fleet.config.overview.file) {
  # read the overview file
  # each column contains a configuration. The function puts them in separate files
  fleet.config.overview.df <- read.table(file.path(fleet.configuration.folder, 
                                                   "fleet_configuration_overview.csv"), 
                                         sep = ",", header = T)
  # the fleet names are the column names (from the eighth to the last) in 
  # the fleet.config.overview.file
  fleets.list <- names(fleet.config.overview.df)[8:NCOL(fleet.config.overview.df)]
  
  # write a configuration file for every fleet
  for (fleet.name in fleets.list) {
    fleet.config.df <- fleet.config.overview.df[,1:7]
    fleet.config.df$pct_AADT <- fleet.config.overview.df[, fleet.name]
    write.table(fleet.config.df, 
                file = file.path(fleet.config.folder, paste0(fleet.name, ".csv")), 
                sep = ",", row.names = F, quote = F)
  }
}

