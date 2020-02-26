# ----------------------------------
# Copy scenario definitions in place
# ----------------------------------

# read the template, substitute year and country code with the city specific values and
# write the file to the city output folder
create.scenario.definition <- function(scenario.template.file, city.df, cityname, fleet.year, city.output.folder) {
  # get the country code
  country.code <- toString(city.df$country[city.df$cityname==cityname])
  
  # open the template scenario definition
  city.scen.def.df <- read.table(scenario.template.file, header = T, sep = ",")
  city.scen.def.df$default_fleet_country <- country.code
  city.scen.def.df$default_fleet_year <- fleet.year
  
  # write the scenario defintion file in the city results folder
  city.scen.def.file <- file.path(city.output.folder, paste0(cityname, "_scenario_definition.csv"))
  write.table(city.scen.def.df, file = city.scen.def.file, row.names = F, sep = ",", quote = F)
  return(city.scen.def.file)
}