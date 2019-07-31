# ----------------------------- #
# Create fleet emission factors #
# ----------------------------- #

# This script creates emission factors per scenario and raod type.
# There are 2 functions:
# - load_fleets_data: an auxiliary function to read the detailed fleets of the 
#                     EU30 countries
# - create_fleet_emission_factors: creates aggregated emission factors per
#   road type for each scenario. The imput is a scenario definition file.
#   This file contains: the country and year of the default fleet and the
#   fleet configuration for each scenario and area.
#   The function combines the detailed fleet data (per country and year) 
#   with the fleet configuration (in/excludes and traffic in/decreases).

# load libraries
library(plyr)


# function to load the data about EU fleets
# -----------------------------------------

# first this were old tremove data, now they come from EMISIA
# In the case of EMISIA the files are created with the script:
# 

load_fleets_data <- function() {
  # where are the data? (To be moved to the sherpa city config file)
  # fleets.file <- "_fleets/fleets.csv"
  # fleets.data.file <- "_fleets/fleets_data.csv"
  
  # fleets.df corresponds to the database table 'fleets'
  # fields are: id_fleet, year and country
  # primary key is id_fleet
  fleets.df <- read.table(fleets.file, sep = ",", quote = '"', header = TRUE)
  
  # fleets.data.df corresponds to the database table 'fleets_data'
  # fields are: "id_data", "id_fleet","network", "sherpacity_hash", "vkm_network_share_sc",
  # "ef_nox_gpkm_sc", "ef_pm_gpkm_sc", "ef_co2_gpkm_sc", "ef_pm25_gpkm_sc"
  # primary key: id_data
  # foreign key id_fleet refers to table 'fleets'
  myColClasses <- c(rep("numeric", times = 9))   # to avoid reading as factor
  myColClasses[3:4] <- "character"
  fleets_data.df <- read.table(fleets.data.file, sep = ",", quote = '"', header = TRUE, 
                               colClasses = myColClasses)
  # split the hash in category, size, fuel and norm
  sc.hash <- unlist(strsplit(fleets_data.df$sherpacity_hash, split = "[$]"))
  n.hash <- length(sc.hash)
  hash.category <- sc.hash[seq(1, n.hash, 4)]
  hash.size <- sc.hash[seq(2, n.hash, 4)]
  hash.fuel <- sc.hash[seq(3, n.hash, 4)]
  hash.norm <- sc.hash[seq(4, n.hash, 4)]
  
  # add columns for category, size, fuel and norm
  fleets_data.df <- cbind(fleets_data.df, 
                          data.frame(category = hash.category,
                                     size = hash.size,
                                     fuel = hash.fuel,
                                     norm = hash.norm))
  # merge feets and fleets_data
  eu.fleets.df <- merge(fleets.df, fleets_data.df)
  eu.fleets.df <- eu.fleets.df[, c("id_fleet", "year", "country", "network", "category",
                                   "size", "fuel", "norm", "vkm_network_share_sc", "ef_nox_gpkm",      
                                   "ef_pmex_gpkm", "ef_co2_gpkm", "ef_pm10_gpkm")]
  return(eu.fleets.df)  
}


# function that calculates emission factors per road type for a default fleet (country + year)
# and a configuration (in/exclude and traffic flow modifactions)
# --------------------------------------------------------------------------------------------

create_fleet_emission_factors <- function(scenario.definition.file) {
  
  # input:
  # scenario.definition.file: file with scenario definitions; for each scenario and zone
  # the country and year of the fleet are given together with the fleet configuration to be applied.
  # output:
  # scenario.efs.df: emission factor data frame with emission factors per zone and road type for each fleet
  # in terms of country, year, and configuration.
  
  # load the EU30 fleets
  eu.fleets.df <- load_fleets_data()
  
  # open the scenario definition file
  scenarios.df <- read.table(scenario.definition.file, sep = ",", header = TRUE)
  scenario.list <- as.vector(unique(scenarios.df$scenario_name))
  
  # combinations of country, year and configuration
  country.year.config <- unique(scenarios.df[,c("default_fleet_country", 
                                                "default_fleet_year", 
                                                "fleet_configuration")])
  
  # data frame with the emission factor for all fleets
  EFs.df <- data.frame()
  
  # loop over all counrty-year-configuration combinations
  # i.cyc <- 3
  for (i.cyc in 1:NROW(country.year.config)) {
    country <- toString(country.year.config$default_fleet_country[i.cyc])
    year <- country.year.config$default_fleet_year[i.cyc]
    config <- toString(country.year.config$fleet_configuration[i.cyc])
    
    # select the fleet of the country and year
    default.fleet.df <- eu.fleets.df[eu.fleets.df$country == country & eu.fleets.df$year == year,]
    
    # read the correct configuration
    fleet.config.file <- file.path(fleet.configuration.folder, paste0(config, ".csv"))
    fleet.config.df <- read.table(fleet.config.file, sep = ",", header = TRUE)
    
    # merge fleet and fleet configuration
    user.fleet.df <- merge(default.fleet.df, fleet.config.df,
                           c("network", "category", "size", "fuel", "norm"))
    user.fleet.df$vkm_network_share_new <- 0
    
    # calculate correction factors for each road type and category by dividing
    # the sum of all the vehicle kilometers by the included ones per category.
    # sum.include is the number of fuel-size-norm combinations included per road type and category.
    # sum.share is the total vkm share of fuel-size-norm combinations per road type and category.
    # sum.included.share is the vkm share of included fuel-size-norm combinations per road type and category.
    # This division may create Inf or NaN. This is handled below.
    vkm.cf <- ddply(user.fleet.df, c("network", "category"), summarise,
                    count.include = NROW(include),
                    sum.include = sum(include),
                    sum.share = sum(vkm_network_share_sc),
                    sum.included.share = sum(vkm_network_share_sc*include),
                    cf = sum(vkm_network_share_sc) / sum(vkm_network_share_sc*include))
    
    # apply correction factors to the vehicle kilometers of the user fleet
    for (i in 1:NROW(user.fleet.df)) {
      network.i <- user.fleet.df$network[i]
      category.i <- toString(user.fleet.df$category[i])
      network.and.category.i <- vkm.cf$network == network.i & vkm.cf$category == category.i
      
      # look up the correction factor for the network-category combination.
      # Handling Inf and NA when needed
      cf.i <- vkm.cf$cf[network.and.category.i]
      if (!is.na(cf.i) & !(is.infinite(cf.i))) {
        # correction factor is a real number, just apply it. If some subcategories are band the CF < 1
        # If there are no bans in the category it is 1.
        user.fleet.df$vkm_network_share_new[i] <- user.fleet.df$vkm_network_share_sc[i] * user.fleet.df$include[i] * cf.i
      } else if (is.na(cf.i)) {
        # NaN is a result of 0/0: keep zero. This happens when a category is abscent on a road type:
        # mopeds on highways.
        user.fleet.df$vkm_network_share_new[i] <- 0
      } else if (is.infinite(cf.i)) {
        # Infinite is the result of x/0, all existing shares in the default fleet are excluded.
        # This happens e.g. when only EVs are allowed but not present in the default fleet.
        # Check if there are still subcategories left
        sum.include.i <- vkm.cf$sum.include[network.and.category.i]
        include.i <- user.fleet.df$include[i]
        if (sum.include.i == 0) {
          # no fuel-size-norm combinations left in the category, put share and %AADT to zero
          user.fleet.df$vkm_network_share_new[i] <- 0
          user.fleet.df$pct_AADT[i] <- 0
        } else {
          # there are some fuel-size-norm combinations left
          if (include.i == 0) {
            # this particular fuel-size-norm combination is excluded > vkm share zero
            user.fleet.df$vkm_network_share_new[i] <- 0
          } else {
            # This particular fuel-size-norm combination is included. 
            # The fuel-size-norm shares are distributed equaly over 
            # remaining fuel-size-norm combinations of the category
            sum.share.i <- vkm.cf$sum.share[network.and.category.i]
            user.fleet.df$vkm_network_share_new[i] <- sum.share.i / sum.include.i
          }
        }
      }
    } # close loop over all network-category-fuel-norm combinations
    
    # calculate emission factors
    # onroad emission factors take into account eventual traffic reductions
    # The emission factors take into account only the vehicle kilometer share
    # of the allowed vehicles.
    efs <- ddply(user.fleet.df, c("network"), summarise,
                 onroad.ef.nox.gkm = sum(ef_nox_gpkm * vkm_network_share_new * pct_AADT / 100),
                 onroad.ef.pmex.gkm = sum(ef_pmex_gpkm * vkm_network_share_new * pct_AADT / 100),
                 ef.nox.gkm = sum(ef_nox_gpkm * vkm_network_share_new * pct_AADT / 100) / sum(vkm_network_share_new * pct_AADT / 100) ,
                 ef.pmex.gkm = sum(ef_pmex_gpkm * vkm_network_share_new * pct_AADT / 100) / sum(vkm_network_share_new * pct_AADT / 100))
    
    EFs.df <- rbind(EFs.df, data.frame(default_fleet_country = country, 
                                       default_fleet_year = year, 
                                       fleet_configuration = config, 
                                       efs))

  } # loop over country-year-config combinations
  
  # add the emission factors to the scenarios
  scenario.efs.df <- merge(scenarios.df, EFs.df, 
                           c("default_fleet_country", "default_fleet_year", "fleet_configuration"))
  # reshuffle columns
  scenario.efs.df <- scenario.efs.df[, c("scenario_name", "default_fleet_country", "default_fleet_year", 
                                         "fleet_configuration", "zone_name", "network", "onroad.ef.nox.gkm", 
                                         "onroad.ef.pmex.gkm", "ef.nox.gkm", "ef.pmex.gkm")]
  # sort by scenario, zone and road type
  scenario.efs.df <- scenario.efs.df[order(scenario.efs.df$scenario_name, 
                                           scenario.efs.df$zone_name, 
                                           scenario.efs.df$network),]
  return(scenario.efs.df)
}


# --- FOR TESTING -----

if (1 == 0) {
  cityname <- "Milano"
  scenario.definition.file <- paste0(cityname, "/", cityname, "_scenario_definition.csv")
  emission.factors.file <- paste0(cityname, "/", cityname, "_emission_factors_TEST.csv")
  
  if (file.exists(scenario.definition.file)) {
    # create a data frame with the emission factors to be used per
    # scenario, zone and road type
    scenario.efs.df <- create_fleet_emission_factors(scenario.definition.file)
    write.table(scenario.efs.df, file = emission.factors.file, sep = ",", row.names = FALSE, quote = FALSE)
  } else {
    print(paste0("No scenarios availble for ", cityname))
  }
}



  
  
  
  
  
  
  
  