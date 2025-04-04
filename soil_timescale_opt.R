library("tidyr")
library("dplyr")
library("nloptr")


# 1. Import data ----------------------------------------------------------

GCAM_soilC <- read.csv("./Data/GCAM_soilC.csv")             # TODO: is this gcam_luce?          
soil_timescales <- read.csv("./Data/soil_timescales.csv")   # regional, eventually want regional_transitional? just transitional?  
GCAM_countries <- read.csv("./Data/GCA_GCAM_regID.csv", header = TRUE)
GCAM_regions <- read.csv("./Data/GCAM_regions.csv")
GLU_codes <- read.csv("./Data/GLU_codes.csv")


# "Net: Net ELUC is the net CO2 flux caused by all anthropogenic land-use activities. 
# It is the sum of the CO2 fluxes from deforestation, forest (re-)growth, wood
# harvest and other forest management, other transitions, and peat emissions."
# From Global Carbon Project's Global Carbon Atlas
global_carbon_atlas_LUC_net <- read.csv("./Data/gca_emissions_LUC_net.csv", skip = 1,
                                 header = TRUE) %>% head(., -3)

# TODO: is this the correct gcam output data
# TODO: if so add before/after cut

# 2. GCAM LUC emissions calcs ---------------------------------------------

soil_carbon_emissions <- function(emissions_t, k, year_curr, t_convert) {
  # Takes matrix of regional rate constants and some other stuff and calcs
  # The soil carbon emissions for that year for all regions
  # k is static through time per case (first-order kinetics)
  
  # Esoil_y = Esoil_t * ((1 - e^(-k*(y - t))) - (1 - e^(-k * (y - t - 1))))
  # Esoil_t = soil C emissions during the initial year of conversion, df
  # k = log(2) / (s/10), rate constant
  # s = soil time scale specified by region
  # t = time of land conversion
  # y = current year
  return(emissions_t * ((1 - exp(-k * (year_curr - t_convert))) - 
                      (1 - exp(-k * (year_curr - t_convert - 1)))))
}

calc_emissions <- function(soil_emit, veg_emit) {
  # Total emissions/uptake = soil_emit + veg_emit
  return(soil_emit + veg_emit)
}


# 3. Set up optimization inputs -------------------------------------------
# TODO: data quality is a hover over property of the map but doesn't download; how to get?

# Aggregate GCA data to GCAM regions, note discrepancies
# There are 48 countries noted by gcam that don't exist in GCA; mostly territories and islands
# value in TgC/yr
gca_net_luce <- global_carbon_atlas_LUC_net %>%
  rename_with(~ gsub("\\.", " ", .x)) %>%
  mutate(across(everything(), as.numeric)) %>%
  rename(year = X) %>%
  pivot_longer(names_to = "GCA_country_name", values_to = "value", cols = -year) %>%
  left_join(GCAM_countries, by = "GCA_country_name") %>%
  left_join(GCAM_regions, by = "GCAM_region_ID") %>%
  select(year, country = country_name, region_id = GCAM_region_ID, region, value) 

actuals <- gca_net_luce %>%
  group_by(region, year) %>%
  summarize(TgC = sum(value), .groups = "drop")

# TODO: pre-allocate memory?
iterations <- error <- results <- transition_soil_timescales <- list()


# 4.. Optimizer -----------------------------------------------------------


# model_time <- system.time()    # wrap it in a timer



# 5. Process results ------------------------------------------------------



# 6. Visualize results ----------------------------------------------------


