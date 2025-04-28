library("rgcam")
library("tidyverse")


# 1. Get project from outputdb --------------------------------------------
conn <- localDBConn(dbPath = "../gcam-v7.1-released/output", 
                    dbFile = "database_basexdb")

proj <- addScenario(conn, 
                    scenario = NULL, 
                    proj = "LUC_veg_emissions.dat", 
                    clobber = TRUE, 
                    queryFile = "BatchQuery_above_ground_c.xml")


# 2. Get data from project ------------------------------------------------
above_ground_c <- getQuery(proj, "vegetative carbon stock by region")

# Aggregate carbon stock per region and calculate change from last time step
veg_emissions <- above_ground_c %>%
  select(-scenario) %>%
  group_by(region, year, Units) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  mutate(c_stock_emit = lag(value) - value) %>%
  filter(year %in% 2010:2020)


# Compare total GCAM LUCe against GCP
all_GCAM_LUCe <- getQuery(proj, "LUC emissions by region") 

luc_emit <- all_GCAM_LUCe %>%
  select(-scenario) %>%
  group_by(region, year, Units) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  filter(year %in% 2010:2020)


# 3. Write to file --------------------------------------------------------
write.csv(veg_emissions, "./Data/GCAM_ref_veg_emissions.csv", row.names = FALSE)
write.csv(luc_emit, "./Data/GCAM_ref_luc_emit_total.csv", row.names = FALSE)
