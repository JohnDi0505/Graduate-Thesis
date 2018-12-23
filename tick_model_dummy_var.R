library("AICcmodavg")

setwd("Python Scripts for Thesis/")
tick_db <- read.csv("tick_DB_V3_dummy_var.csv")

# 3.creates candidate models

# 3.1 all sites surveyed across spring and fall seasons
all_sites <- tick_db[which(!is.na(tick_db$temp)),]

# 3.1.1 candidate model 1.1: all rows with microclimatic & landcover variables
model_1.1 <- lm(t_adult ~ season + temp + rh + elv + solar + ndvi
                + mixed_hardwood + dwarf_shrub + grassland
                + pine_forest + residential_area + ecotone_length,
                data = all_sites)

# 3.1.2 candidate model 1.2: all rows with microclimatic & landcover variables
model_1.2 <- lm(t_adult ~ temp + rh + elv + solar + ndvi
                + mixed_hardwood + dwarf_shrub + grassland
                + pine_forest + residential_area + ecotone_length,
                data = all_sites)

# 3.1.3 candidate model 1.3: investigate relationship between response & landcover variables
model_1.3 <- lm(t_adult ~ mixed_hardwood + dwarf_shrub + grassland
                + pine_forest + residential_area + sand + ecotone_length,
                data = all_sites)

# 3.1.4 candidate model 1.4: investigate relationship between response & microclimatic variables
model_1.4 <- lm(t_adult ~ season + temp + rh + elv + solar + ndvi,
                data = all_sites)

# 3.1.5 candidate model 1.5: investigate relationship between response & microclimatic variables
model_1.5 <- lm(t_adult ~ temp + rh + elv + solar + ndvi,
                data = all_sites)

# 4. stepwise variable reduction based on AIC

# 4.1.1: model 1.1
stepaic_1.1 <- step(model_1.1)
out_summary1.1 <- summary(stepaic_1.1)
# 4.1.2: model 1.2
stepaic_1.2 <- step(model_1.2)
out_summary1.2 <- summary(stepaic_1.2)
# 4.1.3: model 1.3
stepaic_1.3 <- step(model_1.3)
out_summary1.3 <- summary(stepaic_1.3)
# 4.1.4: model 1.4
stepaic_1.4 <- step(model_1.4)
out_summary1.4 <- summary(stepaic_1.4)
# 4.1.5: model 1.5
stepaic_1.5 <- step(model_1.5)
out_summary1.5 <- summary(stepaic_1.5)

# 5. Rank models with AICc

# 5.1 create list containing candidate models outputs for scenario 1
cand.models.1 <- list(stepaic_1.1,stepaic_1.2,stepaic_1.3,stepaic_1.4,stepaic_1.5)

# 5.2 specify alias for candidate models
cand.name.1 <- c("model_1.1", "model_1.2", "model_1.3", "model_1.4", "model_1.5")

# 5.3 perform AICc approach to compare candidate models
AICc_out_table.1 <- aictab(cand.set = cand.models.1, modnames = cand.name.1)

# 6. write AICc table to file
output_data.1 <- list(AICc_out_table.1, out_summary1.1, out_summary1.3, out_summary1.2)