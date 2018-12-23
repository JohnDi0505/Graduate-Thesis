# 1.load library for regression diagnostics & AICc statistics
library("AICcmodavg", lib.loc="~/R/win-library/3.5")
library("car", lib.loc="~/R/win-library/3.5")

# 2.load tick data
setwd("Python Scripts for Thesis/")
tick_db <- read.csv("tick_DB_V2.csv")

# 3.creates candidate models

# 3.1 get rid of missing data & select spring data
all_sites <- tick_db[which(!is.na(tick_db$temp)),]
spr_sites <- all_sites[which(all_sites$pid != 3),]

# 3.2.1 candidate model 2.1: spring data with microclimatic & landcover variables
model_2.1 <- lm(t_adult ~ temp + rh + elv + solar + ndvi
              + mixed_hardwood + dwarf_shrub + grassland
              + pine_forest + residential_area + ecotone_length,
              data = spr_sites)
# 3.2.2 candidate model 2.2: investigate relationship between response & landcover variables
model_2.2 <- lm(t_adult ~ mixed_hardwood + dwarf_shrub + grassland
              + pine_forest + residential_area + ecotone_length,
              data = spr_sites)

# 3.2.3 candidate model 2.3: investigate relationship between response & microclimatic variables
model_2.3 <- lm(t_adult ~ temp + rh + elv + solar + ndvi,
              data = spr_sites)

# 4. stepwise variable reduction based on AIC

# 4.2.1: model 2.1
stepaic_2.1 <- step(model_2.1)
out_summary2.1 <- summary(stepaic_2.1)
# 4.2.2: model 2.2
stepaic_2.2 <- step(model_2.2)
out_summary2.2 <- summary(stepaic_2.2)
# 4.2.3: model 2.3
stepaic_2.3 <- step(model_2.3)
out_summary2.3 <- summary(stepaic_2.3)

# 5. Rank model with AICc

# 5.1.2 create list containing candidate models outputs for scenario 2
cand.models.2 <- list(stepaic_2.1,stepaic_2.1,stepaic_2.3)

# 5.2 specify alias for candidate models
cand.name.2 <- c("model_2.1", "model_2.2", "model_2.3")

# 5.3 perform AICc approach to compare candidate models
AICc_out_table.2 <- aictab(cand.set = cand.models.2, modnames = cand.name.2)

# 6. write AICc table to file
output_data.2 <- list(AICc_out_table.2, out_summary2.1, out_summary2.3)
capture.output(output_data.2, file = "AICc_output_table_S2.txt")