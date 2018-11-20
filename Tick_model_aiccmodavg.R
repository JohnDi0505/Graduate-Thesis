# 1.load library for regression diagnostics & AICc statistics
library("AICcmodavg", lib.loc="~/R/win-library/3.5")
library("car", lib.loc="~/R/win-library/3.5")

# 2.load tick data
setwd("Python Scripts for Thesis/")
tick_db <- read.csv("tick_DB_V2.csv")
mo
# 3.creates candidate models
# 3.1 all sites surveyed across spring and fall seasons
all_sites <- tick_db[which(!is.na(tick_db$temp)),]
# 3.1.1 scenario 1.1: all rows with microclimatic & landcover variables
model_1 <- lm(t_adult ~ temp + rh + elv + solar + ndvi
           + mixed_hardwood + dwarf_shrub + grassland
           + pine_forest + residential_area + ecotone_length,
           data = all_sites)

# 3.2 sites surveyed in spring
spr_sites <- all_sites[which(all_sites$pid != 3),]
# 3.2.1 scenario 2.1: rows (surveyed in spring) with microclimatic & landcover variables
model_2 <- lm(t_adult ~ temp + rh + elv + solar + ndvi
             + mixed_hardwood + dwarf_shrub + grassland
             + pine_forest + residential_area + ecotone_length,
             data = spr_sites)

# 3.3 investigate relationship between response & habitat variables
model_3 <- lm(t_adult ~ mixed_hardwood + dwarf_shrub + grassland
              + pine_forest + residential_area + ecotone_length,
              data = tick_db)

# 3.4 investigate relationship between response & microclimatic variables
model_4 <- lm(t_adult ~ temp + rh + elv + solar + ndvi,
              data = all_sites)

# 4. variable reduction based on multicollinearity diagnostics results
# 4.1: model 1
stepaic_1 <- step(model_1)
out_summary1 <- summary(stepaic_1)
# 4.2: model 2
stepaic_2 <- step(model_2)
out_summary2 <- summary(stepaic_2)
# 4.3: model 3
stepaic_3 <- step(model_3)
out_summary3 <- summary(stepaic_3)
# 4.4: model 4
stepaic_4 <- step(model_4)
out_summary4 <- summary(stepaic_4)

# 5. model selection with AICc
# 5.1 create list containing 4 candidate model outputs
cand.models <- list(stepaic_1,stepaic_2,stepaic_3,stepaic_4)
# 5.2 specify alias for candidate models
cand.name <- c("model_1", "model_2", "model_3", "model_4")
# 5.3 perform AICc approach to compare candidate models
AICc_out_table <- aictab(cand.set = cand.models, modnames = cand.name)

# 6. write AICc table to file
output_data <- list(AICc_out_table, out_summary2, out_summary1, out_summary4, out_summary3)
capture.output(output_data, file = "AICc_output_table.txt")