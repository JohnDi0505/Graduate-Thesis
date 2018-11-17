# set working directories
setwd("Python Scripts for Thesis/ArcGIS_data_output")

# load "tick_db" data sheet
tick_db <- read.csv("tick_DB_V2.csv")

# perform stepwise linear model with AIC

# Senario 1: invloving all independent varibles
# get rid of rows where "temp" & "rh" are missing
df1 <- tick_db[which(!is.na(tick_db$temp)),]
fit1 <- lm(t_adult ~ temp + rh + elv + solar + ndvi
           + mixed_hardwood + dwarf_shrub + grassland
           + pine_forest + residential_area + sand + ecotone_length,
           data = df1)
summary(fit1)
# stepwise model selection with AIC
step_aic1 <- step(fit1)
output_1 <- summary(step_aic1)
capture.output(output_1, file = "senario1_OLS.txt")

# Senario 2: involving all sampling sites surveyed in spring
df2 <- tick_db[which(!is.na(tick_db$temp) & (tick_db$pid != 3 & tick_db$pid != 4)),]
fit2 <- lm(t_adult ~ temp + rh + elv + solar + ndvi
           + mixed_hardwood + dwarf_shrub + grassland
           + pine_forest + residential_area + sand + ecotone_length,
           data = df2)
summary(fit2)
# stepwise model selection with AIC
step_aic2 <- step(fit2)
output_2 <- summary(step_aic2)
capture.output(output_2, file = "senario2_OLS.txt")

# Senario 3: investigate woodland effects on tick habitat
fit3 <- lm(t_adult ~ mixed_hardwood + dwarf_shrub + grassland
           + pine_forest + residential_area + sand + ecotone_length,
           data = tick_db)
summary(fit3)
# stepwise model selection with AIC
step_aic3 <- step(fit3)
summary(step_aic3)

# Senario 4: investigate microlimatic effects on tick questing activities
# 4.1 all sites
fit4_1 <- lm(t_adult ~ temp + rh + elv + solar + ndvi,
           data = tick_db)
summary(fit4_1)
# stepwise model selection with AIC
step_aic4_1 <- step(fit4_1)
summary(step_aic4_1)

# 4.2 sites surveyed in spring
df4_2 <- tick_db[which(!is.na(tick_db$temp) & (tick_db$pid != 3 & tick_db$pid != 4)),]
fit4_2 <- lm(t_adult ~ temp + rh + elv + solar + ndvi,
             data = tick_db)
summary(fit4_2)
# stepwise model selection with AIC
step_aic4_2 <- step(fit4_2)
output_4_2 <- summary(step_aic4_2)
capture.output(output_4_2, file = "senario2_2_OLS.txt")