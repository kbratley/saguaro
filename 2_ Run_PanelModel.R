# BE SURE TO RUN 'Organize_PanelData.R' FIRST

library(foreign)
library(gplots)
library(ggplot2)
library(stargazer)
library(plm)
library(dplyr)
library(magrittr)

# Remove NA's 
Panel[is.na(Panel)] <- 0

# plotmeans( greennessImpact ~ Year, main="Heterogeneity across years", data=Panel)

ols <- lm(greennessImpact ~ treated + treated_2yrs_consecutively + treated_prev1Yr_notCurrentYr + treated_2yrsPrior_notFollowingYrs +
            aspect_transform + elevation + slope + preSprayGreenness, data = Panel)
stargazer(ols, type="text")
#summary(ols)


# # Visualize the residuals
# ggplot(ols, aes(x = fitted(ols), y = residuals(ols))) +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#   labs(title = "Residual Plot",
#        x = "Fitted Values",
#        y = "Residuals") +
#   theme_minimal()


# Assess whether boom-spray treatment is effective in reducing greenness for individual years

# Fit the linear regression model with interactions
# ols <- lm(greennessImpact ~ treated_Prev1Y * as.factor(Year) + treated_Prev2Y * as.factor(Year) + 
# ols <- lm(greennessImpact ~ treated_Prev1Y +# treated_Prev2Y +
#             aspect_transform + elevation + slope + preSprayGreenness, data = Panel)
# stargazer(ols, type="text")
# summary(ols)


# Typical "workflow" of panel modeling, is to FIRST - estimate the pooled, random effects, and fixed effects models, 
# and then SECOND - run a couple of tests to determine which model is the best

# greennessImpact is direct difference of greenness
pooled <- plm(postSprayGreenness ~ treated + treated_2yrs_consecutively + treated_1yrPrior_notFollowingYr + 
                aspect_transform + elevation + slope + preSprayGreenness, data = Panel, model="pooling")
fixed <- plm(postSprayGreenness ~ treated + treated_2yrs_consecutively + treated_1yrPrior_notFollowingYr + 
               aspect_transform + elevation + slope + preSprayGreenness, data = Panel, index=c("Pixel_ID", "Year"), model="within")
random <- plm(postSprayGreenness ~ treated + treated_2yrs_consecutively + treated_1yrPrior_notFollowingYr + 
                aspect_transform + elevation + slope + preSprayGreenness, data = Panel, index=c("Pixel_ID", "Year"), model="random")
stargazer(pooled, fixed, random, column.labels = c("Pooled", "Fixed", "Random"), type = "text")
stargazer(fixed, column.labels = c("Fixed"), type = "text")


# # greenness change as a percent
# pooled <- plm(greennessChange ~ treated + treated_Prev1Y  + treated_Prev2Y  +
#                 aspect_transform + elevation + slope + preSprayGreenness, data = Panel, model="pooling")
# fixed <- plm(greennessChange ~ treated + treated_Prev1Y + treated_Prev2Y + 
#                aspect_transform + elevation + slope + preSprayGreenness, data=Panel, index=c("Pixel_ID", "Year"), model="within")
# random <- plm(greennessChange ~ treated + treated_Prev1Y + treated_Prev2Y + 
#                 aspect_transform + elevation + slope + preSprayGreenness, data=Panel, index=c("Pixel_ID", "Year"), model="random")
# stargazer(pooled, fixed, random, column.labels = c("Pooled", "Fixed", "Random"), type = "text")


# # greenness change as a percent - ln data transform
# Panel <- Panel %>% 
#   mutate(lnPreSprayGreenness = log(preSprayGreenness)) %>% 
#   mutate(lnGreennessChange = log(greennessImpact))
# 
# pooled <- plm(lnGreennessChange ~ treated + treated_Prev1Y  + treated_Prev2Y  +
#                 aspect_transform + elevation + slope + lnPreSprayGreenness, data = Panel, model="pooling")
# fixed <- plm(lnGreennessChange ~ treated + treated_Prev1Y + treated_Prev2Y + 
#                aspect_transform + elevation + slope + lnPreSprayGreenness, data=Panel, index=c("Pixel_ID", "Year"), model="within")
# random <- plm(lnGreennessChange ~ treated + treated_Prev1Y + treated_Prev2Y + 
#                 aspect_transform + elevation + slope + lnPreSprayGreenness, data=Panel, index=c("Pixel_ID", "Year"), model="random")
# stargazer(pooled, fixed, random, column.labels = c("Pooled", "Fixed", "Random"), type = "text")


# Lagrange Multiplier Test - run to determine whether random effects or pooled model is better
plmtest(pooled)
# The null hypothesis of the test is that the pooled model is the best model; however, because the p-value output is less than .05, 
#     we can conclude that the random effects model is preferred

# Hausman test
# The Hausman-test tests whether the individual characteristics are correlated with the regressors
#(see Green, 2008, chapter 9). (i.e., test whether fixed or random effects model is preferred)
phtest(fixed, random)
#The null hypothesis is that random effects is preferred to the fixed effects model. 
# our p.val is actually less than 0.5, so we reject the null and conclude that the fixed effects model is actually preferred to the random

# Look at regressions for each year individually
reg_2016 <-Panel %>% 
  filter(Year == 2016) %>% 
  lm(postSprayGreenness ~ treated + treated_2yrs_consecutively + treated_1yrPrior_notFollowingYr + 
       aspect_transform + elevation + slope + preSprayGreenness, data=., column.labels=c("Pixel_ID", "Year"))

reg_2017 <-Panel %>% 
  filter(Year == 2017) %>% 
  lm(postSprayGreenness ~ treated + treated_2yrs_consecutively + treated_1yrPrior_notFollowingYr + 
       aspect_transform + elevation + slope + preSprayGreenness, data=., column.labels=c("Pixel_ID", "Year"))
  
reg_2018 <-Panel %>% 
  filter(Year == 2018) %>% 
  lm(postSprayGreenness ~ treated + treated_2yrs_consecutively + treated_1yrPrior_notFollowingYr + 
       aspect_transform + elevation + slope + preSprayGreenness, data=., column.labels=c("Pixel_ID", "Year"))
  
reg_2019 <-Panel %>% 
  filter(Year == 2019) %>% 
  lm(postSprayGreenness ~ treated + treated_2yrs_consecutively + treated_1yrPrior_notFollowingYr + 
       aspect_transform + elevation + slope + preSprayGreenness, data=., column.labels=c("Pixel_ID", "Year"))

reg_2021 <-Panel %>% 
  filter(Year == 2021) %>% 
  lm(postSprayGreenness ~ treated + treated_2yrs_consecutively + treated_1yrPrior_notFollowingYr + 
       aspect_transform + elevation + slope + preSprayGreenness, data=., column.labels=c("Pixel_ID", "Year"))
  
reg_2022 <-Panel %>% 
  filter(Year == 2022) %>% 
  lm(postSprayGreenness ~ treated + treated_2yrs_consecutively + treated_1yrPrior_notFollowingYr + 
       aspect_transform + elevation + slope + preSprayGreenness, data=., column.labels=c("Pixel_ID", "Year"))

stargazer(reg_2016, reg_2017, reg_2018, reg_2019, reg_2021, reg_2022, column.labels = c("2016","2017","2018","2019","2021","2022"), title="Yearly OLS", align=TRUE, type = "text")
