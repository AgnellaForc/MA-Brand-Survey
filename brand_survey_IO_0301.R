## Reserach Project: Store brands vs. manufacturer brands
# By Group 1B 
getwd()
brand_survey <- read.csv("./brand_survey.csv", header = TRUE,
                      sep = ";")
head(test_data)

## 1. Step: Preparation

# First we load our needed packages: 
library(dplyr)
library(ggplot2)
library(car)
library(Hmisc)

# Here are our collected responses:
head(brand_survey)

# For our analysis we convert all our variables into factors: 
brand_survey$chocolate_consumption <- as.factor(brand_survey$chocolate_consumption)
brand_survey$preferred_brand <- as.factor(brand_survey$preferred_brand)
brand_survey$frequency_preferred_brand <- as.factor(brand_survey$frequency_preferred_brand)
brand_survey$emotion_delisting <- as.factor(brand_survey$emotion_delisting)
brand_survey$scale_milka <- as.factor(brand_survey$scale_milka)
brand_survey$scale_clever <- as.factor(brand_survey$scale_clever)
brand_survey$scale_rittersport <- as.factor(brand_survey$scale_rittersport)
brand_survey$reaction_delisting_price <- as.factor(brand_survey$reaction_delisting_price)
brand_survey$decision_price <- as.factor(brand_survey$decision_price)
brand_survey$reaction_delisting_no_price <- as.factor(brand_survey$reaction_delisting_no_price)
brand_survey$decision_no_price <- as.factor(brand_survey$decision_no_price)
brand_survey$share_groceries <- as.factor(brand_survey$share_groceries)
brand_survey$frequency_groceries <- as.factor(brand_survey$frequency_groceries)
brand_survey$gender <- as.factor(brand_survey$gender)
brand_survey$age <- as.factor(brand_survey$age)
brand_survey$education <- as.factor(brand_survey$education)
brand_survey$income_monthlz <- as.factor(brand_survey$income_monthly)
brand_survey$area <- as.factor(brand_survey$area)
brand_survey$gender <- as.numeric(brand_survey$gender)
brand_survey$age <- as.numeric(brand_survey$age)
brand_survey$education <- as.numeric(brand_survey$education)
brand_survey$income_monthlz <- as.numeric(brand_survey$income_monthlz)
brand_survey$area <- as.numeric(brand_survey$area)

# The next step is to generate descriptive statistics: 
descriptives <- summary(brand_survey)

# We have to "delisting" columns we need to combine for further analysis. Since we did an experiment with 2 groups (one group saw the price of the chocolate, the control group did not), we need to analyze both. 
dplyr::coalesce(brand_survey$reaction_delisting_no_price, brand_survey$reaction_delisting_price)
brand_survey$reaction_delisting <- dplyr::coalesce(brand_survey$reaction_delisting_no_price, brand_survey$reaction_delisting_price)
brand_survey[, c("reaction_delisting_price", "reaction_delisting_no_price", "reaction_delisting")]

# Now we create new column "did_change_store" (which indicates if they survey participant chose to change store after the delisting of the favorite brand):
brand_survey$did_change_store <- brand_survey$reaction_delisting == 3
View(brand_survey)

# We transform the data by creating new columns (did_change_store and group) based the reaction to the delisting (reaction_delisting, reaction_delisting_no_price).
# Also, we make sure that the share_groceries (how much of the typical grocery shopping is done in only one store) column is of numeric type:
brand_survey$did_change_store <- as.numeric(brand_survey$reaction_delisting == 3)
brand_survey$group <- as.factor(ifelse(is.na(brand_survey$reaction_delisting_no_price),"A","B"))
brand_survey$share_groceries <- as.numeric(brand_survey$share_groceries)

#------

## 2. Step: Analysis of the demographics

# After preparing the data, we can run the first regression models. 
# 1. Gender
brand_survey<- brand_survey[brand_survey$gender %in% c(1, 2), ]
  # Overview: 
  table(brand_survey$gender)
  # We are creating a new binary factor variable named "female" based on the values in the gender column. 
  # "Female" equals 1 here:
  brand_survey$female <- as.factor(ifelse(brand_survey$gender == 2, 1, 0))
  table(brand_survey$female)
  # Regression model: 
  m5 <- glm(did_change_store ~ group + female + group:female, family = binomial(link="logit"), data = brand_survey)
  summary(m5)
  # This shows no statistical significance regarding the gender. 

# 2. Area of living 
  # We create new columns based on the area of living of the survey participants. We chose the 5 categories "rural area", "small area", "town", "city" and "megalopolis" do differentiate between the areas of living: 
  brand_survey$rural_area <- ifelse(brand_survey$area == 1, 1, 0)
  brand_survey$small_Area <- ifelse(brand_survey$area == 2, 1, 0)
  brand_survey$town <- ifelse(brand_survey$area == 3, 1, 0)
  brand_survey$city <- ifelse(brand_survey$area == 4, 1, 0)
  brand_survey$megalopolis <- ifelse(brand_survey$area == 5, 1, 0)
  # Regression model: 
  m7 <- glm(did_change_store ~ rural_area + small_Area + town + city, 
          data = brand_survey, family = "binomial")
  summary(m7)
  # This model shows no statistical significance regarding the area of living. But our sample is not very representative for this model. 

# 3. Income level
table(brand_survey$income_monthly)

  # We create new columns based on the income categories. We have 5 categories: 
  brand_survey$Income_monthly_1 <- ifelse(brand_survey$income_monthly == 1, 1, 0)
  brand_survey$Income_monthly_2 <- ifelse(brand_survey$income_monthly == 2, 1, 0)
  brand_survey$Income_monthly_3 <- ifelse(brand_survey$income_monthly == 3, 1, 0)
  brand_survey$Income_monthly_4 <- ifelse(brand_survey$income_monthly == 4, 1, 0)
  brand_survey$Income_monthly_5 <- ifelse(brand_survey$income_monthly == 5, 1, 0)
  # Regression model: 
  m8 <- glm(did_change_store ~ Income_monthly_1 + Income_monthly_2 + Income_monthly_3 + Income_monthly_4, 
          data = brand_survey, family = "binomial")
  summary(m8)
  exp(coef(m8))
  # This model shows no statistical significance regarding the income. But our sample is not very representative for this model. 

#------

## Analysis of the 2 main models:
  
# Model A
# Our first main model is focused on survey participants, who chose to change store when their favorite brand is not available. 

# 1. Model A (basic model)
m1 <- glm(did_change_store ~ group, family = binomial(link="logit"), data = brand_survey)
summary(m1)
# The result is insignificant since the p-value is 0.749

# 2. Model A: relationship between "did change store" and "share of bought groceries in one store":
m2 <- glm(did_change_store ~ group + share_groceries + group:share_groceries, family = binomial(link="logit"), data = brand_survey)
summary(m2)
# This shows, the higher the moderator (percentage shared groceries) the more people remained loyal to the store and chose one of the alternatives provided at the store. 
exp(coef(m2))
  
# Assumptions: 
  # YES Influential observations
#1. Cook's distance
  plot(m2, 4) #visualize the Cook’s distance values
  #no obs near 1
#2. Standardized residuals
  #Data points with an absolute standardized residuals above 3 represent possible outliers
library(broom)
  model.data <- augment(m2) %>% 
    mutate(index = 1:n())  
  model.data %>% top_n(3, .cooksd) #top 3 largest values
  ggplot(model.data, aes(index, .std.resid)) + 
    geom_point(aes(color = did_change_store), alpha = .5) +
    theme_bw() #plot standardized residuals 
  model.data %>% 
    filter(abs(.std.resid) > 3) #filter potential influential data points
  
  plot(m2, 5) #no groups of obs in the upper or lower right corner
  
# YES Non-linear relationships: we use the added variable plots. 
  avPlots(m2, col.lines = palette()[2]) #NO, use log odds 

#Inspecting the scatter plot between each predictor and the logit values.
 
 
  # Get predicted probabilities from the model
  predicted_probs <- predict(m2, type = "response")
  
  # Calculate predicted log odds from predicted probabilities
  predicted_log_odds <- log(predicted_probs / (1 - predicted_probs))
 
  new_dataset <- brand_survey %>%
    filter(!is.na(did_change_store)) #exclude NA from the variable did_change_store
  
  # Create scatter plot for 'group' variable
  plot(new_dataset$group, predicted_log_odds, xlab = "Group", ylab = "Predicted Log Odds", main = "Scatter Plot: Group vs. Log Odds")
  

#YES Multicollinearity: we first test the bivariate correlations for any extremely high correlations (i.e., >0.8).
  rcorr(as.matrix(brand_survey[, c("did_change_store", "share_groceries")]))
#secondly we look at the VIF values
  library(car)
  vif(m2) 
  #two VIF values are higher that the threshold of 4
#solution: variable selection procedures or principal component analysis
  
# 3. Model A: we now want to know, whether a strong preference for the brand milka has a significant impact.
# For that we take the column "scale_milka" and only select values higher than 3 (meaning they like Milka):
brand_survey$milka_pref <- as.factor(ifelse(as.numeric(brand_survey$scale_milka)>3,1,0))
table(brand_survey$milka_pref)
# Regression model:
m3 <- glm(did_change_store ~ group + milka_pref + group:milka_pref, family = binomial(link="logit"), data = brand_survey)
summary(m3)
# This model shows that there would be a significant effect (switching stores) if the sample was bigger. 
exp(coef(m3))

#assumption test!

# 4. Model A: this model is about survey participants with a strong reaction to the delisting.
# We therefore check the column "emotion_delisting" for values 2 (Disappointed) and 3 (Angry):
table(brand_survey$emotion_delisting)
brand_survey$strong_reaction <- as.factor(ifelse(brand_survey$emotion_delisting %in% c(2, 3), 1, 0))
table(brand_survey$strong_reaction)
# Regression model: 
m4 <- glm(did_change_store ~ group + strong_reaction + group:strong_reaction, family = binomial(link="logit"), data = brand_survey)
summary(m4)
exp(coef(m4))
# This model shows no statistical significance regarding a strong emotion towards the delisting. 

# Testing for the best model: group, share groceries, milka preference, strong reaction and interaction terms
bestmodel_AIC <- glm(did_change_store ~ group + share_groceries + milka_pref + strong_reaction + group:share_groceries + group:milka_pref + group:strong_reaction , family = binomial(link="logit"), data = brand_survey)
model_AIC <- step(bestmodel_AIC) 
# lowest AIC: 168.88; did_change_store ~ share_groceries + milka_pref + strong_reaction

# Model B
# The second main model is focused on survey participants, who chose NOT to change store when their favorite brand is not available. 

# We need to create a subset for those, who did not want to change store after the delisting:
no_churn_data <- subset(brand_survey, did_change_store != 1)
# Another subset for the survey participant that would buy the private label "Clever" after the delisting of their favorite brand: 
no_churn_data$buy_clever <- as.numeric(no_churn_data$reaction_delisting == 2)

# 1. Model B:
# Hypothesis: The price has a negative effect on the decision to buy the store brand, because cheaper brands are perceived as lower quality.
m1_B <- glm(buy_clever ~ group, family = binomial(link="logit"), data = no_churn_data) 
summary(m1_B) 
# The coefficient is not significant. This means our first hypothesis cannot be proven. 

#assumption test!

# 2. Model B:
# Hypothesis: People who are loyal to one store are more likely to buy the store brand after the delisting of their favorite brand.
m2_B <- glm(buy_clever ~ group + share_groceries + group:share_groceries, family = binomial(link="logit"), data = no_churn_data)
summary(m2_B) 
# The interaction coefficient slightly significant. There might be a connection between store loyalty and the likelihood of choosing the store brand. 

#assumption test!

# 3. Model B:
# Hypothesis: People who like the brand is Clever are more likely to choose it as a substitute after the delisting of their favorite brand.
# People who like clever: 
no_churn_data$clever_pref <- as.factor(ifelse(as.numeric(no_churn_data$scale_clever)>3,1,0))
# Regression model: 
m3_B <- glm(buy_clever ~ group + clever_pref + group:clever_pref, family = binomial(link="logit"), data = no_churn_data)
summary(m3_B) 
# The group variable slightly significant, which means there is some indication that the preference for the Clever brand influences the choice of it as a substitute. 

#assumption test!

# 4. Model B:
# Hypothesis: People who are indifferent to the delisting will be unlikely to change the store after the delisting of their favorite brand. 
# People with no strong emotional reaction to the delisting: 
table(brand_survey$emotion_delisting)
brand_survey$indifference <- as.factor(ifelse(brand_survey$emotion_delisting %in% c(1, 4), 1, 0))
table(brand_survey$indifference)
# Regression model: 
m4_B <- glm(did_change_store ~ group + indifference + group:indifference, family = binomial(link="logit"), data = brand_survey)
summary(m4_B)
exp(coef(m4_B))
# This is not statistically significant. There is no connection between being indifferent about the delisting and changing store.

#assumption test!

# Testing for the best model:
bestmodelB_AIC <- glm(buy_clever ~ group + share_groceries + clever_pref + indifference + group:share_groceries + group:clever_pref + group:indifference , family = binomial(link="logit"), data = no_churn_data)
model_AIC <- step(bestmodelB_AIC) 
#lowest AIC = 86.72: buy_clever ~ group + share_groceries + clever_pref + group:share_groceries + group:clever_pref
