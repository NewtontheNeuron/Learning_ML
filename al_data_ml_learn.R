library(tidymodels)
library(tidyverse)
library(caret)
data(ad_data)
ad_data

#### Data science ####
# What are the column namses
names(ad_data)
# A lot of the columns contain protein levels in the blood
range(ad_data$Adiponectin)
range(ad_data$VCAM_1)
range(ad_data$p_tau)
# Why is age stored weirdly?
ad_data$age

# Usually data science comes before ml but for the purpose of learning today we
# make an exception.

#### Machine learning practice ####
# Create the training and testing sets.
ml_ad <- initial_split(ad_data, prop = 3/4)
trainset <- training(ml_ad)
testset <- testing(ml_ad)

# TODO Try different splitting techniques


# Now we will train linear regression model on the trainset
# We will predict Ab_42 levels based the information available
model1 <- lm(Ab_42 ~ Apolipoprotein_E, data = trainset)
summary(model1)
# A pvalue less than 0.05 shows us that ApoeE protein levels significantly
# predict the the Ab_42 protein levels. Mean Ab_42 levels increases by 0.6826
# units fore every 1 increase in ApoeE blood concentration.
sigma(model1)
sigma(model1)^2
confint(model1)
# Visualize ApoeE against Ab_42
trainset %>%
  ggplot(aes(Apolipoprotein_E, Ab_42)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)
# We can do another model this time including RANTES for example
model2 <- update(model1, . ~ . + RANTES)
summary(model2)
# RANTES does not have a significant effect on the levels of Ab_42
sigma(model2)
sigma(model2)^2
# Then we can add everything
model3 <- lm(Ab_42 ~ ., data = trainset)
summary(model3)
sigma(model3)
sigma(model3)^2
# We can do this with cross validation and some feature selection too.

