rm(list = ls())
library(readxl)
library(twang)
library(survey)
# library(tidyverse)


ours.df <- read_excel("ours.xlsx")
# ours.df <- arrange(ours.df, vollaard_id)
ours.df <- ours.df[order(ours.df$vollaard_id),]

names(ours.df)
str(ours.df)
attach(ours.df)

table(Repeat)
Repeat[is.na(Repeat)] <- "_"
table(Repeat)

final_score <- eindcijfer

weight <- weight

temp <- temp

fat <- fat_percentage

price <- price_per_100g

freshly_cleaned <- vers_van_het_mes

freshly_cleaned[freshly_cleaned == "Ja"] <- "Yes"
freshly_cleaned[freshly_cleaned == "Nee"] <- "No"
freshly_cleaned <- factor(freshly_cleaned, levels = c("No", "Yes"))

micro[micro == "Zeer goed"] <- "Good"
micro[micro == "Goed"] <- "Good"
micro[micro == "Voldoende"] <- "Sufficient"
micro[micro == "Slecht"] <- "Bad"
micro[micro == "Waarschuwingsfase"] <- "Warning"
micro[micro == "Afkeurenswaardig"] <- "Danger"
micro[micro == "Afgekeurd"] <- "Danger"

micro <- factor(micro, levels = c("Good", "Sufficient", "Bad", "Warning", "Danger"))

ripeness[ripeness == "Groen"] <- "Green"
ripeness[ripeness == "Licht"] <- "Light"
ripeness[ripeness == "Gemiddeld"] <- "Average"
ripeness[ripeness == "Sterk"] <- "Strong"
ripeness[ripeness == "Bedorven"] <- "Rotten"
ripeness <- factor(ripeness, levels = c("Light", "Average", "Green", "Strong", "Rotten")) # light is the reference category of Vollaard

cleaning <- cleanness

cleaning[cleaning == "Zeer goed"] <- "Very good"
cleaning[cleaning == "Goed"] <- "Good"
cleaning[cleaning == "Matig"] <- "Poor"
cleaning[cleaning == "Slecht"] <- "Bad"

cleaning <- factor(cleaning, levels = c("Very good", "Good", "Poor", "Bad"))

vollaard.df <- read_excel("vollaard.xlsx")
names(vollaard.df)

k30 <- as.factor(vollaard.df$k30)
# k30 <- as.character(k30)
# k30[k30 == "0"] <- "<30km"
# k30[k30 == "1"] <- ">30km"
# k30 <- factor(k30, levels = c("<30km", ">30km"))

year <- vollaard.df$yr2017
year <- as.character(year)
year[year == "0"] <- "2016"
year[year == "1"] <- "2017"
year <- factor(year, levels = c("2016", "2017"))

supplier_AD[supplier_AD == "A"] <- 1
supplier_AD[supplier_AD == "X"] <- 0
atlantic <- as.factor(supplier_AD)

Return <- as.factor(Repeat == "*")

ours <- data.frame(final_score, weight, temp, fat, price, freshly_cleaned, micro, ripeness, cleaning, k30, year, atlantic, Return)

# first study on the k30 variable, it is by far not statistically significant with even the most basic causal analysis via propensity score weighting
# we now focus only on Atlantic
ours$k30 <- as.numeric(ours$k30) - 1 # go back to 0-1 numeric values for now 
ps.herring.gbm = ps(k30 ~ weight + temp + fat + price + freshly_cleaned + micro + ripeness + cleaning + year + atlantic,
                    data=ours,
                    n.trees=10000,
                    interaction.depth=2,
                    shrinkage=0.01,
                    estimand = "ATT",
                    stop.method=c("es.mean","ks.max"),
                    n.minobsinnode = 10,
                    verbose=FALSE)
plot(ps.herring.gbm)

ours$psweight <- get.weights(ps.herring.gbm, stop.method="es.mean")

design.ps <- svydesign(ids=~1, weights=~psweight, data=ours)

glm1 <- svyglm(final_score ~ k30, design=design.ps)

summary(glm1)
ours <- dplyr::select(ours,-psweight)
ours$k30 <- as.factor(ours$k30)


# second model to study atlantic without Return as features in propensity score weighing 
# all data points
# atlantic is barely statistically significant (p-value 0.08)

ours$atlantic <- as.numeric(ours$atlantic) - 1 # go back to numeric representation for now
ps.herring.gbm2 <- ps(atlantic ~ weight + temp + fat + price + freshly_cleaned + micro + ripeness + cleaning + year + k30,
                      data=ours,
                      n.trees=10000,
                      interaction.depth=2,
                      shrinkage=0.01,
                      estimand = "ATT",
                      stop.method=c("es.mean","ks.max"),
                      n.minobsinnode = 10,
                      verbose=FALSE)
plot(ps.herring.gbm2)

ours$psweight <- get.weights(ps.herring.gbm2, stop.method="es.mean")

design.ps <- svydesign(ids=~1, weights=~psweight, data=ours)

glm2 <- svyglm(final_score ~ atlantic, design=design.ps)

summary(glm2)
ours <- dplyr::select(ours,-psweight)

# third model, all data points, with Return put into the covariates for the analysis 
# atlantic is no longer statistically significant
ps.herring.gbm3 <- ps(atlantic ~ weight + temp + fat + price + freshly_cleaned + micro + ripeness + cleaning + year + k30 + Return,
                      data=ours,
                      n.trees=10000,
                      interaction.depth=2,
                      shrinkage=0.01,
                      estimand = "ATT",
                      stop.method=c("es.mean","ks.max"),
                      n.minobsinnode = 10,
                      verbose=FALSE)
plot(ps.herring.gbm3)

ours$psweight <- get.weights(ps.herring.gbm3, stop.method="es.mean")

design.ps <- svydesign(ids=~1, weights=~psweight, data=ours)

glm3 <- svyglm(final_score ~ atlantic, design=design.ps)

summary(glm3)
ours <- dplyr::select(ours, -psweight)

# model 4, first getting rid of those with final_score zero
# and repeat the experiment in model 3
# atlantic is not statistically significant at all

temp <- subset(ours, (ours$final_score > 0)) 

ps.herring.gbm4 <- ps(atlantic ~ weight + temp + fat + price + freshly_cleaned + micro + ripeness + cleaning + k30 + Return,
                      data=temp,
                      n.trees=10000,
                      interaction.depth=2,
                      shrinkage=0.01,
                      estimand = "ATT",
                      stop.method=c("es.mean","ks.max"),
                      n.minobsinnode = 10,
                      verbose=FALSE)
plot(ps.herring.gbm4)

temp$psweight <- get.weights(ps.herring.gbm4, stop.method="es.mean")

design.ps <- svydesign(ids=~1, weights=~psweight, data=temp)

glm4 <- svyglm(final_score ~ atlantic, design=design.ps)

summary(glm4)
temp <- dplyr::select(temp, -psweight) 

# model 5, first getting rid of those with final_score zero and in year 2016
# and repeat the experiment in model 3
# atlantic is not even relevant anymore

temp <- subset(ours, (ours$final_score > 0 & ours$year =="2017"))

ps.herring.gbm4 <- ps(atlantic ~ weight + temp + fat + price + freshly_cleaned + micro + ripeness + cleaning + k30 + Return,
                      data=temp,
                      n.trees=10000,
                      interaction.depth=2,
                      shrinkage=0.01,
                      estimand = "ATT",
                      stop.method=c("es.mean","ks.max"),
                      n.minobsinnode = 10,
                      verbose=FALSE)
plot(ps.herring.gbm4)

temp$psweight <- get.weights(ps.herring.gbm4, stop.method="es.mean")

design.ps <- svydesign(ids=~1, weights=~psweight, data=temp)

glm4 <- svyglm(final_score ~ atlantic, design=design.ps)

summary(glm4)
temp <- dplyr::select(temp, -psweight) 
