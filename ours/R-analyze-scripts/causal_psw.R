rm(list = ls())
library(readxl)
library(twang)
library(survey)
# library(tidyverse)
source('./import_data_universal.R')
# final_score <- vollaard.df$finalscore

vvo <- TRUE
if (!vvo) {
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


# second model to study atlantic without Return as features in propensity score 
# weighting 
# all data points
# atlantic is barely statistically significant (p-value 0.08)

ours$atlantic <- as.numeric(ours$atlantic) - 1 # go back to numeric representation for now
ps.herring.gbm2 <- ps(atlantic ~ weight + temp + fat + price + freshly_cleaned 
                      + micro + ripeness + cleaning + year + k30,
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
ps.herring.gbm3 <- ps(atlantic ~ weight + temp + fat + price + freshly_cleaned 
                      + micro + ripeness + cleaning + year + k30 + Return,
                      data=ours,
                      n.trees=20000,
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

ps.herring.gbm5 <- ps(atlantic ~ weight + temp + fat + price + freshly_cleaned + micro + ripeness + cleaning + k30 + Return,
                      data=temp,
                      n.trees=10000,
                      interaction.depth=2,
                      shrinkage=0.01,
                      estimand = "ATT",
                      stop.method=c("es.mean","ks.max"),
                      n.minobsinnode = 10,
                      verbose=FALSE)
plot(ps.herring.gbm5)

temp$psweight <- get.weights(ps.herring.gbm5, stop.method="es.mean")

design.ps <- svydesign(ids=~1, weights=~psweight, data=temp)

glm5 <- svyglm(final_score ~ atlantic, design=design.ps)

summary(glm5)
temp <- dplyr::select(temp, -psweight) 
}

## for vollaard and van ours 2021
if (vvo){

# ours.df <- arrange(ours.df, vvo_id)
# ours.df$weight - vollaardvanours.df$Weight*10
# ours.df$fat_percentage - vollaardvanours.df$Fat
# ours.df$temp - vollaardvanours.df$Temperature
# table(ours.df$supplier_AD, vollaardvanours.df$Supplier_A)
# vollaard.df.vvo <- vollaard.df[ours.df$vollaard_id,]
# max(abs(vollaard.df.vvo$weight - vollaardvanours.df$Weight*10))
# table(vollaard.df.vvo$supplier_AD, vollaardvanours.df$Supplier_A)
# table(vollaard.df.vvo$supplier_AD, ours.df$supplier_AD)

vollaardvanours.df.tmp <- data.frame(vollaardvanours.df)
vvo.lm <- lm(Provisional_rating~1+verbaljudgment_num+Ripening+Cleaning+
               Fresh_num + Supplier_A, data=vollaardvanours.df.tmp )
summary(vvo.lm)

par(mfrow = c(2, 2)); plot(vvo.lm)
vvo.design <- as.matrix(model.matrix(vvo.lm))
kappa(vvo.design, exact=TRUE)

vvo.rlm <- MASS::rlm(Provisional_rating~1+verbaljudgment_num+Ripening+Cleaning+
                       Fresh_num + Supplier_A, data=vollaardvanours.df.tmp )
summary(vvo.rlm)
par(mfrow = c(2, 2)); plot(vvo.rlm)


vollaardvanours.df.tmp$Supplier_A <- as.numeric(as.character(vollaardvanours.df$Supplier_A))
ps.vvo.gbm = ps(Supplier_A ~ verbaljudgment_num + Ripening+Cleaning+Fresh_num,
                    data=vollaardvanours.df.tmp,
                    n.trees=2000,
                    interaction.depth=2,
                    shrinkage=0.01,
                    estimand = "ATT",
                    stop.method=c("es.mean","ks.max"),
                    n.minobsinnode = 10,
                    verbose=FALSE)
plot(ps.vvo.gbm)
bal.table(ps.vvo.gbm)

vollaardvanours.df.tmp$psweight <- get.weights(ps.vvo.gbm, stop.method="es.mean")

design.ps <- svydesign(ids=~1, weights=~psweight, data=vollaardvanours.df.tmp)

glm1 <- svyglm(Provisional_rating ~ Supplier_A, design=design.ps)

summary(glm1)

}
