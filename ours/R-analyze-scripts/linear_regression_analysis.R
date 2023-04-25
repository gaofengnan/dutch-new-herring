rm(list = ls())
# detach(ours.df)
# setwd("~/Dropbox/fengnan_richard/code")
# setwd("/Users/richardgill/Desktop/Haring/code")
library(readxl)
library(tidyverse)

source('./ancillary.R')

ours.df <- read_excel("datasets/ours.xlsx")
ours.df <- arrange(ours.df, vollaard_id)
names(ours.df)
str(ours.df)
attach(ours.df)

table(Repeat)
Repeat[is.na(Repeat)] <- "_"
table(Repeat)

final_score <- eindcijfer

weight <- weight

temp <- temp

temp_cat <- cut(temp, breaks = c(-3, 7, 10, 20), right = TRUE)

fat <- fat_percentage

fat_cat <-cut(fat_percentage, c(0, 10, 14, 20), right = TRUE)

price <- price_per_100g

price_cat <- cut(ours.df$price_per_100g, breaks = c(1, 2.84, 3.48, 5.1 ), right = TRUE)

freshly_cleaned <- vers_van_het_mes

freshly_cleaned[freshly_cleaned == "Ja"] <- "Yes"
freshly_cleaned[freshly_cleaned == "Nee"] <- "No"
freshly_cleaned <- factor(freshly_cleaned, levels = c("No", "Yes"))

micro[micro == "Zeer goed"] <- "Goed"
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

summary(weight)
str(weight)

summary(temp)
table(temp_cat)
str(temp_cat)

summary(fat)
table(fat_cat)
str(fat_cat)

summary(price)
table(price_cat)
str(price_cat)

table(freshly_cleaned)
str(freshly_cleaned)

table(micro)
str(micro)

table(ripeness)
str(ripeness)

table(cleaning)
str(cleaning)

vollaard.df <- read_excel("datasets/vollaard.xlsx")
# vollaard.df.rescaled <- scale(vollaard.df)
names(vollaard.df)

k30 <- vollaard.df$k30
k30 <- as.character(k30)
k30[k30 == "0"] <- "<30km"
k30[k30 == "1"] <- ">30km"
k30 <- factor(k30, levels = c("<30km", ">30km"))

year <- vollaard.df$yr2017
year <- as.character(year)
year[year == "0"] <- "2016"
year[year == "1"] <- "2017"
year <- factor(year, levels = c("2016", "2017"))

atlantic <- (supplier_AD == "A")

Return <- (Repeat == "*")

sum(vollaard.df$id == vollaard_id)

## for fig. 1 in the paper
## plot two years' scores separately
tiff("plots/hist.tiff", res=300, bg="transparent", height=16, width=15, 
     unit="cm")
par(mfrow=c(2,1))
hist(final_score[year==2016], breaks=20, ylim=c(0,16),
     main = "Histogram of final scores in 2016", col = "royalblue",
     xlab = "Final test score", ylab = "Numer of outlets")
hist(final_score[year==2017], breaks=20, ylim=c(0,16),
     main = "Histogram of final scores in 2017", col="royalblue",
     xlab = "Final test score", ylab = "Numer of outlets")
dev.off()

## First analysis with our data
## I try to reproduce something like Vollaard's first report, second model.
## See: "haringtest_vollaard.pdf", appendix: two models, one without, one with k30
## The results are similar except for "micro". However, a number of
## variables now are significant at astronomically small significance levels,
## while the variable "micro" is now hardly significant at all.
## Notice the effects of ">30 Km distant from Rotterdam"
## The differences seem to be due to the small differences in coding of "final_score"
## Vollaard reduces it to whole integers (plus 7.5, God knows why) whereas we have
## distinguished between, for instance, 8-, 8, and *+ (We read 8- as 8 - epsilon, with epsilon = 0.03)
## There are strange differences between the results of lm() and of aov(). 
## The models are the same. The t-test of lm() and the F-test of aov() for a numerical variable
## should lead to identical p-values

final_score_vollaardized <- score_pm_rearrange(final_score,eps_new = 0.2) + 7.5
ripeness_vollaardized <- ripeness
ripeness_vollaardized[ripeness_vollaardized=="Green"] = "Light"

first.lm <- lm(final_score_vollaardized ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness_vollaardized + cleaning + k30 + year)
summary(first.lm)
# summary(aov(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + year))
drop1(first.lm, test="F")

## Second analysis with our data
## I now include a dummy variable for "Atlantic", according to AD
## The effect of "Atlantic" is highly significant, the effect of ">30Km distance" is a lot smaller and no longer significant, -0.3
## This is completely the opposite to what Vollaard would have got,
## had he fitted this model on his data and using his classification of Atlantic outlets
second.lm <- lm(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + year + atlantic)
summary(second.lm)
# summary(aov(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + year + atlantic))
drop1(second.lm, test="F")

## Third analysis with our data
## I only use the data from the second year, 2017, and I leave out outlets which
## scored "zero" because this really means their herring do not pass basic
## hygiene requirements.
## I add a variable called "Return" which says whether or not the outlet
## was also (according to me) part of the 2016 ranking.
## Now, none of being >30 Km away, being an Atlantic supplied outlet, 
## and being a "Return" outlet are significant
## The effect of ">30 Km" though not significant is of about the same size, -0.3, as in the last models; 
## and being a Returning outlet has an effect of similar size, +0.3
third.lm <- lm(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic + Return, subset = (year == "2017" & final_score > 0))
summary(third.lm)

## Fourth analysis with our data
## Again, I only use the data from the second year, 2017, and only outlets with final score > 0.
## I include the variables "Return", "atlantic" and distance from Rotterdam
## but none are significant.
## The estimated effect of ">30 Km" still is of about the same size, -0.4, as in the last models. 
## The estimated effects of being an Atlantic supplied outlet, and being a returning outlet, are small
fourth.lm <- lm(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic + Return, subset = (year == "2017" & final_score > 0))
summary(fourth.lm)


## Fifth analysis with our data
## Again, I only use the data from the second year, 2017, and only outlets with final score > 0.
## I leave out "Return" and "Atlantic"
fifth.lm <- lm(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30, subset = (year == "2017" & final_score > 0))
summary(fifth.lm)
fifth.aov <- aov(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30, subset = (year == "2017" & final_score > 0))
summary(fifth.aov)

## Sixth analysis with our data
## Again, I only use the data from the second year, 2017, and only outlets with final score > 0.
## I leave out "Return", put "Atlantic" back in.
sixth.lm <- lm(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic, subset = (year == "2017" & final_score > 0))
summary(fifth.lm)
sixth.aov <- aov(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic, subset = (year == "2017" & final_score > 0))
summary(fifth.aov)


## Now a model with all numerical variables fitted as quadratic effects

weight1 <- (weight - 70)/10
weight2 <- weight1^2
temp1 <- temp - 7
temp2 <- temp1^2
fat1 <- (fat - 12)/10
fat2 <- fat1^2
price1 <- price - 3
price2 <- price1^2

new.lm <- lm(final_score ~ weight1 + weight2 + temp1 + temp2 + fat1 + fat2 + price1 + price2 + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic)
summary(new.lm)

new.lm <- lm(final_score ~ weight + temp + fat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic)
summary(new.lm)

new.lm <- lm(final_score ~ weight + temp + fat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic, subset = (ripeness != "Rotten"))
summary(new.lm)

new.lm <- lm(final_score ~ weight + freshly_cleaned + ripeness + cleaning + k30 + atlantic, subset = (ripeness != "Rotten" & micro != "Danger" & temp < 10))
summary(new.lm)

new.lm <- lm(final_score ~ weight + freshly_cleaned + cleaning + k30 + atlantic, subset = (ripeness != "Rotten" & micro != "Danger" & temp < 10))
summary(new.lm)

new.lm <- lm(final_score ~ weight + freshly_cleaned + cleaning + price + atlantic + Repeat, subset = (ripeness != "Rotten" & micro != "Danger" & temp < 10 & year == "2017"))
summary(new.lm)

new.lm <- lm(final_score ~ weight + freshly_cleaned + cleaning + price + atlantic + k30, subset = (ripeness != "Rotten" & micro != "Danger" & temp < 10 & year == "2017"))
summary(new.lm)

new.lm <- lm(final_score ~ weight + freshly_cleaned + cleaning + price + atlantic + k30, subset = (ripeness != "Rotten" & micro != "Danger" & temp < 10))
summary(new.lm)

# FN: function score_pm_rearrange has been moved to ancilary.R, which has been sourced in the beginning, on 07-06-2022


## FN: Now we do a model search
# first we do all the drop-one-variable model search
new_df <- data.frame(vollaard_id, final_score, atlantic, cleaning, fat_cat, freshly_cleaned, k30, micro, price_cat, Repeat, Return, ripeness, temp_cat, weight, year) # form a new data frame for convenience
lm_all <- lm(final_score ~ atlantic + cleaning +fat_cat + freshly_cleaned + k30+ micro + price_cat + Repeat + Return+ ripeness+ temp_cat+ weight+ year, data=new_df)
summary(lm_all)

lm_list <- list()  ## all lm are stored in lm_list, you can access each lm model by 
## for instance, lm_list[["weight"]] for the lm without the variable weight
for (j in 3:ncol(new_df)) {
    # j <- 7
    cat(paste0("\n=======Beginning processing the lm model without variable\t", names(new_df)[j], "\n"))
    xnames <- paste(names(new_df)[c(-1,-2,-j)])
    fmla <- as.formula(paste("final_score ~ ", paste(xnames, collapse= "+")))
    lm_j <- lm(fmla, data=new_df)
    # print(summary(lmj))
    anova_j <- anova(lm_all, lm_j)
    print(anova_j)
    lm_list[[names(new_df)[j]]] <- lm_j
    cat(paste0("========Ending processing the lm model without variable\t", names(new_df)[j]), "\n\n")
}


## Compare with drop1 function from R
print(drop1(lm_all, test = "F"))

## FN: Now we do exhaustive search with leaps from leaps package
# require("ExhaustiveSearch")
# df_tmp <- df[,c(2,3,4,5)]
# df_tmp <- new_df[,c(-1)]
# ES <- ExhaustiveSearch::ExhaustiveSearch(final_score ~ weight + price_cat + temp_cat, data=df_tmp, family="gaussian", performanceMeasure = "AIC")
# change the formula to final_score ~ . for all variable search, warning: very slow and computational errors prone! 
# print(ES)


## Meulman type stuff
year_2017 <- year==2017
meulman_func <- function(rank) {
  
}
