# Second year only
# Remove "disqualified outlets"
# Keep close to Vollaard's second ("definitive") model but remove variable "top ten"

rm(list = ls())
# setwd("/Users/richardgill/Desktop/Haring/code")
library(readxl)
library(tidyverse)

ours.df <- read_excel("datasets/ours.xlsx")
ours.df <- arrange(ours.df, vollaard_id)

attach(ours.df)

Repeat[is.na(Repeat)] <- "_"

final_score <- eindcijfer

weight <- weight

temp <- temp

temp_cat <- cut(temp, breaks = c(-3, 7, 10, 20), right = FALSE)

fat <- fat_percentage

fat_cat <-cut(fat_percentage, c(0, 10, 14, 20), right = FALSE)

price <- price_per_100g

price_cat <- cut(ours.df$price_per_100g, breaks = c(1, 2.84, 3.48, 5.1 ), right = FALSE)

freshly_cleaned <- vers_van_het_mes

freshly_cleaned[freshly_cleaned == "Ja"] <- "Yes"
freshly_cleaned[freshly_cleaned == "Nee"] <- "No"
freshly_cleaned <- factor(freshly_cleaned, levels = c("No", "Yes"))

micro[micro == "Zeer goed"] <- "Goed"
micro[micro == "Goed"] <- "Good"
micro[micro == "Voldoende"] <- "Sufficient"
micro[micro == "Slecht"] <- "Bad"
micro[micro == "Waarschuwingsfase"] <- "Danger"
micro[micro == "Afkeurenswaardig"] <- "Danger"
micro[micro == "Afgekeurd"] <- "Danger"

micro <- factor(micro, levels = c("Good", "Sufficient", "Bad", "Danger"))

ripeness[ripeness == "Groen"] <- "Green"
ripeness[ripeness == "Licht"] <- "Light"
ripeness[ripeness == "Gemiddeld"] <- "Average"
ripeness[ripeness == "Sterk"] <- "Strong"
ripeness[ripeness == "Bedorven"] <- "Rotten"
ripeness <- factor(ripeness, levels = c("Green", "Light", "Average", "Strong", "Rotten"))

cleaning <- cleanness

cleaning[cleaning == "Zeer goed"] <- "Very good"
cleaning[cleaning == "Goed"] <- "Good"
cleaning[cleaning == "Matig"] <- "Poor"
cleaning[cleaning == "Slecht"] <- "Bad"

cleaning <- factor(cleaning, levels = c("Very good", "Good", "Poor", "Bad"))

vollaard.df <- read_excel("datasets/vollaard.xlsx")

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
atlanticV <- (vollaard.df$supplier_AD == "A")

Return <- (Repeat == "*")