
rm(list = ls())

source("./import_data_universal.R")

printMeanSD <- function(at_idx, variable) {
  print(paste0("mean atlantic ", round(mean(variable[at_idx]),digits = 2)))
  print(paste0("mean non-atlantic ", round(mean(variable[!at_idx]), digits=2)))
  print(paste0("sd atlantic ", round(sd(variable[at_idx]), digits = 2)))
  print(paste0("sd non-atlantic ", round(sd(variable[!at_idx]), digits=2)))
}
summary(final_score[atlantic & final_score>2])
summary(final_score[!atlantic & final_score>2])

printMeanSD(atlantic, final_score)
printMeanSD(atlantic, fat)
printMeanSD(atlantic, weight)
price_per_piece_normalized <- price_per_100g*weight/100
printMeanSD(atlantic, price_per_piece_normalized)
printMeanSD(atlanticV, price)

sd(fat[atlanticV])/mean(fat[atlanticV])
sd(fat[!atlanticV])/mean(fat[!atlanticV])
min(fat[atlanticV]) #& final_score>2])
min(fat[!atlanticV]) # & final_score>2])
mean(price[!atlanticV])

mean(weight[atlanticV])
sd(weight[atlanticV]) 

mean(weight[!atlanticV])
sd(weight[!atlanticV])
