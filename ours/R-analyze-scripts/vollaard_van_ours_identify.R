
rm(list = ls())
source("./import_data_universal.R")


vollaardvanours.df <- haven::read_dta("datasets/Herring-Data-JEBO-2022.dta")

if (FALSE) {
# now we find new vollaard and van ours id 
new_vvo_id <- rep(-1, nrow(ours.df)) 
# new_vvo_id[i] is the new id number in vollaard and van ours for the i-th row
# in ours.df, which follows vollaard's first sequence definition

for (i in 1:length(new_vvo_id)) {
# for (i in 1:10) {
  weight_match <- (as.numeric(ours.df$weight[i]) - as.vector(vollaardvanours.df$Weight*10))^2 <= 0.000001
  fat_match <-  (as.numeric(ours.df$fat_percentage[i]) - vollaardvanours.df$Fat)^2 <= 0.00001
  temp_match <-  (as.numeric(ours.df$temp[i]) - vollaardvanours.df$Temperature)^2 <= 0.00001
  final_match <- weight_match & fat_match & temp_match
  
  if (sum(final_match)!= 1)  {print(paste("problem at i = ", i)) } else {
    new_vvo_id[i] <- which(final_match)
  }
}

ours.df$vvo_id <- new_vvo_id
xlsx::write.xlsx(ours.df, file="datasets/ours_w_vvo.xlsx")
}

vvo_vollaard_id <- rep(-1, nrow(vollaardvanours.df))
for (i in 1:length(vollaardvanours.df)) {
  # for (i in 1:10) {
  weight_match <- (as.numeric(vollaardvanours.df$Weight[i])*10 
                   - as.vector(ours.df$weight))^2 <= 0.000001
  fat_match <- (as.numeric(vollaardvanours.df$Fat[i]) 
                - as.vector(ours.df$fat_percentage))^2 <= 0.000001
  temp_match <- (as.numeric(vollaardvanours.df$Temperature[i]) 
                 - as.vector(ours.df$temp))^2 <= 0.000001
  final_match <- weight_match & fat_match & temp_match
  
  if (sum(final_match)!= 1)  {print(paste("problem at i = ", i)) } else {
    vvo_vollaard_id[i] <- which(final_match)
  }
}
vollaardvanours.df$vollaard_id <- vvo_vollaard_id
xlsx::write.xlsx(vollaardvanours.df, file="datasets/vollaardvanours_ided.xlsx")
