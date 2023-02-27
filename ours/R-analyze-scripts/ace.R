
source("./import_data_universal.R")


### Start the ace stuff



library(acepack)


### Year = 2017

final_score_temp <- final_score
final_score_temp[final_score == 0 | year == "2016"] <- NA
plain.lm <- lm(final_score_temp ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic)
summary(plain.lm)
par(mfrow = c(2,2))
plot(plain.lm)

x <- plain.lm$fitted
y <- abs(plain.lm$residuals)

par(mfrow = c(1, 1))
scatter.smooth(x, y, span = 1/3)

smooth <- loess(y ~ x, span = 1/3)
weights <- rep(0, 292)
weights[final_score > 0 & year == "2017"] <- predict(smooth, x)^2
weights[final_score == 0 | year == "2016"] <- NA
weighted.lm <- lm(final_score_temp ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic, weights = weights)
summary(weighted.lm)

mat <- model.matrix(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic)
mat <- mat[final_score > 0 & year == "2017", ]
y_dat <- final_score[final_score > 0 & year == "2017"]
wts <- weights[final_score > 0 & year == "2017"]

lm.no_wts.ace <- ace(mat, y_dat, mon = 0)
plot(y_dat, lm.no_wts.ace$ty)

lm.ace <- ace(mat, y_dat, wt = wts, mon = 0)
plot(y_dat, lm.ace$ty)

ytrans <- rep(0,292)
ytrans[final_score > 0 & year == "2017"] <- lm.ace$ty
ytrans[final_score == 0 | year == "2016"] <- NA
ace_trans.lm <- lm(ytrans ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic, weights = weights)
summary(ace_trans.lm)
par(mfrow = c(2,2)); plot(ace_trans.lm); par(mfrow = c(1,1))



### Year = 2016

final_score_temp <- final_score
final_score_temp[final_score == 0 | year == "2017"] <- NA
plain.lm <- lm(final_score_temp ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic)
summary(plain.lm)
par(mfrow = c(2,2))
plot(plain.lm)

x <- plain.lm$fitted
y <- abs(plain.lm$residuals)

par(mfrow = c(1, 1))
scatter.smooth(x, y, span = 1/3)

smooth <- loess(y ~ x, span = 1/3)
weights <- rep(0, 292)
weights[final_score > 0 & year == "2016"] <- predict(smooth, x)^2
weights[final_score == 0 | year == "2017"] <- NA
weighted.lm <- lm(final_score_temp ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic, weights = weights)
summary(weighted.lm)

mat <- model.matrix(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic)
mat <- mat[final_score > 0 & year == "2016", ]
y_dat <- final_score[final_score > 0 & year == "2016"]
wts <- weights[final_score > 0 & year == "2016"]

lm.no_wts.ace <- ace(mat, y_dat, mon = 0)
plot(y_dat, lm.no_wts.ace$ty)

lm.ace <- ace(mat, y_dat, wt = wts, mon = 0)
plot(y_dat, lm.ace$ty)

ytrans <- rep(0,292)
ytrans[final_score > 0 & year == "2016"] <- lm.ace$ty
ytrans[final_score == 0 | year == "2017"] <- NA
ace_trans.lm <- lm(ytrans ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic, weights = weights)
summary(ace_trans.lm)
par(mfrow = c(2,2)); plot(ace_trans.lm); par(mfrow = c(1,1))




###  Both years combined (ignore autocorrelation)

final_score_temp <- final_score
final_score_temp[final_score == 0] <- NA
plain.lm <- lm(final_score_temp ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic + year)
summary(plain.lm)
par(mfrow = c(2,2))
plot(plain.lm)

x <- plain.lm$fitted
y <- abs(plain.lm$residuals)

par(mfrow = c(1, 1))
scatter.smooth(x, y, span = 1/3)

smooth <- loess(y ~ x, span = 1/3)
weights <- rep(0, 292)
weights[final_score > 0] <- predict(smooth, x)^2
weights[final_score == 0] <- NA
weighted.lm <- lm(final_score_temp ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic + year, weights = weights)
summary(weighted.lm)

mat <- model.matrix(final_score ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic + year)
mat <- mat[final_score > 0, ]
y_dat <- final_score[final_score > 0]
wts <- weights[final_score > 0]

lm.no_wts.ace <- ace(mat, y_dat, mon = 0)
plot(y_dat, lm.no_wts.ace$ty)

lm.ace <- ace(mat, y_dat, wt = wts, mon = 0)
plot(y_dat, lm.ace$ty)

ytrans <- rep(0,292)
ytrans[final_score > 0] <- lm.ace$ty
ytrans[final_score == 0] <- NA
ace_trans.lm <- lm(ytrans ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic + year, weights = weights)
summary(ace_trans.lm)
par(mfrow = c(2,2)); plot(ace_trans.lm); par(mfrow = c(1,1))
