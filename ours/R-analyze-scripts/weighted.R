
source('./import_data_universal.R')

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
weights <- rep(-1, 292)
weights[final_score > 0 & year == "2017"] <- predict(smooth, x)^2
weights[final_score == 0 | year == "2016"] <- NA
weighted.lm <- lm(final_score_temp ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + k30 + atlantic, weights = weights)
summary(weighted.lm)

par(mfrow = c(2,2))
plot(weighted.lm)

resid <- rep(0, 292)
resid[final_score > 0 & year == "2017"] <- weighted.lm$residuals
fit <- rep(0, 292)
fit[final_score > 0 & year == "2017"] <- weighted.lm$fitted
resid[c(21, 116, 254, 82, 120, 113)]
fit[c(21, 116, 254, 82, 120, 113)]
final_score[c(21, 116, 254, 82, 120, 113)]


## test for heteroscedasticity 
lmtest::bptest(plain.lm)
