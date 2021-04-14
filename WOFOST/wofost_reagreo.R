# Example C&P from: https://reagro.org/methods/explanatory/wofost/example.html 
library(Rwofost)

f <- system.file("extdata/Netherlands_Swifterbant.csv", package="meteor")

wth <- read.csv(f)
wth$date <- as.Date(wth$date)
head(wth)

crop <- wofost_crop("barley")

soil <- wofost_soil('ec1')

contr <- wofost_control()
contr$modelstart <- as.Date("1976-02-06")

out <- wofost(crop, wth, soil, contr)

head(out)

plot(out[,'step'], out[, 'LAI'])