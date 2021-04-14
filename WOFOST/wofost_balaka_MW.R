library(Rwofost)
library(data.table)

weather <- data.table(read.csv("weather_wofost_balaka_MW.csv"))

weather$wind_ms <- sqrt(weather$windU_m.s^2 + weather$windV_m.s^2)

p1 <- weather[ weather$X == 34.825 & weather$Y == -15.325, ]

cols2delete <- c("X", "Y", "windU_m.s", "windV_m.s")
p1 <- p1[, (cols2delete) := NULL]

p1 <- setcolorder(p1, c("date", "srad_J.m2", "tmin_K", "tmax_K", "vapr_kg.m2", "wind_ms", "prec_mm"))