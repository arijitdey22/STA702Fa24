rm(list = ls())
load("01 Monsoon_rain_imd.Rdata")

## =============================================================================
## Linear regression with time (all pixel combined)

annual.rainfall <- rowMeans(all.data)

df.lm <- data.frame(rainfall = annual.rainfall,
                    t.1 = (1:122)^1, t.2 = (1:122)^2, t.3 = (1:122)^3, t.4 = (1:122)^4,
                    t.5 = (1:122)^5, t.6 = (1:122)^6, t.7 = (1:122)^7, t.8 = (1:122)^8,
                    t.9 = (1:122)^9, t.10 = (1:122)^10)

lm.t.1 <- lm(data = df.lm, rainfall ~ t.1)
lm.t.2 <- lm(data = df.lm, formula = rainfall ~ t.1 + t.2)
lm.t.3 <- lm(data = df.lm, rainfall ~ t.1 + t.2 + t.3)
lm.t.5 <- lm(data = df.lm, rainfall ~ t.1 + t.2 + t.3 + t.4 + t.5)
lm.t.10 <- lm(data = df.lm, rainfall ~ .)

library(ggplot2)

ggplot() + 
  geom_line(aes(x = 1901:2022, annual.rainfall, col = "Data"), lwd = 0.8) +
  geom_line(aes(x = 1901:2022, lm.t.1$fitted.values, col = "F.1"), lwd = 0.8) +
  geom_line(aes(x = 1901:2022, lm.t.2$fitted.values, col = "F.2"), lwd = 0.8) +
  geom_line(aes(x = 1901:2022, lm.t.3$fitted.values, col = "F.3"), lwd = 0.8) +
  geom_line(aes(x = 1901:2022, lm.t.5$fitted.values, col = "F.5"), lwd = 0.8) +
  geom_line(aes(x = 1901:2022, lm.t.10$fitted.values, col = "F.10"), lwd = 0.8) +
  labs(x = "Year", y = "Rainfall", col = "",
       caption = "F.i is the fitted regression line with time variable taken up to i-th order") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 14),
        plot.caption = element_text(hjust = 0.5, size = 17),
        legend.position = "bottom")

## =============================================================================
## Basis Spline Regression on time

library(splines)

set.seed(123)

years <- 1901:2022
annual.rainfall <- rowMeans(all.data)
rainfall.df <- data.frame(Year = years, Rainfall = annual.rainfall)

knots <- seq(1900, 2020, 10)
rainfall.df$Spline  <- bs(rainfall.df$Year, knots = knots, degree = 3, intercept = TRUE)

sp.model <- lm(Rainfall ~ Spline, data = rainfall.df)

Predicted.Rainfall <- predict(sp.model, newdata = rainfall.df)

ggplot() + 
  geom_line(aes(x = 1901:2022, annual.rainfall, col = "Ovserved Data"), lwd = 0.8) +
  geom_line(aes(x = 1901:2022, Predicted.Rainfall, col = "Fitted Line"), lwd = 0.8) +
  labs(x = "Year", y = "Rainfall", color = "") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.position = "bottom",
        legend.text = element_text(size = 14))
