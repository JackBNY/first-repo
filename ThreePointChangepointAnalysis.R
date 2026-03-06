#Project One: Three Point Shot Change Point Analysis

#Load Packages
library(changepoint)
library(dplyr)
library(ggplot2)

#Read in the Data
df <- ThreePointAttemptsByYearAll

#Vizualize the Data
ggplot(df, aes(Season, ThreePerGame)) +
  geom_line()

#Changepoint Analysis
cp_mean <- cpt.mean(df$ThreePerGame, method = "PELT")
summary(cp_mean)

plot(cp_mean)

df$cp <- NA
df$cp[cpts(cp_mean)] <- df$ThreePerGame[cpts(cp_mean)]

ggplot(df, aes(x = Season, y = ThreePerGame)) +
  geom_line() +
  geom_vline(xintercept = df$Season[cpts(cp_mean)],
             linetype = "dashed",
             color = "red") +
  theme_minimal()

ChangeYears <- df$Season[cpts(cp_mean)]

#What I found is that the years 1988, 1994, 2006, 2013, 2016, 2018 and 2021 were
#all important in terms of the three-point evolution in the NBA. 



