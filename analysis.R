# Analysis of the world happiness repot 2019
# by: Mohsin Uddin

# libraries used
library(tidyverse)
library(corrplot)
library(ggthemes)
library(lubridate)

# read data
data = read.csv(file.choose(), header = T)
colnames(data) = c("rank", "country", "score", "GDP", "socialSupp", 
                   "lifeExp", "freedom", "generosity", "corruptionP")
str(data)

# correlation
corrplot(cor(data[3:9]), method = "number", type = "lower")

# Top 10 country
data %>%
  filter(rank >= 1 & rank <= 10)%>%
  ggplot(aes(country, score, fill = country))+
  geom_bar(stat = "identity")+
  xlab("Country")+
  ylab("Happiness score")+
  ggtitle("Top 10 happy countries in the world")

# GDP vs score
ggplot(data, aes(GDP, score))+
  geom_point(alpha = 0.4, color = "blue")+
  ggtitle("GDP vs Happiness Score")+
  geom_smooth(method = "lm")

# Life expectency vs score
ggplot(data, aes(lifeExp, score))+
  geom_point(shape = 16, size = 2, color = "red")+
  ggtitle("Life expectency vs Happiness Score")+
  theme_minimal()+
  geom_smooth(method = "lm")

# Social support vs score
ggplot(data, aes(socialSupp, score))+
  geom_point(alpha = 0.7, shape = 4, color = "blue")+
  ggtitle("Social Support vs Happiness Score")+
  geom_smooth(method = "lm")

# GDP vs life expentency
ggplot(data, aes(GDP, lifeExp))+
  geom_point( color = "green")+
  theme_wsj()+
  ggtitle("GDP vs Life Expectency")+
  geom_smooth(method = "lm")

# GDP vs social support
ggplot(data, aes(GDP, socialSupp))+
  geom_point(alpha = 0.4, size = 3, color = "red")+
  theme_solarized()+
  ggtitle("GDP vs Socal Support")+
  geom_smooth(method = "lm")

# GDP vs freedom
ggplot(data, aes(GDP, freedom))+
  geom_point(shape = 18, color = "red")+
  ggtitle("GDP vs Freedom of making choice")+
  theme_economist()+
  geom_smooth(method = "lm")

# GDP vs generosity
ggplot(data, aes(GDP, generosity))+
  geom_point(shape = 16, color = "black")+
  ggtitle("GDP vs Generosity")+
  geom_smooth(method = "lm")+
  theme_few()

## Situation of Bangladesh
# dataset
d1 = data.frame(year = c("2016", "2017", "2018", "2019"), 
                rank = c(110, 110, 115, 125), 
                score = c(4.643, 4.608, 4.500, 4.456))
d1$year = ymd(d1$year, truncated = 2L)

# line plot of trend
ggplot(d1, aes(year, score))+
  geom_line(lwd = 0.75, color = "steelblue", linetype = "twodash")+
  geom_point(size = 2, color = "darkred")+
  ggtitle("Happiness score of Bangladesh")
