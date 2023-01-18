#Analyze life expectancy and CO2 emissions vs population w gapminder data
#Date: Jan 17th, 2023
#Author: Emma Roszkowski

#load in packages necessary for analysis
setwd("/Users/ekr44/Desktop/un-report/data")
getwd()
library("tidyverse")
library("readr")
library("ggprism")
library("extrafont")

#read in data for analysis
gapminder_1997 <- read_csv("gapminder_1997.csv")

#name objects
name <- "Ben"
age <- 26
name <- "Harry potter"

#read files 
read_csv()
read_csv("gapminder_1997.csv")

#other functions such as finding date, getting functions, rounding
Sys.Date()
getwd()
sum(5,6)
sum(5)
round(3.1415)
round(3.1415,3)
round(x = 3.1415)
round(x = 3.1415, digits = 2)
round(digits = 2, x = 3.1415)
round(2, 3.1415)
install.packages("readxl")
library(readxl)
read_excel("")

#plotting data for visualization (practice using plus signs)
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap) +
  labs(x="GDP Per Capita")+
  aes(y = lifeExp)+
  labs(y="Life Expecancy (yrs)")+
  geom_point()+
  labs(title = "Do people in wealthy countries live longer?")+
  aes(color = continent)+
  scale_color_brewer(palette="Set1")+
  aes(size=pop/1000000)+
  labs(size = "Population (in millions)")+
  aes(shape = continent)

#short handed ggplot
ggplot(data = gapminder_1997, 
       aes(x = gdpPercap, y = lifeExp, color = continent, shape = continent,
           size = pop)) + 
         labs(x = "GDP per Capita", y = "Life Expectancy",
              title = "Do people in wealthy countries live longer?",
              size = "Population (in millions)")+
  geom_point()

#Read in all the data from gapminder (more years than 1997!)
gapminder_data <- read_csv("gapminder_data.csv")
dim(gapminder_data)
head(gapminder_data)
tail(gapminder_data)
ggplot(data = gapminder_data) +
  aes(x=year, y=lifeExp, color=continent) +
  geom_point()

#learn about data
str(gapminder_data)

#line plots
ggplot(gapminder_data)+
  aes(x=year, y=lifeExp, color=continent, group = country) +
  geom_line()

#box plots
ggplot(gapminder_1997)+
  aes(x=continent, y=lifeExp, color = continent) +
  geom_boxplot()

#violin plots
ggplot(gapminder_1997)+
  aes(x=continent, y=lifeExp, color = continent) +
  geom_violin()+
  geom_jitter()
#could also do geom_point instead of geom_jitter and all the points would be 
#in a straight line

ggplot(gapminder_1997)+
  aes(x=continent, y=lifeExp, color = continent) +
  geom_jitter()+
  geom_violin()
# ^ now the violin covers the points rather than vice versa based on command order ^

ggplot(gapminder_1997)+
  aes(x=continent, y=lifeExp, color=continent) +
  geom_violin(fill = "pink", color="cornflowerblue")+
  geom_jitter(aes(size=pop))

#making a histogram to undersatnd range of data
ggplot(gapminder_1997)+
  aes(x=lifeExp)+
  geom_histogram(bins=20)+
  theme_prism()
install.packages("ggthemes")
install.packages("ggprism")

ggplot(gapminder_1997)+
  aes(x=lifeExp)+
  geom_histogram(bins=20)+
  theme_prism()

#making multiple plots in the same panel
ggplot(gapminder_1997)+
  aes(x=gdpPercap, y=lifeExp, color=continent)+
  geom_point()+
  facet_grid(rows=vars(continent))
ggsave("awesome_plot.jpg", width=6, height=4, units = "in")

install.packages("extrafont")

