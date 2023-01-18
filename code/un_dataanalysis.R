#Day 2 notes 
#January 18th, 2023

library(tidyverse)
gapminder_data <- read_csv("data/gapminder_data.csv")
summarize(gapminder_data, averageLifeExp=mean(lifeExp), medianLifeExp=median(lifeExp))

#learning to pipe
gapminder_summary<-gapminder_data%>%
  summarize(averageLifeExp=mean(lifeExp))

gapminder_summary

#filtering data (for choosing rows)
gapminder_summary_2007<-gapminder_data%>%
  filter(year == 2007)%>%
  summarize(average = mean(lifeExp))

#find the average gdp per cap for the 1st year in the dataset
gapminder_data%>%
  summarize(Firstyear=min(year))
gapminder_data%>%
  filter(year == 1952)%>%
  summarize(averageGdpPerCap = mean(gdpPercap))

#using the group_by() function
gapminder_data%>%
  group_by(year, continent)%>%
  summarize(average = mean(lifeExp),
            error = sd(lifeExp))

#expanding data with the mutate function
gapminder_data%>%
  mutate(gdp = pop * gdpPercap)

#mutate a new column which is pop in millions
gapminder_data%>%
  mutate(popInMil = pop / 1000000)

#select fx (for choosing columns)
gapminder_data%>%
  select(pop, year) 

gapminder_data%>%
  select(-continent, -pop)

#converting data from wide form to long form (kinda like "paste>transpose")
#Pivot_wider()
gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp)%>%
  View()
