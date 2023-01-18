#Day 2 notes 
#January 18th, 2023

library("tidyverse")
library("readr")
library("ggprism")
library("extrafont")
library("ggpubr")
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



#challenge 
gapminder_data%>%
  filter(year == 2007)%>%
  filter("Americas")%>%
  select(-year, -continent)



#working w messy data
read_csv("data/co2-un-data.csv", skip = 1)
#renaming columns/pivoting to make new columns
co2_emissions_dirty<-read_csv("data/co2-un-data.csv", skip =2, 
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))
co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "ttl_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_cap_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)

#combining co2 data and pop data
# 1.look for common column titles across the two tables (country, year)
# 2. filter co2 data for only 2005, get rid of the year column, and filter the pop data table for 2007 and we will combine them
co2_emissions_dirty<-read_csv("data/co2-un-data.csv", skip =2, 
                              col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions<-co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "ttl_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_cap_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)%>%
  filter(year == 2005)%>%
  select(-year)

gapminder_data_2007<-read_csv("data/gapminder_data.csv")%>%
  filter(year == 2007)%>%
  select(country, pop, lifeExp, gdpPercap)

#joining 2 data frames together (using column that they have in common)

inner_join(co2_emissions, gapminder_data_2007, by = "country")

anti_join(gapminder_data_2007, co2_emissions, by = "country")

full_join(co2_emissions, gapminder_data_2007)%>%View()

co2_emissions%>%
  left_join(gapminder_data_2007)

joined_co2_pop<-inner_join(co2_emissions, gapminder_data_2007)         

#Writing an object to a CSV
write_csv(joined_co2_pop, file = "data/joined_co2_pop.csv")

joined_co2_pop<-read_csv("data/joined_co2_pop.csv")

View(joined_co2_pop)
ggplot(joined_co2_pop)+
  aes(x = gdpPercap)+
  labs(x = "GDP per Capita", y = "Count")+
  geom_histogram()+
  theme_prism()

ggplot(joined_co2_pop)+
  aes(x = lifeExp)+
  labs(x="Life Expectancy (Yrs)", y = "Count")+
  geom_histogram()+
  theme_prism()

ggplot(joined_co2_pop)+
  aes(x = per_cap_emissions)+
  labs(x="CO2 emissions per capita", y = "Count")+
  geom_histogram()+
  theme_prism()

gdp_co2_plote<-joined_co2_pop%>%
  ggplot(aes(x = gdpPercap, y = per_cap_emissions))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "GDP per capita", y = "CO2 emissions per capita", title = "CO2 emissions per capita vs GDP per capita")+
  theme_prism()+
  ggpubr::stat_regline_equation(aes(label = after_stat(rr.label)))
ggsave(gdp_co2_plote, file = "figures/gdp_co2_plot.png", height = 4, width = 6, units = "in", dpi = 300)  
