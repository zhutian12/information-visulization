# bus240viz2
#  data intake and wrangling for assignment #2
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggpubr)


### read the WDI data from disk--modify next lines for your R installation.
setwd("d:/Brandeis/Brandeis 2019 Spring/Information Visualization/")  
wi <- read_csv("d:/Brandeis/Brandeis 2019 Spring/Information Visualization/Assignment/Assignment2/World Indicators.csv") 
wi2 <- select(wi, c("BirthRate", "LifeExpectancy", "InfantMortalityRate", "Region", "Year"))
wi2 <- filter(wi2, Year > "12/1/2003" & Year < "12/31/2010")
glimpse(wi2)
View(wi2)
#### 
#  Need to strip out '%' from 2 of the columns

wi2$birth <- str_sub(wi2$`BirthRate`, 1, str_length(wi2$`BirthRate`)-1)
wi2$birth <- as.numeric(wi2$birth)
wi2$im <- str_sub(wi2$`InfantMortalityRate`, 1, str_length(wi2$`InfantMortalityRate`)-1)
wi2$im <- as.numeric(wi2$im)
wi2$life <- as.numeric(wi2$`LifeExpectancy`)
#### Convert Year to numeric year
wi2$Yr <- year(as.Date.character(wi2$Year, "%m/%d/%Y"))
wi2$Region <- as.factor(wi2$Region)
###############  Now summarize average rates by region
wi3 <- wi2 %>%
     group_by(Region, Yr) %>%
     summarize(birthrate = mean(birth, na.rm = T),
               life_exp = mean(life, na.rm = T), 
               infant = mean(im, na.rm = T))
View(wi3)
###########  NOW make your plots
wi.birthrate <- ggplot(wi3, aes(x = Yr, y=birthrate)) + 
  geom_line(size = 3, colour = "red1") + labs(x = "Year", y = "Birthrate%") + ggtitle("Birthrate for six regions")  + 
  theme_classic() + theme(plot.title = element_text(color = "red1", size = 10, face = "bold")) +
  theme(panel.background = element_rect(fill = "grey98", colour = "grey98")) +  
  facet_grid(cols = vars(Region)) + scale_y_continuous(breaks = c(seq(0,4,0.5)))
                     
wi.life_exp <- ggplot(wi3, aes(x = Yr, y=life_exp)) + 
  geom_line(size = 3, colour = "grey40") + labs(x = "Year", y = "Life Expectance") + ggtitle("Life expectance for six regions")  + 
  theme_classic()+ theme(panel.background = element_rect(fill = "grey98", colour = "grey98")) +  
  theme(plot.title = element_text(color = "red1", size = 10, face = "bold")) +
  scale_y_continuous(breaks = c(seq(55,80,5))) + facet_grid(cols = vars(Region))

wi.infantmortality <- ggplot(wi3, aes(x = Yr, y=infant)) + 
  geom_line(size = 3, colour = "steelblue2") + labs(x = "Year", y = "infant morality%") + ggtitle("infant mortality for six regions")  + 
  theme_classic()+ theme(panel.background = element_rect(fill = "grey98", colour = "grey98")) +  
  theme(plot.title = element_text(color = "red1", size = 10, face = "bold")) + 
  scale_y_continuous(breaks = c(seq(0.5,7.5,1))) + facet_grid(cols = vars(Region))

combination <- ggarrange(wi.life_exp, wi.birthrate, wi.infantmortality,
                         ncol = 1, nrow = 3)

annotate_figure(combination,
                top = text_grob("Human Well-Being of the World", color = "Black", face = "bold", size = 14, ))
combination
