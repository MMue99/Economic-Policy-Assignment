#Assignment for Economic Policy, Winter Semester 21/22


#Authors:

#Marvin Müller
#Fabian Sturmberg
#Lionel Lütgering


#Libraries --------
#install.packages("haven")
library (haven)
library(ggplot2)
library(tidyverse)

#Data Import --------
carbontax_data <- read_dta("Data/carbontax_fullsample_data.dta")
CO2_data <- read_dta("Data/CO2_Graph.dta")


#Nr. 2 ------
figure_3 <- ggplot2::ggplot(data = CO2_data,
                            mapping = aes(year)
                            )+
  geom_line(aes(y=CO2_OECD), linetype = "dashed")+
  geom_line(aes(y=CO2_Sweden))+
  coord_cartesian(xlim = c(1960,2005), ylim = c(0.0,3.0),expand = FALSE)+
  xlab("")+
  ylab("Metric tons per capita (CO2 from transport)")+
  labs(title = "CARBON TAXES AND CO2 EMISSIONS")+
  geom_vline(xintercept = 1990, linetype = "dotted",size=1)


figure_3


#Nr. 5 -----
##Generating Indicator Variables---------

carbontax_working <- carbontax_data
carbontax_working <- carbontax_working %>%
  mutate(sweden_indicator = ifelse(country == "Sweden", 1, 0))

carbontax_working <- carbontax_working %>%
  mutate(post_indicator = ifelse(year >= 1990, 1, 0))

##Creating Regressions -----

#reg1 <- lm(CO2_transport_capita ~ . + post_indicator*sweden_indicator, data = carbontax_working)
#summary(reg1)

