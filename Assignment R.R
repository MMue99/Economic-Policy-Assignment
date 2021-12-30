#Assignment for Economic Policy, Winter Semester 21/22


#Authors:

#Marvin Müller
#Fabian Sturmberg
#Lionel Lütgering


#install.packages("haven")
library (haven)
library(ggplot2)
carbontax_data <- read_dta("Data/carbontax_fullsample_data.dta")
CO2_data <- read_dta("Data/CO2_Graph.dta")


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
