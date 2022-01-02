#Assignment for Economic Policy, Winter Semester 21/22


#Authors:

#Marvin Müller
#Fabian Sturmberg
#Lionel Lütgering


#install.packages("haven")
library (haven)
library(ggplot2)
library(dplyr)
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


carbontax_data_sample <- carbontax_data%>%
  filter(country=="Sweden" |country=="Denmark"|country=="Finland"|
           country=="Norway"|country=="Germany"|country=="France")%>%
  glimpse()

task_3_plot_1 <- ggplot2::ggplot(data=carbontax_data_sample,
                                 aes(year,CO2_transport_capita,
                                     color = country))+
  geom_point()+
  geom_smooth(se = F)

task_3_plot_1



carbontax_data_sample_no_swe <- carbontax_data%>%
  filter(country=="Denmark"|country=="Finland"|
           country=="Norway"|country=="Germany"|country=="France")%>%
  glimpse()

carbontax_data_sample_swe <- carbontax_data%>%
  filter(country=="Sweden")%>%
  glimpse()


task_3_plot_2 <- ggplot2::ggplot(data=carbontax_data_sample_no_swe,
                                 aes(year,CO2_transport_capita))+
  geom_point()+
  geom_point(data=carbontax_data_sample_swe, color = "orange")
  #geom_smooth(se = F)

task_3_plot_2



