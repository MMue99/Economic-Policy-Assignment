#Assignment for Economic Policy, Winter Semester 21/22


#Authors:

#Marvin Müller
#Fabian Sturmberg
#Lionel Lütgering


#Libraries --------
#install.packages("haven")
library (haven)
library(ggplot2)
library(dplyr)

library(tidyverse)
library(sandwich)
library(lmtest)

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


#Nr. 3 -------

carbontax_data_sample <- carbontax_data%>%
  filter(country=="Sweden" |country=="Denmark"|country=="Finland"|
           country=="Norway"|country=="Germany"|country=="France")

summary(carbontax_data_sample)
glimpse(carbontax_data_sample)

countries <- c("Sweden","Denmark","Finland","Norway","Germany","France")


carbontax_data_sample_pre <- carbontax_data_sample %>%
  filter(year <= 1989 & year >= 1980)

carbontax_data_sample_post <- carbontax_data_sample %>%
  filter(year > 1990 & year <=2000)

avg_table_pre <- matrix(c(1:36), ncol=6, byrow=TRUE)
colnames(avg_table_pre) <- c('Sweden','Denmark','Finland','Norway','Germany','France')
rownames(avg_table_pre) <- c('CO2 from transport','GDP per capita','Gasoline consumption per capita', 
                         'Motor vehicles (per 1,000 people', 'Urban population','Population density')
#avg_table_pre <- as.table(avg_table_pre)

avg_table_post <- matrix(c(1:36), ncol=6, byrow=TRUE)
colnames(avg_table_post) <- c('Sweden','Denmark','Finland','Norway','Germany','France')
rownames(avg_table_post) <- c('CO2 from transport','GDP per capita','Gasoline consumption per capita', 
                             'Motor vehicles (per 1,000 people', 'Urban population','Population density')
#avg_table_post <- as.table(avg_table_post)

#pre
swe_df_pre <- carbontax_data_sample_pre%>%
  filter(country == "Sweden")
den_df_pre <- carbontax_data_sample_pre%>%
  filter(country == "Denmark")
fin_df_pre <- carbontax_data_sample_pre%>%
  filter(country == "Finland")
nor_df_pre <- carbontax_data_sample_pre%>%
  filter(country == "Norway")
ger_df_pre <- carbontax_data_sample_pre%>%
  filter(country == "Germany")
fra_df_pre <- carbontax_data_sample_pre%>%
  filter(country == "France")

#post
swe_df_post <- carbontax_data_sample_post%>%
  filter(country == "Sweden")
den_df_post <- carbontax_data_sample_post%>%
  filter(country == "Denmark")
fin_df_post <- carbontax_data_sample_post%>%
  filter(country == "Finland")
nor_df_post <- carbontax_data_sample_post%>%
  filter(country == "Norway")
ger_df_post <- carbontax_data_sample_post%>%
  filter(country == "Germany")
fra_df_post <- carbontax_data_sample_post%>%
  filter(country == "France")

#pre
for(i in 4:9){
    avg_table_pre[i-3,1] = mean(swe_df_pre[[i]])
    avg_table_pre[i-3,2] = mean(den_df_pre[[i]])
    avg_table_pre[i-3,3] = mean(fin_df_pre[[i]])
    avg_table_pre[i-3,4] = mean(nor_df_pre[[i]])
    avg_table_pre[i-3,5] = mean(ger_df_pre[[i]])
    avg_table_pre[i-3,6] = mean(fra_df_pre[[i]])
    }

#post
for(i in 4:9){
  avg_table_post[i-3,1] = mean(swe_df_post[[i]])
  avg_table_post[i-3,2] = mean(den_df_post[[i]])
  avg_table_post[i-3,3] = mean(fin_df_post[[i]])
  avg_table_post[i-3,4] = mean(nor_df_post[[i]])
  avg_table_post[i-3,5] = mean(ger_df_post[[i]])
  avg_table_post[i-3,6] = mean(fra_df_post[[i]])
  }



task_3_plot_1 <- ggplot2::ggplot(data=carbontax_data_sample,
                                 aes(year,CO2_transport_capita,
                                     group = country,
                                     color = country))+
  geom_smooth(se=F,linetype = "dashed",size=0.15)+#dashed line that shows prediction of regression for each country
  geom_line()+#straight line that connects data points of each country
  geom_vline(xintercept = 1990, linetype = "dotted",size=0.2)#Sweden Treatment

task_3_plot_1




#Nr. 5 -----
##Generating Indicator Variables---------

carbontax_working <- carbontax_data
carbontax_working <- carbontax_working %>%
  mutate(sweden_indicator = ifelse(country == "Sweden", 1, 0))

carbontax_working <- carbontax_working %>%
  mutate(post_indicator = ifelse(year >= 1990, 1, 0))

##Creating Regressions -----

reg1 <- lm(CO2_transport_capita ~ . + post_indicator*sweden_indicator - Countryno, data = carbontax_working)
summary(reg1) #using non robust standard errors
coeftest(reg1, vcov = vcovHAC(reg1)) #using heteroskedasticity and autocorrelation-consistent standard errors
#with robust standard errors variables are a lot less significant overall
#Treatment Dummy is highly significant
#sweden_indicator is NA because there are too many dummy variables


reg2_sample <- carbontax_working %>%
  filter(country == "Sweden" | country == "Finland" | country == "Norway" | country == "Germany" | country == "France" | country == "Denmark")

reg2 <- lm(CO2_transport_capita ~ . + post_indicator*sweden_indicator - Countryno, data = reg2_sample)
summary(reg2)
coeftest(reg2, vcov = vcovHAC(reg2)) #using robust standard errors
#Treatment Dummy is highly significant but lower in value


reg3_sample <- carbontax_working %>%
  filter(country == "Sweden" | country == "Finland" | country == "Norway" | country == "Denmark")

reg3 <- lm(CO2_transport_capita ~ . + post_indicator*sweden_indicator - Countryno, data = reg3_sample)
summary(reg3)
coeftest(reg3, vcov = vcovHAC(reg3))
#Treatment Dummy is still significant


reg4_sample <- carbontax_working %>%
  filter(country == "Sweden" | country == "Finland" | country == "Norway" | country == "Denmark")

reg4 <- lm(CO2_transport_capita ~ country + year + sweden_indicator + post_indicator + post_indicator*sweden_indicator, data = reg4_sample)
summary(reg4)
coeftest(reg4, vcov = vcovHAC(reg4))
#Treatment Dummy is still significant
