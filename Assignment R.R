#Assignment for Economic Policy, Winter Semester 21/22


#Authors:

#Marvin Müller
#Fabian Sturmberg
#Lionel Lütgering


#Libraries --------
#install.packages("haven")
#install.packages("stargazer")
#install.packages("sandwich")
#install.packages("lmtest")
library (haven)
library(ggplot2)
library(dplyr)
library(stargazer)
library(tidyverse)
library(sandwich)
library(lmtest)

#Data Import --------
carbontax_data <- read_dta("Data/carbontax_fullsample_data.dta")
CO2_data <- read_dta("Data/CO2_Graph.dta")


#Nr. 2 ------
figure_2 <- ggplot2::ggplot(data = CO2_data,
                            mapping = aes(year)
                            )+
  geom_line(aes(y=CO2_OECD,lty = 'OECD'))+
  geom_line(aes(y=CO2_Sweden,lty='Sweden'))+
  scale_linetype('Transport Sectors')+
  coord_cartesian(xlim = c(1960,2005), ylim = c(0.0,3.0),expand = FALSE)+
  xlab("")+
  ylab("Metric tons per capita (CO2 from transport)")+
  geom_vline(xintercept = 1990, linetype = "dotted",size=1)+
  annotate("text", x = 1984, y = 0.8, label = "VAT + Carbon Tax")+
  theme(axis.line = element_line(size = 0.3,
    linetype = "solid"), axis.text.y = element_text(size = 10,
    vjust = 0.25), plot.title = element_text(family = "Helvetica"))+ 
  labs(x = NULL) + theme(plot.title = element_text(family = "serif"))

figure_2

ggsave("figure_2.pdf")

#Nr. 3 -------

carbontax_data_sample <- carbontax_data%>%
  filter(country=="Sweden" |country=="Denmark"|country=="Finland"|
           country=="Norway"|country=="Germany"|country=="France")


countries <- c("Sweden","Denmark","Finland","Norway","Germany","France")


##Data for Table of pre- and post-treatment averages------
carbontax_data_sample_pre <- carbontax_data_sample %>%
  filter(year <= 1989 & year >= 1980)

carbontax_data_sample_post <- carbontax_data_sample %>%
  filter(year > 1990 & year <=2000)

avg_table_pre <- matrix(c(1:36), ncol=6, byrow=TRUE)
colnames(avg_table_pre) <- c('Sweden','Denmark','Finland','Norway','Germany','France')
rownames(avg_table_pre) <- c('CO2 from transport','GDP per capita (in 1,000)','Gasoline consumption per capita', 
'Motor vehicles (per 1,000 people)', 'Urban population','Population density')


avg_table_post <- matrix(c(1:36), ncol=6, byrow=TRUE)
colnames(avg_table_post) <- c('Sweden','Denmark','Finland','Norway','Germany','France')
rownames(avg_table_post) <- c('CO2 from transport (in metric tons)','GDP per capita (PPP, 2005)','Gasoline consumption per capita (in kilograms of oil equivalent)', 
                             'Motor vehicles (per 1,000 people)', 'Urban population (as percentage of total population)','Population density (people per square km)')

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

#AVG's from sample data from 1980 - 1989 (pre treatment)
avg_table_pre[2,] <- avg_table_pre[2,]/1000
avg_table_pre
#AVG's from sample data from 1990 - 2000 (post treatment)
avg_table_post


##Table to show CO2 transport emissions in 3 points in time---------

table_3 <- matrix(c(1:18), ncol=6, byrow=TRUE)
colnames(table_3) <- c('Denmark','Finland','France','Germany','Norway','Sweden')
rownames(table_3) <- c('CO2 from transport (in metric tons) 1989','CO2 from transport (in metric tons) 1980',
                             'CO2 from transport (in metric tons) 1970')


CO2_1970 <- carbontax_data_sample%>%
  filter(year==1970)
CO2_1980 <- carbontax_data_sample%>%
  filter(year==1980)
CO2_1989 <- carbontax_data_sample%>%
  filter(year==1989)

table_3[3,] <- CO2_1970$CO2_transport_capita
table_3[2,] <- CO2_1980$CO2_transport_capita
table_3[1,] <- CO2_1989$CO2_transport_capita

table_3

stargazer(table_3, avg_table_pre)

#Nr. 4 -----------------

#Plot Sweden and Average of all other countries in sample

##Create Data Frame with mean of all other countries but Sweden--------

sample_df <- as.data.frame(do.call(cbind,carbontax_data_sample))
sapply(sample_df, class)
# defining the vector of columns to convert to numeric
vec <- c(3:9)
# apply the conversion on columns
sample_df[ , vec] <- apply(sample_df[ , vec,drop=F], 2,           
                            function(x) as.numeric(as.character(x)))
# indicating the data type of each variable
sapply(sample_df, class)

sample_avg <- sample_df%>%
  filter(country != "Sweden")%>%
  group_by(year)%>%
  do(mutate(., mean_CO2_transport = mean(.$CO2_transport_capita)))

#new try
sample_avg_ska <- sample_df%>%
  filter(country == "Norway" | country == "Denmark" | country == "Finland")%>%
  group_by(year)%>%
  do(mutate(., mean_CO2_transport = mean(.$CO2_transport_capita)))


swe_df <- sample_df%>%
  filter(country == "Sweden")

OECD_sample <- data.frame(year = sample_avg$year,country = rep("Sample Countries",230),CO2_transport_capita = sample_avg$mean_CO2_transport)
OECD_sample <- OECD_sample[!duplicated(OECD_sample), ]

#new try contd
ska_sample <- data.frame(year = sample_avg_ska$year,country = rep("Scandinavian",138),CO2_transport_capita = sample_avg_ska$mean_CO2_transport)
ska_sample <- ska_sample[!duplicated(ska_sample), ]


##Plot all countries' data from Sample + smooth (?) --------
task_3_plot_1 <- ggplot2::ggplot(data=carbontax_data_sample,
                                 aes(year,CO2_transport_capita,
                                     group = country,
                                     color = country))+
  geom_line()+#straight line that connects data points of each country
  geom_vline(xintercept = 1990, linetype = "dotted",size=1)+
  annotate("text", x = 1983, y = 0.9, label = "VAT + Carbon Tax")+
  coord_cartesian(xlim = c(1960,2005), ylim = c(0.0,3.0),expand = FALSE)+
  xlab("")+
  ylab("Metric tons per capita (CO2 from transport)")+
  scale_colour_discrete('Transport Sectors')+
  theme(axis.line = element_line(size = 0.3,
                                 linetype = "solid"), axis.text.y = element_text(size = 10,
                                                                                 vjust = 0.25), plot.title = element_text(family = "Helvetica"))+ 
  labs(x = NULL) + theme(plot.title = element_text(family = "serif"))

task_3_plot_1

ggsave("task_3_plot_1.pdf")

##Plot with Sweden and Rest of Sample Countries-------

task_3_plot_2 <- ggplot2::ggplot(data = OECD_sample,
                                 aes(year, CO2_transport_capita,
                                     group = country,
                                     color = country))+
  geom_line()+
  geom_line(data = swe_df, aes(year, CO2_transport_capita))+
  geom_vline(xintercept = 1990, linetype = "dotted",size=1)+
  annotate("text", x = 1982, y = 1.1, label = "VAT + Carbon Tax")+
  coord_cartesian(xlim = c(1960,2005), ylim = c(0.0,3.0),expand = FALSE)+
  xlab("")+
  ylab("Metric tons per capita (CO2 from transport)")+
  scale_colour_discrete('Transport Sectors')+
  theme(axis.line = element_line(size = 0.3,
                               linetype = "solid"), axis.text.y = element_text(size = 10,
                                                                               vjust = 0.25), plot.title = element_text(family = "Helvetica"))+ 
  labs(x = NULL) + theme(plot.title = element_text(family = "serif"))


task_3_plot_2

ggsave("task_3_plot_2.pdf")

##Plot with Sweden and Skandinavian--------

task_3_plot_3 <- ggplot2::ggplot(data = ska_sample,
                                 aes(year, CO2_transport_capita,
                                     group = country,
                                     color = country))+
  geom_line()+
  geom_line(data = swe_df, aes(year, CO2_transport_capita))+
  geom_vline(xintercept = 1990, linetype = "dotted",size=1)+
  annotate("text", x = 1982, y = 1.1, label = "VAT + Carbon Tax")+
  coord_cartesian(xlim = c(1960,2005), ylim = c(0.0,3.0),expand = FALSE)+
  xlab("")+
  ylab("Metric tons per capita (CO2 from transport)")+
  scale_colour_discrete('Transport Sectors')+
  theme(axis.line = element_line(size = 0.3,
                                 linetype = "solid"), axis.text.y = element_text(size = 10,
                                                                                 vjust = 0.25), plot.title = element_text(family = "Helvetica"))+ 
  labs(x = NULL) + theme(plot.title = element_text(family = "serif"))


task_3_plot_3

ggsave("task_3_plot_3.pdf")



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

reg1_alter <- lm(CO2_transport_capita ~ . + post_indicator*sweden_indicator - Countryno- country, data = carbontax_working)
summary(reg1_alter)
robust_coef1 <- coeftest(reg1_alter, vcov = vcovHAC(reg1_alter))
robust_coef1

reg2_sample <- carbontax_working %>%
  filter(country == "Sweden" | country == "Finland" | country == "Norway" | country == "Germany" | country == "France" | country == "Denmark")

reg2 <- lm(CO2_transport_capita ~ . + post_indicator*sweden_indicator - Countryno - country, data = reg2_sample)
summary(reg2)
robust_coef2 <- coeftest(reg2, vcov = vcovHAC(reg2)) #using robust standard errors
robust_coef2
#Treatment Dummy is highly significant but lower in value


reg3_sample <- carbontax_working %>%
  filter(country == "Sweden" | country == "Finland" | country == "Norway" | country == "Denmark")

reg3 <- lm(CO2_transport_capita ~ . + post_indicator*sweden_indicator - Countryno - country, data = reg3_sample)
summary(reg3)
robust_coef3 <- coeftest(reg3, vcov = vcovHAC(reg3))
robust_coef3
#Treatment Dummy is still significant

reg4_sample <- carbontax_working %>%
  filter(country == "Sweden" | country == "Finland" | country == "Norway" | country == "Denmark")

reg4 <- lm(CO2_transport_capita ~ year + sweden_indicator + post_indicator + post_indicator*sweden_indicator, data = reg4_sample)
summary(reg4)
robust_coef4 <- coeftest(reg4, vcov = vcovHAC(reg4))
robust_coef4
#Treatment Dummy is still significant

unique(carbontax_data$country)

##Exporting the Regressions ----------

stargazer(robust_coef1, robust_coef2, robust_coef3, robust_coef4)








