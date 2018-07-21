setwd("C:/Users/LIHI/Desktop/hadar/GG/2018b/seminar/RS")
data=read.csv("C:/Users/LIHI/Desktop/hadar/GG/2018b/seminar/RS/data.csv",stringsAsFactors = FALSE)
years=read.csv("C:/Users/LIHI/Desktop/hadar/GG/2018b/seminar/RS/years.csv",stringsAsFactors = FALSE)
citiesjoingis=read.csv("C:/Users/LIHI/Desktop/hadar/GG/2018b/seminar/RS/citiesjoingis.csv",stringsAsFactors = FALSE)
datagis=read.csv("C:/Users/LIHI/Desktop/hadar/GG/2018b/seminar/RS/datagis.csv",stringsAsFactors = FALSE)

library(ggplot2)
library(plotly)
library(crosstalk)
library(dplyr)
library(plyr)
library(tidyverse)
# Change column type to date
data$date= as.Date(data$date, format= "%d/%m/%Y", sep="-")
# Create a new column - Month
data$month= as.numeric(format(data$תאריך.הזמנה, "%m"))
# Create a new column - Season
data$season[data$month %in% c(12, 1:2)]= "winter"
data$season[data$month %in% c(3:5)]= "spring"
data$season[data$month %in% c(6:8)]= "summer"
data$season[data$month %in% c(9:11)]= "fall"
# Create a new column - year
data$year= as.numeric(format(data$תאריך.הזמנה, "%Y"))
#Delete unnecessary columns
data =data[,-14:-23]
data= data[,-4]                           
#Save the file
write.csv(data, "data.csv", row.names = FALSE)

# Count all cases in each year
years= data %>%
  count(year)
# save as data frame
as.data.frame(years)
write.csv(years, "years.csv", row.names = FALSE)


# Count all cases in each city
cities.count= data%>%
  count(city)
# save as data frame
as.data.frame(cities.count)
write.csv(cities.count, "cities.count.csv", row.names = FALSE)
#read citiesS
CitiesS=read.csv("C:/Users/LIHI/Desktop/hadar/GG/2018b/seminar/RS/CitiesS.csv",stringsAsFactors = FALSE)
#rename to CITY before join
citiess=rename(CitiesS,city=CITY)
#join cities with cities.count
citiesjoin= left_join(citiess,cities.count,by= "city")
#save the table
write.csv(citiesjoin, "citiesjoin.csv", row.names = FALSE)

#sorologic >160
sorolbig =data[data$IgG.Value >159,]
# count all sorolbig by city
sorolbig_city= sorolbig%>%
  count(city)
#count sorolbig by year
sorolbig_year=sorolbig%>%
  count(year)
sorolbig_year=rename(sorolbig_year,number_of_patient=n)
#remove 2018
sorolbig_year=sorolbig_year[-5,]

#sorologic<160
sorolles160= data[data$IgG.Value<159,]
# count all sorolles160 by city
sorolles160_city= sorolles160%>%
  count(city)
#count sorolles160 by year
sorolles160_year=sorolles160%>%
  count(year)
#remove 2018
sorolles160_year=sorolles160_year[-5,]
#rename
sorolles160_year=rename(sorolles160_year,number_of_patient=n)
#positive culture
posit_cultu=data[data$Positive.culture=="1",]
# count all posit_cultu by city
posit_cultu_city= posit_cultu%>%
  count(city)
#count posit_cultu by year
posit_cultu_year=posit_cultu%>%
  count(year)

# sorologic>160 and positive culture
sorolbig_posicoltu=data[data$IgG.Value>159 & data$Positive.culture=="1",]
#count sorolbig_posicoltu by city
sorolbig_posicoltu_city= sorolbig_posicoltu%>%
  count(city)
#count sorolbig_posicoltu by year
sorolbig_posicoltu_year= sorolbig_posicoltu%>%
  count(year)
#
# sorologic>160 or positive culture
Bacteria_exposed=data[data$IgG.Value>159 | data$Positive.culture=="1",]
#count sorolbig_posicoltu by city
Bacteria_exposed_city= Bacteria_exposed%>%
  count(city)
#
lm1=lm(datagis$log_normalised~datagis$distance_cluster+datagis$clinic+datagis$Socioeconomic_cluster)
summary(lm1)
#

# Count all cases in each socioeconomic cluster
socioeconomic=datagis%>%
  count(Socioeconomic_cluster)
socioeconomic=rename(socioeconomic,count=nn)
#plotly of count patient by socioeconomic cluster
plot_ly(socioeconomic, x = ~Socioeconomic_cluster,y= ~count,
        color = ~Socioeconomic_cluster,legendgroup = ~Socioeconomic_cluster) %>%
  add_bars()%>%
  layout(title="Number of patients by socioeconomic cluster",
         xaxis=list(type="category",title="socioeconomic cluster",range=c(0,9)),
         yaxis=list(title="number of patient",range=c(0,30)))

#count by month in every year
month=data%>%
  count(month,year)
# graph of count patient by month in year
plot_ly(month, x = ~year,y=~n, color = ~month,legendgroup=~month) %>%
  add_bars()%>%
  layout(yaxis=list(title="number of patient"),
         title="number of patient by month in each year")

#
datagis=rename(datagis,number_of_patient=n)
ggplot(data=datagis, aes(x=pop_cluster, y=number_of_patient, fill=as.factor(pop_cluster))) +
  geom_bar(colour="black", stat="identity")+ 
  scale_fill_discrete(labels=c("223-1836", "1837-3449","3450-5062","5063-17000","17001-27000","27001-65000","65001-205810"),
                      name="population cluster",
                      breaks=c("1","2","3","4","5","6","7"))+
  ggtitle("number of patient by population cluster")
#
ggplot(data=sorolbig_year, aes(x=year, y=number_of_patient, fill=year)) +
  geom_bar(colour="black", stat="identity")+ 
  ggtitle("Number of patients each year with \na diagnosis of over 160 antibodies")
#
ggplot(data=sorolles160_year, aes(x=year, y=number_of_patient, fill=year)) +
  geom_bar(colour="black", stat="identity")+ 
  ggtitle("Number of patients each year with \na diagnosis of les 160 antibodies")
#
ggplot(data=posit_cultu_year, aes(x=year, y=number_of_patient, fill=year)) +
  geom_bar(colour="black", stat="identity")+ 
  ggtitle("The number of patients each year \nwith a positive culture diagnosis")
#
ggplot(data=sorolbig_posicoltu_year, aes(x=year, y=number_of_patient, fill=year)) +
  geom_bar(colour="black", stat="identity")+ 
  ggtitle("The number of patients each year \nwith a positive culture diagnosis \nand over 160 antibodies")
#count all cases in each year in every city
j=data%>%count(city,year)
#make 4 coloumns for each year
spread(j, year, n)
#delete colomn 2018- too many NA
j=j[,-6]
#
datagis=left_join(datagis,j,by="city")
#
lm2=lm(datagis$positcultu~datagis$sorolloes160+datagis$sorolbig_positcultu)
summary(lm2)
