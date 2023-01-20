#Houseprice

#Average house price graph joining town and houseprice_clean

Houseprice_Clean=read_csv("Houseprice_Clean.csv")

Houseprice_Clean=Houseprice_Clean %>% 
  left_join(TOWN, by ="ShortPostcode") %>% 
  na.omit()






#house 

House_town = Houseprice %>%
  filter(county =="LANCASHIRE" | county=="LEICESTERSHIRE") %>% 
  group_by(town,district,county) %>% 
  summarise(Average_Price= mean(price)) %>% 
  ungroup(town,district,county) %>%
  arrange(county) %>% 
  na.omit()




#Average house prices by district (2019-2021)  BOXPLOT 

House_town %>% 
  group_by(district) %>% 
  ggplot(aes(x = district, y = Average_Price)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title=" Average house prices 2019-2021 by district")





# Average house prices by district (2019-2021)  BOXPLOT

Houseprice_Clean %>% 
  group_by(district) %>% 
  ggplot(aes(x = district, y = price)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="Average house prices 2019-2021 by district")










#LINEGRAPH Average house prices by year (2019-2021)


Houseprice_Clean  %>% 
  group_by(year) %>% 
  summarise(Average_Price = mean(price)) %>% 
  ggplot(aes(x = year, y = Average_Price)) +
  geom_line(size = 1.5, 
            color = "red") +
  
  scale_x_continuous(breaks = 2019:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "2019-2021 Average house prices by year")








# BOXPLOT Average house prices by district (2021)

Houseprice_Clean %>% 
  filter(year == 2021) %>% 
  group_by(district) %>% 
  ggplot(aes(x = district, y = price)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title=" Average house price 2021 by district")




#  house prices by district (2021) BARGRAPH


Houseprice_Clean %>% 
  filter(year == 2021) %>% 
  group_by(district) %>% 
  summarise(Average_Price = mean(price)) %>% 
  ggplot(aes(x = district, y = Average_Price)) +
  geom_bar(position = "stack",stat = "identity", fill = "blue") +
  
  labs(title = "Average house price 2021 by district") +
  coord_flip()




options(scipen = 10000)



#house price end





# internet speed graph


TOWN = read_csv("TOWN.csv") %>% 
  select(ShortPostcode, town, district, county)

TOWN=TOWN [!duplicated(TOWN$district),]

clean_internet_speed=read_csv("clean_internet_speed.csv")


internet=clean_internet_speed %>% 
  
  left_join(TOWN, by = "ShortPostcode") %>% 
  select(ShortPostcode,AvgDownload,MinDownload,AvgUpload,MinUpload,town, district, county ) %>% 
  na.omit()


internet <- internet[!duplicated(internet), ]





# LANCASHIRE Internet Speeds

internet %>% 
  filter(county == "LANCASHIRE") %>% 
  ggplot(aes(y=town)) + 
  labs(title = "Average and Minimum download internet speed of Lancashire", x = "Intenet Speed (Mbit/s)", y = "TOWN") + 
  geom_bar(aes(x=AvgDownload, fill = "Average"), stat = "Identity") + 
  geom_bar(aes(x=MinDownload, fill = "Minimum"), stat = "Identity") + 
  guides(fill=guide_legend("Download Speeds"))







# LEICESTERSHIRE Internet Speeds

internet %>% 
  filter(county == "LEICESTERSHIRE") %>% 
  ggplot(aes(y=town)) + 
  labs(title = "Average and Minimum download internet speed of Leicestershire", x = "Intenet Speed (Mbit/s)", y = "TOWN") + 
  geom_bar(aes(x=AvgDownload, fill = "Average"), stat = "Identity") + 
  geom_bar(aes(x=MinDownload, fill = "Minimum"), stat = "Identity") + 
  guides(fill=guide_legend("Download Speeds"))







# Average download internet speed of both county
ggplot(internet,aes(x=AvgDownload,y=district))+
  geom_boxplot(outlier.colour="green")+
  scale_x_continuous(limits=c(25,70), breaks=seq(25,70,10)) +
  labs(y="district",x="Speeds (Mbits/s)",title="Average download internet speed of lancashire and leicestershire")



# internet speed end 


# Graph for crime


Crime_Data = read_csv("cleanCrime.csv")



crime_Data = Crime_Data %>% 
  left_join(TOWN, by = "ShortPostcode") %>% 
  na.omit()





# Robbery by District for 2021 pie chart

Robbery_Data = crime_Data   %>% 
  filter(Crime_Type=="Robbery", year == 2021) %>% 
  group_by(town) %>%
  mutate(sumCount = sum(n)) %>%+ 
  ungroup() %>%
  mutate(perc =sumCount / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(town, sumCount, perc, labels) %>% 
  select(town, sumCount, perc, labels)


Robbery_Data %>% 
  ggplot(aes(x = "", y = perc, fill =town)) +
  geom_col(color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = .5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  theme_void()+
  labs(title="Robbery by Town 2021")


new_row = c("town" = "min", "sumCount" = 3, "perc" = 0.005405405,"labels"=0.54)
new_row1 = c("town" = "max", "sumCount" = 307, "perc" = 	
               0.553153153153153,"labels"=55.32)

data_robbery<-rbind(Robbery_Data,new_row,new_row1) 
select(sumCount,labels,perc)

df_robbery<-data_robbery[c("max","min","WIGAN"),]
radarchart(data_robbery)




data2 <- data_robbery  # Duplicate example data

data2 <- tibble::rownames_to_column(data_robbery, "town") # Apply rownames_to_column
data2  


# Graph for crime end

#school

# Average Attainment8Score by year line graph

schoolData %>% 
  group_by(year) %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = year, y = AverageAttainment)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = AverageAttainment), 
            vjust = -0.85) +
  scale_x_continuous(breaks = 2017:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "Average Attainment8Score by year")




# Leicestershire year 2017-2021 box plot

schoolData %>% 
  filter(county == "LEICESTERSHIRE") %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2017-2021 Attainment8Score of Schools")




# Lancashire year 2017-2021 box plot

schoolData %>% 
  filter(county == "LANCASHIRE") %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2017-2021 Attainment8Score of Schools")

#school end
