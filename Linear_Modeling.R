#linear modeling

TOWN = read_csv("TOWN.csv") %>% 
  select(ShortPostcode, town, district, county)
PRICES = read_csv("Houseprice_Clean.csv") %>% 
  na.omit()



SPEED = read_csv("InternetSpeed.csv") %>% 
  na.omit()  



CRIME=read_csv("CleanCrime.csv") %>% 
  rename("DrugCount"="n") %>% 
  na.omit()



SCHOOLS=read_csv("Clean_School.csv") %>% 
  na.omit()






# Correlation of House prices and Download Speed




options(scipen=999)



Houseprice = PRICES %>%
  filter(year=="2020") %>% 
  left_join(TOWN,by="ShortPostcode") %>%  
  group_by(town,county) %>%
  summarise(Price=mean(price))



internet_speed = SPEED %>% 
  left_join(TOWN,by="ShortPostcode") %>%
  group_by(town,county) %>%
  summarise(AvgDownload=mean(AvgDownload))



lm_res = Houseprice %>% left_join(internet_speed,by="town")
model = lm(data= lm_res, PRICES~AvgDownload)
summary(model)



color= c("LANCASHIRE" = "red", "LEICESTERSHIRE " = "blue")



ggplot(lm_res,aes(x=AvgDownload,y=PRICES)) +
  geom_point(data = filter(lm_res,county.x=="LEICESTERSHIRE"),aes(color="Leicestershire"))+
  geom_point(data = filter(lm_res,county.x=="LANCASHIRE"), aes(color="lancashire")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Download Speed (Mbit/s)",y="price (£)",title="House Prices vs Download Speed",color="county")


# Correlation of House price and drug offence




Houseprice = PRICES %>% 
  filter(year=="2020") %>%
  left_join(TOWN,by="ShortPostcode") %>%  
  group_by(town,county) %>%
  summarise(price=mean(price))




Drugs = CRIME %>%  
  left_join(TOWN,by="ShortPostcode") %>%
  group_by(town,county) %>%
  filter(Crime_Type=="Drugs") %>% 
  tally() %>% 
  na.omit()



colnames(Drugs)=c("town","county","DrugCount")



lm_res1 = Houseprice %>% left_join(Drugs ,by="town") %>%
  # group_by(town,county.x,price) %>% 
  na.omit()



model1 = lm(data= lm_res1, price~n)
summary(model1)



color= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")



ggplot(lm_res6,aes(x=n,y=price)) +
  geom_point(data = filter(lm_res1,county.x=="LEICESTERSHIRE"))+
  geom_point(data = filter(lm_res1,county.x=="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="count",y="price(?)",title="House Prices vs Drug",color="county")








# Correlation of house price and school

school_lm= SCHOOLS %>% 
  left_join(TOWN,by=c("ShortPostCode"="ShortPostcode") )%>%  
  group_by(town,county,district) %>%
  summarise(score=mean(Attainment8Score)) %>% 
  na.omit()



House_Prices = PRICES %>%
  filter(year=="2020") %>%
  left_join(TOWN,by="ShortPostcode") %>%  
  group_by(town,county) %>%
  summarise(price=mean(price))




lm_res4 = House_Prices %>% left_join(school_lm,by="county") %>% 
  na.omit()
model4 = lm(data= lm_res4, price~score)
summary(model4)



colors1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Pink")



ggplot(lm_res4,aes(x=score,y=price)) +
  geom_point(data = filter(lm_res4,county=="LEICESTERSHIRE"))+
  geom_point(data = filter(lm_res4,county=="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="gold")+
  labs(x="Score",y="Average house price",title="Average House Price vs Attainment at School",color="county")









# Correlation of download speed and drugs 




InternetSpeed = SPEED %>%
  left_join(TOWN,by="ShortPostcode") %>%
  group_by(town,county,district) %>%
  summarise(AvgDownload=mean(AvgDownload))




Drugs = crime %>%
  left_join(TOWN,by="ShortPostcode") %>%
  group_by(town,county) %>%
  filter(Crime_Type=="Drugs") %>% 
  na.omit()



lm_res3 = InternetSpeed %>% left_join(Drugs ,by="town") %>% 
  na.omit()
model3 = lm(data= lm_res3, AvgDownload~DrugCount)
summary(model3)



colors1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")




ggplot(lm_res3,aes(x=DrugCount,y=AvgDownload)) +
  geom_point(data = filter(lm_res3,county.x=="LEICESTERSHIRE "))+
  geom_point(data = filter(lm_res3,county.x=="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Drug count",y="Average Download",title="Corelation between download speed and Drug Offence rate",color="County")









# Correlation of average download and school

InternetSpeed = SPEEDS %>%
  left_join(TOWN,by="ShortPostcode") %>%
  group_by(town,county) %>%
  summarise(AvgDownload=mean(AvgDownload))




school_lm= SCHOOLS %>%
  left_join(TOWN,by=c("ShortPostCode"="ShortPostcode") )%>%  
  group_by(town,county,district) %>%
  summarise(score=mean(Attainment8Score)) %>% 
  na.omit()





lm_res4 = InternetSpeed %>% left_join(school_lm,by="county") %>% 
  na.omit()
model4 = lm(data= lm_res4, AvgDownload~score)
summary(model4)



colors1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")



ggplot(lm_res4,aes(x=score,y=AvgDownload)) +
  geom_point(data = filter(lm_res4,county=="LEICESTERSHIRE"))+
  geom_point(data = filter(lm_res4,county=="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Score",y="Average Download",title="score vs AverageDownload",color="County")



