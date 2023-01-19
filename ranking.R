# Ranking house

TOWN = read_csv("TOWN.csv") %>% 
  select(ShortPostcode, town, district, county)

duplicated(TOWN$district)
TOWN=TOWN [!duplicated(TOWN$district),]


House_price = read_csv("Houseprice_Clean.csv") %>% 
  na.omit()

#house rank

Houseprice= House_price %>% 
  left_join(TOWN,by="ShortPostcode") %>% 
  na.omit()

housePrices=Houseprice  %>% 
  filter(year=="2020") %>% 
  group_by(district) %>% 
  summarise(price=mean(price)) %>% 
  arrange(price) %>% 
  mutate(HouseScore=10-(Price/120000)) %>% 
  select(district,HouseScore)
housePrices


#download rank

speed_downloads = read_csv("InternetSpeed.csv") %>% 
  na.omit()

Speed_Download = speed_downloads %>% 
  left_join(TOWN,by="ShortPostcode") %>% 
  na.omit()
colnames(speed_downloads)=c("1","ID","ShortPostcode","AvgDownload","MaxDownload","Avgupload","Maxupload")

download_speed=Speed_Download %>% 
  group_by(district) %>% 
  summarise(downloadSpeed=AvgDownload) %>% 
  arrange(-downloadSpeed) %>% 
  mutate(DownloadScore=10-(downloadSpeed/1200)) %>% 
  select(district,DownloadScore)



download_speed



#crime score rank

crime_score=read_csv("CleanCrime.csv") %>%  
  rename("CrimeCount"="n")
crime_rank = crime_score %>% 
  left_join(TOWN,by="ShortPostcode") %>% 
  na.omit()


crime_rank=crime_rank %>% 
  group_by(district) %>% 
  summarise(score=mean(CrimeCount)) %>% 
  arrange(desc(score)) %>% 
  mutate(score=10-(score/1200)) %>% 
  rename("crimescore"="score") %>% 
  select(District,crimescore) 
crime_rank


#school score

school_score=read_csv("Clean_School.csv") %>% 
  school_rank = school_score %>% 
  rename(ShortPostcode=ShortPostCode) %>% 
  left_join(TOWN,by="ShortPostcode") %>% 
  na.omit()

school_rank=school_rank %>% 
  group_by(SchoolName,district) %>% 
  summarise(score=mean(Attainment8Score)) %>% 
  arrange(score) %>% 
  mutate(score=10-(score/100)) %>% 
  select(SchoolName,score,district)
school_rank




#overall rank


RankingMerge = housePrice %>% 
  left_join(download_speed, by = "district") %>% 
  left_join(crime_rank, by = "district") %>% 
  left_join(school_rank, by = "district")


RankingMerge$SchoolName[is.na(RankingMerge$SchoolName)] <- "Not available"
RankingMerge$SchoolScore[is.na(RankingMerge$score)] <- 0

overallRank = RankingMerge %>% 
  group_by(HouseScore, score,DownloadScore,crimescore) %>%
  mutate(overallScore = (HouseScore + score + DownloadScore +crimescore)/4) %>% 
  arrange(-overallScore)

overallRank




