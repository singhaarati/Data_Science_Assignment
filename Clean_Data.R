library(tidyverse)
library(dplyr)


# house price 

houseprice_2019 = read_csv("pp-2019.csv")
houseprice_2020 = read_csv("pp-2020.csv")
houseprice_2021 = read_csv("pp-2021.csv")

colnames(houseprice_2019) = c("id" , "price", "date", "postCode" , "paon", "saon", "FL", "houseNumber", "flat", "streetName",
                              "locality", "town" , "district", "county", "type1", "type2" );
colnames(houseprice_2020) = c("id" , "price", "date", "postCode" , "paon", "saon", "FL", "houseNumber", "flat", "streetName",
                              "locality", "town" , "district", "county", "type1", "type2" );
colnames(houseprice_2021) = c("id" , "price", "date", "postCode" , "paon", "saon", "FL", "houseNumber", "flat", "streetName",
                              "locality", "town" , "district", "county", "type1", "type2" );







# data from price 2019 to 2021

Houseprice = houseprice_2019 %>% 
  add_row(houseprice_2020) %>% 
  add_row(houseprice_2021)




# data clean of house price 

CleaningHouseprice = Houseprice%>%
  filter(county =="LANCASHIRE" | county=="LEICESTERSHIRE")%>%
  mutate( ID = row_number()) %>% 
  na.omit()





# to export house price cleaning

write.csv(CleaningHouseprice, "CleanHousePrice.csv")







House_Prices_clean = Houseprice %>%
  filter(county =="LANCASHIRE" | county=="LEICESTERSHIRE")  %>%
  mutate(ShortPostcode = str_trim(substring(postCode, 1,4))) %>% 
  mutate(year=substring(date,1,4)) %>% 
  arrange(county) %>% 
  select(postCode,ShortPostcode,price,year,paon) %>% 
  na.omit()







# to export file
write.csv(House_Prices_clean, "Houseprice_Clean.csv")








# mutating population with house price
population_Data = read_csv("Population2011_1656567141570.csv")

population_Data = population_Data %>%  mutate(ShortPostcode = str_trim(substring(Postcode, 1,4))) %>% 
  group_by(ShortPostcode) %>% 
  summarise_at(vars(Population),list(Population2011 = sum)) %>%  
  mutate(Population2012= (1.00695353132322269 * Population2011)) %>% 
  mutate(Population2013= (1.00669740535540783 * Population2012)) %>%
  mutate(Population2014= (1.00736463978721671 * Population2013)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2016= (1.00757874492811929 * Population2015)) %>%
  mutate(Population2017= (1.00679374473924223 * Population2016)) %>%
  mutate(Population2018= (1.00605929132212552 * Population2017)) %>%
  mutate(Population2019= (1.00561255390388033 * Population2018)) %>%
  mutate(Population2020= (1.00561255390388033 * Population2019)) %>% 
  mutate(Population2021= (1.00561255390388033 * Population2020)) %>%
  mutate(Population2022= (1.00561255390388033 * Population2021)) %>% 
  
  select(ShortPostcode,Population2018,Population2019,Population2020,Population2021,Population2022)






# create town variable

TOWN = Houseprice %>%
  filter(county =="LANCASHIRE" | county=="LEICESTERSHIRE") %>% 
  mutate(ShortPostcode = str_trim(substring(postCode, 1,4))) %>% 
  left_join(population_Data,by = "ShortPostcode") %>% 
  select(ShortPostcode,town,district,county,Population2018,Population2019,Population2020,
         Population2021,Population2022) %>% 
  group_by(ShortPostcode) %>% 
  filter(row_number()==1) %>% 
  arrange(county) %>% 
  na.omit()








TOWN=TOWN [!duplicated(TOWN$district),]


# to export TOWN

write.csv(TOWN, "TOWN.csv")


 #house price end 








#internet speed



internet_speed = read_csv("internet_speed.csv")


#eliminating N.A

internet_speed = replace(internet_speed,is.na(internet_speed), 0)



# to export internet speed
write.csv(internet_speed, "InternetSpeed.csv")




#clean data 

internet_speed = read_csv("InternetSpeed.csv")

clean_internet_speed = internet_speed %>% 
  mutate(ShortPostcode = str_trim(substring(postcode_space, 1,4))) %>% 
  group_by(ShortPostcode) %>% 
  summarise_at(vars("Average download speed (Mbit/s)","Minimum download speed (Mbit/s)","Average upload speed (Mbit/s)",
                    "Minimum upload speed (Mbit/s)"),
               list(name = mean)) %>% 
  left_join(TOWN,by="ShortPostcode") %>%  
  filter(county =="LANCASHIRE" | county=="LEICESTERSHIRE") %>% 
  arrange(county) %>% 
  select(-county,-town,-district,-Population2018,-Population2019,-Population2020,
         -Population2021,-Population2022) %>% 
  rename("AvgDownload"="Average download speed (Mbit/s)_name","MinDownload"="Minimum download speed (Mbit/s)_name",
         "AvgUpload"="Average upload speed (Mbit/s)_name","MinUpload"="Minimum upload speed (Mbit/s)_name") %>% 
  na.omit()





#to export clean internet speed
write.csv(clean_internet_speed,"clean_internet_speed.csv")



#internet speed end 





#Crime


Crime_2020_01_lancashire = read.csv('Crime_Data/2020-01/2020-01-lancashire-street.csv')
Crime_2020_01_leicestershire = read.csv('Crime_Data/2020-01/2020-01-leicestershire-street.csv')

Crime_2020_02_lancashire = read.csv('Crime_Data/2020-02/2020-02-lancashire-street.csv')
Crime_2020_02_leicestershire = read.csv('Crime_Data/2020-02/2020-02-leicestershire-street.csv')

Crime_2020_03_lancashire = read.csv('Crime_Data/2020-03/2020-03-lancashire-street.csv')
Crime_2020_03_leicestershire = read.csv('Crime_Data/2020-03/2020-03-leicestershire-street.csv')

Crime_2020_04_lancashire = read.csv('Crime_Data/2020-04/2020-04-lancashire-street.csv')
Crime_2020_04_leicestershire = read.csv('Crime_Data/2020-04/2020-04-leicestershire-street.csv')

Crime_2020_05_lancashire = read.csv('Crime_Data/2020-05/2020-05-lancashire-street.csv')
Crime_2020_05_leicestershire = read.csv('Crime_Data/2020-05/2020-05-leicestershire-street.csv')

Crime_2020_06_lancashire = read.csv('Crime_Data/2020-06/2020-06-lancashire-street.csv')
Crime_2020_06_leicestershire = read.csv('Crime_Data/2020-06/2020-06-leicestershire-street.csv')

Crime_2020_07_lancashire = read.csv('Crime_Data/2020-07/2020-07-lancashire-street.csv')
Crime_2020_07_leicestershire = read.csv('Crime_Data/2020-07/2020-07-leicestershire-street.csv')

Crime_2020_08_lancashire = read.csv('Crime_Data/2020-08/2020-08-lancashire-street.csv')
Crime_2020_08_leicestershire = read.csv('Crime_Data/2020-08/2020-08-leicestershire-street.csv')

Crime_2020_09_lancashire = read.csv('Crime_Data/2020-09/2020-09-lancashire-street.csv')
Crime_2020_09_leicestershire = read.csv('Crime_Data/2020-09/2020-09-leicestershire-street.csv')

Crime_2020_10_lancashire = read.csv('Crime_Data/2020-10/2020-10-lancashire-street.csv')
Crime_2020_10_leicestershire = read.csv('Crime_Data/2020-10/2020-10-leicestershire-street.csv')

Crime_2020_11_lancashire = read.csv('Crime_Data/2020-11/2020-11-lancashire-street.csv')
Crime_2020_11_leicestershire = read.csv('Crime_Data/2020-11/2020-11-leicestershire-street.csv')

Crime_2020_12_lancashire = read.csv('Crime_Data/2020-12/2020-12-lancashire-street.csv')
Crime_2020_12_leicestershire = read.csv('Crime_Data/2020-12/2020-12-leicestershire-street.csv')




Crime_2021_01_lancashire = read.csv('Crime_Data/2021-01/2021-01-lancashire-street.csv')
Crime_2021_01_leicestershire = read.csv('Crime_Data/2021-01/2021-01-leicestershire-street.csv')

Crime_2021_02_lancashire = read.csv('Crime_Data/2021-02/2021-02-lancashire-street.csv')
Crime_2021_02_leicestershire = read.csv('Crime_Data/2021-02/2021-02-leicestershire-street.csv')

Crime_2021_03_lancashire = read.csv('Crime_Data/2021-03/2021-03-lancashire-street.csv')
Crime_2021_03_leicestershire = read.csv('Crime_Data/2021-03/2021-03-leicestershire-street.csv')

Crime_2021_04_lancashire = read.csv('Crime_Data/2021-04/2021-04-lancashire-street.csv')
Crime_2021_04_leicestershire = read.csv('Crime_Data/2021-04/2021-04-leicestershire-street.csv')

Crime_2021_05_lancashire = read.csv('Crime_Data/2021-05/2021-05-lancashire-street.csv')
Crime_2021_05_leicestershire = read.csv('Crime_Data/2021-05/2021-05-leicestershire-street.csv')

Crime_2021_06_lancashire = read.csv('Crime_Data/2021-06/2021-06-lancashire-street.csv')
Crime_2021_06_leicestershire = read.csv('Crime_Data/2021-06/2021-06-leicestershire-street.csv')

Crime_2021_07_lancashire = read.csv('Crime_Data/2021-07/2021-07-lancashire-street.csv')
Crime_2021_07_leicestershire = read.csv('Crime_Data/2021-07/2021-07-leicestershire-street.csv')

Crime_2021_08_lancashire = read.csv('Crime_Data/2021-08/2021-08-lancashire-street.csv')
Crime_2021_08_leicestershire = read.csv('Crime_Data/2021-08/2021-08-leicestershire-street.csv')

Crime_2021_09_lancashire = read.csv('Crime_Data/2021-09/2021-09-lancashire-street.csv')
Crime_2021_09_leicestershire = read.csv('Crime_Data/2021-09/2021-09-leicestershire-street.csv')

Crime_2021_10_lancashire = read.csv('Crime_Data/2021-10/2021-10-lancashire-street.csv')
Crime_2021_10_leicestershire = read.csv('Crime_Data/2021-10/2021-10-leicestershire-street.csv')

Crime_2021_11_lancashire = read.csv('Crime_Data/2021-11/2021-11-lancashire-street.csv')
Crime_2021_11_leicestershire = read.csv('Crime_Data/2021-11/2021-11-leicestershire-street.csv')

Crime_2021_12_lancashire = read.csv('Crime_Data/2021-12/2021-12-lancashire-street.csv')
Crime_2021_12_leicestershire = read.csv('Crime_Data/2021-12/2021-12-leicestershire-street.csv')




Crime_2022_01_lancashire = read.csv('Crime_Data/2022-01/2022-01-lancashire-street.csv')
Crime_2022_01_leicestershire = read.csv('Crime_Data/2022-01/2022-01-leicestershire-street.csv')

Crime_2022_02_lancashire = read.csv('Crime_Data/2022-02/2022-02-lancashire-street.csv')
Crime_2022_02_leicestershire = read.csv('Crime_Data/2022-02/2022-02-leicestershire-street.csv')

Crime_2022_03_lancashire = read.csv('Crime_Data/2022-03/2022-03-lancashire-street.csv')
Crime_2022_03_leicestershire = read.csv('Crime_Data/2022-03/2022-03-leicestershire-street.csv')

Crime_2022_04_lancashire = read.csv('Crime_Data/2022-04/2022-04-lancashire-street.csv')
Crime_2022_04_leicestershire = read.csv('Crime_Data/2022-04/2022-04-leicestershire-street.csv')

Crime_2022_05_lancashire = read.csv('Crime_Data/2022-05/2022-05-lancashire-street.csv')
Crime_2022_05_leicestershire = read.csv('Crime_Data/2022-05/2022-05-leicestershire-street.csv')

Crime_2022_06_lancashire = read.csv('Crime_Data/2022-06/2022-06-lancashire-street.csv')
Crime_2022_06_leicestershire = read.csv('Crime_Data/2022-06/2022-06-leicestershire-street.csv')

Crime_2022_07_lancashire = read.csv('Crime_Data/2022-07/2022-07-lancashire-street.csv')
Crime_2022_07_leicestershire = read.csv('Crime_Data/2022-07/2022-07-leicestershire-street.csv')

Crime_2022_08_lancashire = read.csv('Crime_Data/2022-08/2022-08-lancashire-street.csv')
Crime_2022_08_leicestershire = read.csv('Crime_Data/2022-08/2022-08-leicestershire-street.csv')

Crime_2022_09_lancashire = read.csv('Crime_Data/2022-09/2022-09-lancashire-street.csv')
Crime_2022_09_leicestershire = read.csv('Crime_Data/2022-09/2022-09-leicestershire-street.csv')

Crime_2022_10_lancashire = read.csv('Crime_Data/2022-10/2022-10-lancashire-street.csv')
Crime_2022_10_leicestershire = read.csv('Crime_Data/2022-10/2022-10-leicestershire-street.csv')

Crimes= Crime_2020_01_lancashire %>% 
  add_row(Crime_2020_01_leicestershire) %>%
  
  add_row(Crime_2020_02_lancashire) %>%
  add_row(Crime_2020_02_leicestershire) %>%
  
  add_row(Crime_2020_03_lancashire) %>%
  add_row(Crime_2020_03_leicestershire) %>%
  
  add_row(Crime_2020_04_lancashire) %>%
  add_row(Crime_2020_04_leicestershire) %>%
  
  add_row(Crime_2020_05_lancashire) %>%
  add_row(Crime_2020_05_leicestershire) %>%
  
  add_row(Crime_2020_06_lancashire) %>%
  add_row(Crime_2020_06_leicestershire) %>%
  
  add_row(Crime_2020_07_lancashire) %>%
  add_row(Crime_2020_07_leicestershire) %>%
  
  add_row(Crime_2020_08_lancashire) %>%
  add_row(Crime_2020_08_leicestershire) %>%
  
  add_row(Crime_2020_09_lancashire) %>%
  add_row(Crime_2020_09_leicestershire) %>%
  
  add_row(Crime_2020_10_lancashire) %>%
  add_row(Crime_2020_10_leicestershire) %>%
  
  add_row(Crime_2020_11_lancashire) %>%
  add_row(Crime_2020_11_leicestershire) %>%
  
  add_row(Crime_2020_12_lancashire) %>%
  add_row(Crime_2020_12_leicestershire) %>%
  
  
  
  
  add_row(Crime_2021_01_lancashire) %>%
  add_row(Crime_2021_01_leicestershire) %>%
  
  add_row(Crime_2021_02_lancashire) %>%
  add_row(Crime_2021_02_leicestershire) %>%
  
  add_row(Crime_2021_03_lancashire) %>%
  add_row(Crime_2021_03_leicestershire) %>%
  
  add_row(Crime_2021_04_lancashire) %>%
  add_row(Crime_2021_04_leicestershire) %>%
  
  add_row(Crime_2021_05_lancashire) %>%
  add_row(Crime_2021_05_leicestershire) %>%
  
  add_row(Crime_2021_06_lancashire) %>%
  add_row(Crime_2021_06_leicestershire) %>%
  
  add_row(Crime_2021_07_lancashire) %>%
  add_row(Crime_2021_07_leicestershire) %>%
  
  add_row(Crime_2021_08_lancashire) %>%
  add_row(Crime_2021_08_leicestershire) %>%
  
  add_row(Crime_2021_09_lancashire) %>%
  add_row(Crime_2021_09_leicestershire) %>%
  
  add_row(Crime_2021_10_lancashire) %>%
  add_row(Crime_2021_10_leicestershire) %>%
  
  add_row(Crime_2021_11_lancashire) %>%
  add_row(Crime_2021_11_leicestershire) %>%
  
  add_row(Crime_2021_12_lancashire) %>%
  add_row(Crime_2021_12_leicestershire) %>%
  
  
  
  
  add_row(Crime_2022_01_lancashire) %>%
  add_row(Crime_2022_01_leicestershire) %>%
  
  add_row(Crime_2022_02_lancashire) %>%
  add_row(Crime_2022_02_leicestershire) %>%
  
  add_row(Crime_2022_03_lancashire) %>%
  add_row(Crime_2022_03_leicestershire) %>%
  
  add_row(Crime_2022_04_lancashire) %>%
  add_row(Crime_2022_04_leicestershire) %>%
  
  add_row(Crime_2022_05_lancashire) %>%
  add_row(Crime_2022_05_leicestershire) %>%
  
  add_row(Crime_2022_06_lancashire) %>%
  add_row(Crime_2022_06_leicestershire) %>%
  
  add_row(Crime_2022_07_lancashire) %>%
  add_row(Crime_2022_07_leicestershire) %>% 
  
  
  
  mutate(year=substring(Month, 1,4)) %>% 
  rename(lsoa11cd="LSOA.code",Crime_Type="Crime.type") %>% 
  select(lsoa11cd,year,Crime_Type) %>% 
  distinct()



TOWN = read_csv("TOWN.csv")
lSOA = read_csv("Postcode to LSOA.csv")

lSOA  = lSOA %>% 
  mutate(ShortPostcode = str_trim(substring(pcds, 1,4))) %>% 
  left_join(TOWN,by="ShortPostcode") %>% 
  filter(county =="LANCASHIRE" | county=="LEICESTERSHIRE")  %>%  
  group_by(lsoa11cd) %>% 
  filter(row_number()==1) %>% 
  select(lsoa11cd,ShortPostcode,town,district,county)  




#remove duplicate data  of lsoa

lSOA[!duplicated(lSOA), ]


Clean_Crime = Crimes %>% 
  left_join(lSOA,by="lsoa11cd")%>% 
  group_by(ShortPostcode,year,Crime_Type)  %>% 
  select(ShortPostcode,year,Crime_Type) %>% 
  tally()




# to export clean crime data

write.csv(Clean_Crime, "CleanCrime.csv")






#school


School_Data = read.csv('School_2021.csv')
School_Data = select(School_Data, PCODE, SCHNAME, ATT8SCR) %>% 
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()
colnames(Schooldata) = c("ID", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")






# to export clean school data



write.csv(Schooldata, "Clean_School.csv") 



#end school data
