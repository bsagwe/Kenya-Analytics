### Project: Input Drops 
### This script will be used calcuate the units of inputs at diffrent given points of the season
### Written by: Benjamin Sagwe
### Written In: January 2020
# install.packages("dplyr")
# install.packages("readr")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("tidyverse")
# 

# clear environment and screen
rm(list = ls())
cat("\014")

# load libraries
suppressMessages(library(dplyr))
suppressMessages(library(readr))
suppressMessages(library(stringr))
suppressMessages(library(tidyr))
suppressMessages(library(tidyverse))

#set Path
setwd("G:/My Drive/Analytics/LR2020 Requests/LR2020 Input Drops")
list.files()
scd20 = read_csv("Season Clients Detailed_20200220-104413.csv")


HomaDistricts = c("Bungoma","Webuye","Kimilili","Awendo","Migori","Ndhiwa","Rongo","Kabondo"
                  ,"Rachuonyo","Green Shamba","Borabu","Suneka","Masaba","Sotik","Lugari","Baharini"
                  ,"Cherangany","Kabiyet","Keiyo","Ndalu")
# clean trial product summary
Maize_Crop = scd20 %>%
  group_by(GlobalClientID) %>% 
  summarize(
    RegionName = first(RegionName)
    ,DistrictName = first(DistrictName)
    ,SiteName = first(SiteName)
    ,FirstName = first(FirstName)
    ,LastName = first(LastName)
    ,`Core Maize` = `Core Maize acres`
    ,`Maize Urea`=`Maize Urea Option acres`
    ,`Dryland Maize` = `Dryland Maize acres` 
    ,`DK 8031 JiT Maize acres` = `DK 8031 JiT Maize acres`
    ,`DK 8033 JiT Maize acres` = `DK 8033 JiT Maize acres`
    ,`DK 90-89 JiT Maize acres` = `DK 90-89 JiT Maize acres`
    ,`H 614D JiT Maize acres` = `H 614D JiT Maize acres`
    ,`H 6213 JiT Maize acres` = `H 6213 JiT Maize acres`
    ,`H 6218 JiT Maize acres` = `H 6218 JiT Maize acres`
    ,`PAN 691 JiT Maize acres` = `PAN 691 JiT Maize acres`
    ,`Pioneer P3812W JiT Maize acres` = `Pioneer P3812W JiT Maize acres`
    ,`SC DUMA 43 JiT Maize acres` = `SC DUMA 43 JiT Maize acres`
    ,`SC PUNDA MILIA 53 JiT Maize acres` = `SC PUNDA MILIA 53 JiT Maize acres`
    ,`Sungura 301 JiT Maize acres`  = `Sungura 301 JiT Maize acres`
    ,`WH 505 JiT Maize acres` = `WH 505 JiT Maize acres`
    , `Beans` = `Beans acres`
    ,`KMR13 High Zinc Beans` = `KMR13 High-Zinc Beans acres`
    ,`KMR11 High Zinc Beans`=`KMR11 High-Zinc Beans acres`
    ,`KK8 Beans`=`KK8 acres`
    ,`Green Grams` = `Green Gram Monocrop acres`
    ,Sorghum = `Sorghum acres`  
    ,`Banana Fertilizer` = `Banana Fertilizer qty`
    ,`Coffee Fertilizer` = `Coffee Fertilizer qty`  
    ,`Garden Fruit Fertilizer` = `Garden Fruit Fertilizer qty`
    ,`Leafy Veg Fertilizer` = `Leafy Veg Fertilizer qty`
    ,`Extra CAN`=`Extra CAN acres`
    ,`Extra Urea`=`Extra Urea acres`
    ,`Soil Improver`=`Soil Improver acres`)%>% ungroup() %>% mutate(ARMLime = if_else(!DistrictName %in% HomaDistricts, `Soil Improver`,0),
                          HomaLime = if_else(DistrictName %in% HomaDistricts, `Soil Improver`,0))





Maize_Crop$AllMaize = Maize_Crop$`Core Maize`+ Maize_Crop$`Maize Urea`+ Maize_Crop$`Dryland Maize`
Maize_Crop$JiTMaize= Maize_Crop$`DK 8031 JiT Maize acres`+ Maize_Crop$`DK 8033 JiT Maize acres`+ Maize_Crop$`DK 90-89 JiT Maize acres`+
  Maize_Crop$`H 614D JiT Maize acres` +Maize_Crop$`H 6213 JiT Maize acres` +Maize_Crop$`H 6218 JiT Maize acres` +Maize_Crop$`PAN 691 JiT Maize acres`+
  Maize_Crop$`Pioneer P3812W JiT Maize acres` +Maize_Crop$`SC DUMA 43 JiT Maize acres` +Maize_Crop$`SC PUNDA MILIA 53 JiT Maize acres`+
  Maize_Crop$`Sungura 301 JiT Maize acres` +Maize_Crop$`WH 505 JiT Maize acres` 
#Re-organizing columns
Maize_Crop = Maize_Crop %>%
  group_by(GlobalClientID) %>% select(GlobalClientID,RegionName,DistrictName,SiteName,FirstName,LastName,AllMaize,JiTMaize,`Core Maize`,`Maize Urea`,`Dryland Maize`
               ,`DK 8031 JiT Maize acres`,`DK 8033 JiT Maize acres`,`DK 90-89 JiT Maize acres`
               ,`H 614D JiT Maize acres`,`H 6213 JiT Maize acres`,`H 6218 JiT Maize acres`,`PAN 691 JiT Maize acres`
               ,`Pioneer P3812W JiT Maize acres`,`SC DUMA 43 JiT Maize acres`,`SC PUNDA MILIA 53 JiT Maize acres`
               ,`Sungura 301 JiT Maize acres`,`WH 505 JiT Maize acres`
               , `Beans`,`KMR13 High Zinc Beans`,`KMR11 High Zinc Beans`,`KK8 Beans`,`Green Grams`,Sorghum,ARMLime,HomaLime)
                

names(Maize_Crop)
#Introducing 12.5kg bags column 
Maize_Crop$Maize12.5kg <- if_else(Maize_Crop$AllMaize==0.25,1,
                               if_else(Maize_Crop$AllMaize==0.75,1,
                                       if_else(Maize_Crop$AllMaize==1.25,1,
                                               if_else(Maize_Crop$AllMaize==1.75,1,
                                                       if_else(Maize_Crop$AllMaize==2.25,1,
                                                               if_else(Maize_Crop$AllMaize==2.75,1,
                                                                       if_else(Maize_Crop$AllMaize==3.25,1,0)))))))
Maize_Crop$Maize25kg <- if_else(Maize_Crop$AllMaize==0.5,1,
                               if_else(Maize_Crop$AllMaize==0.75,1,
                                       if_else(Maize_Crop$AllMaize==1.5,1,
                                               if_else(Maize_Crop$AllMaize==1.75,1,
                                                       if_else(Maize_Crop$AllMaize==2.5,1,
                                                               if_else(Maize_Crop$AllMaize==2.75,1,
                                                                       if_else(Maize_Crop$AllMaize==3.5,1,0)))))))
Maize_Crop$Maize50kg <- if_else(Maize_Crop$AllMaize==1,1,
                               if_else(Maize_Crop$AllMaize==1.25,1,
                                       if_else(Maize_Crop$AllMaize==1.5,1,
                                               if_else(Maize_Crop$AllMaize==1.75,1,
                                                       if_else(Maize_Crop$AllMaize==2,2,
                                                               if_else(Maize_Crop$AllMaize==2.25,2,
                                                                       if_else(Maize_Crop$AllMaize==2.5,2,
                                                                               if_else(Maize_Crop$AllMaize==2.75,2,
                                                                                       if_else(Maize_Crop$AllMaize==3,3,
                                                                                               if_else(Maize_Crop$AllMaize==3.5,3,0))))))))))

#JiTMaize
Maize_Crop$JiTDAP12.5kg <- if_else(Maize_Crop$JiTMaize==0.25,1,
                                  if_else(Maize_Crop$JiTMaize==0.75,1,
                                          if_else(Maize_Crop$JiTMaize==1.25,1,
                                                  if_else(Maize_Crop$JiTMaize==1.75,1,
                                                          if_else(Maize_Crop$JiTMaize==2.25,1,
                                                                  if_else(Maize_Crop$JiTMaize==2.75,1,
                                                                          if_else(Maize_Crop$JiTMaize==3.25,1,0)))))))
Maize_Crop$JiTDAP25kg <- if_else(Maize_Crop$JiTMaize==0.5,1,
                                if_else(Maize_Crop$JiTMaize==0.75,1,
                                        if_else(Maize_Crop$JiTMaize==1.5,1,
                                                if_else(Maize_Crop$JiTMaize==1.75,1,
                                                        if_else(Maize_Crop$JiTMaize==2.5,1,
                                                                if_else(Maize_Crop$JiTMaize==2.75,1,
                                                                        if_else(Maize_Crop$JiTMaize==3.5,1,0)))))))
Maize_Crop$JiTDAP50kg <- if_else(Maize_Crop$JiTMaize==1,1,
                                if_else(Maize_Crop$JiTMaize==1.25,1,
                                        if_else(Maize_Crop$JiTMaize==1.5,1,
                                                if_else(Maize_Crop$JiTMaize==1.75,1,
                                                        if_else(Maize_Crop$JiTMaize==2,2,
                                                                if_else(Maize_Crop$JiTMaize==2.25,2,
                                                                        if_else(Maize_Crop$JiTMaize==2.5,2,
                                                                                if_else(Maize_Crop$JiTMaize==2.75,2,
                                                                                        if_else(Maize_Crop$JiTMaize==3,3,
                                                                                                if_else(Maize_Crop$JiTMaize==3.5,3,0))))))))))
#JiTCAN
Maize_Crop$JiTCAN12.5kg=Maize_Crop$JiTDAP12.5kg
Maize_Crop$JiTCAN25kg  =Maize_Crop$JiTDAP25kg
Maize_Crop$JiTCAN50kg  =Maize_Crop$JiTDAP50kg


#Summing up beans
Maize_Crop$Allbeans =Maize_Crop$Beans + Maize_Crop$`KMR13 High Zinc Beans`+Maize_Crop$`KMR11 High Zinc Beans`+Maize_Crop$`KK8 Beans`

#Breaking down beans into units from acrage
Maize_Crop$Beans6.25kg <- if_else(Maize_Crop$Allbeans==0.25,1,
                                  if_else(Maize_Crop$Allbeans==0.75,1,
                                          if_else(Maize_Crop$Allbeans==1.25,1,
                                                  if_else(Maize_Crop$Allbeans==1.75,1,
                                                          if_else(Maize_Crop$Allbeans==2.25,1,
                                                                  if_else(Maize_Crop$Allbeans==2.75,1,
                                                                          if_else(Maize_Crop$Allbeans==3.25,1,0)))))))
Maize_Crop$Beans12.5kg <- if_else(Maize_Crop$Allbeans==0.5,1,
                                if_else(Maize_Crop$Allbeans==0.75,1,
                                        if_else(Maize_Crop$Allbeans==1.5,1,
                                                if_else(Maize_Crop$Allbeans==1.75,1,
                                                        if_else(Maize_Crop$Allbeans==2.5,1,
                                                                if_else(Maize_Crop$Allbeans==2.75,1,
                                                                        if_else(Maize_Crop$Allbeans==3.5,1,0)))))))
Maize_Crop$Beans25kg <- if_else(Maize_Crop$Allbeans==1,1,
                                if_else(Maize_Crop$Allbeans==1.25,1,
                                        if_else(Maize_Crop$Allbeans==1.5,1,
                                                if_else(Maize_Crop$Allbeans==1.75,1,
                                                        if_else(Maize_Crop$Allbeans==3,1,
                                                                if_else(Maize_Crop$Allbeans==3.25,1,
                                                                        if_else(Maize_Crop$Allbeans==3.5,1,0)))))))
Maize_Crop$Beans50kg <- if_else(Maize_Crop$Allbeans==2,1,
                                if_else(Maize_Crop$Allbeans==2.25,1,
                                        if_else(Maize_Crop$Allbeans==2.5,1,
                                                if_else(Maize_Crop$Allbeans==2.75,1,
                                                        if_else(Maize_Crop$Allbeans==3,1,
                                                                if_else(Maize_Crop$Allbeans==3.25,1,
                                                                        if_else(Maize_Crop$Allbeans==3.5,1,0)))))))
#Greengrams fertilizer breakdown
#Beaking down green grams and converting it into units
Maize_Crop$GGrams6.25kg <- if_else(Maize_Crop$`Green Grams` ==0.25,1,
                                  if_else(Maize_Crop$`Green Grams`==0.75,1,
                                          if_else(Maize_Crop$`Green Grams`==1.25,1,
                                                  if_else(Maize_Crop$`Green Grams`==1.75,1,
                                                          if_else(Maize_Crop$`Green Grams`==2.25,1,
                                                                  if_else(Maize_Crop$`Green Grams`==2.75,1,
                                                                          if_else(Maize_Crop$`Green Grams`==3.25,1,0)))))))
Maize_Crop$GGrams12.5kg <- if_else(Maize_Crop$`Green Grams`==0.5,1,
                                  if_else(Maize_Crop$`Green Grams`==0.75,1,
                                          if_else(Maize_Crop$`Green Grams`==1.5,1,
                                                  if_else(Maize_Crop$`Green Grams`==1.75,1,
                                                          if_else(Maize_Crop$`Green Grams`==2.5,1,
                                                                  if_else(Maize_Crop$`Green Grams`==2.75,1,
                                                                          if_else(Maize_Crop$`Green Grams`==3.5,1,0)))))))
Maize_Crop$GGrams25kg <- if_else(Maize_Crop$`Green Grams`==1,1,
                                if_else(Maize_Crop$`Green Grams`==1.25,1,
                                        if_else(Maize_Crop$`Green Grams`==1.5,1,
                                                if_else(Maize_Crop$`Green Grams`==1.75,1,
                                                        if_else(Maize_Crop$`Green Grams`==3,1,
                                                                if_else(Maize_Crop$`Green Grams`==3.25,1,
                                                                        if_else(Maize_Crop$`Green Grams`==3.5,1,0)))))))
Maize_Crop$GGrams50kg <- if_else(Maize_Crop$`Green Grams`==2,1,
                                if_else(Maize_Crop$`Green Grams`==2.25,1,
                                        if_else(Maize_Crop$`Green Grams`==2.5,1,
                                                if_else(Maize_Crop$`Green Grams`==2.75,1,
                                                        if_else(Maize_Crop$`Green Grams`==3,1,
                                                                if_else(Maize_Crop$`Green Grams`==3.25,1,
                                                                        if_else(Maize_Crop$`Green Grams`==3.5,1,0)))))))
#Breaking Sorghum into units
Maize_Crop$Sorghum12.5kg <- if_else(Maize_Crop$Sorghum ==0.25,1,
                                  if_else(Maize_Crop$Sorghum==0.75,1,
                                          if_else(Maize_Crop$Sorghum==1.25,1,
                                                  if_else(Maize_Crop$Sorghum==1.75,1,
                                                          if_else(Maize_Crop$Sorghum==2.25,1,
                                                                  if_else(Maize_Crop$Sorghum==2.75,1,
                                                                          if_else(Maize_Crop$Sorghum==3.25,1,0)))))))
Maize_Crop$Sorghum25kg <- if_else(Maize_Crop$Sorghum==0.5,1,
                                if_else(Maize_Crop$Sorghum==0.75,1,
                                        if_else(Maize_Crop$Sorghum==1.5,1,
                                                if_else(Maize_Crop$Sorghum==1.75,1,
                                                        if_else(Maize_Crop$Sorghum==2.5,1,
                                                                if_else(Maize_Crop$Sorghum==2.75,1,
                                                                        if_else(Maize_Crop$Sorghum==3.5,1,0)))))))
Maize_Crop$Sorghum50kg <- if_else(Maize_Crop$Sorghum==1,1,
                                if_else(Maize_Crop$Sorghum==1.25,1,
                                        if_else(Maize_Crop$Sorghum==1.5,1,
                                                if_else(Maize_Crop$Sorghum==1.75,1,
                                                        if_else(Maize_Crop$Sorghum==2,2,
                                                                if_else(Maize_Crop$Sorghum==2.25,2,
                                                                        if_else(Maize_Crop$Sorghum==2.5,2,
                                                                                if_else(Maize_Crop$Sorghum==2.75,2,
                                                                                        if_else(Maize_Crop$Sorghum==3,3,
                                                                                                if_else(Maize_Crop$Sorghum==3.5,3,0))))))))))
names(Maize_Crop)
#Summing up all DAP Fertilizer
Maize_Crop$DAP6.25Kg= Maize_Crop$Beans6.25kg + Maize_Crop$GGrams6.25kg 
Maize_Crop$DAP12.5Kg= Maize_Crop$Maize12.5kg + Maize_Crop$Beans12.5kg+ Maize_Crop$GGrams12.5kg +Maize_Crop$Sorghum12.5kg
Maize_Crop$DAP25Kg  = Maize_Crop$Maize25kg   + Maize_Crop$Beans25kg  + Maize_Crop$GGrams25kg   +Maize_Crop$Sorghum25kg
Maize_Crop$DAP50Kg  = Maize_Crop$Maize50kg   + Maize_Crop$Beans50kg  + Maize_Crop$GGrams50kg   +Maize_Crop$Sorghum50kg

names(scd20)
Fertilizer = scd20 %>%
  group_by(GlobalClientID) %>% select(GlobalClientID,RegionName,DistrictName,SiteName,FirstName,LastName
                                      ,`Maize Urea`=`Maize Urea Option acres`,`Extra CAN`=`Extra CAN acres`,`Extra Urea`=`Extra Urea acres`
                                      ,`Banana Fertilizer`=`Banana Fertilizer qty`,`Coffee Fertilizer`=`Coffee Fertilizer qty`,`Garden Fruit Fertilizer`=`Garden Fruit Fertilizer qty`,`Leafy Veg Fertilizer`=`Leafy Veg Fertilizer qty`)


Fertilizer$Urea=Fertilizer$`Maize Urea`+Fertilizer$`Extra Urea`


Fertilizer$NPK1Kg=Fertilizer$`Garden Fruit Fertilizer`+Fertilizer$`Leafy Veg Fertilizer`
Fertilizer$Mavuno25Kg=Fertilizer$`Banana Fertilizer`+Fertilizer$`Coffee Fertilizer`

FertilizerBreakdown = left_join(Maize_Crop,Fertilizer, by="GlobalClientID")%>%select(-RegionName.y,-DistrictName.y,-SiteName.y,-FirstName.y,-LastName.y,-`Maize Urea.y`,-`Extra Urea`,-`Banana Fertilizer`,-`Coffee Fertilizer`,-`Garden Fruit Fertilizer`,-`Leafy Veg Fertilizer`)

FertilizerBreakdown$MaizeCAN=FertilizerBreakdown$AllMaize-FertilizerBreakdown$`Maize Urea.x`                                                         

# Breaking Down CAN
FertilizerBreakdown$MaizeCAN12.5kg <- if_else(FertilizerBreakdown$MaizeCAN==0.25,1,
                                          if_else(FertilizerBreakdown$MaizeCAN==0.75,1,
                                                  if_else(FertilizerBreakdown$MaizeCAN==1.25,1,
                                                          if_else(FertilizerBreakdown$MaizeCAN==1.75,1,
                                                                  if_else(FertilizerBreakdown$MaizeCAN==2.25,1,
                                                                          if_else(FertilizerBreakdown$MaizeCAN==2.75,1,
                                                                                  if_else(FertilizerBreakdown$MaizeCAN==3.25,1,0)))))))
FertilizerBreakdown$MaizeCAN25kg <- if_else(FertilizerBreakdown$MaizeCAN==0.5,1,
                                        if_else(FertilizerBreakdown$MaizeCAN==0.75,1,
                                                if_else(FertilizerBreakdown$MaizeCAN==1.5,1,
                                                        if_else(FertilizerBreakdown$MaizeCAN==1.75,1,
                                                                if_else(FertilizerBreakdown$MaizeCAN==2.5,1,
                                                                        if_else(FertilizerBreakdown$MaizeCAN==2.75,1,
                                                                                if_else(FertilizerBreakdown$MaizeCAN==3.5,1,0)))))))
FertilizerBreakdown$MaizeCAN50kg <- if_else(FertilizerBreakdown$MaizeCAN==1,1,
                                        if_else(FertilizerBreakdown$MaizeCAN==1.25,1,
                                                if_else(FertilizerBreakdown$MaizeCAN==1.5,1,
                                                        if_else(FertilizerBreakdown$MaizeCAN==1.75,1,
                                                                if_else(FertilizerBreakdown$MaizeCAN==2,2,
                                                                        if_else(FertilizerBreakdown$MaizeCAN==2.25,2,
                                                                                if_else(FertilizerBreakdown$MaizeCAN==2.5,2,
                                                                                        if_else(FertilizerBreakdown$MaizeCAN==2.75,2,
                                                                                                if_else(FertilizerBreakdown$MaizeCAN==3,3,
                                                                                                        if_else(FertilizerBreakdown$MaizeCAN==3.5,3,0))))))))))
#Extra CAN
FertilizerBreakdown$ExtraCAN12.5kg <- if_else(FertilizerBreakdown$`Extra CAN`==0.25,1,
                                              if_else(FertilizerBreakdown$`Extra CAN`==0.75,1,
                                                      if_else(FertilizerBreakdown$`Extra CAN`==1.25,1,
                                                              if_else(FertilizerBreakdown$`Extra CAN`==1.75,1,
                                                                      if_else(FertilizerBreakdown$`Extra CAN`==2.25,1,
                                                                              if_else(FertilizerBreakdown$`Extra CAN`==2.75,1,
                                                                                      if_else(FertilizerBreakdown$`Extra CAN`==3.25,1,0)))))))
FertilizerBreakdown$ExtraCAN25kg <- if_else(FertilizerBreakdown$`Extra CAN`==0.5,1,
                                            if_else(FertilizerBreakdown$`Extra CAN`==0.75,1,
                                                    if_else(FertilizerBreakdown$`Extra CAN`==1.5,1,
                                                            if_else(FertilizerBreakdown$`Extra CAN`==1.75,1,
                                                                    if_else(FertilizerBreakdown$`Extra CAN`==2.5,1,
                                                                            if_else(FertilizerBreakdown$`Extra CAN`==2.75,1,
                                                                                    if_else(FertilizerBreakdown$`Extra CAN`==3.5,1,0)))))))
FertilizerBreakdown$ExtraCAN50kg <- if_else(FertilizerBreakdown$`Extra CAN`==1,1,
                                        if_else(FertilizerBreakdown$`Extra CAN`==1.25,1,
                                                if_else(FertilizerBreakdown$`Extra CAN`==1.5,1,
                                                        if_else(FertilizerBreakdown$`Extra CAN`==1.75,1,
                                                                if_else(FertilizerBreakdown$`Extra CAN`==2,2,
                                                                        if_else(FertilizerBreakdown$`Extra CAN`==2.25,2,
                                                                                if_else(FertilizerBreakdown$`Extra CAN`==2.5,2,
                                                                                        if_else(FertilizerBreakdown$`Extra CAN`==2.75,2,
                                                                                                if_else(FertilizerBreakdown$`Extra CAN`==3,3,
                                                                                                    if_else(FertilizerBreakdown$`Extra CAN`==3.5,3,0))))))))))
#Combining CAN
FertilizerBreakdown$CAN12.5Kg <- FertilizerBreakdown$Sorghum12.5kg+FertilizerBreakdown$MaizeCAN12.5kg+FertilizerBreakdown$ExtraCAN12.5kg
FertilizerBreakdown$CAN25Kg <- FertilizerBreakdown$Sorghum25kg+FertilizerBreakdown$MaizeCAN25kg+FertilizerBreakdown$ExtraCAN25kg
FertilizerBreakdown$CAN50kg<-FertilizerBreakdown$Sorghum50kg+FertilizerBreakdown$MaizeCAN50kg+FertilizerBreakdown$ExtraCAN50kg

#Introducing and Breaking Down Urea
FertilizerBreakdown$Urea12.5kg <- if_else(FertilizerBreakdown$Urea==0.25,1,
                                  if_else(FertilizerBreakdown$Urea==0.75,1,
                                          if_else(FertilizerBreakdown$Urea==1.25,1,
                                                  if_else(FertilizerBreakdown$Urea==1.75,1,
                                                          if_else(FertilizerBreakdown$Urea==2.25,1,
                                                                  if_else(FertilizerBreakdown$Urea==2.75,1,
                                                                          if_else(FertilizerBreakdown$Urea==3.25,1,0)))))))
FertilizerBreakdown$Urea25kg <- if_else(FertilizerBreakdown$Urea==0.5,1,
                                if_else(FertilizerBreakdown$Urea==0.75,1,
                                        if_else(FertilizerBreakdown$Urea==1.5,1,
                                                if_else(FertilizerBreakdown$Urea==1.75,1,
                                                        if_else(FertilizerBreakdown$Urea==2.5,1,
                                                                if_else(FertilizerBreakdown$Urea==2.75,1,
                                                                        if_else(FertilizerBreakdown$Urea==3.5,1,0)))))))
FertilizerBreakdown$Urea50kg <- if_else(FertilizerBreakdown$Urea==1,1,
                                if_else(FertilizerBreakdown$Urea==1.25,1,
                                        if_else(FertilizerBreakdown$Urea==1.5,1,
                                                if_else(FertilizerBreakdown$Urea==1.75,1,
                                                        if_else(FertilizerBreakdown$Urea==2,2,
                                                                if_else(FertilizerBreakdown$Urea==2.25,2,
                                                                        if_else(FertilizerBreakdown$Urea==2.5,2,
                                                                                if_else(FertilizerBreakdown$Urea==2.75,2,
                                                                                        if_else(FertilizerBreakdown$Urea==3,3,
                                                                                                if_else(FertilizerBreakdown$Urea==3.5,3,0))))))))))



#ARM Lime
FertilizerBreakdown$ARMLime12.5kg <- if_else(FertilizerBreakdown$ARMLime ==0.125,1,0)

FertilizerBreakdown$ARMLime25kg <- if_else(FertilizerBreakdown$ARMLime==0.25,1,
                                if_else(FertilizerBreakdown$ARMLime==0.75,1,
                                        if_else(FertilizerBreakdown$ARMLime==1.25,1,
                                                if_else(FertilizerBreakdown$ARMLime==1.75,1,
                                                        if_else(FertilizerBreakdown$ARMLime==2.25,1,
                                                                if_else(FertilizerBreakdown$ARMLime==2.75,1,0))))))
FertilizerBreakdown$ARMLime50kg <- if_else(FertilizerBreakdown$ARMLime==0.5,1,
                                if_else(FertilizerBreakdown$ARMLime==0.75,1,
                                        if_else(FertilizerBreakdown$ARMLime==1,2,
                                                if_else(FertilizerBreakdown$ARMLime==1.25,2,
                                                        if_else(FertilizerBreakdown$ARMLime==1.5,3,
                                                                if_else(FertilizerBreakdown$ARMLime==1.75,3,
                                                                        if_else(FertilizerBreakdown$ARMLime==2,4,
                                                                                if_else(FertilizerBreakdown$ARMLime==2.25,4,
                                                                                        if_else(FertilizerBreakdown$ARMLime==2.5,5,
                                                                                              if_else(FertilizerBreakdown$ARMLime==2.75,5,
                                                                                                       if_else(FertilizerBreakdown$ARMLime==3,6,0)))))))))))


#HomaLime
FertilizerBreakdown$HomaLime25kg <- if_else(FertilizerBreakdown$HomaLime==0.125,1,0)

FertilizerBreakdown$HomaLime50kg <- if_else(FertilizerBreakdown$ARMLime==0.25,1,
                                           if_else(FertilizerBreakdown$ARMLime==0.5,2,
                                               if_else(FertilizerBreakdown$ARMLime==0.75,3,
                                                       if_else(FertilizerBreakdown$ARMLime==1,4,
                                                               if_else(FertilizerBreakdown$ARMLime==1.25,5,
                                                                       if_else(FertilizerBreakdown$ARMLime==1.5,6,
                                                                               if_else(FertilizerBreakdown$ARMLime==1.75,7,
                                                                                       if_else(FertilizerBreakdown$ARMLime==2,8,
                                                                                               if_else(FertilizerBreakdown$ARMLime==2.25,9,
                                                                                                       if_else(FertilizerBreakdown$ARMLime==2.5,10,
                                                                                                               if_else(FertilizerBreakdown$ARMLime==2.75,11,
                                                                                                                       if_else(FertilizerBreakdown$ARMLime==3,12,0))))))))))))
    
    








FinalFertilizer <- FertilizerBreakdown  %>% 
group_by(GlobalClientID)%>% select(GlobalClientID,RegionName.x,DistrictName.x,SiteName.x,FirstName.x,LastName.x
                                     ,DAP6.25Kg,DAP12.5Kg,DAP25Kg,DAP50Kg,CAN12.5Kg,CAN25Kg,CAN50kg,JiTDAP12.5kg,JiTDAP25kg,JiTDAP50kg,JiTCAN12.5kg,JiTCAN25kg,JiTCAN50kg
                                     ,Urea12.5kg,Urea25kg,Urea50kg,NPK1Kg,Mavuno25Kg,ARMLime12.5kg,ARMLime25kg,ARMLime50kg,HomaLime25kg,HomaLime50kg)
names(FinalFertilizer)
colnames(FinalFertilizer)[2] = "RegionName"
colnames(FinalFertilizer)[3] = "DistrictName"
colnames(FinalFertilizer)[4] = "SiteName"
colnames(FinalFertilizer)[5] = "FirstName"
colnames(FinalFertilizer)[6] = "LastName"
write.csv(FinalFertilizer,"Fertilizer Breakdown.csv, row.names = F")

#maize variety breakdown
#core maize
seed_core = scd20 %>% group_by(GlobalClientID) %>%
  summarize(
    RegionName = first(RegionName)
    ,DistrictName = first(DistrictName)
    ,Maize = `Core Maize acres`
    ,Seed = `Core Maize Maize Seed choice`)

#seed_core = seed_core %>% drop_na() %>% filter(Seed != "")

#dryland maize
seed_dryland = scd20 %>% group_by(GlobalClientID) %>%
  summarize(
    RegionName = first(RegionName)
    ,DistrictName = first(DistrictName)
    ,Maize = `Dryland Maize acres`
    ,Seed = `Dryland Maize Maize Seed choice`)

#seed_dryland = seed_dryland %>% drop_na() %>% filter(Seed != "")

#Maize Urea
seed_urea = scd20 %>% group_by(GlobalClientID) %>%
  summarize(
    RegionName = first(RegionName)
    ,DistrictName = first(DistrictName)
    ,Maize = `Maize Urea Option acres`
    ,Seed = `Maize Urea Option Maize Seed choice`) 

# Merge the maize types
seed_p1 = rbind(seed_core, seed_dryland)
seed_p = rbind(seed_p1, seed_urea)
seed_p = seed_p %>% drop_na() %>% filter(Maize != 0)

#spread
seed_Maize = seed_p %>% mutate(i = row_number()) %>% spread(Seed, Maize) %>% select(-i)
seed_Maize[is.na(seed_Maize)] <-0
seed_Maize$`DK 777 Seed`=if_else(seed_Maize$`DK 777`==0.25,4,
                                 if_else(seed_Maize$`DK 777`==0.5,6,
                                         if_else(seed_Maize$`DK 777`==0.75,8,
                                                 if_else(seed_Maize$`DK 777`==1,10,
                                                         if_else(seed_Maize$`DK 777`==1.25,12,
                                                                 if_else(seed_Maize$`DK 777`==1.5,14,
                                                                         if_else(seed_Maize$`DK 777`==1.75,16,
                                                                                 if_else(seed_Maize$`DK 777`==2,18,
                                                                                         if_else(seed_Maize$`DK 777`==2.25,20,
                                                                                                 if_else(seed_Maize$`DK 777`==2.5,22,
                                                                                                         if_else(seed_Maize$`DK 777`==2.75,24,
                                                                                                                 if_else(seed_Maize$`DK 777`==3,26,0))))))))))))

seed_Maize$`DK 8031 Seed`=if_else(seed_Maize$`DK 8031`==0.25,4,
                                 if_else(seed_Maize$`DK 8031`==0.5,6,
                                         if_else(seed_Maize$`DK 8031`==0.75,8,
                                                 if_else(seed_Maize$`DK 8031`==1,10,
                                                         if_else(seed_Maize$`DK 8031`==1.25,12,
                                                                 if_else(seed_Maize$`DK 8031`==1.5,14,
                                                                         if_else(seed_Maize$`DK 8031`==1.75,16,
                                                                                 if_else(seed_Maize$`DK 8031`==2,18,
                                                                                         if_else(seed_Maize$`DK 8031`==2.25,20,
                                                                                                 if_else(seed_Maize$`DK 8031`==2.5,22,
                                                                                                         if_else(seed_Maize$`DK 8031`==2.75,24,
                                                                                                                 if_else(seed_Maize$`DK 8031`==3,26,0))))))))))))
seed_Maize$`DK 8033 Seed`=if_else(seed_Maize$`DK 8033`==0.25,4,
                                  if_else(seed_Maize$`DK 8033`==0.5,6,
                                          if_else(seed_Maize$`DK 8033`==0.75,8,
                                                  if_else(seed_Maize$`DK 8033`==1,10,
                                                          if_else(seed_Maize$`DK 8033`==1.25,12,
                                                                  if_else(seed_Maize$`DK 8033`==1.5,14,
                                                                          if_else(seed_Maize$`DK 8033`==1.75,16,
                                                                                  if_else(seed_Maize$`DK 8033`==2,18,
                                                                                          if_else(seed_Maize$`DK 8033`==2.25,20,
                                                                                                  if_else(seed_Maize$`DK 8033`==2.5,22,
                                                                                                          if_else(seed_Maize$`DK 8033`==2.75,24,
                                                                                                                  if_else(seed_Maize$`DK 8033`==3,26,0))))))))))))
seed_Maize$`DK 90-89 Seed`=if_else(seed_Maize$`DK 90-89`==0.25,4,
                                  if_else(seed_Maize$`DK 90-89`==0.5,6,
                                          if_else(seed_Maize$`DK 90-89`==0.75,8,
                                                  if_else(seed_Maize$`DK 90-89`==1,10,
                                                          if_else(seed_Maize$`DK 90-89`==1.25,12,
                                                                  if_else(seed_Maize$`DK 90-89`==1.5,14,
                                                                          if_else(seed_Maize$`DK 90-89`==1.75,16,
                                                                                  if_else(seed_Maize$`DK 90-89`==2,18,
                                                                                          if_else(seed_Maize$`DK 90-89`==2.25,20,
                                                                                                  if_else(seed_Maize$`DK 90-89`==2.5,22,
                                                                                                          if_else(seed_Maize$`DK 90-89`==2.75,24,
                                                                                                                  if_else(seed_Maize$`DK 90-89`==3,26,0))))))))))))
seed_Maize$`H 614D Seed`=if_else(seed_Maize$`H 614D`==0.25,4,
                                   if_else(seed_Maize$`H 614D`==0.5,6,
                                           if_else(seed_Maize$`H 614D`==0.75,8,
                                                   if_else(seed_Maize$`H 614D`==1,10,
                                                           if_else(seed_Maize$`H 614D`==1.25,12,
                                                                   if_else(seed_Maize$`H 614D`==1.5,14,
                                                                           if_else(seed_Maize$`H 614D`==1.75,16,
                                                                                   if_else(seed_Maize$`H 614D`==2,18,
                                                                                           if_else(seed_Maize$`H 614D`==2.25,20,
                                                                                                   if_else(seed_Maize$`H 614D`==2.5,22,
                                                                                                           if_else(seed_Maize$`H 614D`==2.75,24,
                                                                                                                   if_else(seed_Maize$`H 614D`==3,26,0))))))))))))

seed_Maize$`H 6213 Seed`=if_else(seed_Maize$`H 6213`==0.25,4,
                                 if_else(seed_Maize$`H 6213`==0.5,6,
                                         if_else(seed_Maize$`H 6213`==0.75,8,
                                                 if_else(seed_Maize$`H 6213`==1,10,
                                                         if_else(seed_Maize$`H 6213`==1.25,12,
                                                                 if_else(seed_Maize$`H 6213`==1.5,14,
                                                                         if_else(seed_Maize$`H 6213`==1.75,16,
                                                                                 if_else(seed_Maize$`H 6213`==2,18,
                                                                                         if_else(seed_Maize$`H 6213`==2.25,20,
                                                                                                 if_else(seed_Maize$`H 6213`==2.5,22,
                                                                                                         if_else(seed_Maize$`H 6213`==2.75,24,
                                                                                                                 if_else(seed_Maize$`H 6213`==3,26,0))))))))))))
seed_Maize$`H 6218 Seed`=if_else(seed_Maize$`H 6218`==0.25,4,
                                 if_else(seed_Maize$`H 6218`==0.5,6,
                                         if_else(seed_Maize$`H 6218`==0.75,8,
                                                 if_else(seed_Maize$`H 6218`==1,10,
                                                         if_else(seed_Maize$`H 6218`==1.25,12,
                                                                 if_else(seed_Maize$`H 6218`==1.5,14,
                                                                         if_else(seed_Maize$`H 6218`==1.75,16,
                                                                                 if_else(seed_Maize$`H 6218`==2,18,
                                                                                         if_else(seed_Maize$`H 6218`==2.25,20,
                                                                                                 if_else(seed_Maize$`H 6218`==2.5,22,
                                                                                                         if_else(seed_Maize$`H 6218`==2.75,24,
                                                                                                                 if_else(seed_Maize$`H 6218`==3,26,0))))))))))))

seed_Maize$`PAN 691 Seed`=if_else(seed_Maize$`PAN 691`==0.25,4,
                                  if_else(seed_Maize$`PAN 691`==0.5,6,
                                          if_else(seed_Maize$`PAN 691`==0.75,8,
                                                  if_else(seed_Maize$`PAN 691`==1,10,
                                                          if_else(seed_Maize$`PAN 691`==1.25,12,
                                                                  if_else(seed_Maize$`PAN 691`==1.5,14,
                                                                          if_else(seed_Maize$`PAN 691`==1.75,16,
                                                                                  if_else(seed_Maize$`PAN 691`==2,18,
                                                                                          if_else(seed_Maize$`PAN 691`==2.25,20,
                                                                                                  if_else(seed_Maize$`PAN 691`==2.5,22,
                                                                                                          if_else(seed_Maize$`PAN 691`==2.75,24,
                                                                                                                  if_else(seed_Maize$`PAN 691`==3,26,0))))))))))))

seed_Maize$`Pioneer P3812W Seed`=if_else(seed_Maize$`Pioneer P3812W`==0.25,4,
                                  if_else(seed_Maize$`Pioneer P3812W`==0.5,6,
                                          if_else(seed_Maize$`Pioneer P3812W`==0.75,8,
                                                  if_else(seed_Maize$`Pioneer P3812W`==1,10,
                                                          if_else(seed_Maize$`Pioneer P3812W`==1.25,12,
                                                                  if_else(seed_Maize$`Pioneer P3812W`==1.5,14,
                                                                          if_else(seed_Maize$`Pioneer P3812W`==1.75,16,
                                                                                  if_else(seed_Maize$`Pioneer P3812W`==2,18,
                                                                                          if_else(seed_Maize$`Pioneer P3812W`==2.25,20,
                                                                                                  if_else(seed_Maize$`Pioneer P3812W`==2.5,22,
                                                                                                          if_else(seed_Maize$`Pioneer P3812W`==2.75,24,
                                                                                                                  if_else(seed_Maize$`Pioneer P3812W`==3,26,0))))))))))))

seed_Maize$`PN 15 Seed`=if_else(seed_Maize$`PN 15`==0.25,4,
                                         if_else(seed_Maize$`PN 15`==0.5,6,
                                                 if_else(seed_Maize$`PN 15`==0.75,8,
                                                         if_else(seed_Maize$`PN 15`==1,10,
                                                                 if_else(seed_Maize$`PN 15`==1.25,12,
                                                                         if_else(seed_Maize$`PN 15`==1.5,14,
                                                                                 if_else(seed_Maize$`PN 15`==1.75,16,
                                                                                         if_else(seed_Maize$`PN 15`==2,18,
                                                                                                 if_else(seed_Maize$`PN 15`==2.25,20,
                                                                                                         if_else(seed_Maize$`PN 15`==2.5,22,
                                                                                                                 if_else(seed_Maize$`PN 15`==2.75,24,
                                                                                                                         if_else(seed_Maize$`PN 15`==3,26,0))))))))))))
seed_Maize$`SC DUMA 43 Seed`=if_else(seed_Maize$`SC DUMA 43`==0.25,4,
                                if_else(seed_Maize$`SC DUMA 43`==0.5,6,
                                        if_else(seed_Maize$`SC DUMA 43`==0.75,8,
                                                if_else(seed_Maize$`SC DUMA 43`==1,10,
                                                        if_else(seed_Maize$`SC DUMA 43`==1.25,12,
                                                                if_else(seed_Maize$`SC DUMA 43`==1.5,14,
                                                                        if_else(seed_Maize$`SC DUMA 43`==1.75,16,
                                                                                if_else(seed_Maize$`SC DUMA 43`==2,18,
                                                                                        if_else(seed_Maize$`SC DUMA 43`==2.25,20,
                                                                                                if_else(seed_Maize$`SC DUMA 43`==2.5,22,
                                                                                                        if_else(seed_Maize$`SC DUMA 43`==2.75,24,
                                                                                                                if_else(seed_Maize$`SC DUMA 43`==3,26,0))))))))))))

seed_Maize$`SC PUNDA MILIA 53 Seed`=if_else(seed_Maize$`SC PUNDA MILIA 53`==0.25,4,
                                if_else(seed_Maize$`SC PUNDA MILIA 53`==0.5,6,
                                        if_else(seed_Maize$`SC PUNDA MILIA 53`==0.75,8,
                                                if_else(seed_Maize$`SC PUNDA MILIA 53`==1,10,
                                                        if_else(seed_Maize$`SC PUNDA MILIA 53`==1.25,12,
                                                                if_else(seed_Maize$`SC PUNDA MILIA 53`==1.5,14,
                                                                        if_else(seed_Maize$`SC PUNDA MILIA 53`==1.75,16,
                                                                                if_else(seed_Maize$`SC PUNDA MILIA 53`==2,18,
                                                                                        if_else(seed_Maize$`SC PUNDA MILIA 53`==2.25,20,
                                                                                                if_else(seed_Maize$`SC PUNDA MILIA 53`==2.5,22,
                                                                                                        if_else(seed_Maize$`SC PUNDA MILIA 53`==2.75,24,
                                                                                                                if_else(seed_Maize$`SC PUNDA MILIA 53`==3,26,0))))))))))))
seed_Maize$`SC SIMBA 61 Seed`=if_else(seed_Maize$`SC SIMBA 61`==0.25,4,
                                     if_else(seed_Maize$`SC SIMBA 61`==0.5,6,
                                             if_else(seed_Maize$`SC SIMBA 61`==0.75,8,
                                                     if_else(seed_Maize$`SC SIMBA 61`==1,10,
                                                             if_else(seed_Maize$`SC SIMBA 61`==1.25,12,
                                                                     if_else(seed_Maize$`SC SIMBA 61`==1.5,14,
                                                                             if_else(seed_Maize$`SC SIMBA 61`==1.75,16,
                                                                                     if_else(seed_Maize$`SC SIMBA 61`==2,18,
                                                                                             if_else(seed_Maize$`SC SIMBA 61`==2.25,20,
                                                                                                     if_else(seed_Maize$`SC SIMBA 61`==2.5,22,
                                                                                                             if_else(seed_Maize$`SC SIMBA 61`==2.75,24,
                                                                                                                     if_else(seed_Maize$`SC SIMBA 61`==3,26,0))))))))))))
seed_Maize$`SC Sungura 301 Seed`=if_else(seed_Maize$`SC Sungura 301`==0.25,4,
                                      if_else(seed_Maize$`SC Sungura 301`==0.5,6,
                                              if_else(seed_Maize$`SC Sungura 301`==0.75,8,
                                                      if_else(seed_Maize$`SC Sungura 301`==1,10,
                                                              if_else(seed_Maize$`SC Sungura 301`==1.25,12,
                                                                      if_else(seed_Maize$`SC Sungura 301`==1.5,14,
                                                                              if_else(seed_Maize$`SC Sungura 301`==1.75,16,
                                                                                      if_else(seed_Maize$`SC Sungura 301`==2,18,
                                                                                              if_else(seed_Maize$`SC Sungura 301`==2.25,20,
                                                                                                      if_else(seed_Maize$`SC Sungura 301`==2.5,22,
                                                                                                              if_else(seed_Maize$`SC Sungura 301`==2.75,24,
                                                                                                                      if_else(seed_Maize$`SC Sungura 301`==3,26,0))))))))))))
seed_Maize$`SC Tembo 73 Seed`=if_else(seed_Maize$`SC Tembo 73`==0.25,4,
                                         if_else(seed_Maize$`SC Tembo 73`==0.5,6,
                                                 if_else(seed_Maize$`SC Tembo 73`==0.75,8,
                                                         if_else(seed_Maize$`SC Tembo 73`==1,10,
                                                                 if_else(seed_Maize$`SC Tembo 73`==1.25,12,
                                                                         if_else(seed_Maize$`SC Tembo 73`==1.5,14,
                                                                                 if_else(seed_Maize$`SC Tembo 73`==1.75,16,
                                                                                         if_else(seed_Maize$`SC Tembo 73`==2,18,
                                                                                                 if_else(seed_Maize$`SC Tembo 73`==2.25,20,
                                                                                                         if_else(seed_Maize$`SC Tembo 73`==2.5,22,
                                                                                                                 if_else(seed_Maize$`SC Tembo 73`==2.75,24,
                                                                                                                         if_else(seed_Maize$`SC Tembo 73`==3,26,0))))))))))))
seed_Maize$`SY 594 Seed`=if_else(seed_Maize$`SY 594`==0.25,4,
                                      if_else(seed_Maize$`SY 594`==0.5,6,
                                              if_else(seed_Maize$`SY 594`==0.75,8,
                                                      if_else(seed_Maize$`SY 594`==1,10,
                                                              if_else(seed_Maize$`SY 594`==1.25,12,
                                                                      if_else(seed_Maize$`SY 594`==1.5,14,
                                                                              if_else(seed_Maize$`SY 594`==1.75,16,
                                                                                      if_else(seed_Maize$`SY 594`==2,18,
                                                                                              if_else(seed_Maize$`SY 594`==2.25,20,
                                                                                                      if_else(seed_Maize$`SY 594`==2.5,22,
                                                                                                              if_else(seed_Maize$`SY 594`==2.75,24,
                                                                                                                      if_else(seed_Maize$`SY 594`==3,26,0))))))))))))
seed_Maize$`WH 505 Seed`=if_else(seed_Maize$`WH 505`==0.25,4,
                                 if_else(seed_Maize$`WH 505`==0.5,6,
                                         if_else(seed_Maize$`WH 505`==0.75,8,
                                                 if_else(seed_Maize$`WH 505`==1,10,
                                                         if_else(seed_Maize$`WH 505`==1.25,12,
                                                                 if_else(seed_Maize$`WH 505`==1.5,14,
                                                                         if_else(seed_Maize$`WH 505`==1.75,16,
                                                                                 if_else(seed_Maize$`WH 505`==2,18,
                                                                                         if_else(seed_Maize$`WH 505`==2.25,20,
                                                                                                 if_else(seed_Maize$`WH 505`==2.5,22,
                                                                                                         if_else(seed_Maize$`WH 505`==2.75,24,
                                                                                                                 if_else(seed_Maize$`WH 505`==3,26,0))))))))))))
seed_Maize$`WH 509 Seed`=if_else(seed_Maize$`WH 509`==0.25,4,
                                 if_else(seed_Maize$`WH 509`==0.5,6,
                                         if_else(seed_Maize$`WH 509`==0.75,8,
                                                 if_else(seed_Maize$`WH 509`==1,10,
                                                         if_else(seed_Maize$`WH 509`==1.25,12,
                                                                 if_else(seed_Maize$`WH 509`==1.5,14,
                                                                         if_else(seed_Maize$`WH 509`==1.75,16,
                                                                                 if_else(seed_Maize$`WH 509`==2,18,
                                                                                         if_else(seed_Maize$`WH 509`==2.25,20,
                                                                                                 if_else(seed_Maize$`WH 509`==2.5,22,
                                                                                                         if_else(seed_Maize$`WH 509`==2.75,24,
                                                                                                                 if_else(seed_Maize$`WH 509`==3,26,0))))))))))))
names(seed_Maize)
Maizefinal = seed_Maize %>% group_by(GlobalClientID) %>%select(GlobalClientID,RegionName,DistrictName
                          ,`DK 777 Seed`,`DK 8031 Seed`,`DK 90-89 Seed`,`H 614D Seed`,`H 6213 Seed`,`H 6218 Seed`,`PAN 691 Seed`
                          ,`Pioneer P3812W Seed`,`PN 15 Seed`,`SC DUMA 43 Seed`,`SC PUNDA MILIA 53 Seed`,`SC SIMBA 61 Seed`,`SC Sungura 301 Seed`
                          ,`SC Tembo 73 Seed`,`SY 594 Seed`,`WH 505 Seed`,`WH 509 Seed`)

#Converting JitMaize into kgs
#Beans 
names(scd20)
JiTMaizeseed <-  Maize_Crop %>% group_by(GlobalClientID) %>% select(RegionName,DistrictName,`DK 8031 JiT Maize acres`,`DK 8033 JiT Maize acres`,`DK 90-89 JiT Maize acres`
                ,`H 614D JiT Maize acres`,`H 6213 JiT Maize acres`,`H 6218 JiT Maize acres`,`PAN 691 JiT Maize acres`,`Pioneer P3812W JiT Maize acres`
                ,`SC DUMA 43 JiT Maize acres`,`SC PUNDA MILIA 53 JiT Maize acres`,`Sungura 301 JiT Maize acres`,`WH 505 JiT Maize acres`)

JiTMaizeseed$`DK 8031 JiT`=if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==0.25,4,
                                 if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==0.5,6,
                                         if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==0.75,8,
                                                 if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==1,10,
                                                         if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==1.25,12,
                                                                 if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==1.5,14,
                                                                         if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==1.75,16,
                                                                                 if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==2,18,
                                                                                         if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==2.25,20,
                                                                                                 if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==2.5,22,
                                                                                                         if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==2.75,24,
                                                                                                                 if_else(JiTMaizeseed$`DK 8031 JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`DK 8033 JiT`=if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==0.25,4,
                                   if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==0.5,6,
                                           if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==0.75,8,
                                                   if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==1,10,
                                                           if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==1.25,12,
                                                                   if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==1.5,14,
                                                                           if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==1.75,16,
                                                                                   if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==2,18,
                                                                                           if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==2.25,20,
                                                                                                   if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==2.5,22,
                                                                                                           if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==2.75,24,
                                                                                                                   if_else(JiTMaizeseed$`DK 8033 JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`DK 90-89 JiT`=if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==0.25,4,
                                   if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==0.5,6,
                                           if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==0.75,8,
                                                   if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==1,10,
                                                           if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==1.25,12,
                                                                   if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==1.5,14,
                                                                           if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==1.75,16,
                                                                                   if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==2,18,
                                                                                           if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==2.25,20,
                                                                                                   if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==2.5,22,
                                                                                                           if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==2.75,24,
                                                                                                                   if_else(JiTMaizeseed$`DK 90-89 JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`H 614D JiT`=if_else(JiTMaizeseed$`H 614D JiT Maize acres`==0.25,4,
                                    if_else(JiTMaizeseed$`H 614D JiT Maize acres`==0.5,6,
                                            if_else(JiTMaizeseed$`H 614D JiT Maize acres`==0.75,8,
                                                    if_else(JiTMaizeseed$`H 614D JiT Maize acres`==1,10,
                                                            if_else(JiTMaizeseed$`H 614D JiT Maize acres`==1.25,12,
                                                                    if_else(JiTMaizeseed$`H 614D JiT Maize acres`==1.5,14,
                                                                            if_else(JiTMaizeseed$`H 614D JiT Maize acres`==1.75,16,
                                                                                    if_else(JiTMaizeseed$`H 614D JiT Maize acres`==2,18,
                                                                                            if_else(JiTMaizeseed$`H 614D JiT Maize acres`==2.25,20,
                                                                                                    if_else(JiTMaizeseed$`H 614D JiT Maize acres`==2.5,22,
                                                                                                            if_else(JiTMaizeseed$`H 614D JiT Maize acres`==2.75,24,
                                                                                                                    if_else(JiTMaizeseed$`H 614D JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`H 6213 JiT`=if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==0.25,4,
                                  if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==0.5,6,
                                          if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==0.75,8,
                                                  if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==1,10,
                                                          if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==1.25,12,
                                                                  if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==1.5,14,
                                                                          if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==1.75,16,
                                                                                  if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==2,18,
                                                                                          if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==2.25,20,
                                                                                                  if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==2.5,22,
                                                                                                          if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==2.75,24,
                                                                                                                  if_else(JiTMaizeseed$`H 6213 JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`H 6218 JiT`=if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==0.25,4,
                                  if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==0.5,6,
                                          if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==0.75,8,
                                                  if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==1,10,
                                                          if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==1.25,12,
                                                                  if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==1.5,14,
                                                                          if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==1.75,16,
                                                                                  if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==2,18,
                                                                                          if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==2.25,20,
                                                                                                  if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==2.5,22,
                                                                                                          if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==2.75,24,
                                                                                                                  if_else(JiTMaizeseed$`H 6218 JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`PAN 691 JiT`=if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==0.25,4,
                                  if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==0.5,6,
                                          if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==0.75,8,
                                                  if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==1,10,
                                                          if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==1.25,12,
                                                                  if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==1.5,14,
                                                                          if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==1.75,16,
                                                                                  if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==2,18,
                                                                                          if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==2.25,20,
                                                                                                  if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==2.5,22,
                                                                                                          if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==2.75,24,
                                                                                                                  if_else(JiTMaizeseed$`PAN 691 JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`Pioneer P3812W JiT`=if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==0.25,4,
                                   if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==0.5,6,
                                           if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==0.75,8,
                                                   if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==1,10,
                                                           if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==1.25,12,
                                                                   if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==1.5,14,
                                                                           if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==1.75,16,
                                                                                   if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==2,18,
                                                                                           if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==2.25,20,
                                                                                                   if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==2.5,22,
                                                                                                           if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==2.75,24,
                                                                                                                   if_else(JiTMaizeseed$`Pioneer P3812W JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`SC DUMA 43 JiT`=if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==0.25,4,
                                          if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==0.5,6,
                                                  if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==0.75,8,
                                                          if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==1,10,
                                                                  if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==1.25,12,
                                                                          if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==1.5,14,
                                                                                  if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==1.75,16,
                                                                                          if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==2,18,
                                                                                                  if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==2.25,20,
                                                                                                          if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==2.5,22,
                                                                                                                  if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==2.75,24,
                                                                                                                          if_else(JiTMaizeseed$`SC DUMA 43 JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`SC PUNDA MILIA 53 JiT`=if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==0.25,4,
                                      if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==0.5,6,
                                              if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==0.75,8,
                                                      if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==1,10,
                                                              if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==1.25,12,
                                                                      if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==1.5,14,
                                                                              if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==1.75,16,
                                                                                      if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==2,18,
                                                                                              if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==2.25,20,
                                                                                                      if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==2.5,22,
                                                                                                              if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==2.75,24,
                                                                                                                      if_else(JiTMaizeseed$`SC PUNDA MILIA 53 JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`Sungura 301 JiT`=if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==0.25,4,
                                             if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==0.5,6,
                                                     if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==0.75,8,
                                                             if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==1,10,
                                                                     if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==1.25,12,
                                                                             if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==1.5,14,
                                                                                     if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==1.75,16,
                                                                                             if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==2,18,
                                                                                                     if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==2.25,20,
                                                                                                             if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==2.5,22,
                                                                                                                     if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==2.75,24,
                                                                                                                             if_else(JiTMaizeseed$`Sungura 301 JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed$`WH 505 JiT`=if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==0.25,4,
                                       if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==0.5,6,
                                               if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==0.75,8,
                                                       if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==1,10,
                                                               if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==1.25,12,
                                                                       if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==1.5,14,
                                                                               if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==1.75,16,
                                                                                       if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==2,18,
                                                                                               if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==2.25,20,
                                                                                                       if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==2.5,22,
                                                                                                               if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==2.75,24,
                                                                                                                       if_else(JiTMaizeseed$`WH 505 JiT Maize acres`==3,26,0))))))))))))
JiTMaizeseed_Breakdown= JiTMaizeseed %>% group_by(GlobalClientID)%>% 
  select(GlobalClientID,RegionName,DistrictName,`DK 8031 JiT`,`DK 8033 JiT`,`DK 90-89 JiT`,`H 6218 JiT`,`PAN 691 JiT`
         ,`Pioneer P3812W JiT`,`SC DUMA 43 JiT`,`SC PUNDA MILIA 53 JiT`,`Sungura 301 JiT`,`WH 505 JiT`,`H 614D JiT`,`H 6213 JiT`)

Maizeseed = left_join(JiTMaizeseed_Breakdown, Maizefinal)
Maizeseed[is.na(Maizeseed)] <- 0
write.csv(Maizeseed,"Maize Breakdown.csv", row.names = F)




seed_beans = scd20 %>% group_by(GlobalClientID) %>%
  summarize(
    RegionName = first(RegionName)
    ,DistrictName = first(DistrictName)
    ,Beans = `Beans acres`
    ,Seed = `Beans Bean Seed choice`) 
#filter out NAs
seed_b = seed_beans %>% drop_na() %>% filter(Seed != "None")

#spread the seed choices
beansseed= spread(seed_b, Seed, Beans)
beansseed[is.na(beansseed)] <- 0

beansseed$`KK-8 Seeds`=if_else(beansseed$`KK-8`==0.25,1,
                                  if_else(beansseed$`KK-8`==0.5,2,
                                          if_else(beansseed$`KK-8`==0.75,3,
                                                  if_else(beansseed$`KK-8`==1,4,
                                                          if_else(beansseed$`KK-8`==1.25,5,
                                                                  if_else(beansseed$`KK-8`==1.5,6,
                                                                          if_else(beansseed$`KK-8`==1.75,7,
                                                                                  if_else(beansseed$`KK-8`==2,8,
                                                                                          if_else(beansseed$`KK-8`==2.25,9,
                                                                                                  if_else(beansseed$`KK-8`==2.5,10,
                                                                                                          if_else(beansseed$`KK-8`==2.75,11,
                                                                                                                  if_else(beansseed$`KK-8`==3,12,0))))))))))))
beansseed$`Rosecoco Seeds`=if_else(beansseed$Rosecoco==0.25,1,
                               if_else(beansseed$Rosecoco==0.5,2,
                                       if_else(beansseed$Rosecoco==0.75,3,
                                               if_else(beansseed$Rosecoco==1,4,
                                                       if_else(beansseed$Rosecoco==1.25,5,
                                                               if_else(beansseed$Rosecoco==1.5,6,
                                                                       if_else(beansseed$Rosecoco==1.75,7,
                                                                               if_else(beansseed$Rosecoco==2,8,
                                                                                       if_else(beansseed$Rosecoco==2.25,9,
                                                                                               if_else(beansseed$Rosecoco==2.5,10,
                                                                                                       if_else(beansseed$Rosecoco==2.75,11,
                                                                                                               if_else(beansseed$Rosecoco==3,12,0))))))))))))
beansseed$`Wairimu Seeds`=if_else(beansseed$Wairimu==0.25,1,
                                   if_else(beansseed$Wairimu==0.5,2,
                                           if_else(beansseed$Wairimu==0.75,3,
                                                   if_else(beansseed$Wairimu==1,4,
                                                           if_else(beansseed$Wairimu==1.25,5,
                                                                   if_else(beansseed$Wairimu==1.5,6,
                                                                           if_else(beansseed$Wairimu==1.75,7,
                                                                                   if_else(beansseed$Wairimu==2,8,
                                                                                           if_else(beansseed$Wairimu==2.25,9,
                                                                                                   if_else(beansseed$Wairimu==2.5,10,
                                                                                                           if_else(beansseed$Wairimu==2.75,11,
                                                                                                                   if_else(beansseed$Wairimu==3,12,0))))))))))))


#High Zinc Beans
HZB = scd20 %>% group_by(GlobalClientID) %>%
  summarize(
    RegionName = first(RegionName)
    ,DistrictName = first(DistrictName)
    ,`High Zinc Beans` = `KMR13 High-Zinc Beans acres` + `KMR11 High-Zinc Beans acres`
    ,`Sorghum acres`,`Green Gram Monocrop acres`) 
HZB$Greengrams=if_else(HZB$`Green Gram Monocrop acres`==0.25,1,
                                  if_else(HZB$`Green Gram Monocrop acres`==0.5,2,
                                          if_else(HZB$`Green Gram Monocrop acres`==0.75,3,
                                                  if_else(HZB$`Green Gram Monocrop acres`==1,4,
                                                          if_else(HZB$`Green Gram Monocrop acres`==1.25,5,
                                                                  if_else(HZB$`Green Gram Monocrop acres`==1.5,6,
                                                                          if_else(HZB$`Green Gram Monocrop acres`==1.75,7,
                                                                                  if_else(HZB$`Green Gram Monocrop acres`==2,8,
                                                                                          if_else(HZB$`Green Gram Monocrop acres`==2.25,9,
                                                                                                  if_else(HZB$`Green Gram Monocrop acres`==2.5,10,
                                                                                                          if_else(HZB$`Green Gram Monocrop acres`==2.75,11,
                                                                                                                  if_else(HZB$`Green Gram Monocrop acres`==3,12,0))))))))))))
HZB$Sorghum =if_else(HZB$`Sorghum acres`==0.25,1,
                                  if_else(HZB$`Sorghum acres`==0.5,2,
                                          if_else(HZB$`Sorghum acres`==0.75,3,
                                                  if_else(HZB$`Sorghum acres`==1,4,
                                                          if_else(HZB$`Sorghum acres`==1.25,5,
                                                                  if_else(HZB$`Sorghum acres`==1.5,6,
                                                                          if_else(HZB$`Sorghum acres`==1.75,7,
                                                                                  if_else(HZB$`Sorghum acres`==2,8,
                                                                                          if_else(HZB$`Sorghum acres`==2.25,9,
                                                                                                  if_else(HZB$`Sorghum acres`==2.5,10,
                                                                                                          if_else(HZB$`Sorghum acres`==2.75,11,
                                                                                                                  if_else(HZB$`Sorghum acres`==3,12,0))))))))))))
HZB$High_Zinc =if_else(HZB$`High Zinc Beans`==0.25,5,
                     if_else(HZB$`High Zinc Beans`==0.5,10,
                             if_else(HZB$`High Zinc Beans`==0.75,15,
                                     if_else(HZB$`High Zinc Beans`==1,20,
                                             if_else(HZB$`High Zinc Beans`==1.25,25,
                                                     if_else(HZB$`High Zinc Beans`==1.5,30,
                                                             if_else(HZB$`High Zinc Beans`==1.75,35,
                                                                     if_else(HZB$`High Zinc Beans`==2,40,
                                                                             if_else(HZB$`High Zinc Beans`==2.25,45,
                                                                                     if_else(HZB$`High Zinc Beans`==2.5,50,
                                                                                             if_else(HZB$`High Zinc Beans`==2.75,55,
                                                                                                     if_else(HZB$`High Zinc Beans`==3,60,0))))))))))))
Beans_final = left_join(HZB, beansseed) 
Beans_final[is.na(Beans_final)] <- 0
Beans_Breakdown =Beans_final%>%select(GlobalClientID,RegionName,DistrictName,`KK-8 Seeds`,`Rosecoco Seeds`,`Wairimu Seeds`,High_Zinc,Sorghum,Greengrams)
SeedsBreakdown <- left_join(Beans_Breakdown,Maizeseed)
SeedsBreakdown[is.na(SeedsBreakdown)] <- 0
write.csv(SeedsBreakdown,"raw seeds Breakdown.csv",row.names = F)
#Summarizing all seeds per district
Final_Seed_Breakdown = SeedsBreakdown %>%  group_by(DistrictName) %>%
  summarize_all(funs(sum(., na.rm = T)))
write.csv(Final_Seed_Breakdown,"Seeds Breakdown.csv",row.names = F)




names(scd20)
Vegetables <- scd20 %>%
  group_by(GlobalClientID)%>% select(GlobalClientID,RegionName,DistrictName,SiteName,FirstName,LastName
                                     #,`Overall Vegetables` = `Tomato qty` + `Red Onion qty` + `Carrot qty` + `Sukuma qty` + `Managu qty` + `Saga qty` + `Spinach qty` + `Cabbage qty`
                                     ,`Tomato` = `Tomato qty`
                                     ,`Red Onion` = `Red Onion qty`
                                     ,Carrot = `Carrot qty`
                                     ,Sukuma = `Sukuma qty`
                                     ,Managu = `Managu qty`
                                     ,Saga = `Saga qty`
                                     ,Spinach = `Spinach qty`
                                     ,Cabbage = `Cabbage qty`)
names(scd20)
# clean Solar  products
Solar = scd20 %>%
  group_by(GlobalClientID) %>% 
  summarize(RegionName = first(RegionName)
            ,DistrictName = first(DistrictName)
            ,SiteName    = first(SiteName)
            ,FirstName   = first(FirstName)
            ,LastName    = first(LastName)
            ,`SK Boom`   =`Sunking Boom qty`+`JiT Sunking Boom qty`
            ,`SK Charge` = `Sunking Charge qty` 
            ,`SK Home   `=`Sunking Home System qty`
            ,`SK Home Radio`= `Sunking Home with Radio qty`
            ,`SK Pico Plus`   =`Sunking Pico Plus qty`
            ,`SK Pro 300` =`Sunking Pro 300 qty`
            ,`Biolite SHS `=`Biolite SHS qty`)
# clean Lifestyle  products
Lifestyles = scd20 %>%
  group_by(GlobalClientID) %>% 
  summarize(RegionName = first(RegionName)
            ,DistrictName = first(DistrictName)
            ,SiteName    = first(SiteName)
            ,FirstName   = first(FirstName)
            ,LastName    = first(LastName)
            ,`Smart Phone`=`Smart Phone qty`
            ,`Basic Phone`=`Basic Phone qty`+`JiT BasicPhone qty`
            ,`Iron Sheets`=`Iron Sheet qty`
            ,`Knapsack Sprayer`=`Knapsack Sprayer qty`
            ,`Burn Cookstove` =`Burn Cookstove qty` 
            ,Wheelbarrow=`Wheelbarrow qty`)
# clean Health  products
Health = scd20 %>%
  group_by(GlobalClientID) %>% 
  summarize(RegionName = first(RegionName)
            ,DistrictName = first(DistrictName)
            ,SiteName    = first(SiteName)
            ,FirstName   = first(FirstName)
            ,LastName    = first(LastName)
            ,`Sanitary Pads`=`So Sure Sanitary Pads qty`
            ,`Health Insurance`=`Health Insurance qty`) 
# clean Harvest  products
Harvest = scd20 %>%
  group_by(GlobalClientID) %>% 
  summarize(RegionName = first(RegionName)
            ,DistrictName = first(DistrictName)
            ,SiteName    = first(SiteName)
            ,FirstName   = first(FirstName)
            ,LastName    = first(LastName)
            ,`Actellic Gold`=`Actellic Gold Dust qty`
            ,`Storage bag`=`Storage Bag qty`
            ,`PICS Bag`=`PICS Bag qty`
            ,`Harvest Drying Sheet`= `Harvest Drying Sheet qty`+`JiT Harvest Drying Sheet qty`)

#Joining all other products into one dataset
otherproducts = left_join(Vegetables,Lifestyles) 
Healtharvest=left_join(Health,Harvest)
All_otherproducts=left_join(otherproducts,Healtharvest)

#Deselecting some columns
All_otherproducts=All_otherproducts %>% select(-GlobalClientID,-RegionName,-SiteName,-FirstName,-LastName)

#Replacing NA with zeros
All_otherproducts[is.na(All_otherproducts)] <- 0

#Summarizing data at district level
Final_otherproductsBreakdown = All_otherproducts[,c(2:22)] %>% group_by(DistrictName) %>%
  summarize_all(funs(sum(., na.rm = T)))
#Writing Output
write.csv(Final_otherproductsBreakdown,"All other products.csv",row.names = F)


