#Clear global environment
rm(list = ls())
cat("\014")


#Load libraries
library(ISLR)
library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(openxlsx)

#Import the files
setwd("G:/My Drive/Analytics/Repayment Status Reports/LR2021 Repayment/Feb Reports/Feb 8th")
list.files()
#Loading todays seasons clients data
sc2021 = read_csv("Season Clients Detailed_20210208-103001.csv")
#Filtering out VEDD districts from the sC data
sc2021 = sc2021 %>% filter(DistrictName != "Sotik" & DistrictName != "Masaba" & DistrictName != "Nyamira" )
#Separating the Trial Districts since they fall under different qualification creterion
TrialDistricts= sc2021 %>% filter( DistrictName == "Chwele"| DistrictName == "Kipkelion")
#Removing the  Trial Districts from the main export SC client
sc2021= sc2021 %>% filter( DistrictName != "Chwele" & DistrictName != "Kipkelion")
#Separating theCashprice since they fall under different qualification creterion
Cashprice = sc2021 %>% filter(GroupName == "Cash Price" |GroupName =="CASH PRICE 1" |GroupName == "Cash Price 2")
#Selecting Cashprice clients who meet creteria
Cashprice=Cashprice%>% filter(`% Repaid` ==100)
#Dropping all Cashprice clients from main export to avoid duplication
sc2021= sc2021 %>% filter(GroupName != "Cash Price" |GroupName !="CASH PRICE 1" |GroupName != "Cash Price 2")
sc2021=sc2021%>% filter(sc2021$TotalRepaid>=500)
##
##Getting clients from trial Districts who meet requalied qualification creteria
TrialDistricts$status <- if_else(TrialDistricts$TotalCredit<=15000 & TrialDistricts$`% Repaid`>=10,1,0)
TrialDistricts$status1 <-if_else(TrialDistricts$TotalCredit >15000 & TrialDistricts$TotalRepaid>=1500,1,0)
TrialDistricts$status2 <-TrialDistricts$status+TrialDistricts$status1
TrialDistricts =TrialDistricts %>% filter(TrialDistricts$status2==1) %>% select(-status,-status1,-status2)
##
##
#Joining back the exports of cashprice clients, TrialDistrict  to the main export
sc=rbind(sc2021, Cashprice,TrialDistricts)

##
##Proceeding with normal data manupilation
sc = sc %>% filter(DistrictName != "KENYA STAFF" & DistrictName != "OAF Duka")
names(sc)
vertical = read_csv("Light By Repayment Date_20210119-113606.csv")
attach(vertical)

#First Monday date of the week,current time & current day
Monday = as.Date(cut(Sys.Date(), "weeks"))
Time = Sys.time()
Today = Sys.Date()
FirstWeekDate = "2020-12-28"
LastWeekDate = "2021-01-03"
DateCreated1 = "2021-01-04"
hp = 16
Target = 30
TopPerformer = 50.01
MiddlePerformerUpper = 50.00
MiddlePerformerLower = 35.01
NeedsImprovementUpper = 35.00
NeedsImprovementLower = 10.01
NeedsUrgentImprovement = 10.00
#Weekstart
Monday1 = as.Date(cut(Sys.Date(), "weeks"))
Monday2 = paste(month(Monday1),day(Monday1),year(Monday1),sep = "/")
Monday = as.Date(Monday2, format = "%m/%d/%Y")
WeekStart = FirstWeekDate

#Set week number
WeekNumber = strftime(FirstWeekDate, format = "%V")

strftime(FirstWeekDate, format = "%V")
lubridate::week(ymd(FirstWeekDate))

#Change Repayment date format
sc$LastRepayment = as.Date(sc$LastRepayment, format = "%m/%d/%Y")

#At site level
names(sc)
colnames(sc)[22] = "X..Repaid"
DataToDate = sc %>% mutate(UniqueSite = paste(DistrictName,SiteName),DateCreated = DateCreated1,WeekStart = WeekStart,Country = "Kenya",Region = RegionName) %>%
  group_by(DistrictName,SiteName,UniqueSite,SectorName,DateCreated,WeekStart,Country,Region) %>%
  mutate(RepaymentWeekNumber = lubridate::week(ymd(LastRepayment)),
         UniqueGroup = paste(DistrictName,SiteName,GroupName),
         TotalStruggling = if_else(X..Repaid < hp,1,0),
         TotalBehindPlayer = if_else(is.na(as.Date(LastRepayment, format = "%m/%d/%Y")) == TRUE,1,if_else(ymd(LastRepayment) < ymd(DateCreated1) - days(30),1,0)),
         TotalStagnating = if_else((TotalStruggling + TotalBehindPlayer) == 2,1,0),
         TotalAtorAboveHP = if_else(X..Repaid >= Target,1,0)) %>%
  summarise(TotalClients = n(),
            TotalCredit = sum(TotalCredit),
            TotalRepaid = sum(TotalRepaid),
            TotalStruggling = sum(TotalStruggling),
            TotalBehindPlayer = sum(TotalBehindPlayer),
            TotalStagnating = sum(TotalStagnating),
            TotalAtorAboveHP = sum(TotalAtorAboveHP),
            PercentStruggling = (TotalStruggling/TotalClients)*100)

WeeklyData = vertical %>%
  mutate(UniqueSite = paste(District,Site)) %>%
  group_by(District,Site,UniqueSite) %>%
  mutate(unique = paste(District,Site,Group,OAFID)) %>%
  filter(Type == "MobileMoney",Amount > 0,SeasonName == "2021, Long Rain") %>%
  summarise(WeekRepaid = sum(Amount),ClientsParticipated = n_distinct(unique))

#Merge DataToDate and WeeklyData
names(DataToDate)

AllData1 = merge(DataToDate,WeeklyData,by = "UniqueSite",all.x = TRUE) %>%
  mutate(SiteRanking = if_else(PercentStruggling >= NeedsUrgentImprovement,"Needs Urgent Improvement",
                               if_else(PercentStruggling >= NeedsImprovementUpper & PercentStruggling < NeedsImprovementLower,"Needs Improvement",
                                       if_else(PercentStruggling >= MiddlePerformerUpper & PercentStruggling < MiddlePerformerLower,"Middle Performer",
                                               if_else(PercentStruggling >= TopPerformer,"Top Performer","")))))



names(AllData1)
AllData = AllData1 %>%
  select(WeekStart,Country,Region,SectorName,DistrictName,SiteName,TotalClients,TotalCredit,TotalRepaid,WeekRepaid,ClientsParticipated,TotalStruggling,TotalStagnating,TotalAtorAboveHP,SiteRanking)

write.csv(AllData, "Weekly Repayment Data Core.csv", row.names = F)


