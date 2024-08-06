library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(DiagrammeR)
library(glue)
library(googlesheets4)
library(dplyr)
library(shinycssloaders)
library(tidyr)

source('configure.R')
source('functions.R')

log <- googlesheets4::read_sheet(sheetId, sheet="log")
lastupdatetime <- max(log$UpdateTime)

entityOfInterest <- googlesheets4::read_sheet(sheetId, sheet="entityOfInterest")

diseaseOfInterest <- entityOfInterest[entityOfInterest$Type == "diseaseOfInterest", ]$Item

longlistDrugs <- read_sheet(Sys.getenv("relisyr_ad_gsheet"), 'longlist')$Drug

reviewerSession <- googlesheets4::read_sheet(sheetId, sheet = "reviewerSession")
reviewStage <- reviewerSession%>%
  filter(StageIdStr == "2c400348-d871-4055-9c68-2bc529ac9ccc")%>%
  group_by(StudyIdStr)%>%
  summarise(nReviews = length(InvestigatorIdStr))%>%
  rename(idStr = StudyIdStr)

masterDrugList <- read.csv("2024-08-06masterDrugList.csv") %>%
  mutate(approved = ifelse(max_phase==4, TRUE, FALSE),
         prescription_only = ifelse(availability_type == 1, TRUE, FALSE)
         )


masterDrugList <-masterDrugList %>% 
  rename(chemblRo5Violations = ro5Violations)%>%
  mutate(ro5Violations = ifelse(!is.na(admetSAR_ro5Violations) & admetSAR_ro5Violations != "NA", admetSAR_ro5Violations, chemblRo5Violations))

drugDatabaseOutput <- masterDrugList%>%
  select(Name, oral, max_phase, availability_type, ro5Violations, BBB, BNF, BNFgeneric, admetSAR3_BBB, B3DB_BBB)%>%
  mutate(admetSAR3_BBB = round(admetSAR3_BBB, 3))
