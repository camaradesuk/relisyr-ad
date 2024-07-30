library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(googlesheets4)
library(dplyr)
library(shinycssloaders)
library(tidyr)

source('configure.R')

log <- googlesheets4::read_sheet(sheetId, sheet="log")
lastupdatetime <- max(log$UpdateTime)

entityOfInterest <- googlesheets4::read_sheet(sheetId, sheet="entityOfInterest")

diseaseOfInterest <- entityOfInterest[entityOfInterest$Type == "diseaseOfInterest", ]$Item

longlistDrugs <- entityOfInterest[entityOfInterest$Type == "drugOfInterestAD", ]$Item

reviewerSession <- googlesheets4::read_sheet(sheetId, sheet = "reviewerSession")
reviewStage <- reviewerSession%>%
  filter(StageIdStr == "2c400348-d871-4055-9c68-2bc529ac9ccc")%>%
  group_by(StudyIdStr)%>%
  summarise(nReviews = length(InvestigatorIdStr))%>%
  rename(idStr = StudyIdStr)
