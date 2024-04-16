library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(googlesheets4)
library(dplyr)
library(shinycssloaders)

source('configure.R')

log <- googlesheets4::read_sheet(sheetId, sheet="log")
lastupdatetime <- max(log$UpdateTime)

entityOfInterest <- googlesheets4::read_sheet(sheetId, sheet="entityOfInterest")

diseaseOfInterest <- entityOfInterest[entityOfInterest$Type == "diseaseOfInterest", ]$Item

