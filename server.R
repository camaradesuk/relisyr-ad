shinyServer(function(input, output, session) {
  #project dashboard----
  progressSummary <-reactive({
    progressSummary <-googlesheets4::read_sheet(Sys.getenv("relisyr_ad_gsheet"), 
                                                        sheet = "progressSummary")  
    })
  
  createStatistics <- function(progressSummary,irow){
    reviewSummary = progressSummary[irow, ]
    list(
      UpdateDate = reviewSummary$lastUpdate,
      nUniquePubs = reviewSummary$nUniquePublications,
      nIncludedPubs =      reviewSummary$nIncludedPublications,
      nDrugMeetLogic = reviewSummary$nDrugMeetLogic,
      nPublicationsMeetLogic = reviewSummary$nPublicationsMeetLogic,
      nCoreDrugs = reviewSummary$nCoreDrugs,
      nCoreDrugsPubs =  reviewSummary$nCoreDrugPublications,
      nSingleAnnotated = reviewSummary$nSingleAnnotated,
      percentSingleAnnotated =  round(reviewSummary$nSingleAnnotated / reviewSummary$nCoreDrugPublications * 100,2),
      nDualAnnotated = reviewSummary$nDualAnnotated,
      percentDualAnnotated =  round(reviewSummary$nDualAnnotated / reviewSummary$nCoreDrugPublications * 100,2),
      nReconciled = reviewSummary$nReconciled,
      percentReconciled = round(reviewSummary$nReconciled / reviewSummary$nCoreDrugPublications * 100,2)
    )
  }
  
  clinicalStatistics <- reactive({
    progressSummary<-progressSummary()
    clinicalStatistics <- createStatistics(progressSummary, 1)    
    })
  
  invivoStatistics <- reactive({   
    progressSummary<-progressSummary()
    invivoStatistics <-createStatistics(progressSummary, 2)    
    })
  
  clinicalprisma <- reactive({
    clinicalStatistics <- clinicalStatistics()
    clinicalprisma <- createPrisma(clinicalStatistics)
    return(clinicalprisma)
  })


  output$clinicalPrismaOutput <- renderGrViz({
    grViz({
      clinicalprisma()
    })
  })
  
  invivoprisma <- reactive({
    invivoStatistics <- invivoStatistics()
    invivoprisma <- createPrisma(invivoStatistics)
    return(invivoprisma)
  })
  
  
  output$animalPrismaOutput <- renderGrViz({
    grViz({
      invivoprisma()
    })
  })
  # 
  ## drug prioritisation ----
  longlistedDrugs <- reactive({
    longlistedDrugs <- read_sheet(Sys.getenv("relisyr_ad_gsheet"), 'longlist')
    
  })
  
  output$currentLonglist<- DT::renderDataTable(DT::datatable({
    longlistTable <- longlistedDrugs()
    return(longlistTable)
    
  }), extensions = 'Buttons'
  , filter = 'top', 
  options = list(
    pageLength = 50, lengthMenu = c(10,25,50,100,1000), escape=F)) 
  
  output$lastupdatetime <- reactive({
    return(paste0("Data last updated on ", format(as.Date(lastupdatetime), "%Y-%m-%d")))
  })
  
  clinicalStudyList <- reactive({
    clinicalStudyList <- RMySQL::dbReadTable(con, "ReLiSyRClinicalStudies") 
  })
  
 invivoStudies <- reactive({
   invivoStudies<- RMySQL::dbReadTable(con, "ad_invivo_citations")
  })
  
  filteredClinicalStudyList <- reactive({
    clinicalStudyList <- clinicalStudyList()
    clinicalStudyList <- left_join(clinicalStudyList, reviewStage, by = "idStr")
    clinicalStudyList <- clinicalStudyList%>%mutate(nReviews = ifelse(is.na(nReviews), 0, nReviews))
    return(clinicalStudyList)
  })
  

  
  clinicalOutputCrossTable <- reactive({
    filteredClinicalStudyList <- filteredClinicalStudyList()
    filteredClinicalStudyList$Disease <- factor(filteredClinicalStudyList$Disease, levels = diseaseOfInterest)
    clinicalOutputCrossTable <- as.data.frame.matrix(table(filteredClinicalStudyList[,c("Drug","Disease")]))
    
    return(clinicalOutputCrossTable[, diseaseOfInterest])
  })
  
  
  invivoOutputTable <- reactive({
    invivoOutputTable<- invivoStudies()%>%
      group_by(intervention)%>%
      summarise(invivo = length(unique(uid)))%>%
      textshape::column_to_rownames("intervention")
    return(invivoOutputTable)
  })
  
  
  combinedOutputCrossTable <- reactive({
    combinedOutputCrossTable <- merge(invivoOutputTable(), 
                                      clinicalOutputCrossTable(), 
                                      by = 0, all = TRUE)%>%
      textshape::column_to_rownames(1)
    
    combinedOutputCrossTable <- combinedOutputCrossTable%>%mutate(across(everything(),~tidyr::replace_na(., 0)))
    
    return(combinedOutputCrossTable)
  })
  

  feasibleFilterDrugs <- reactive ({
    feasibleFilterDrugs<- masterDrugList %>%
      dplyr::select(Name, input$feasibilityFilter)%>%
      filter(if_all(where(is.logical)), TRUE)
    feasibleFilterDrugs <- feasibleFilterDrugs$Name
    return(feasibleFilterDrugs)
  })
  
  ro5passDrugs <- reactive({
    ro5passDrugs <- masterDrugList%>%
      filter(ro5Violations <= input$ro5violation | is.na(ro5Violations))
    
    ro5passDrugs <- ro5passDrugs$Name
    return(ro5passDrugs)
  })
  
  filteredDrugs <-  reactive({
    myOutputCrossTable <- combinedOutputCrossTable()
    if(input$candidateCategory == "logicOnly"){
      myOutputCrossTable$select1  <- F
      myOutputCrossTable$score1 <- rowSums(myOutputCrossTable[, "AD", drop = F])
      myOutputCrossTable$score2 <- rowSums(myOutputCrossTable[, setdiff(diseaseOfInterest, "AD"), drop=F] > 0)
      myOutputCrossTable$select1 <- myOutputCrossTable$select | (myOutputCrossTable$score1 > 0 | myOutputCrossTable$score2 >= 2)
      
      filteredOutputCrossTable1 <- myOutputCrossTable[which(myOutputCrossTable$select1), ]
    } else if(input$candidateCategory == "bothDomains"){
          myOutputCrossTable$score3<- rowSums(myOutputCrossTable[, diseaseOfInterest])
          myOutputCrossTable <- myOutputCrossTable%>%mutate(select2 = ifelse(score3 >0 & invivo>0, TRUE, FALSE))
          
          filteredOutputCrossTable1 <- myOutputCrossTable[which(myOutputCrossTable$select2), ]
    } else {
      
      filteredOutputCrossTable1 <- myOutputCrossTable
    }
    
    chosenDrugs <- rownames(filteredOutputCrossTable1)
    
    if(input$candidateCategory == "longlist")  chosenDrugs <- intersect(longlistDrugs, chosenDrugs)
    
    chosenDrugs <- intersect(chosenDrugs, feasibleFilterDrugs())
    chosenDrugs <- intersect(chosenDrugs, ro5passDrugs())
    
    return(chosenDrugs)
  })
  
  
  frequencyCrossTable <- reactive({
    myOutputCrossTable <- combinedOutputCrossTable()
    filteredDrugs <- filteredDrugs()
    return(myOutputCrossTable[filteredDrugs, ])
  })
  
  output$clinicalStudyTable <- DT::renderDataTable(DT::datatable({
    myTable <-  filteredClinicalPublicationTable()
    myTable$Title <- paste0(myTable$Title, "(",myTable$Author,")")
    myTable$Title <- paste0("<a href='",myTable$Link ,"'target='_blank'>" , myTable$Title,"</a>" )
    
    index <- which(names(myTable) %in% c("X","Journal","Abstract","OldIdStr", "idStr", "Author","Link"))
    return(   
      myTable[,-index]
    )
  }) ,extensions = 'Buttons'
  , filter = 'top', options = list(
    pageLength = 10,lengthMenu = c(10,25,50,100,1000),autoWidth = TRUE
  ), escape=F) 
  
  
  
  output$animalStudyTable <- DT::renderDataTable(DT::datatable({
    myTable <-  filteredAnimalPublicationTable()
    myTable$Title <- paste0(myTable$Title, "(",myTable$Author,")")
    myTable$Title <- paste0("<a href='",myTable$Link ,"'target='_blank'>" , myTable$Title,"</a>" )
    
    index <- which(names(myTable) %in% c("X","Journal","Abstract","OldIdStr", "idStr", "Author","Link"))
    return(   
      myTable[,-index]
    )
  }) ,extensions = 'Buttons'
  , filter = 'top', options = list(
    pageLength = 10,lengthMenu = c(10,25,50,100,1000),autoWidth = TRUE
  ), escape=F) 
  
  output$frequencyCrossTable <- DT::renderDataTable(DT::datatable({
    return(    frequencyCrossTable())
  }), 
  filter = 'top', options = list(
    pageLength = 50,lengthMenu = c(10,25,50,100,1000)
    , dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ),
  extensions = c("Buttons", "Responsive"))
  
  filteredClinicalPublicationTable <- reactive({
    myOutputCrossTable <- frequencyCrossTable()
    chosenDrugs <- filteredDrugs()
    filteredClinicalStudyList <- filteredClinicalStudyList()
    chosenStudies <- filteredClinicalStudyList[filteredClinicalStudyList$Drug %in% chosenDrugs,] %>%
      group_by(idStr, nReviews) %>%
      summarise(Title = first(Title),
                Author = first(Author),
                Journal = first(Journal),
                Abstract = first(Abstract),
                Year = first(Year),
                Disease = paste0(unique(Disease), collapse = "; "),
                Drug = paste0(unique(Drug), collapse = "; "),
                MSType = paste0(unique(MSType), collapse = "; "),
                Link = first(Link)
      )%>%
      arrange(nReviews, Drug)
    return(chosenStudies)
  })
  
  

  
  output$DownloadFilteredClinicalPublications <-  downloadHandler(
    filename = paste0(Sys.Date(), "FilteredClinicalPublications.csv"), content = function(file){
      write.csv({
        filteredClinicalPublicationTable()
      }
      , file, na = "", row.names = F
      )
    })
  
  selectedClinicalPublicationTable <- reactive({
    myOutputCrossTable <- frequencyCrossTable()
    chosenDrugs <- rownames(myOutputCrossTable)[input$frequencyCrossTable_rows_selected]
    filteredClinicalStudyList <- filteredClinicalStudyList()
    chosenStudies <- filteredClinicalStudyList[filteredClinicalStudyList$Drug %in% chosenDrugs,] %>%
      group_by(idStr, nReviews) %>%
      summarise(Title = first(Title),
                Author = first(Author),
                Journal = first(Journal),
                Abstract = first(Abstract),
                Year = first(Year),
                Disease = paste0(unique(Disease), collapse = "; "),
                Drug = paste0(unique(Drug), collapse = "; "),
                MSType = paste0(unique(MSType), collapse = "; "),
                Link = first(Link)
      )%>%
      arrange(nReviews, Drug)
    return(chosenStudies)
  })
  
  output$DownloadSelectedClinicalPublications <-
    downloadHandler(
      filename = paste0(Sys.Date(), "SelectedClinicalPublications.csv"),
      content = function(file){
        write.csv({
          selectedClinicalPublicationTable()
        }
        , file, na = "", row.names = F
        )
      }
    )
  
  
  filteredAnimalPublicationTable <- reactive({
    myOutputCrossTable <- frequencyCrossTable()
    chosenDrugs <- filteredDrugs()
    invivoStudies <- invivoStudies()
    chosenStudies <- invivoStudies[invivoStudies$intervention %in% chosenDrugs,] %>%
      group_by(uid) %>%
      summarise(Title = first(title),
                Authors = first(author),
                PublicationName = first(journal),
                Abstract = first(abstract),
                Year = first(year),
                DOI = first(doi),
                AlternateName = first(secondarytitle),
                Url = first(url),
                AuthorAddress = first(author_affiliation),
                ReferenceType = first(ptype),
                Keywords = first(keywords),
                PDFRelativePath = "",
                CustomId = uid,
                Drug = paste0(unique(intervention), collapse = "; ")
      )%>%
      arrange(Drug)%>%
      select(-uid)
    return(chosenStudies)
  })

  
  output$DownloadFilteredAnimalPublications<-  downloadHandler(
    filename = "FilteredAnimalPublications.csv", content = function(file){
      write.csv({
        filteredAnimalPublicationTable()
      }
      , file, na = "", row.names = F
      )
    })
  
  
  
  selectedAnimalPublicationTable <- reactive({
    myOutputCrossTable <- frequencyCrossTable()
    chosenDrugs <- rownames(myOutputCrossTable)[input$frequencyCrossTable_rows_selected]
    invivoStudies <- invivoStudies()
    chosenStudies <- invivoStudies[invivoStudies$intervention %in% chosenDrugs,] %>%
      group_by(uid) %>%
      summarise(Title = first(title),
                Authors = first(author),
                PublicationName = first(journal),
                Abstract = first(abstract),
                Year = first(year),
                DOI = first(doi),
                AlternateName = first(secondarytitle),
                Url = first(url),
                AuthorAddress = first(author_affiliation),
                ReferenceType = first(ptype),
                Keywords = first(keywords),
                PDFRelativePath = "",
                CustomId = uid,
                Drug = paste0(unique(intervention), collapse = "; ")
      )%>%
      arrange(Drug)%>%
      select(-uid)
    return(chosenStudies)
  })
  
  

  output$DownloadSelectedAnimalPublications<-
    downloadHandler(
      filename = "SelectedAnimalPublications.csv",
      content = function(file){
        write.csv({
          selectedAnimalPublicationTable()
        }
        , file, na = "", row.names = F
        )
      }
    )
})
