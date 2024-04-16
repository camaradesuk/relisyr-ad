shinyServer(function(input, output, session) {
  
  output$lastupdatetime <- reactive({
    return(paste0("Data last updated on ", format(as.Date(lastupdatetime), "%Y-%m-%d")))
  })
  
  clinicalStudyList <- reactive({
    RMySQL::dbReadTable(con, "ReLiSyRClinicalStudies") 
  })
  
  ExtractinvivoStudies <- reactive({
    RMySQL::dbReadTable(con, "ReLiSyRinvivoStudies")
  })
  
  filteredClinicalStudyList <- reactive({
    clinicalStudyList <- clinicalStudyList()
    return(clinicalStudyList)
  })
  
  outputCrossTable <- reactive({
    filteredClinicalStudyList <- filteredClinicalStudyList()
    filteredClinicalStudyList$Disease <- factor(filteredClinicalStudyList$Disease, levels = diseaseOfInterest)
    clinicalOutputCrossTable <- as.data.frame.matrix(table(filteredClinicalStudyList[,c("Drug","Disease")]))
    
    return(clinicalOutputCrossTable[, diseaseOfInterest])
  })
  
  filteredDrugs <-  reactive({
    myOutputCrossTable <- outputCrossTable()
    if(input$candidateCategory == "logicOnly"){
      myOutputCrossTable$select1  <- F
      myOutputCrossTable$score1 <- rowSums(myOutputCrossTable[, "AD", drop = F])
      myOutputCrossTable$score2 <- rowSums(myOutputCrossTable[, setdiff(diseaseOfInterest, "AD"), drop=F] > 0)
      myOutputCrossTable$select1 <- myOutputCrossTable$select | (myOutputCrossTable$score1 > 0 | myOutputCrossTable$score2 >= 2)

      filteredOutputCrossTable1 <- myOutputCrossTable[which(myOutputCrossTable$select1), ]
    } else{
      filteredOutputCrossTable1 <- myOutputCrossTable
    }
    chosenDrugs <- rownames(filteredOutputCrossTable1)
    
    return(chosenDrugs)
  })
  
  frequencyCrossTable <- reactive({
    myOutputCrossTable <- outputCrossTable()
    filteredDrugs <- filteredDrugs()
    return(myOutputCrossTable[filteredDrugs, ])
  })
  
  output$studyTable <- DT::renderDataTable(DT::datatable({
    myTable <-  filiteredPublicationTable()
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
  
  filiteredPublicationTable <- reactive({
    myOutputCrossTable <- frequencyCrossTable()
    chosenDrugs <- filteredDrugs()
    filteredClinicalStudyList <- filteredClinicalStudyList()
    chosenStudies <- filteredClinicalStudyList[filteredClinicalStudyList$Drug %in% chosenDrugs,] %>%
      group_by(OldIdStr) %>%
      summarise(Title = first(Title),
                Author = first(Author),
                Journal = first(Journal),
                Abstract = first(Abstract),
                Year = first(Year),
                idStr = first(idStr),
                HistoricalID = first(HistoricalID),
                PublicationID = first(PublicationID),
                Disease = paste0(unique(Disease), collapse = "; "),
                Drug = paste0(unique(Drug), collapse = "; "),
                MSType = paste0(unique(MSType), collapse = "; "),
                Link = first(Link)
      )
    return(chosenStudies)
  })
  
  output$DownloadFilteredPublications <-  downloadHandler(
    filename = "FilteredPublications.csv", content = function(file){
      write.csv({
        filiteredPublicationTable()
      }
      , file, na = "", row.names = F
      )
    })
  
  selectedPublicationTable <- reactive({
    myOutputCrossTable <- frequencyCrossTable()
    chosenDrugs <- rownames(myOutputCrossTable)[input$frequencyCrossTable_rows_selected]
    filteredClinicalStudyList <- filteredClinicalStudyList()
    chosenStudies <- filteredClinicalStudyList[filteredClinicalStudyList$Drug %in% chosenDrugs,] %>%
      group_by(OldIdStr) %>%
      summarise(Title = first(Title),
                Author = first(Author),
                Journal = first(Journal),
                Abstract = first(Abstract),
                Year = first(Year),
                idStr = first(idStr),
                HistoricalID = first(HistoricalID),
                PublicationID = first(PublicationID),
                Disease = paste0(Disease, collapse = "; "),
                Drug = paste0(Drug, collapse = "; "),
                MSType = paste0(MSType, collapse = "; "),
                Link = first(Link)
      )
    return(chosenStudies)
  })
  
  output$DownloadSelectedPublications <-
    downloadHandler(
      filename = "SelectedPublications.csv",
      content = function(file){
        write.csv({
          selectedPublicationTable()
        }
        , file, na = "", row.names = F
        )
      }
    )
  
  # filiteredPublicationTableinvivo <- reactive({
  #   invivoStudyList <- ExtractinvivoStudies()
  #   chosenDrugs <- filteredDrugs()
  #   choseninvivoStudies <- invivoStudyList[invivoStudyList$Drug %in% chosenDrugs,] %>%
  #     group_by(idStr) %>%
  #     summarise(Title = first(Title),
  #               Author = first(Author),
  #               Journal = first(Journal),
  #               Abstract = first(Abstract),
  #               Year = first(Year),
  #               Disease = paste0(Disease, collapse = "; "),
  #               Drug = paste0(Drug, collapse = "; ")
  #     )
  #   
  #   return(choseninvivoStudies)
  # })
  # 
  # output$DownloadFilteredPublicationsinvivo <-  downloadHandler(
  #   filename = "invivoFilteredPublications.csv", content = function(file){
  #     write.csv({
  #       filiteredPublicationTableinvivo()
  #     }
  #     , file, na = "", row.names = F
  #     )
  #   })
  # 
  # selectedPublicationTableinvivo <- reactive({
  #   invivoStudyList <- ExtractinvivoStudies()
  #   myOutputCrossTable <- frequencyCrossTable()
  #   
  #   chosenDrugs <- rownames(myOutputCrossTable)[input$frequencyCrossTable_rows_selected] 
  #   choseninvivoStudies <- invivoStudyList[invivoStudyList$Drug %in% chosenDrugs,] %>%
  #     group_by(idStr) %>%
  #     summarise(Title = first(Title),
  #               Author = first(Author),
  #               Journal = first(Journal),
  #               Abstract = first(Abstract),
  #               Year = first(Year),
  #               Disease = paste0(Disease, collapse = "; "),
  #               Drug = paste0(Drug, collapse = "; ")
  #     )
  #   
  #   return(choseninvivoStudies)
  # })
  # 
  # output$DownloadSelectedPublicationsinvivo <-
  #   downloadHandler(
  #     filename = "invivoSelectedPublications.csv",
  #     content = function(file){
  #       write.csv({
  #         selectedPublicationTableinvivo()
  #       }
  #       , file, na = "", row.names = F
  #       )
  #     }
  #   )
})
