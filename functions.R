createStatistics <- function(progressSummary, irow){
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


#type: clinicalStatistics/invivoStatistics
createPrisma <- function(typeStatistics){
  nUniquePublications <- paste0("ReLiSyR-AD Living Search:\\n", paste( typeStatistics$nUniquePubs, "publications")) 
  nIncludedPublications <-  paste("ML-assisted citation screening using Title/Abstract:\\n", paste( typeStatistics$nIncludedPubs, "publications included")) 
  nPublicationsMeetLogic <- paste("Filtering by ReLiSyR-AD drug/disease logic:<br></br><i>(≥1 clinical publication in AD OR<br></br>clinical publications in ≥2 other diseases of interest)<br></br></i>", typeStatistics$nDrugMeetLogic, "drugs;", typeStatistics$nPublicationsMeetLogic, "publications") 
  nCoreDrugPublications <-  paste("Longlisting of drugs by AD trial investigators:\\n", typeStatistics$nCoreDrugs, "drugs;", typeStatistics$nCoreDrugsPubs, "publications") 
  
  annotationProgress <- paste0(
    "Annotation Progress\\nSingle Annotated: ", typeStatistics$nSingleAnnotated, " publications (", typeStatistics$percentSingleAnnotated, "%)\\nDual Annotated: ", typeStatistics$nDualAnnotated, " publications (", typeStatistics$percentDualAnnotated, "%)\\nReconciled: ", typeStatistics$nReconciled, " publications (", typeStatistics$percentReconciled, "%)") 
  
  prisma<-glue("
                 digraph {{
                 
                 graph [overlap = true, fontsize = 8, splines=ortho, nodesep=1]
                 
                 node [shape = box,
                 fontname = Helvetica]
                 A [label = '@@1'];
                 B [label = '@@2'];
                 C [label = '@@3'];
                 D [label = <@@4>];
                 E [label = '@@5'];
                 F [label = '@@6'];

                 
                 node [shape = point, width = 0, height = 0]

                 
                 A->B
                 B->C
                 C->D
                 D->E
                 E->F

                 
                 }}
                 
                 [1]: '{nUniquePublications}'
                 [2]: '{nIncludedPublications}'
                 [3]: 'Drug and disease annotation using RegEx'
                 [4]: '{nPublicationsMeetLogic}'
                 [5]: '{nCoreDrugPublications}'
                 [6]: '{annotationProgress}'

                 
                 "
  )

  return(prisma)
}

